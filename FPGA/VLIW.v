/*
 * VLIW processor
 *
 * NOTES:
 * - Re{z} in reg 0, Im{z} in reg 1
 * - behavior or saving to z from two pipes in the same cycle is undefined
 */

module VLIW (
             input wire        clk,
             input wire        reset,
             input wire        start,
             output reg        done,

             // instructions
             input wire        load_enable_input,
             input wire [26:0] load_value,
             input wire [9:0]  load_dest_addr,

             input wire        neg_enable_input,
             input wire [9:0]  neg_src_addr,
             input wire [9:0]  neg_dest_addr,

             input wire        add_enable_input,
             input wire [9:0]  add_src1_addr,
             input wire [9:0]  add_src2_addr,
             input wire [9:0]  add_dest_addr,

             input wire        mul_enable_input,
             input wire [9:0]  mul_src1_addr,
             input wire [9:0]  mul_src2_addr,
             input wire [9:0]  mul_dest_addr
             );

   //=======================================================
   //  PARAMETER declarations
   //=======================================================


   //=======================================================
   //  REG/WIRE declarations
   //=======================================================

   wire [26:0]                 max_magnitude;
   wire                        magnitude_geq_max; // set by comparator

   // ENABLE LOGIC
   wire                        load_enabled = load_enable_input;
   wire                        neg_enabled  = neg_enable_input;
   wire                        add_enabled_cycle1 = add_enable_input;
   reg                         add_enabled_cycle2;
   wire                        mul_enabled  = mul_enable_input;

   // Z REG
   reg [26:0]                  z_regRe; // value at address 0
   reg [26:0]                  z_regIm; // value at address 1

   // Magnitude of current iteration. When written with a sufficiently large
   // value the processor will return done.
   reg [26:0]                  magnitude_reg;

   // SRC/DEST translation
   wire [7:0]                  load_dest_translated = load_dest_addr % 256;

   wire [7:0]                  neg_src_translated   = neg_src_addr  % 256;
   wire [7:0]                  neg_dest_translated  = neg_dest_addr % 256;
   wire [26:0]                 ld_out_neg_src, neg_out_neg_src, add_out_neg_src, mul_out_neg_src;
   wire [26:0]                 neg_src = (neg_src_translated == 0) ? z_regRe    :
                                         (neg_src_translated == 1) ? z_regIm    :
                                         (neg_src_translated == 2) ? magnitude_reg :
                                         (neg_src_addr < 256) ? ld_out_neg_src  :
                                         (neg_src_addr < 512) ? neg_out_neg_src :
                                         (neg_src_addr < 768) ? add_out_neg_src :
                                                                mul_out_neg_src ;
   wire [26:0]                 neg_value_output;

   reg [9:0]                   add_dest_addr_cycle2;
   wire [7:0]                  add_src1_translated  = add_src1_addr % 256;
   wire [7:0]                  add_src2_translated  = add_src2_addr % 256;
   wire [7:0]                  add_dest_translated  = add_dest_addr_cycle2 % 256;
   wire [26:0]                 ld_out_add_src1, neg_out_add_src1, add_out_add_src1, mul_out_add_src1;
   wire [26:0]                 add_src1 = (add_src1_translated == 0) ? z_regRe     :
                                          (add_src1_translated == 1) ? z_regIm     :
                                          (add_src1_translated == 2) ? magnitude_reg :
                                          (add_src1_addr < 256) ? ld_out_add_src1  :
                                          (add_src1_addr < 512) ? neg_out_add_src1 :
                                          (add_src1_addr < 768) ? add_out_add_src1 :
                                                                  mul_out_add_src1 ;
   wire [26:0]                 ld_out_add_src2, neg_out_add_src2, add_out_add_src2, mul_out_add_src2;
   wire [26:0]                 add_src2 = (add_src2_translated == 0) ? z_regRe     :
                                          (add_src2_translated == 1) ? z_regIm     :
                                          (add_src2_translated == 2) ? magnitude_reg :
                                          (add_src2_addr < 256) ?  ld_out_add_src2 :
                                          (add_src2_addr < 512) ? neg_out_add_src2 :
                                          (add_src2_addr < 768) ? add_out_add_src2 :
                                                                  mul_out_add_src2 ;
   wire [26:0]                 add_value_output;

   wire [7:0]                  mul_src1_translated  = mul_src1_addr % 256;
   wire [7:0]                  mul_src2_translated  = mul_src2_addr % 256;
   wire [7:0]                  mul_dest_translated  = mul_dest_addr % 256;
   wire [26:0]                 ld_out_mul_src1, neg_out_mul_src1, add_out_mul_src1, mul_out_mul_src1;
   wire [26:0]                 mul_src1 = (mul_src1_translated == 0) ? z_regRe     :
                                          (mul_src1_translated == 1) ? z_regIm     :
                                          (mul_src1_translated == 2) ? magnitude_reg :
                                          (mul_src1_addr < 256) ? ld_out_mul_src1  :
                                          (mul_src1_addr < 512) ? neg_out_mul_src1 :
                                          (mul_src1_addr < 768) ? add_out_mul_src1 :
                                                                  mul_out_mul_src1 ;
   wire [26:0]                 ld_out_mul_src2, neg_out_mul_src2, add_out_mul_src2, mul_out_mul_src2;
   wire [26:0]                 mul_src2 = (mul_src2_translated == 0) ? z_regRe     :
                                          (mul_src2_translated == 1) ? z_regIm     :
                                          (mul_src2_translated == 2) ? magnitude_reg :
                                          (mul_src2_addr < 256) ? ld_out_mul_src2  :
                                          (mul_src2_addr < 512) ? neg_out_mul_src2 :
                                          (mul_src2_addr < 768) ? add_out_mul_src2 :
                                                                  mul_out_mul_src2 ;
   wire [26:0]                 mul_value_output;

   //=======================================================
   //  State Machines
   //=======================================================

   always @(posedge clk) begin
      if (reset | done) begin
         z_regRe <= 0;
         z_regIm <= 0;
         magnitude_reg <= 0;
      end
      else if (load_enabled       && load_dest_translated == 0) z_regRe <= load_value;
      else if (load_enabled       && load_dest_translated == 1) z_regIm <= load_value;
      else if (load_enabled       && load_dest_translated == 2) magnitude_reg <= load_value;

      else if (neg_enabled        && neg_dest_translated  == 0) z_regRe <= neg_value_output;
      else if (neg_enabled        && neg_dest_translated  == 1) z_regIm <= neg_value_output;
      else if (neg_enabled        && neg_dest_translated  == 2) magnitude_reg <= neg_value_output;

      else if (add_enabled_cycle2 && add_dest_translated  == 0) z_regRe <= add_value_output;
      else if (add_enabled_cycle2 && add_dest_translated  == 1) z_regIm <= add_value_output;
      else if (add_enabled_cycle2 && add_dest_translated  == 2) magnitude_reg <= add_value_output;

      else if (mul_enabled        && mul_dest_translated  == 0) z_regRe <= mul_value_output;
      else if (mul_enabled        && mul_dest_translated  == 1) z_regIm <= mul_value_output;
      else if (mul_enabled        && mul_dest_translated  == 2) magnitude_reg <= mul_value_output;

      // check the magnitudes
      done <= 0; // strobed only
      if (magnitude_geq_max) done <= 1;
   end

   // state machine to run the add pipe
   always @(posedge clk) begin
      if(reset) begin
         add_enabled_cycle2 <= 0;
         add_dest_addr_cycle2 <= 0;
      end
      else begin
         add_enabled_cycle2 <= add_enabled_cycle1;
         add_dest_addr_cycle2 <= add_dest_addr;
      end
   end

   //=======================================================
   //  Structural coding
   //=======================================================

   // Magnitude checks
   wire [26:0] fpOne;
   Int2Fp ConvertFP_MaxMagnitude(16'd1, fpOne);
   FpShift halfer(fpOne, -8'sd1, max_magnitude);
   FpCompare MagnitudeCheck(magnitude_reg, max_magnitude, magnitude_geq_max);

   // REG FILES

   // LD OUTPUT
   Register_File ld_out_neg_in (
                                .address_a (load_dest_translated),
                                .address_b (neg_src_translated),
                                .clock     (clk),
                                .data_a    (load_value),
                                .data_b    (27'b0), // never written
                                .wren_a    (load_enabled),
                                .wren_b    (1'b0),
                                .q_a       (),
                                .q_b       (ld_out_neg_src)
                                );
   Register_File ld_out_add1_in (
                                .address_a (load_dest_translated),
                                .address_b (add_src1_translated),
                                .clock     (clk),
                                .data_a    (load_value),
                                .data_b    (27'b0),
                                .wren_a    (load_enabled),
                                .wren_b    (1'b0),
                                .q_a       (),
                                .q_b       (ld_out_add_src1)
                                );
   Register_File ld_out_add2_in (
                                .address_a (load_dest_translated),
                                .address_b (add_src2_translated),
                                .clock     (clk),
                                .data_a    (load_value),
                                .data_b    (27'b0),
                                .wren_a    (load_enabled),
                                .wren_b    (1'b0),
                                .q_a       (),
                                .q_b       (ld_out_add_src2)
                                );
   Register_File ld_out_mul1_in (
                                .address_a (load_dest_translated),
                                .address_b (mul_src1_translated),
                                .clock     (clk),
                                .data_a    (load_value),
                                .data_b    (27'b0),
                                .wren_a    (load_enabled),
                                .wren_b    (1'b0),
                                .q_a       (),
                                .q_b       (ld_out_mul_src1)
                                );
   Register_File ld_out_mul2_in (
                                .address_a (load_dest_translated),
                                .address_b (mul_src2_translated),
                                .clock     (clk),
                                .data_a    (load_value),
                                .data_b    (27'b0),
                                .wren_a    (load_enabled),
                                .wren_b    (1'b0),
                                .q_a       (),
                                .q_b       (ld_out_mul_src2)
                                );

   // NEG OUTPUT
   FpNegate fpnegate(neg_src, neg_value_output);

   Register_File neg_out_neg_in (
                                 .address_a (neg_dest_translated),
                                 .address_b (neg_src_translated),
                                 .clock     (clk),
                                 .data_a    (neg_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (neg_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (neg_out_neg_src)
                                 );
   Register_File neg_out_add1_in (
                                 .address_a (neg_dest_translated),
                                 .address_b (add_src1_translated),
                                 .clock     (clk),
                                 .data_a    (neg_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (neg_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (neg_out_add_src1)
                                 );
      Register_File neg_out_add2_in (
                                 .address_a (neg_dest_translated),
                                 .address_b (add_src2_translated),
                                 .clock     (clk),
                                 .data_a    (neg_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (neg_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (neg_out_add_src2)
                                 );
   Register_File neg_out_mul1_in (
                                 .address_a (neg_dest_translated),
                                 .address_b (mul_src1_translated),
                                 .clock     (clk),
                                 .data_a    (neg_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (neg_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (neg_out_mul_src1)
                                 );
   Register_File neg_out_mul2_in (
                                 .address_a (neg_dest_translated),
                                 .address_b (mul_src2_translated),
                                 .clock     (clk),
                                 .data_a    (neg_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (neg_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (neg_out_mul_src2)
                                 );

   // ADD OUTPUT
   FpAdd FpAdder(clk, add_src1, add_src2, add_value_output);

   Register_File add_out_neg_in (
                                 .address_a (add_dest_translated),
                                 .address_b (neg_src_translated),
                                 .clock     (clk),
                                 .data_a    (add_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (add_enabled_cycle2),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (add_out_neg_src)
                                 );
   Register_File add_out_add1_in (
                                 .address_a (add_dest_translated),
                                 .address_b (add_src1_translated),
                                 .clock     (clk),
                                 .data_a    (add_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (add_enabled_cycle2),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (add_out_add_src1)
                                 );
   Register_File add_out_add2_in (
                                 .address_a (add_dest_translated),
                                 .address_b (add_src2_translated),
                                 .clock     (clk),
                                 .data_a    (add_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (add_enabled_cycle2),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (add_out_add_src2)
                                 );
   Register_File add_out_mul1_in (
                                 .address_a (add_dest_translated),
                                 .address_b (mul_src1_translated),
                                 .clock     (clk),
                                 .data_a    (add_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (add_enabled_cycle2),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (add_out_mul_src1)
                                 );
   Register_File add_out_mul2_in (
                                 .address_a (add_dest_translated),
                                 .address_b (mul_src2_translated),
                                 .clock     (clk),
                                 .data_a    (add_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (add_enabled_cycle2),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (add_out_mul_src2)
                                 );

   // MUL OUT
   FpMul FpMultiplier(mul_src1, mul_src2, mul_value_output);

   Register_File mul_out_neg_in (
                                 .address_a (mul_dest_translated),
                                 .address_b (neg_src_translated),
                                 .clock     (clk),
                                 .data_a    (mul_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (mul_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (mul_out_neg_src)
                                 );
   Register_File mul_out_add1_in (
                                 .address_a (mul_dest_translated),
                                 .address_b (add_src1_translated),
                                 .clock     (clk),
                                 .data_a    (mul_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (mul_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (mul_out_add_src1)
                                 );
   Register_File mul_out_add2_in (
                                 .address_a (mul_dest_translated),
                                 .address_b (add_src1_translated),
                                 .clock     (clk),
                                 .data_a    (mul_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (mul_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (mul_out_add_src2)
                                 );
   Register_File mul_out_mul1_in (
                                 .address_a (mul_dest_translated),
                                 .address_b (mul_src1_translated),
                                 .clock     (clk),
                                 .data_a    (mul_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (mul_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (mul_out_mul_src1)
                                 );
   Register_File mul_out_mul2_in (
                                 .address_a (mul_dest_translated),
                                 .address_b (mul_src2_translated),
                                 .clock     (clk),
                                 .data_a    (mul_value_output),
                                 .data_b    (27'b0),
                                 .wren_a    (mul_enabled),
                                 .wren_b    (1'b0),
                                 .q_a       (),
                                 .q_b       (mul_out_mul_src2)
                                 );

endmodule
