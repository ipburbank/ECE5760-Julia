/*
 * VLIW processor
 *
 * NOTES:
 * - behavior or saving to z from two pipes in the same cycle is undefined
 */

module VLIW (
             input wire        clk,
             input wire        reset,
             input wire        start,
             input wire [9:0]  max_iterations,
             output reg        done,
             output wire [9:0] num_iterations,
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

   // ENABLE LOGIC
   wire                        load_enabled = load_enable_input;
   wire                        neg_enabled  = neg_enable_input;
   wire                        add_write_enable, mul_write_enable;

   // Z REG
   reg [26:0]                  z_reg; // value at address 0

   // SRC/DEST translation
   wire [7:0]                  load_dest_translated = load_dest_addr % 256;

   wire [7:0]                  neg_src_translated   = neg_src_addr  % 256;
   wire [7:0]                  neg_dest_translated  = neg_dest_addr % 256;
   wire [26:0]                 ld_out_neg_src, neg_out_neg_src, add_out_neg_src, mul_out_neg_src;
   wire [26:0]                 neg_src = (neg_src_translated == 0) ? z_reg      :
                                         (neg_src_addr < 256) ? ld_out_neg_src  :
                                         (neg_src_addr < 512) ? neg_out_neg_src :
                                         (neg_src_addr < 768) ? add_out_neg_src :
                                                                mul_out_neg_src ;
   wire [26:0]                 neg_value_output;

   wire [7:0]                  add_src1_translated  = add_src1_addr % 256;
   wire [7:0]                  add_src2_translated  = add_src2_addr % 256;
   wire [7:0]                  add_dest_translated  = add_dest_addr % 256;
   wire [26:0]                 ld_out_add_src1, neg_out_add_src1, add_out_add_src1, mul_out_add_src1;
   wire [26:0]                 add_src1 = (add_src1_translated == 0) ? z_reg       :
                                          (add_src1_addr < 256) ? ld_out_add_src1  :
                                          (add_src1_addr < 512) ? neg_out_add_src1 :
                                          (add_src1_addr < 768) ? add_out_add_src1 :
                                                                  mul_out_add_src1 ;
   wire [26:0]                 ld_out_add_src2, neg_out_add_src2, add_out_add_src2, mul_out_add_src2;
   wire [26:0]                 add_src2 = (add_src2_translated == 0) ? z_reg       :
                                          (add_src2_addr < 256) ?  ld_out_add_src2 :
                                          (add_src2_addr < 512) ? neg_out_add_src2 :
                                          (add_src2_addr < 768) ? add_out_add_src2 :
                                                                  mul_out_add_src2 ;


   wire [7:0]                  mul_src1_translated  = mul_src1_addr % 256;
   wire [7:0]                  mul_src2_translated  = mul_src2_addr % 256;
   wire [7:0]                  mul_dest_translated  = mul_dest_addr % 256;
   wire [26:0]                 ld_out_mul_src1, neg_out_mul_src1, add_out_mul_src1, mul_out_mul_src1;
   wire [26:0]                 mul_src1 = (mul_src1_translated == 0) ? z_reg       :
                                          (mul_src1_addr < 256) ? ld_out_mul_src1  :
                                          (mul_src1_addr < 512) ? neg_out_mul_src1 :
                                          (mul_src1_addr < 768) ? add_out_mul_src1 :
                                                                  mul_out_mul_src1 ;
   wire [26:0]                 ld_out_mul_src2, neg_out_mul_src2, add_out_mul_src2, mul_out_mul_src2;
   wire [26:0]                 mul_src2 = (mul_src2_translated == 0) ? z_reg       :
                                          (mul_src2_addr < 256) ? ld_out_mul_src2  :
                                          (mul_src2_addr < 512) ? neg_out_mul_src2 :
                                          (mul_src2_addr < 768) ? add_out_mul_src2 :
                                                                  mul_out_mul_src2 ;

   //=======================================================
   //  State Machines
   //=======================================================

   always @(posedge clk) begin
      if (reset) z_reg <= 0;
      else if (load_enabled && load_dest_translated == 0) z_reg <= load_value;
      else if (neg_enabled  && neg_dest_translated == 0 ) z_reg <= neg_value_output;
   end

   //=======================================================
   //  Structural coding
   //=======================================================

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
   assign neg_value_output = -neg_src; //TODO
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

   // // ADD OUTPUT
   // Register_File add_out_neg_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (add_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File add_out_add1_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (add_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File add_out_add2_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (add_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File add_out_mul1_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (add_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File add_out_mul2_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (add_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );

   // // MUL OUT
   // Register_File mul_out_neg_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (mul_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File mul_out_add1_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (mul_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File mul_out_add2_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (mul_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File mul_out_mul1_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (mul_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );
   // Register_File mul_out_mul2_in (
   //                               .address_a (),
   //                               .address_b (),
   //                               .clock     (),
   //                               .data_a    (),
   //                               .data_b    (),
   //                               .wren_a    (mul_write_enable),
   //                               .wren_b    (1'b0),
   //                               .q_a       (),
   //                               .q_b       ()
   //                               );

endmodule
