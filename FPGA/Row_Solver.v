/*
 */

module Row_Solver (
                   input wire         solver_clk,
                   input wire         reset,
                   output reg         start_request,
                   input wire         start_grant,
                   input wire [26:0]  row_x_reference,
                   input wire [26:0]  row_x_step,
                   input wire [26:0]  row_y,
                   input wire [8:0]   row_y_idx,
                   input wire [9:0]   max_iterations,
                   input wire [120:0] vliw_instruction_broadcast,
                   input wire [7:0]   instruction_number,
                   output reg [9:0]   output_value,
                   output wire [9:0]  output_column_idx,
                   output reg [8:0]   output_row_idx,
                   output reg         output_stb
                   );

   //=======================================================
   //  PARAMETER declarations
   //=======================================================


   //=======================================================
   //  PORT declarations
   //=======================================================


   //=======================================================
   //  REG/WIRE declarations
   //=======================================================

   reg                         solver_reset;
   reg                         solver_start;
   reg [26:0]                  solver_C_A_reference, solver_C_A_step,
                               solver_C_B;
   wire                        solver_done;
   //assign output_stb = solver_done;

   reg [9:0]                   column_idx;
   assign output_column_idx = column_idx;
   wire [26:0]                 col_idx_fp;
   Int2Fp col_idx_2_fp(column_idx, col_idx_fp);

   reg [120:0]                 vliw_instruction;

   wire [120:0]                instruction_nop = {
                               // ld_en,  val,    dest,
                               1'd0,      27'd0,  10'd0,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };

   wire [120:0]                instruction_end = {
                               // ld_en,  val,        dest,
                               1'd0,      solver_C_B, 10'd0,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd1,      10'd0,  10'd1,  10'd2
                               };


   reg [120:0]                 load_instructions [0:4];
   always @(*) begin
      load_instructions[0] <= { // load column_idx --> reg3
                               // ld_en,  val,        dest,
                               1'd1,      col_idx_fp, 10'd3,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[1] <= { // load step --> reg4
                               // ld_en,  val,             dest,
                               1'd1,      solver_C_A_step, 10'd4,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[2] <= { // load reference --> reg5, mult step*col_idx->800
                               // ld_en,  val,                  dest,
                               1'd1,      solver_C_A_reference, 10'd5,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd1,      10'd3,  10'd4,  10'd800
                               };
      load_instructions[3] <= { // put B into reg1
                               // ld_en,  val,        dest,
                               1'd1,      solver_C_B, 10'd1,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[4] <= { // add reference + offset -> reg0. nops after
                               // ld_en,  val,    dest,
                               1'd0,      27'd0,  10'd0,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,    dest,
                               1'd1,      10'd5,  10'd800, 10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
   end

   //=======================================================
   //  State Machines
   //=======================================================

   reg [4:0]                  state;
   reg [2:0]                  load_instruction_half_step;

   parameter state_reset=0,
     state_load0=1, state_load1=2, state_load2=3,
     state_load3=4, state_load4=5, state_load5=6,
     state_wait_for_start=7, state_compute=8;
   always @(posedge solver_clk) begin
      output_stb <= 0;
      solver_reset <= 0;

      if(reset || state == state_reset) begin
         state <= state_reset;
         start_request <= 1;
         vliw_instruction <= instruction_nop;
         output_value <= 0;
         solver_reset <= 1;

         load_instruction_half_step <= 0;

         if (start_grant) begin
            state <= state_load0;
            start_request <= 0;

            output_row_idx <= row_y_idx;

            // start the simulation of the first element of the row
            solver_C_A_reference <= row_x_reference;
            solver_C_A_step      <= row_x_step;
            column_idx           <= 0;
            solver_C_B           <= row_y;
            solver_start         <= 1;
         end
      end
      // load the coords into the regs
      else if (state == state_load0) begin
         load_instruction_half_step <= load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_load1;
            load_instruction_half_step <= 0;
         end
         solver_start <= 0;

         vliw_instruction <= load_instructions[0];
      end
      else if (state == state_load1) begin
         load_instruction_half_step <= load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_load2;
            load_instruction_half_step <= 0;
         end

         vliw_instruction <= load_instructions[1];
      end
      else if (state == state_load2) begin
         load_instruction_half_step <= load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_load3;
            load_instruction_half_step <= 0;
         end

         vliw_instruction <= load_instructions[2];
      end
      else if (state == state_load3) begin
         load_instruction_half_step <= load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_load4;
            load_instruction_half_step <= 0;
         end

         vliw_instruction <= load_instructions[3];
      end
      else if (state == state_load4) begin
         load_instruction_half_step = load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_load5;
            load_instruction_half_step <= 0;
         end

         vliw_instruction <= load_instructions[4];
      end
      else if (state == state_load5) begin
         load_instruction_half_step <= load_instruction_half_step + 1;
         if (load_instruction_half_step == 3) begin
            state <= state_wait_for_start;
            load_instruction_half_step <= 0;
         end

         vliw_instruction <= instruction_nop;
      end
      else if (state == state_wait_for_start) begin
         vliw_instruction <= instruction_nop;
         if (instruction_number == 0) begin
            state <= state_compute;
            output_value <= 0;
         end
      end
      else if (state == state_compute) begin
         vliw_instruction <= instruction_end; //vliw_instruction_broadcast;
         if (instruction_number == 0) output_value <= output_value + 1;
         if (solver_done || output_value > max_iterations) begin
            output_stb <= 1;
            // receive results

            // start on new results
            if (column_idx == 639) begin // just finished last column
               state <= state_reset;
            end
            else begin
               state <= state_load0;

               column_idx <= column_idx + 1;
               solver_start <= 1;
            end
         end
      end
   end

   //=======================================================
   //  Structural coding
   //=======================================================

   VLIW vliw(
             .clk            (solver_clk),
             .reset          (solver_reset),
             .start          (solver_start),
             .done           (solver_done),

             .load_enable_input (vliw_instruction[120]),
             .load_value        (vliw_instruction[119:93]),
             .load_dest_addr    (vliw_instruction[92:83]),

             .neg_enable_input  (vliw_instruction[82]),
             .neg_src_addr      (vliw_instruction[81:72]),
             .neg_dest_addr     (vliw_instruction[71:62]),

             .add_enable_input  (vliw_instruction[61]),
             .add_src1_addr     (vliw_instruction[60:51]),
             .add_src2_addr     (vliw_instruction[50:41]),
             .add_dest_addr     (vliw_instruction[40:31]),

             .mul_enable_input  (vliw_instruction[30]),
             .mul_src1_addr     (vliw_instruction[29:20]),
             .mul_src2_addr     (vliw_instruction[19:10]),
             .mul_dest_addr     (vliw_instruction[9:0])
             );
endmodule
