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
                   input wire [121:0] vliw_instruction_broadcast,
                   output wire [9:0]  output_value,
                   output wire [9:0]  output_column_idx,
                   output reg [8:0]   output_row_idx,
                   output wire        output_stb
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

   reg                         solver_start;
   reg [26:0]                  solver_C_A_reference, solver_C_A_step,
                               solver_C_B;
   wire                        solver_done;
   assign output_stb = solver_done;

   reg [9:0]                   column_idx;
   assign output_column_idx = column_idx;
   wire [26:0]                 column_idx_fp;
   Int2Fp col_idx_fp(column_idx, col_idx_fp);

   wire [120:0]                vliw_instruction;

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

   reg [120:0]                 load_instructions [0:1];
   always @(*) begin
      load_instructions[0] <= { // load column_idx --> reg2
                               // ld_en,  val,        dest,
                               1'd1,      col_idx_fp, 10'd2,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[1] <= { // load step --> reg3
                               // ld_en,  val,             dest,
                               1'd1,      solver_C_A_step, 10'd3,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[2] <= { // load reference --> reg4, mult step*col_idx
                               // ld_en,  val,                  dest,
                               1'd1,      solver_C_A_reference, 10'd4,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd1,      10'd2,  10'd3,  10'd5
                               };
      load_instructions[3] <= { // add reference + offset -> reg1
                               // ld_en,  val,    dest,
                               1'd0,      27'd0,  10'd0,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd1,      10'd4,  10'd5,  10'd1,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
      load_instructions[4] <= { // while add operates, put B into reg0
                               // ld_en,  val,        dest,
                               1'd1,      solver_C_B, 10'd0,
                               // neg_en, src,    dest,
                               1'd0,      10'd0,  10'd0,
                               // add_en, src1,   src2,   dest,
                               1'd0,      10'd0,  10'd0,  10'd0,
                               // mul_en, src1,   src2,   dest
                               1'd0,      10'd0,  10'd0,  10'd0
                               };
   end

   //=======================================================
   //  State Machines
   //=======================================================

   reg [2:0]                  state;

   parameter state_reset=0,
     state_load0, state_load1, state_load2, state_load3, state_load4,
     state_compute=1;
   always @(posedge solver_clk) begin
      if(reset || state == state_reset) begin
         state <= state_reset;
         start_request <= 1;
         vliw_instruction <= instruction_nop;

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
         state <= state_load1;
         solver_start <= 0;

         vliw_instruction <= load_instructions[0];
      end
      else if (state == state_load1) begin
         state <= state_load2;
         vliw_instruction <= load_instructions[1];
      end
      else if (state == state_load2) begin
         state <= state_load3;
         vliw_instruction <= load_instructions[2];
      end
      else if (state == state_load3) begin
         state <= state_load4;
         vliw_instruction <= load_instructions[3];
      end
      else if (state == state_load4) begin
         state <= state_compute;
         vliw_instruction <= load_instructions[4];
      end
      else if (state == state_compute) begin
         vliw_instruction <= vliw_instruction_broadcast;
         if (solver_done) begin
            // receive results

            // start on new results
            if (column_idx == 639) begin // just finished last column
               state <= state_reset;
            end
            else begin
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
             .reset          (reset),
             .start          (solver_start),
             .max_iterations (max_iterations),
             .done           (solver_done),
             .num_iterations (output_value),

             .load_enable_input (vliw_instruction[0]),
             .load_value        (vliw_instruction[27:1]),
             .load_dest_addr    (vliw_instruction[37:28]),

             .neg_enable_input  (vliw_instruction[38]),
             .neg_src_addr      (vliw_instruction[48:39]),
             .neg_dest_addr     (vliw_instruction[58:49]),

             .add_enable_input  (vliw_instruction[59]),
             .add_src1_addr     (vliw_instruction[69:60]),
             .add_src2_addr     (vliw_instruction[79:70]),
             .add_dest_addr     (vliw_instruction[89:80]),

             .mul_enable_input  (vliw_instruction[90]),
             .mul_src1_addr     (vliw_instruction[100:91]),
             .mul_src2_addr     (vliw_instruction[110:101]),
             .mul_dest_addr     (vliw_instruction[120:111])
             );
endmodule
