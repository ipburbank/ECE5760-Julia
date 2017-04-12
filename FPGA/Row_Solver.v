/*
 */

module Row_Solver (
                   input wire        solver_clk,
                   input wire        reset,
                   output reg        start_request,
                   input wire        start_grant,
                   input wire [26:0] row_x_reference,
                   input wire [26:0] row_x_step,
                   input wire [26:0] row_y,
                   output wire [9:0] output_value,
                   output wire [9:0] output_column,
                   output wire       output_stb
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

   //=======================================================
   //  State Machines
   //=======================================================

   reg [2:0]                  state;
   reg [9:0]                  column_idx;
   assign output_column = column_idx;

   parameter state_reset=0, state_compute=1;
   always @(posedge solver_clk) begin
      if(reset || state == state_reset) begin
         state <= state_reset;
         start_request <= 1;

         if (start_grant) begin
            state <= state_compute;
            start_request <= 0;

            // start the simulation of the first element of the row
            solver_C_A_reference <= row_x_reference;
            solver_C_A_step      <= row_x_step;
            column_idx           <= 0;
            solver_C_B           <= row_y;
            solver_start         <= 1;
         end
      end
      else if (state == state_compute) begin
         if (solver_start) solver_start <= 0; // only needs to be hi once

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

   Mandelbrot_Pipe Solver(
                        .clk            (solver_clk),
                        .reset          (reset),
                        .start          (solver_start),
                        .C_A_reference  (solver_C_A_reference),
                        .C_A_step       (solver_C_A_step),
                        .C_A_column     (column_idx),
                        .C_B            (row_y),
                        .done           (solver_done),
                        .num_iterations (output_value)
                        );
endmodule
