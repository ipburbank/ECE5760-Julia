/*
 */

module Frame_Solver (
                     input wire        solver_clk,
                     input wire        reset,
                     input wire [26:0] x_0,
                     input wire [26:0] x_step,
                     input wire [26:0] y_0,
                     input wire [26:0] y_step,
                     input wire        output_clk,
                     output reg [9:0]  output_pixel_x,
                     output reg [8:0]  output_pixel_y,
                     output reg [7:0]  output_pixel_color,
                     output reg        output_pixel_stb
                     );

   //=======================================================
   //  PARAMETER declarations
   //=======================================================

   localparam NUM_ROWS_SOLVERS = 10;

   //=======================================================
   //  PORT declarations
   //=======================================================


   //=======================================================
   //  REG/WIRE declarations
   //=======================================================

   //=======================================================
   //  State Machines
   //=======================================================

   wire [NUM_ROWS_SOLVERS-1:0]                     row_solver_start_request,
                                                   row_solver_start_grant,
                                                   row_solver_start_grant_no_reset;
   assign row_solver_start_grant = reset ? 0 : row_solver_start_grant_no_reset;

   Reqs_To_One_Hot #(NUM_ROWS_SOLVERS) start_arb(
                                                 .reqs(row_solver_start_request),
                                                 .grants(row_solver_start_grant_no_reset)
                                                 );

   wire [NUM_ROWS_SOLVERS-1:0]                     row_solver_return_request,
                                                   row_solver_return_grant;
   wire [26:0]                                     row_solver_return_value[NUM_ROWS_SOLVERS-1:0];

   Reqs_To_One_Hot #(NUM_ROWS_SOLVERS) return_arb(
                                                  .reqs(row_solver_return_request),
                                                  .grants(row_solver_return_grant)
                                                  );

   always @(posedge output_clk) begin
      output_pixel_stb <= 0; // default
   end

   genvar                                          output_select_i;
   generate
      for(output_select_i=0; output_select_i < NUM_ROWS_SOLVERS; output_select_i = output_select_i + 1) begin : output_assigner
         always @(posedge output_clk) begin
            if (row_solver_return_grant[output_select_i]) begin
               output_pixel_x     <= row_solver_return_value[output_select_i][26:17];
               output_pixel_y     <= row_solver_return_value[output_select_i][16:8];
               output_pixel_color <= row_solver_return_value[output_select_i][7:0];
               output_pixel_stb   <= 1;
            end
         end
      end
   endgenerate

   //------------- Y VALUE GEN -------------
   reg [9:0]                           row_next_y_idx;
   reg [26:0]                          row_next_y_value;
   wire [26:0]                         row_next_y_value_adder_out;
   always @(posedge solver_clk) begin
      if (reset) begin
         row_next_y_idx <= 0;
         row_next_y_value <= y_0;
      end
      else if (row_solver_start_grant > 0) begin
         row_next_y_idx <= row_next_y_idx + 1;
         row_next_y_value <= row_next_y_value_adder_out;
      end
   end

   wire [26:0] row_next_y_value_adder_in;
   assign row_next_y_value_adder_in = (reset) ? 0 :
                                      row_next_y_value_adder_out;
   FpAdd FpAdder(solver_clk, row_next_y_value_adder_in, y_step, row_next_y_value_adder_out);

   genvar solver_i;
   generate
      for(solver_i=0; solver_i < NUM_ROWS_SOLVERS; solver_i=solver_i+1) begin : row_solvers
         wire [9:0] solver_output_value;
         wire       solver_output_stb;

         wire       solver_start_request, fifo_full;
         assign row_solver_start_request[solver_i] = solver_start_request && 1'b1; //TODO ~fifo_full;
         wire [9:0] solver_output_column;

         Row_Solver solver(
                           .solver_clk      (solver_clk),
                           .reset           (reset),
                           .start_request   (solver_start_request),
                           .start_grant     (row_solver_start_grant[solver_i]),
                           .row_x_reference (x_0),
                           .row_x_step      (x_step),
                           .row_y           (row_next_y_value),
                           .output_value    (solver_output_value),
                           .output_column   (solver_output_column),
                           .output_stb      (solver_output_stb)
                           );

         Row_Output_FIFO Row_Output_FIFO_inst (
                                               .aclr    (reset),
                                               .rdclk   (output_clk),
                                               .rdreq   (row_solver_return_grant[solver_i]),
                                               .q       (row_solver_return_value[solver_i]),
                                               .rdempty (row_solver_return_request[solver_i]),
                                               .wrclk   (solver_clk),
                                               .data    ({10'b0, 9'b0,
                                                          solver_output_value[9:2]}),
                                               .wrreq   (solver_output_stb),
                                               .wrfull  (fifo_full)
                                               );

      end
   endgenerate

   //=======================================================
   //  Structural coding
   //=======================================================

endmodule
