`timescale 1ns/1ns

module testbench();

   reg clk_50, clk_25, reset;

   reg [31:0] index;

   reg        start;

   //Initialize clocks and index
   initial begin
      clk_50 = 1'b0;
      clk_25 = 1'b0;
      index  = 32'd0;
      //testbench_out = 15'd0 ;
   end

   //Toggle the clocks
   always begin
      #10
        clk_50  = !clk_50;
   end

   always begin
      #20
        clk_25  = !clk_25;
   end

   //Intialize and drive signals
   initial begin
      reset  = 1'b0;
      start  = 1'b0;
      #10 reset    = 1'b1;
      #30 reset    = 1'b0;
      #40 start    = 1'b1;
      #41 start    = 1'b0;
   end


   //=======================================================
   // Pipeline
   //=======================================================

   wire        done;
   wire [9:0] num_iterations;

   VLIW vliw(
             .clk            (clk_50),
             .reset          (reset),
             .start          (start),
             .done           (done),
             .num_iterations (num_iterations),
             .max_iterations (10'd1000),

             .load_enable_input(1'b1),
             .load_value (27'd123),
             .load_dest_addr(10'd1),

             .neg_enable_input(1'b1),
             .neg_src_addr(10'd1),
             .neg_dest_addr(10'd2),

             .add_enable_input(),
             .add_src1_addr(),
             .add_src2_addr(),
             .add_dest_addr(),

             .mul_enable_input(),
             .mul_src1_addr(),
             .mul_src2_addr(),
             .mul_dest_addr()
             );

endmodule
