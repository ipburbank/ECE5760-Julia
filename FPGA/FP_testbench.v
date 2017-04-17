`timescale 1ns/1ns

module testbench_fp();

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

   wire [36:0] data_before, data_after;
   assign data_before = 37'b101000;

   wire [7:0]  shift_amt;
   assign shift_amt = 8'd0;

   Mult_Right_Shift rs(
                       .data(data_before),
                       .shift_amt(shift_amt),
                       .shifted(data_after)
);
endmodule
