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

   wire [26:0] C_A, C_B, C_A_int, C_B_int;
   Int2Fp ConvertFP_C_A(-16'sd3, C_A_int);
   Int2Fp ConvertFP_C_B(16'sd1, C_B_int);
   FpShift SHA(C_A_int, -8'sd2, C_A);
   FpShift SHB(C_B_int, -8'sd1, C_B);

   wire        done;
   wire [10:0] num_iterations;

   Mandelbrot_Pipe Pipe(
                        .clk            (clk_50),
                        .reset          (reset),
                        .start          (start),
                        .C_A            (C_A),
                        .C_B            (C_B),
                        .done           (done),
                        .num_iterations (num_iterations)
                        );

endmodule
