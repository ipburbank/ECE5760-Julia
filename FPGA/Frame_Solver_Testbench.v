`timescale 1ns/1ns

module testbench_frame();

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

      wire [26:0] C_A_reference, C_A_reference_num, C_A_step, C_A_step_num, C_B, C_B_int;
   wire [9:0]  C_A_column;

   Int2Fp ConvertFP_C_A_ref(-16'sd1, C_A_reference_num);
   FpShift SHA(C_A_reference_num, -8'sd1, C_A_reference);

   Int2Fp ConvertFP_C_A_step(16'sd1, C_A_step_num);
   FpShift SHAstep(C_A_step_num, -8'sd2, C_A_step);

   Int2Fp ConvertFP_C_B(16'sd3, C_B_int);
   FpShift SHB(C_B_int, -8'sd2, C_B);

   wire [9:0]  pixel_x;
   wire [8:0]  pixel_y;
   wire [7:0]  pixel_color;
   wire        pixel_stb;

   Frame_Solver solver(
                       .solver_clk (clk_50),
                       .reset      (reset),
                       .x_0        (C_A_reference),
                       .x_step     (C_A_step),
                       .y_0        (C_B),
                       .y_step     (C_A_step),
                       .output_clk (clk_50),
                       .output_pixel_x     (pixel_x),
                       .output_pixel_y     (pixel_y),
                       .output_pixel_color (pixel_color),
                       .output_pixel_stb   (pixel_stb)
                       );

endmodule
