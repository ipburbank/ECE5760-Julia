/*
 * Compute the Z_{n+1} = Z^2_n + C
 *
 * Pipeline finishes computation of one step and starts the next.
 * A few cycles later it is determined if the value left the bounds.
 *
 */

module Mandelbrot_Pipe (
                        input  wire        clk,
                        input  wire        reset,
                        input  wire        start,
                        input  wire [26:0] C_A,
                        input  wire [26:0] C_B,
                        output wire        done,
                        output wire [10:0] num_iterations
                        );

   // Z_{n+1} = Z_{n}^2 + C

   //=======================================================
   //  PARAMETER declarations
   //=======================================================


   //=======================================================
   //  PORT declarations
   //=======================================================


   //=======================================================
   //  REG/WIRE declarations
   //=======================================================

   reg [10:0]                       num_iterations_plus_one;
   assign num_iterations = num_iterations_plus_one - 1;

   reg  [26:0]                             Z_A_reg, Z_B_reg, C_A_reg, C_B_reg;
   wire [26:0]                             Z_A_squared, Z_B_squared;
   wire [26:0]                             Z_A_squared_plus_c, Z_B_squared_plus_c;

   wire [26:0]                             magnitude, fp_four;
   Int2Fp ConvertFP_Four(16'sd4, fp_four);
   FpCompare CheckIfDone(magnitude, fp_four, done);

   //=======================================================
   //  State Machines
   //=======================================================

   reg [3:0]                               state;
   always @(posedge clk) begin
      if(reset || done) begin
         state                   <= 0;
         Z_A_reg                 <= 0;
         Z_B_reg                 <= 0;
         C_B_reg                 <= 0;
         C_A_reg                 <= 0;
         num_iterations_plus_one <= 0;
      end
      else if(state == 0) begin // wait for start
         if(start) begin
            state   <= 1;
            C_A_reg <= C_A;
            C_B_reg <= C_B;
         end
      end
      else if(state == 1) begin // Zsquared Cycle 1
         state <= 2;
         num_iterations_plus_one <= num_iterations_plus_one + 10'd1;
      end
      else if(state == 2) begin // Zsquared Cycle 2
         state <= 3;
      end
      else if(state == 3) begin // Zsquared+C Cycle 1
         state <= 4;
      end
      else if(state == 4) begin // Zsquared+C Cycle 2
         state <= 5;
      end
      else if(state == 5) begin // Finalize
         // we don't yet know if the magnitude is out of bounds,
         // but we will discover that later
         state   <= 1;

         // save results
         Z_A_reg <= Z_A_squared_plus_c;
         Z_B_reg <= Z_B_squared_plus_c;
      end
   end

   //=======================================================
   //  Structural coding
   //=======================================================

   // compute Z^2
   FP_Complex_Mul ZSq(.clk  (clk),
                      .A1   (Z_A_reg),
                      .B1   (Z_B_reg),
                      .A2   (Z_A_reg),
                      .B2   (Z_B_reg),
                      .AOut (Z_A_squared),
                      .BOut (Z_B_squared)
                      );

   // compute Z^2 + C
   FP_Complex_Add ZSqPlusC(.clk  (clk),
                           .A1   (Z_A_squared),
                           .B1   (Z_B_squared),
                           .A2   (C_A_reg),
                           .B2   (C_B_reg),
                           .AOut (Z_A_squared_plus_c),
                           .BOut (Z_B_squared_plus_c)
                           );

   FP_Complex_Mag_Sq MagSq(.clk       (clk),
                           .A         (Z_A_squared_plus_c),
                           .B         (Z_B_squared_plus_c),
                           .magnitude (magnitude)
                           );

endmodule
