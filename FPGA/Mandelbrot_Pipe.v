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
                        input  wire [26:0] C_A_reference,
                        input  wire [26:0] C_A_step,
                        input  wire [9:0]  C_A_column,
                        input  wire [26:0] C_B,
                        output reg         done,
                        output wire [9:0]  num_iterations
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

   reg [9:0]                       num_iterations_internal;
   assign num_iterations = num_iterations_internal - 1;

   wire [26:0]                     C_A_column_fp;
   Int2Fp ConvertC_A_column({6'd0, C_A_column}, C_A_column_fp);

   wire                            magnitude_geq_four;

   reg [26:0]                      C_A;
   wire [26:0]                     addA, addB, addOut, mulA, mulB, mulOut;

   reg [26:0]                      Z_A, Z_B,
                                   Z_A_squared, Z_B_squared,
                                   Z_AB,
                                   Z_Asq_plus_Bsq, Z_Asq_minus_Bsq,
                                   Z_A_squared_plus_c, Z_B_squared_plus_c;

   wire [26:0]                     neg_Z_B_squared; // VALID ONLY IN CYCLE 2

   wire [26:0]                     fp_four, fp_two, fp_zero;
   Int2Fp ConvertFP_Four(16'sd4, fp_four);
   Int2Fp ConvertFP_Two(16'sd2, fp_two);
   Int2Fp ConvertFP_Zero(16'sd0, fp_zero);

   //=======================================================
   //  State Machines
   //=======================================================

   reg [2:0]                       state;
   parameter cycle_reset=0, cycle_1=1, cycle_2=2, cycle_3=3, cycle_4=4;
   reg                             just_did_init_flag; // in cycle 3 need to save C_A

   always @(posedge clk) begin
      if(reset | done) begin
         state            <= cycle_reset;

         // TODO: init things
         done <= 0;
         C_A <= fp_zero;
         Z_A <= fp_zero;
         Z_B <= fp_zero;
         Z_A_squared <= fp_zero;
         Z_B_squared <= fp_zero;
         Z_AB <= fp_zero;
         Z_Asq_plus_Bsq <= fp_zero;
         Z_Asq_minus_Bsq <= fp_zero;
         Z_A_squared_plus_c <= fp_zero;
         Z_B_squared_plus_c <= fp_zero;

         just_did_init_flag <= 0;

         num_iterations_internal <= 0;
      end // if (reset)
      else if (state == cycle_reset && start) begin
         state <= cycle_3;
         just_did_init_flag <= 1;

         // Mul step * idx, add mul + initial
      end
      else begin
         if (num_iterations_internal > 1000) done <= 1;

         case (state)
           //cycle_load:begin
              // compute Z_A^2
              // turns out its always zero, so reset handles it
           //end
           cycle_1:begin
              state <= cycle_2;

              num_iterations_internal <= num_iterations_internal + 1;

              // compute Z_A^2
              Z_A_squared <= mulOut;

              // receive 2*Z_A*Z_B + C_B
              Z_B <= addOut;
           end
           cycle_2:begin
              state <= cycle_3;

              // compute Z_B^2
              Z_B_squared <= mulOut;

              // reveive Z_A^2 + Z_B^2
              Z_Asq_plus_Bsq <= addOut;

              // start Z_A^2 + Z_B^2
              // COMBINATIONAL
           end
           cycle_3:begin
              state <= cycle_4;

              // compute Z_A * Z_B
              Z_AB <= mulOut;

              // reveive Z_A^2 - Z_B^2
              Z_Asq_minus_Bsq <= addOut;

              // start Z_A^2 + Z_B^2 + C_B
              // COMBINATIONAL

              // if we just did init then save C_A
              if (just_did_init_flag) begin
                 C_A <= addOut;
              end
              just_did_init_flag <= 0;
           end
           cycle_4:begin
              state <= cycle_1;

              // compute 2 * (Z_A * Z_B)
              // don't store result, goes right in adder

              // receive Z_A^2 - Z_B^2 + C_B
              Z_A <= addOut;

              // start 2 * (Z_AB) + C_B
              // COMBINATIONAL

              // check if done
              if (magnitude_geq_four) done <= 1;
           end
         endcase
      end
   end

   //=======================================================
   //  Structural coding
   //=======================================================

   assign addA = (state == cycle_reset) ? mulOut        : // step * idx
                 (state == cycle_1) ? Z_A_squared     :
                 (state == cycle_2) ? Z_A_squared     :
                 (state == cycle_3) ? addOut          : // Z_A_sq - Z_B_sq, except after INIT state when its C_A
                 (state == cycle_4) ? mulOut          : // 2*Z_AB
                                      27'bX           ; // don't care

   assign addB = (state == cycle_reset) ? C_A_reference :
                 (state == cycle_1) ? Z_B_squared     :
                 (state == cycle_2) ? neg_Z_B_squared : // comb from mult
                 (state == cycle_3) ? C_A             : // 0 after init state
                 (state == cycle_4) ? C_B             :
                                      27'bX           ; // don't care

   FpAdd FpAdder(clk, addA, addB, addOut);

   //-------------------------------------------------------

   assign mulA = (state == cycle_reset) ? C_A_step :
                 (state == cycle_1) ? Z_A    :
                 (state == cycle_2) ? Z_B    :
                 (state == cycle_3) ? Z_A    :
                 (state == cycle_4) ? fp_two :
                                      27'bX  ; // don't care

   assign mulB = (state == cycle_reset) ? C_A_column_fp :
                 (state == cycle_1) ? Z_A    :
                 (state == cycle_2) ? Z_B    :
                 (state == cycle_3) ? Z_B    :
                 (state == cycle_4) ? Z_AB   :
                                      27'bX  ; // don't care

   FpMul FpMultiplier(mulA, mulB, mulOut);

   //-------------------------------------------------------

   FpNegate Z_B_squaredNegate(mulOut, neg_Z_B_squared);

   //-------------------------------------------------------

   FpCompare MagnitudeCheck(Z_Asq_plus_Bsq, fp_four, magnitude_geq_four);

   wire [15:0] Z_A_squared_int, Z_B_squared_int, Z_Asq_plus_Bsq_int;

   Fp2Int zasqtoint(Z_A_squared, Z_A_squared_int);
   Fp2Int zbsqtoint(Z_B_squared, Z_B_squared_int);
   Fp2Int zabsqtoint(Z_Asq_plus_Bsq, Z_Asq_plus_Bsq_int);
endmodule
