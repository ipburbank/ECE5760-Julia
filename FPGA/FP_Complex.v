/**************************************************************************
 * Floating Point Complex Add
 * 2-stage pipeline
 *************************************************************************/
module FP_Complex_Add (
                       input  wire        clk,
                       input  wire [26:0] A1,
                       input  wire [26:0] B1,
                       input  wire [26:0] A2,
                       input  wire [26:0] B2,
                       output wire [26:0] AOut,
                       output wire [26:0] BOut
                       );
   FpAdd    RealAdder(clk, A1, A2, AOut);
   FpAdd ComplexAdder(clk, B1, B2, BOut);
endmodule // FP_Complex_Add

/**************************************************************************
 * Floating Point Complex Mul
 * 2-stage pipeline
 *************************************************************************/
module FP_Complex_Mul (
                       input  wire        clk,
                       input  wire [26:0] A1,
                       input  wire [26:0] B1,
                       input  wire [26:0] A2,
                       input  wire [26:0] B2,
                       output wire [26:0] AOut,
                       output wire [26:0] BOut
                       );
   wire [26:0]                            A1A2, iA1B2, iB1A2, B1B2;
   FpMul  A1A2Mul(A1, A2, A1A2);
   FpMul iA1B2Mul(A1, B2, iA1B2);
   FpMul iB1A2Mul(B1, A2, iB1A2);
   FpMul  B1B2Mul(B1, B2, B1B2);

   wire [26:0]                            negB1B2;
   FpNegate B1B2Negate(B1B2, negB1B2);

   FpAdd    RealAdder(clk, A1A2,  negB1B2, AOut);
   FpAdd ComplexAdder(clk, iA1B2, iB1A2,   BOut);
endmodule // FP_Complex_Mul

/**************************************************************************
 * Floating Point Complex Magnitude^2
 * 2-stage pipeline
 *************************************************************************/
module FP_Complex_Mag_Sq (
                       input  wire        clk,
                       input  wire [26:0] A,
                       input  wire [26:0] B,
                       output wire [26:0] magnitude
                       );
   wire [26:0]                            A_sq, B_sq;
   FpMul  ASQ(A, A, A_sq);
   FpMul  BSQ(B, B, B_sq);

   FpAdd AplusB(clk, A_sq, B_sq, magnitude);
endmodule // FP_Complex_Mul
