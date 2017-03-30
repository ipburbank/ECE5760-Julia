// megafunction wizard: %LPM_CLSHIFT%
// GENERATION: STANDARD
// VERSION: WM1.0
// MODULE: LPM_CLSHIFT 

// ============================================================
// File Name: CLSHIFT_left_54b.v
// Megafunction Name(s):
// 			LPM_CLSHIFT
//
// Simulation Library Files(s):
// 			
// ============================================================
// ************************************************************
// THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
//
// 16.1.2 Build 203 01/18/2017 SJ Lite Edition
// ************************************************************


//Copyright (C) 2017  Intel Corporation. All rights reserved.
//Your use of Intel Corporation's design tools, logic functions 
//and other software and tools, and its AMPP partner logic 
//functions, and any output files from any of the foregoing 
//(including device programming or simulation files), and any 
//associated documentation or information are expressly subject 
//to the terms and conditions of the Intel Program License 
//Subscription Agreement, the Intel Quartus Prime License Agreement,
//the Intel MegaCore Function License Agreement, or other 
//applicable license agreement, including, without limitation, 
//that your use is for the sole purpose of programming logic 
//devices manufactured by Intel and sold by Intel or its 
//authorized distributors.  Please refer to the applicable 
//agreement for further details.


// synopsys translate_off
`timescale 1 ps / 1 ps
// synopsys translate_on
module CLSHIFT_left_54b (
	data,
	distance,
	result);

	input	[53:0]  data;
	input	[5:0]  distance;
	output	[53:0]  result;

	wire [53:0] sub_wire0;
	wire  sub_wire1 = 1'h0;
	wire [53:0] result = sub_wire0[53:0];

	lpm_clshift	LPM_CLSHIFT_component (
				.data (data),
				.direction (sub_wire1),
				.distance (distance),
				.result (sub_wire0)
				// synopsys translate_off
				,
				.aclr (),
				.clken (),
				.clock (),
				.overflow (),
				.underflow ()
				// synopsys translate_on
				);
	defparam
		LPM_CLSHIFT_component.lpm_shifttype = "LOGICAL",
		LPM_CLSHIFT_component.lpm_type = "LPM_CLSHIFT",
		LPM_CLSHIFT_component.lpm_width = 54,
		LPM_CLSHIFT_component.lpm_widthdist = 6;


endmodule

// ============================================================
// CNX file retrieval info
// ============================================================
// Retrieval info: PRIVATE: INTENDED_DEVICE_FAMILY STRING "Cyclone V"
// Retrieval info: PRIVATE: LPM_SHIFTTYPE NUMERIC "0"
// Retrieval info: PRIVATE: LPM_WIDTH NUMERIC "54"
// Retrieval info: PRIVATE: SYNTH_WRAPPER_GEN_POSTFIX STRING "0"
// Retrieval info: PRIVATE: lpm_widthdist NUMERIC "6"
// Retrieval info: PRIVATE: lpm_widthdist_style NUMERIC "1"
// Retrieval info: PRIVATE: new_diagram STRING "1"
// Retrieval info: PRIVATE: port_direction NUMERIC "0"
// Retrieval info: LIBRARY: lpm lpm.lpm_components.all
// Retrieval info: CONSTANT: LPM_SHIFTTYPE STRING "LOGICAL"
// Retrieval info: CONSTANT: LPM_TYPE STRING "LPM_CLSHIFT"
// Retrieval info: CONSTANT: LPM_WIDTH NUMERIC "54"
// Retrieval info: CONSTANT: LPM_WIDTHDIST NUMERIC "6"
// Retrieval info: USED_PORT: data 0 0 54 0 INPUT NODEFVAL "data[53..0]"
// Retrieval info: USED_PORT: distance 0 0 6 0 INPUT NODEFVAL "distance[5..0]"
// Retrieval info: USED_PORT: result 0 0 54 0 OUTPUT NODEFVAL "result[53..0]"
// Retrieval info: CONNECT: @data 0 0 54 0 data 0 0 54 0
// Retrieval info: CONNECT: @direction 0 0 0 0 GND 0 0 0 0
// Retrieval info: CONNECT: @distance 0 0 6 0 distance 0 0 6 0
// Retrieval info: CONNECT: result 0 0 54 0 @result 0 0 54 0
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b.v TRUE
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b.inc FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b.cmp FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b.bsf FALSE
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b_inst.v TRUE
// Retrieval info: GEN_FILE: TYPE_NORMAL CLSHIFT_left_54b_bb.v TRUE
