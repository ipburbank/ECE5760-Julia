//scfifo ADD_RAM_OUTPUT_REGISTER="OFF" CBX_SINGLE_OUTPUT_FILE="ON" INTENDED_DEVICE_FAMILY=""Cyclone V"" LPM_NUMWORDS=256 LPM_SHOWAHEAD="ON" LPM_TYPE="scfifo" LPM_WIDTH=29 LPM_WIDTHU=8 OVERFLOW_CHECKING="ON" UNDERFLOW_CHECKING="ON" USE_EAB="OFF" clock data empty full q rdreq wrreq
//VERSION_BEGIN 16.1 cbx_mgl 2017:01:11:19:37:47:SJ cbx_stratixii 2017:01:11:18:30:33:SJ cbx_util_mgl 2017:01:11:18:30:33:SJ  VERSION_END
// synthesis VERILOG_INPUT_VERSION VERILOG_2001
// altera message_off 10463



// Copyright (C) 2017  Intel Corporation. All rights reserved.
//  Your use of Intel Corporation's design tools, logic functions 
//  and other software and tools, and its AMPP partner logic 
//  functions, and any output files from any of the foregoing 
//  (including device programming or simulation files), and any 
//  associated documentation or information are expressly subject 
//  to the terms and conditions of the Intel Program License 
//  Subscription Agreement, the Intel Quartus Prime License Agreement,
//  the Intel MegaCore Function License Agreement, or other 
//  applicable license agreement, including, without limitation, 
//  that your use is for the sole purpose of programming logic 
//  devices manufactured by Intel and sold by Intel or its 
//  authorized distributors.  Please refer to the applicable 
//  agreement for further details.



//synthesis_resources = scfifo 1 
//synopsys translate_off
`timescale 1 ps / 1 ps
//synopsys translate_on
module  mgmhp
	( 
	clock,
	data,
	empty,
	full,
	q,
	rdreq,
	wrreq) /* synthesis synthesis_clearbox=1 */;
	input   clock;
	input   [28:0]  data;
	output   empty;
	output   full;
	output   [28:0]  q;
	input   rdreq;
	input   wrreq;

	wire  wire_mgl_prim1_empty;
	wire  wire_mgl_prim1_full;
	wire  [28:0]   wire_mgl_prim1_q;

	scfifo   mgl_prim1
	( 
	.clock(clock),
	.data(data),
	.empty(wire_mgl_prim1_empty),
	.full(wire_mgl_prim1_full),
	.q(wire_mgl_prim1_q),
	.rdreq(rdreq),
	.wrreq(wrreq));
	defparam
		mgl_prim1.add_ram_output_register = "OFF",
		mgl_prim1.intended_device_family = ""Cyclone V"",
		mgl_prim1.lpm_numwords = 256,
		mgl_prim1.lpm_showahead = "ON",
		mgl_prim1.lpm_type = "scfifo",
		mgl_prim1.lpm_width = 29,
		mgl_prim1.lpm_widthu = 8,
		mgl_prim1.overflow_checking = "ON",
		mgl_prim1.underflow_checking = "ON",
		mgl_prim1.use_eab = "OFF";
	assign
		empty = wire_mgl_prim1_empty,
		full = wire_mgl_prim1_full,
		q = wire_mgl_prim1_q;
endmodule //mgmhp
//VALID FILE
