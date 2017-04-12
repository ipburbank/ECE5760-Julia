/*
 * inspired by:
 * https://github.com/cornell-ece5745/ece5745-tut4-verilog/blob/master/sim/vc/arbiters.v
 */

module Reqs_To_One_Hot #(parameter NUM_REQS=2)
   (
    input  wire [NUM_REQS-1:0] reqs,
    output wire [NUM_REQS-1:0] grants
    );

   wire [NUM_REQS:0]           kills;
   assign kills[0] = 1'b0;

   genvar                   i;
   generate
      for ( i = 0; i < NUM_REQS; i = i + 1 )
        begin : per_req_logic

           // Grant is true if this requester is not killed and it is actually
           // making a req.

           assign grants[i] = !kills[i] && reqs[i];

           // Kill is true if this requester was either killed or it received
           // the grant.

           assign kills[i+1] = kills[i] || grants[i];

        end
   endgenerate
endmodule
