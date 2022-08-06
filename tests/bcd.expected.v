module bcd(
    input clk,
    input rst,

    input[3:0] base,
    input[3:0] a,
    input[3:0] b,
    input carryin,

    output[3:0] c,
    output carryout
);

    wire[4:0] sum;

    assign sum = {1'b0, a} + {1'b0, b} + {3'b0, carryin};
    assign carryout = sum >= base;
    assign c = carryout ? sum - base : sum;

endmodule
