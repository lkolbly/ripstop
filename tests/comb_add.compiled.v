module comb_add(
    input clk,
    input rst,
    input[3:0] a,
    input[3:0] b,
    output[3:0] c
);

    assign c = a + b;

endmodule
