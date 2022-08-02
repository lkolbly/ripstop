module delay(
    input clk,
    input rst,
    input[3:0] a,
    output[3:0] b
);

    reg[3:0] a_1_q, a_1_d;
    reg[3:0] a_2_q, a_2_d;
    reg[3:0] a_3_q, a_3_d;

    assign b = a_3_q;

    always @(*) begin
        a_1_d <= a;
        a_2_d <= a_1_q;
        a_3_d <= a_2_q;
    end

    always @(posedge clk) begin
        a_1_q <= a_1_d;
        a_2_q <= a_2_d;
        a_3_q <= a_3_d;
    end

endmodule
