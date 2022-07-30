module delay_mixed_op(
    input clk,
    input rst,
    input[3:0] a,
    input[3:0] b,
    output[3:0] c
);

    reg[3:0] b_1_q, b_1_d, c_q, c_d;
    assign c = c_q;

    always @(*) begin
        b_1_d <= b;
        c_d <= a + b_1_q;
    end

    always @(posedge clk) begin
        if (rst) begin
            b_1_q <= 0;
            c_q <= 0;
        end else begin
            b_1_q <= b_1_d;
            c_q <= c_d;
        end
    end

endmodule
