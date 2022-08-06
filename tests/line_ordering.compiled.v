module line_ordering(
    input clk,
    input rst,
    input[3:0] c,
    output[3:0] a
);

    reg[3:0] a_q;
    wire[3:0] a_d;
    assign a = a_d;

    wire[3:0] b;
    assign b = a_q + c;
    assign a_d = ~b;

    always @(posedge clk) begin
        if (rst == 1) begin
            a_q <= 0;
        end else begin
            a_q <= a_d;
        end
    end

endmodule
