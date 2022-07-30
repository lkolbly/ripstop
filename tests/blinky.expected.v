module blink(
    input clk,
    input rst,
    output led
);

    reg led_d, led_q;
    assign led = led_q;

    always @(*) begin
        led_d <= ~led_q;
    end

    always @(posedge clk) begin
        if (rst) begin
            led_q <= 1'h0;
        end else begin
            led_q <= led_d;
        end
    end

endmodule
