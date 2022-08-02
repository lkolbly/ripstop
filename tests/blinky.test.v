module main();

    reg clk, rst;
    wire led;

    blink blinker(
        .clk(clk),
        .rst(rst),
        .led(led)
    );

    localparam period = 20;

    always begin
        clk = ~clk;
        #10;
    end

    initial begin
        clk = 0;
        rst = 1;

        #period;
        #period;
        #period;

        if (led !== 0) begin
            $display("fail ", led);
        end

        rst = 0;
        #period;

        if (led !== 1) begin
            $display("fail ", led);
        end

        #period;

        if (led !== 0) $display("fail ", led);

        #period;

        if (led !== 1) begin
            $display("fail ", led);
        end

        $finish;
    end

endmodule
