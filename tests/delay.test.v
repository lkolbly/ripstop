module main();

    reg clk, rst;
    reg[3:0] a;
    wire[3:0] b;

    delay dut(
        .clk(clk),
        .rst(rst),
        .a(a),
        .b(b)
    );

    localparam period = 19;

    always begin
        clk = ~clk;
        #10;
    end

    initial begin
        clk = 0;
        rst = 1;

        #1; #period;
        #1; #period;
        rst = 0;
        #1; #period;

        a = 1;
        #1; // We have to advance by 1 so everything updates
        #period;

        a = 2;
        #1; #period;

        a = 3;
        #1; #period;

        a = 4;
        #1;
        if (b !== 1) $display("fail ", b);
        #period;

        a = 5;
        #1;
        if (b !== 2) $display("fail ", b);
        #period;

        a = 6;
        #1;
        if (b !== 3) $display("fail ", b);
        #period;

        a = 7;
        #1;
        if (b !== 4) $display("fail ", b);
        #period;

        a = 8;
        #1;
        if (b !== 5) $display("fail ", b);
        #period;

        $finish;
    end

endmodule
