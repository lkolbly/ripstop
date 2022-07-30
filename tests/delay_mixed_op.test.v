module main();

    reg clk, rst;
    reg[3:0] a, b;
    wire[3:0] c;

    delay_mixed_op dut(
        .clk(clk),
        .rst(rst),
        .a(a),
        .b(b),
        .c(c)
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
        b = 2;
        #1; // We have to advance by 1 so everything updates
        #period;

        a = 2;
        b = 15;
        #1; #period;

        a = 3;
        b = 1;
        if (c !== 4) $display("fail ", c);
        #1; #period;

        a = 4;
        b = 9;
        #1;
        if (c !== 2) $display("fail ", c);
        #period;

        a = 5;
        b = 10;
        #1;
        if (c !== 5) $display("fail ", c);
        #period;

        $finish;
    end

endmodule
