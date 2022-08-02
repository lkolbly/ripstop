module main();

    reg clk, rst;
    reg[3:0] a;
    reg[3:0] b;
    wire[3:0] c;

    comb_add dut(
        .clk(clk),
        .rst(rst),
        .a(a),
        .b(b),
        .c(c)
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

        a = 1;
        b = 2;
        #1; // We have to advance by 1 so everything updates
        if (c !== 3) $display("fail");

        a = 0;
        b = 5;
        #1;
        if (c !== 5) $display("fail");

        a = 15;
        b = 10;
        #1;
        if (c !== 9) $display("fail");

        $finish;
    end

endmodule
