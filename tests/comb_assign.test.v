module main();

    reg clk, rst, a;
    wire b;

    comb_assign dut(
        .clk(clk),
        .rst(rst),
        .a(a),
        .b(b)
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
        #1; // We have to advance by 1 so everything updates
        if (b !== 1) $display("fail");

        a = 0;
        #1;
        if (b !== 0) $display("fail");

        $finish;
    end

endmodule
