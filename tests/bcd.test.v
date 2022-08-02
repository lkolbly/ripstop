module main();

    wire carryout;
    wire[3:0] c;

    bcd_digit_add digit(
        .base(4'd10),
        .a(4'd4),
        .b(4'd8),
        .carryin(1'd0),
        .c(c),
        .carryout(carryout)
    );

    initial begin
        $display("Hi there");

        #100;

        $display("4 + 3 =", c, " + base * ", carryout);

        $finish ;
    end

endmodule
