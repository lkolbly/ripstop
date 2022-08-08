module register(
    input clk,
    input rst,

    input write,
    input[31:0] write_addr,
    input[31:0] write_data,

    input read,
    input[31:0] read_addr,

    output read_valid,
    output[31:0] read_data
);

    reg[31:0] register1_q, register1_d, register2_q, register2_d, write_addr_q, write_data_q, read_addr_q, read_data_q, read_data_d;
    reg write_q, read_q, read_valid_q, read_valid_d;

    assign read_valid = read_valid_d;
    assign read_data = read_data_d;

    always @(*) begin
        register1_d = register1_q;
        register2_d = register2_q;
        read_valid_d = 0;
        read_data_d = 0;

        if (write_q) begin
            if (write_addr_q == 0) begin
                register1_d = write_data_q;
            end else if (write_addr_q == 4) begin
                register2_d = write_data_q;
            end
        end

        if (read_q) begin
            if (read_addr_q == 0) begin
                read_data_d = register1_q;
                read_valid_d = 1;
            end else if (read_addr_q == 4) begin
                read_data_d = register2_q;
                read_valid_d = 1;
            end
        end
    end

    always @(posedge clk) begin
        if (rst == 1) begin
            register1_q <= 0;
            register2_q <= 0;
            write_q <= 0;
            read_q <= 0;
            read_valid_q <= 0;
        end else begin
            register1_q <= register1_d;
            register2_q <= register2_d;
            write_q <= write;
            write_addr_q <= write_addr;
            write_data_q <= write_data;
            read_q <= read;
            read_addr_q <= read_addr;
            read_data_q <= read_data_d;
            read_valid_q <= read_valid_d;
        end
    end

endmodule
