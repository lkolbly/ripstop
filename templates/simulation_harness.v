`timescale 10ns/10ns

{{compiled}}


module main();
    integer c;
    integer continue;

    reg clk, rst;
    reg[{{input_size - 1}}:0] data_in;
    wire[{{output_size - 1}}:0] data_out;

    {{module_name}} dut(
        {% for input in inputs %}
        .{{input.0}}(data_in[{{input.2.high}}:{{input.2.low}}]),
        {% endfor %}
        {% for output in outputs %}
        .{{output.0}}(data_out[{{output.2.high}}:{{output.2.low}}]),
        {% endfor %}
        .clk(clk),
        .rst(rst)
    );

    localparam period = 3;

    always begin
        clk = ~clk;
        #2;
    end

    initial begin
        $dumpfile("dump.vcd");
        $dumpvars(0, dut);

        clk = 0;
        rst = 1;

        $fflush('h8000_0001);
        continue = 1;
        while (continue) begin
            c = $fgetc('h8000_0000);
            if (c == 104) begin
                #1;
                {% for word in output_word_iterator %}
                $fwrite('h8000_0001, "%u", data_out[{{word * 32 + 31}}:{{word * 32}}]);
                {% endfor %}
               $fflush('h8000_0001);
            end else if (c == 105) begin
                continue = 0;
            end else if (c == 106) begin
                rst = 1;
            end else if (c == 107) begin
                rst = 0;
            end else if (c == 108) begin
                #period;
            end else if (c == 109) begin
                for (integer i = 0; i < {{input_bytes}}; i++) begin
                    data_in = {$fgetc('h8000_0000), data_in[{{input_size - 1}}:8]};
                end
            end else begin
                $fwrite('h8000_0002, "Unexpected command %d", c);
                continue = 0;
            end
        end

        $finish;
    end

endmodule
