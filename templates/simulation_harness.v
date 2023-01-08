`timescale 10ns/10ns

{% for module in external_modules %}
module {{module.name}}(
    {% for input in module.inputs %}
    input [{{input.2 - 1}}:0] {{input.0}},
    {% endfor %}
    {% for output in module.outputs %}
    output [{{output.2 - 1}}:0] {{output.0}},
    {% endfor %}
    input clk,
    input rst
);

    {% for output in module.outputs %}
    reg [{{output.2 - 1}}:0] __rp_{{output.0}};
    assign {{output.0}} = __rp_{{output.0}};
    {% endfor %}

endmodule
{% endfor %}

{{compiled}}


module main();
    integer c;
    integer continue;

    reg clk, rst;
    reg[{{input_size - 1}}:0] data_in;
    reg[{{external_output_bytes * 8 - 1}}:0] extern_data_out;
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
        {% if output_dumpfile %}
            $dumpfile("{{dumpfile}}");
            $dumpvars(0, dut);
        {% endif %}

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
                #1;
            end else if (c == 110) begin
                {% for word in external_inputs %}
                $fwrite('h8000_0001, "%u", {
                    {% if not word %}
                    32'd0
                    {% endif %}
                    {% for var in word -%}
                    dut.{{var.name}}[{{var.var_section.high}}:{{var.var_section.low}}]{% if not loop.last %},{% endif %}
                    {% endfor %}
                });
                {% endfor -%}

                //$fwrite('h8000_0001, "%u", dut.add_instance.a);
                //$fwrite('h8000_0001, "%u", dut.add_instance.b);
                $fflush('h8000_0001);
            end else if (c == 111) begin
                //extern_data_in = {$fgetc('h8000_0000), extern_data_in[15:8]};
                //extern_data_in = {$fgetc('h8000_0000), extern_data_in[15:8]};
                //dut.add_instance.__rp_result = extern_data_in[15:0];

                {% if external_output_bytes > 0 %}
                for (integer i = 0; i < {{external_output_bytes}}; i++) begin
                    extern_data_out = {$fgetc('h8000_0000), extern_data_out[{{external_output_bytes * 8 - 1}}:8]};
                end

                {% for output in external_outputs %}
                dut.{{output.0}} = extern_data_out[{{output.2.high}}:{{output.2.low}}];
                {% endfor %}
                {% endif %}

            end else begin
                $fwrite('h8000_0002, "Unexpected command %d", c);
                continue = 0;
            end
        end

        $finish;
    end

endmodule
