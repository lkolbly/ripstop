module main();

    reg clk, rst;

    %for input in dut["inputs"]:
    reg[${input[1] - 1}:0] ${input[0]};
    %endfor
    %for output in dut["outputs"]:
    wire[${output[1] - 1}:0] ${output[0]};
    %endfor

    ${dut["name"]} dut(
        .clk(clk),
        .rst(rst),
        %for connector in dut["inputs"] + dut["outputs"]:
        .${connector[0]}(${connector[0]})
        %if not loop.last:
        ,
        %endif
        %endfor
    );

    localparam period = 19;

    always begin
        clk = ~clk;
        #10;
    end

    initial begin
        clk = 0;

        %for rownum,row in enumerate(data):
        rst = ${row["rst"]};
        %for input in dut["inputs"]:
        %if row[input[0]] == "x":
        ${input[0]} = ${input[1]}'bx;
        %else:
        ${input[0]} = ${row[input[0]]};
        %endif
        %endfor

        #1;

        %for output in dut["outputs"]:
        %if row[output[0]] == "x":
        $display("note: on step ${rownum+1}, ${output[0]} is", ${output[0]});
        %else:
        if (${output[0]} !== ${row[output[0]]}) begin
            $display("fail on step ${rownum+1}, expected ${output[0]} to equal ${row[output[0]]} but it actually was", ${output[0]});
        end
        %endif
        %endfor

        #period;

        %endfor

        $finish;
    end

endmodule
