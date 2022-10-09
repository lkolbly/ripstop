import os
import tempfile
import shutil
import subprocess
import sys
from mako.template import Template
import pytest

def get_files(pattern):
    files = os.listdir(".")
    files = filter(lambda s: s.endswith(pattern), files)
    files = map(lambda s: s.split(".")[0], files)
    files = list(files)
    return files

def pytest_generate_tests(metafunc):
    if "rp_functionality_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_functionality_testname", get_files(".test.v"))

    if "rp_output_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_output_testname", get_files(".output"))

    if "rp_codegen_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_codegen_testname", get_files(".expected.v"))

    if "rp_csvtestdata_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_csvtestdata_testname", get_files(".test.csv"))

    if "rp_combinedtest_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_combinedtest_testname", get_files(".test"))

class TempFile:
    def __init__(self):
        (handle, self.filename) = tempfile.mkstemp()
        os.close(handle)

    def rm(self):
        os.remove(self.filename)

def compile_rp(rptest):
    """
    Generates a temporary file to stuff the generated verilog into, and compiles.
    """
    target = TempFile()

    # TODO: Actually run the compiler. For now, we copy over the ".compiled.v" file
    compiled_filename = rptest + ".compiled.v"
    try:
        shutil.copy(compiled_filename, target.filename)
    except:
        cmd = ["./ripstop", "build", rptest, "-o", target.filename]
        print(" ".join(cmd))
        subprocess.check_output(cmd)

    return target

def expect_rp_compile_failure(rptest):
    """
    Generates a temporary file to stuff the generated verilog into, and compiles.
    """
    target = TempFile()

    cmd = ["./ripstop", "build", rptest, "-o", target.filename]
    print(" ".join(cmd))
    p = subprocess.run(cmd)
    assert p.returncode != 0

    return target

def test_compiler_correct_codegen(rp_codegen_testname):
    """
    Tests that the compiler output is identical to what we expect it to be
    """
    compiled = compile_rp(rp_codegen_testname)
    with open(compiled.filename, "r") as actual:
        actual = actual.read()
    with open(rp_codegen_testname + ".expected.v", "r") as expected:
        expected = expected.read()
    compiled.rm()
    assert actual == expected

@pytest.mark.skip(reason="Not implemented")
def test_compiler_correct_output(rp_output_testname):
    assert False # Unimplemented

def test_correct_compiled_functionality(rp_functionality_testname):
    """
    Tests that the compiled output passes the verilog testbench
    """

    compiledf = compile_rp(rp_functionality_testname)
    with open(compiledf.filename, "r") as f:
        compiled = f.read()

    with open(rp_functionality_testname + ".test.v", "r") as f:
        test = f.read()

    # Concatenate the compiled output and the test into a new file
    target = TempFile()
    with open(target.filename, "w") as t:
        t.write(compiled)
        t.write(test)

    # Build that with iverilog
    test_executable = TempFile()
    subprocess.run("iverilog -o " + test_executable.filename + " " + target.filename, check=True, shell=True)

    # Run the executable
    stdout = subprocess.check_output(test_executable.filename, shell=True).decode("utf-8")
    assert "fail" not in stdout

    compiledf.rm()
    target.rm()

def test_matches_csv(rp_csvtestdata_testname):
    """
    Tests that the compiled output matches the CSV data
    """

    test_harness = TempFile()

    dut_inputs, dut_outputs, test_data = parse_test_data("{}.test.csv".format(rp_csvtestdata_testname))

    with open(test_harness.filename, "w") as f:
        generate_test_verilog({
            "name": rp_csvtestdata_testname,
            "inputs": dut_inputs,
            "outputs": dut_outputs,
        }, test_data, f)

    compiledf = compile_rp(rp_csvtestdata_testname)
    with open(compiledf.filename, "r") as f:
        compiled = f.read()

    with open(test_harness.filename, "r") as f:
        test = f.read()

    # Concatenate the compiled output and the test into a new file
    target = TempFile()
    with open(target.filename, "w") as t:
        t.write(compiled)
        t.write(test)

    # Build that with iverilog
    test_executable = TempFile()
    subprocess.run("iverilog -o " + test_executable.filename + " " + target.filename, check=True, shell=True)

    # Run the executable
    stdout = subprocess.check_output(test_executable.filename, shell=True).decode("utf-8")
    assert "fail" not in stdout, test_harness.filename

    compiledf.rm()
    target.rm()
    test_harness.rm()

def test_combined_file(rp_combinedtest_testname):
    """
    Performs a compile test on a single combined test file
    """
    with open("{}.test".format(rp_combinedtest_testname)) as f:
        lines = list(map(lambda line: line.strip("\r\n"), f.read().split("\n")))
        segments = {}
        current_segment = None
        for line in lines:
            if line == "### CODE":
                current_segment = "code"
                segments[current_segment] = []
            elif line == "### DATA":
                current_segment = "data"
                segments[current_segment] = []
            elif line == "### COMPILED":
                current_segment = "compiled"
                segments[current_segment] = []
            elif line == "### OUTPUT":
                current_segment = "output"
                segments[current_segment] = []
            elif current_segment != None:
                segments[current_segment].append(line)
        pass

    assert "code" in segments

    codefile = TempFile()
    datafile = TempFile()

    with open(codefile.filename, "w") as f:
        f.write("\n".join(segments["code"]))

    compiledf = None
    if "compiled" in segments:
        compiled = "\n".join(segments["compiled"])
    elif "output" in segments:
        # Compiling should fail
        expect_rp_compile_failure(codefile.filename)
        return
    else:
        compiledf = compile_rp(codefile.filename)
        with open(compiledf.filename, "r") as f:
            compiled = f.read()

    with open(datafile.filename, "w") as f:
        f.write("\n".join(segments["data"]))

    test_harness = TempFile()

    dut_inputs, dut_outputs, test_data = parse_test_data(datafile.filename)

    with open(test_harness.filename, "w") as f:
        generate_test_verilog({
            "name": rp_combinedtest_testname,
            "inputs": dut_inputs,
            "outputs": dut_outputs,
        }, test_data, f)

    with open(test_harness.filename, "r") as f:
        test = f.read()

    #print(compiledf.filename)

    # Concatenate the compiled output and the test into a new file
    target = TempFile()
    with open(target.filename, "w") as t:
        t.write(compiled)
        t.write(test)

    # Build that with iverilog
    test_executable = TempFile()
    subprocess.run("iverilog -o " + test_executable.filename + " " + target.filename, check=True, shell=True)

    # Run the executable
    stdout = subprocess.check_output(test_executable.filename, shell=True).decode("utf-8")
    assert "fail" not in stdout, test_harness.filename

    if compiledf is not None:
        compiledf.rm()
    target.rm()
    test_harness.rm()
    codefile.rm()
    datafile.rm()

def generate_test_verilog(dutspec, testdata, output):
    result = Template(open("test.template.v.mako").read()).render(
        dut=dutspec,
        data=testdata,
    )
    output.write(result)

def parse_test_data(testdata):
    with open(testdata, "r") as f:
        lines = f.read().split("\n")

    header = lines[0]
    fields = list(map(lambda column: column.strip(" \r\n").split(" "), header.split(",")))
    inputs = list(map(lambda field: (field[0], int(field[2])), filter(lambda field: field[0] != "rst" and field[1] == "in", fields)))
    outputs = list(map(lambda field: (field[0], int(field[2])), filter(lambda field: field[0] != "rst" and field[1] == "out", fields)))
    fields = list(map(lambda field: field[0], fields))

    data = []
    for (rownum,row) in enumerate(lines[1:]):
        row = row.split("//")[0].strip(" \r\n")
        if len(row) == 0:
            continue
        values = list(map(lambda value: value.strip(" \r\n"), row.split(",")))
        if len(values) != len(fields):
            raise RuntimeError("Incorrect number of values on row " + str(rownum + 1))
        row = {}
        for k,v in zip(fields, values):
            try:
                v = int(v)
            except:
                if v != "x":
                    raise RuntimeError("Unrecognized value of v on row {}, '{}'".format(rownum + 1, v))
            row[k] = v
        data.append(row)

    return inputs, outputs, data
