import os
import tempfile
import shutil
import subprocess

def get_files(pattern):
    files = os.listdir(".")
    files = filter(lambda s: pattern in s, files)
    files = map(lambda s: s.split(".")[0], files)
    files = list(files)
    return files

def pytest_generate_tests(metafunc):
    if "rp_functionality_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_functionality_testname", get_files(".test.v"))

    if "rp_output_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_output_testname", get_files(".output"))

    if "rp_codegen_testname" in metafunc.fixturenames:
        metafunc.parametrize("rp_codegen_testname", get_files(".compiled.v"))
    pass

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
    shutil.copy(compiled_filename, target.filename)

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
