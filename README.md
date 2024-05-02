# Ripstop Programming Language

This is the source code repository for Ripstop, a ⚠️highly experimental⚠️ hardware description language (a more modern alternative for Verilog or VHDL).

In fact, this language is so experimental right now, that you should probably only try it if an Authorized Ripstop Representative is holding your hand. It's pretty early even for early adopters. So, you know, be careful if you try to use it for anything safety-critical.

Feel free to open issues and/or PRs!

# Usage

TODO: Fill out this section

# Running compiler tests

The compiler contains a (soon-to-be) rigorous test suite. To run it, follow these steps:
1. Build the docker image using the `tests/docker/build.sh` script:
   ```
   $ cd tests/docker/
   $ sh ./build.sh
   ```
   This will build a docker image `lkolbly/rptest` which contains pytest and iverilog.
2. From the tests directory, run the docker image to get a bash prompt:
   ```
   $ cd <project root>/tests
   $ docker run -it -v `pwd`:/work lkolbly/rptest
   root@e2a2da983389:/#
   ```
3. In the `/work` subdirectory, run `pytest ./run_tests.py`:
   ```
   root@e2a2da983389:/# cd work/
   root@e2a2da983389:/work# pytest ./run_tests.py
   ```
   The tests should run, and eventually print out a line that looks like:
   ```
   ========================= 4 passed, 1 skipped in 0.07s =========================
   ```

# Developing the Python module

1. Create a python virtual environment:
   ```
   $ python3 -m venv venv
   $ . ./venv/bin/activate
   ```
2. Install maturin
   ```
   $ pip install maturin
   ```
3. Run `maturin develop` to build the python module and install it into the virtual environment.
