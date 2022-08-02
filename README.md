# Ripstop Programming Language

This is the source code repository for Ripstop, a ⚠️highly experimental⚠️ hardware description language (a more modern alternative for Verilog or VHDL).

In fact, this language is so experimental right now, that you shouldn't even try using it. It's too early even for early adopters. Check back in early 2023 - we'll have some cool stuff for you then!

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
