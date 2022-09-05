# Chapter 1: Hello, Ripstop!

Welcome to Ripstop!

Ripstop is a cycle-based language. That means that the language represents computation in terms of how state changes from one cycle to the next.

What does that mean? Let's start with a simple example, the classic blinky:
```
module blinky() -> (bit led) {
    led[0] = 1'd0;
    led[t] = ~led[t-1];
}
```
Let's go through this line-by-line.

```
module blinky() -> (bit led) {
```
This instantiates a new module called `blinky`. This module has no inputs, and it has a single output called `led` with the 1-bit type `bit`.

```
    led[0] = 1'd0;
```
This means that in reset, at t=0, `led` should be set to the 1-bit (`1'`) decimal (`d`) value 0. Other bases you can use are hex (`h`, e.g. `4'h15`), octal (`o`, e.g. `4'o17`), and binary (`b`, e.g. `4'b1111`).

You might also sometimes see `led[-1] = ...`. We'll cover that in a later chapter.

```
    led[t] = ~led[t-1];
```
This says that, at every cycle, `led` should be the bitwise inverse of what it was on the cycle before. For example, if `led` is 0 on one cycle, it should be 1 on the next.

What does that look like if we look at the waveform?
```wavedrom
{signal: [
  {name: 'clk', wave: 'p.........'},
  {name: 'rst', wave: '1..0......'},
  {name: 'led', wave: '0...101010'},
]}
```

It's worth talking about how Ripstop maps to the underlying RTL, because there are some subtleties. Note that we did not specify any inputs to the `blinky` module, yet in the graph we show `clk` and `rst`: These two inputs are implicitly added by the Ripstop compiler. Reset is active-high: so when reset is high, the module is resetting. Variables change on the positive edge of the clock. To see the relationship between one cycle to the next, consider the state immediately before a positive clock edge and compute it to the state immediately after.

## Compiling to Verilog

Of course, writing code in Ripstop is great, but it's not very useful if you can't compile it. Ripstop compiles to Verilog using the Ripstop compiler. First, you must either get a copy of it, or build it yourself from the latest `main` branch:
```
$ cargo build
```

If you get an error like `error: failed to run custom build command for pyo3-ffi v0.17.1`, you should specify a Python interpreter greater than version 3.7 using the `PYO3_PYTHON_VERSION` environment variable:
```
$ PYO3_PYTHON=/usr/local/bin/python3.10 cargo build
```
Building the compiler will place the executable at `target/debug/ripstop`

Once you have the compiler built, you can run it by specifying both the input file and the output file:
```
$ ripstop blinky.rp -o output.v
```

And there you have it, your first Ripstop program!
