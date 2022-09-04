# Chapter 2: Combinatorial and non-combinatorial

In the previous chapter, we alluded to a construct `led[-1] = ...`. What does that mean? If `t=0` is during reset, what does `t=-1` mean?

The answer lies in how Ripstop treats combinatorial and non-combinatorial variables. All resets are synchronous. Some variables, however, are computed combinatorially. For example:
```
module accumulator(bits<16> add) -> (bits<16> value) {
    value[-1] = 16'd0;
    value[t] = value[t-1] + add[t];
}
```
`value` must be computed combinatorially. Because resets are synchronous, that means that Ripstop is unable to enforce the value during reset. So `-1` indicates the value "before" reset: During reset, at `t=0`, we must apply the rest of the code in order to determine the value:
```
value[0] = value[-1] + add[0];
value[0] = 16'd0 + add[0];
```
So the value during reset will be zero plus whatever value `add` has in reset.
