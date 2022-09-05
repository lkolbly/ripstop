import base64
import sys
import time
from ripstop_lib import SimulationInstance

class Adder:
    def __enter__(self):
        self.instance = SimulationInstance("examples/adder.rp", "adder")
        self.instance.__enter__()
        return self

    def __exit__(self, *args):
        self.instance.__exit__()

    def reset_step(self, args):
        return self.instance.reset_step(args)

    def step(self, args):
        return self.instance.step(args)

with Adder() as decoder:
    decoder.reset_step({"a": 0, "b": 0})
    decoder.step({"a": 2, "b": 4})
    decoder.step({"a": 3, "b": 6})
    decoder.step({"a": 4, "b": 8})
    decoder.step({"a": 5, "b": 10})
    print(decoder.step({"a": 6, "b": 12}))
