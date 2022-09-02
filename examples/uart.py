import base64
import sys
import time
from ripstop_lib import SimulationInstance

class Uart:
    def __init__(self):
        self.buffers = []
        self.buffer = []
        pass

    def __enter__(self):
        self.instance = SimulationInstance("examples/uart.rp", "doubleecho")
        self.instance.__enter__()
        return self

    def __exit__(self, *args):
        self.instance.__exit__()

    def reset_step(self, args):
        return self.instance.reset_step(args)

    def step(self, args):
        res = self.instance.step(args)
        return res

with Uart() as decoder:
    # Test doubleecho
    low = {"rxd": 0, "prescaler": 9}
    high = {"rxd": 1, "prescaler": 9}
    for i in range(2):
        decoder.reset_step(high)
    for i in range(10):
        decoder.step(high)

    bits = [low, low, high, high, low, low, low, high, low, high, high, high, high, high]
    for b in bits:
        for i in range(10):
            decoder.step(b)

    for i in range(200):
        decoder.step(high)

    # Test repeater
    """for i in range(2):
        decoder.reset_step({})
    for i in range(1000):
        decoder.step({})"""

    # Test RX
    """low = {"rx": 0, "prescaler": 9}
    high = {"rx": 1, "prescaler": 9}
    decoder.reset_step(high)
    for i in range(10):
        decoder.step(high)

    bits = [low, low, high, high, low, low, low, high, low, high, high, high, high, high]
    for b in bits:
        for i in range(10):
            decoder.step(b)"""

    # Test TX
    """default = {"data_in_valid": 0, "data_in": 0, "prescaler": 10}
    decoder.reset_step(default)
    for i in range(10):
        decoder.step(default)
    print(decoder.step(default))

    decoder.step({"data_in_valid": 1, "data_in": 0x62, "prescaler": 10})

    for i in range(200):
        decoder.step(default)"""
