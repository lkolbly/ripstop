import base64
import sys
import time
import struct
import hashlib
import tempfile
from ripstop_lib import SimulationInstance


def add(inputs):
    return {"result": inputs["a"] + inputs["b"]}


class Framework:
    def __init__(self):
        self.digest = []
        pass

    def __enter__(self):
        self.instance = SimulationInstance("test_extern.rp", "my_module", {"add_instance": add})
        self.instance.__enter__()
        return self

    def __exit__(self, *args):
        self.instance.__exit__()

    def reset_step(self, args):
        return self.instance.reset_step(args)

    def step(self, args):
        res = self.instance.step(args)
        return res


def test_extern():
    with Framework() as test:
        default = {"a": 0, "b": 0}
        test.reset_step(default)
        test.reset_step(default)

        res = test.step({"a": 1, "b": 2})
        assert res["result"] == 3

        res = test.step({"a": 4, "b": 1234})
        assert res["result"] == 1238
