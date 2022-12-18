import base64
import sys
import time
import struct
import hashlib
from ripstop_lib import SimulationInstance

class Sha:
    def __init__(self):
        self.digest = []
        pass

    def __enter__(self):
        self.instance = SimulationInstance("sha2.rp", "hash_bytes")
        self.instance.__enter__()
        return self

    def __exit__(self, *args):
        self.instance.__exit__()

    def reset_step(self, args):
        return self.instance.reset_step(args)

    def step(self, args):
        res = self.instance.step(args)
        return res

    def reset_hash(self):
        self.instance.step({"reset_hash": 1})

    def hash_block(self, data):
        assert len(data) == 64
        for d in data:
            self.step({"data_in_valid": 1, "data_in": d})

        while True:
            res = self.step({})
            if res["digest_out_valid"]:
                self.digest.append(res["digest_out"])
                if len(self.digest) == 32:
                    digest = ""
                    for word in range(8):
                        for b in range(4):
                            x = self.digest[word * 4 + 3 - b]
                            digest += f"{x:02x}"
                    #digest = "".join([f"{x:x}" for x in self.digest])
                    print(f"Have digest {digest}")
                    self.digest = []
                    return digest

    def hash_data(self, data):
        expected = hashlib.sha256(data).hexdigest()

        data = [x for x in data]
        datalen = len(data)

        # Append the 1 (since data is a multiple of 8, append 0x80)
        data.append(0x80)
        while (len(data) + 8) % 64 != 0:
            data.append(0)
        data += struct.pack(">Q", datalen * 8)

        #print(data)
        #print(len(data))

        self.reset_hash()
        for block in range(0,len(data),64):
            block = data[block:block+64]
            #print(block, len(block))
            digest = self.hash_block(block)
        assert digest == expected
        return digest

def generate():
    K = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    ]

    for i,k in enumerate(K):
        print(f"else if round[t] == 8'd{i} {{ k[t] = 32'd{k}; }}")
        pass
    pass

def test_sha():
    with Sha() as hasher:
        default = {"data_in_valid": 0, "data_in": 0, "reset_hash": 0}
        hasher.reset_step(default)
        hasher.reset_step(default)
        hasher.reset_step(default)

        for i in range(10):
            hasher.step(default)
        """hasher.step({"data_in_valid": 0, "data_in": 0, "reset_hash": 1})
        for i in range(10):
            hasher.step(default)

        #data = [0x80] + [0] * 63
        data = [x for x in range(64)]
        for d in data:
            hasher.step({"data_in_valid": 1, "data_in": d, "reset_hash": 0})

        for _ in range(500):
            hasher.step(default)"""

        hasher.reset_hash()
        data = [x for x in range(64)]
        assert hasher.hash_block(data) == "fc99a2df88f42a7a7bb9d18033cdc6a20256755f9d5b9a5044a9cc315abe84a7"

        for _ in range(500):
            hasher.step(default)

        data = [0x80] + [0] * 59 + [0x0, 0x0, 0x2, 0x0]
        assert hasher.hash_block(data) == "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108"

        #for _ in range(500):
        #    hasher.step(default)

        hasher.reset_hash()
        data = [0x80] + [0] * 63
        print(hashlib.sha256(b"").hexdigest())
        assert hasher.hash_block(data) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

        #hasher.reset_hash()
        hasher.hash_data(b"a")
        hasher.hash_data(b"This is a test vector. It needs to be more than 512 bits long.")
        hasher.hash_data(b"The quick brown fox")
