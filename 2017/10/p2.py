#!/usr/bin/env python3

from threading import Thread
from queue import Queue, Empty

instructions = [ line for line in open('input') ]
q = [ Queue(), Queue() ]

def value(reg: dict, val: str) -> int:
    if val.isdigit() or val[0] == '-': 
        return int(val)
    else:
        if val not in reg:
            reg[val] = 0
        return reg[val]

class VM(Thread):
    def __init__(self, pid):
        Thread.__init__(self)
        self.pid = pid
        self.reg = {"p": pid}
        self.count = 0
    def run(self):
        eip = 0
        while eip < len(instructions):
            arr = instructions[eip].split()
            #print(self.pid, eip, arr)
            if arr[0] == "snd": 
                self.count += 1
                print(self.pid, "snd", value(self.reg, arr[1]), self.count)
                q[self.pid].put(value(self.reg, arr[1]))
            elif arr[0] == "rcv":
                try:
                    val = q[ (self.pid + 1) % len(q) ].get(True, 1)
                    self.reg[arr[1]] = val
                    print(self.pid, "rcv", val)
                except Empty:
                    print(self.pid, "deadlock?")
                    break
            elif arr[0] == "set": self.reg[arr[1]] = value(self.reg, arr[2])
            elif arr[0] == "add": self.reg[arr[1]] = value(self.reg, arr[1]) + value(self.reg, arr[2])
            elif arr[0] == "mul": self.reg[arr[1]] = value(self.reg, arr[1]) * value(self.reg, arr[2])
            elif arr[0] == "mod": self.reg[arr[1]] = value(self.reg, arr[1]) % value(self.reg, arr[2])
            elif arr[0] == "jgz" and value(self.reg, arr[1]) > 0: eip += value(self.reg, arr[2]) - 1
            eip += 1
        print(self.pid, eip, "Finished, sent ", self.count)

a, b = VM(0), VM(1)
a.start()
b.start()
