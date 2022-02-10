#!/usr/bin/env python3

reg = dict() # str -> int
instructions = [ line for line in open('input') ]
for i in range(8): reg[chr(97+i)] = 0

def val(r:str) -> int:
    if r.startswith('-') or r.isdigit(): return int(r)
    return reg[r]

def run() -> None:
    eip, count = 0, 0
    while True:
        op, x, y = instructions[eip].split()
        if op == "set":
            reg[x] = val(y)
        elif op == "sub":
            reg[x] -= val(y)
        elif op == "mul":
            reg[x] *= val(y)
            count += 1
        elif op == "jnz":
            if val(x) != 0:
                eip += val(y) - 1
        eip += 1
        if eip >= len(instructions): return count

print("P1:", run())
