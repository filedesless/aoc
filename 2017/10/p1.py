#!/usr/bin/env python3

instructions = [ line for line in open('input') ]
eip, last_played = 0, 0
reg = dict()

def value(reg: dict, val: str) -> int:
    if val.isdigit() or val[0] == '-': 
        return int(val)
    else:
        if val not in reg:
            reg[val] = 0
        return reg[val]

while eip < len(instructions):
    arr = instructions[eip].split()
    if arr[0] == "snd": last_played = value(reg, arr[1])
    elif arr[0] == "rcv" and value(reg, arr[1]) != 0: break
    elif arr[0] == "set": reg[arr[1]] = value(reg, arr[2])
    elif arr[0] == "add": reg[arr[1]] = value(reg, arr[1]) + value(reg, arr[2])
    elif arr[0] == "mul": reg[arr[1]] = value(reg, arr[1]) * value(reg, arr[2])
    elif arr[0] == "mod": reg[arr[1]] = value(reg, arr[1]) % value(reg, arr[2])
    elif arr[0] == "jgz" and value(reg, arr[1]) > 0: eip += value(reg, arr[2]) - 1
    eip += 1

print("P1: ", last_played)
