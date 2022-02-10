#!/usr/bin/env python3

from operator import eq, ne, le, ge, lt, gt, add, sub

reg = {}

op = {
    "==": eq,
    "!=": ne,
    "<=": le,
    ">=": ge,
    "<": lt,
    ">": gt,
    "inc": add,
    "dec": sub
}

highest = 0
for line in open('input'):
    line = line.split()
    if line[4] not in reg:
        reg[line[4]] = 0
    if op[line[5]](reg[line[4]], int(line[6])):
        if line[0] not in reg:
            reg[line[0]] = 0

        reg[line[0]] = op[line[1]](reg[line[0]], int(line[2]))
        m = max(reg.values())
        if m > highest:
            highest = m

print(max(reg.values()))
print(highest)
