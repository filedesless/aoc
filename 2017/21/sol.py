#!/usr/bin/env python3

def compute(P2 = False):
    count = 0
    lines = [int(line) for line in open('input')]
    index = lines[0]
    while index >= 0 and index < len(lines):
        offset = lines[index]
        if P2 and lines[index] >= 3:
            lines[index] -= 1
        else:
            lines[index] += 1
        index += offset
        count += 1
    return count

print("P1: ", compute())
print("P2: ", compute(True))
