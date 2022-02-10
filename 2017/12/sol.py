#!/usr/bin/env python3

x = []
matrix = []

for line in open('input'):
    arr = [ int(s) for s in line.split() ]
    matrix.append(arr)
    x.append(max(arr) - min(arr))

print("P1: ", sum(x))

total = 0
for line in matrix:
    for value in line:
        for other in line:
            if value != other and value % other == 0:
                total += value / other

print("P2: ", total)
