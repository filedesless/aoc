#!/usr/bin/env python3

step = 312
pos = 0
l = [0]

for i in range(1, 2017 + 1):
    pos = (pos + step) % i + 1
    l.insert(pos, i)

print("P1: ", l[pos+1])

pos = 0
val = 0

for i in range(1, 50_000_000 + 1):
    pos = (pos + step) % i + 1
    if pos == 1:
        val = i
    if i % 1_000_000 == 0:
        print(val)

print(val)
