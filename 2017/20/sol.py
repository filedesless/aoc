#!/usr/bin/env python3

total = 0

for line in open('input'):
    arr = line.split()
    if sorted(arr) == sorted(list(set(arr))):
        total += 1

print("P1: ", total)

total = 0
for line in open('input'):
    bag = set(["".join(sorted(word)) for word in line.split()])
    if len(bag) == len(line.split()):
        total += 1

print("P2: ", total)
