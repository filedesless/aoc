#!/usr/bin/env python3

total = 0
layers = {}

for line in open('input'):
    arr = line.split(':')
    n = int(arr[0])
    d = int(arr[1].strip())
    layers[n] = d
    if n % (2 * (d - 1)) == 0:
        total += n * d

print("P1: ", total)

i = 0
while True:
    i += 1
    for n, d in layers.items():
        if (n + i) % (2 * (d - 1)) == 0: # been caught
            break
    else: # never been caught
        break

print("P2: ", i)
