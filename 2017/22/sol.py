#!/usr/bin/env python3

banks = tuple([int(x) for x in open('input').readline().split()])
states = {}

while banks not in states:
    states[banks] = len(states)
    highest_index = banks.index(max(banks))
    count = banks[highest_index]
    banks = list(banks)
    banks[highest_index] = 0
    i = 1
    while count > 0:
        banks[(highest_index + i) % len(banks)] += 1
        count -= 1
        i += 1
    banks = tuple(banks)

print("P1: ", len(states))
print("P2: ", len(states) - states[banks])
