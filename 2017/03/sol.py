#!/usr/bin/env python3

import operator

route = open('input').readline().strip().split(',')

moves = {
    "n": [1, 0, -1],
    "ne": [1, -1, 0],
    "se": [0, -1, 1],
    "s": [-1, 0, 1],
    "sw": [-1, 1, 0],
    "nw": [0, 1, -1]
}

def find(route):
    coord = [0, 0, 0]
    highest = last = 0
    for move in route:
        coord = list(map(operator.add, coord, moves[move]))
        last = max(list(map(abs, coord)))
        if last > highest:
            highest = last
    return last, highest

dist, highest = find(route)
print("P1: ", dist)
print("P2: ", highest)
