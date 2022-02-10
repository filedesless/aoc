#!/usr/bin/env python3

import math

rules = {}
pattern = ".#./..#/###"

for line in open('input'):
    arr = line.split()
    rules[arr[0]] = arr[2]

def divided(matrix:str) -> str:
    matrix = matrix.split('/')
    n = len(matrix)
    size = 2 if n % 2 == 0 else 3
    for x in range(n // size):
        for y in range(n // size):
            sub = []
            for i in range(size):
                sub.append(matrix[size*x+i][size*y:size*y+size])
            yield "/".join(sub)

def flipped(matrix:str) -> str:
    yield matrix
    yield matrix[::-1]
    yield "/".join([ sub[::-1] for sub in matrix.split('/') ])
    yield "/".join([ sub[::-1] for sub in matrix[::-1].split('/') ])

def rotated(matrix:str) -> str:
    yield from flipped(matrix)
    tmp = matrix.split('/')
    ret = []
    for x in range(len(tmp)):
        ret.append("")
        for y in range(len(tmp[x])):
            ret[x] += tmp[y][x]
    yield from flipped("/".join(ret))
    
def expand(pattern:str, depth=1) -> str:
    tmp, out = [], []
    subs = list(divided(pattern))
    if depth == 0: 
        return pattern
    for sub in subs:
        for line in rotated(sub):
            if line in rules:
                tmp += rules[line].split('/')
                break
    if len(subs) > 1:
        m = len(tmp[0])
        n = int(math.sqrt(len(tmp) // m))
        for x in range(n):
            for y in range(m):
                line = ""
                for z in range(n):
                    index = x * m * n + y + z * m
                    line += tmp[index]
                out.append(line)
    else:
        out = tmp
    out = "/".join(out)
    return expand(out, depth - 1)

out = expand(pattern, 5)
print("P1:", out.count('#'))

out = expand(pattern, 18)
print("P2:", out.count('#'))
