#!/usr/bin/env python3

stream = open('input').readline().strip()

i = collected = depth = score = 0
garbage = False
while i < len(stream):
    c = stream[i]
    if not garbage:
        if c == '{':
            depth += 1
        if c == '}':
            score += depth
            depth -= 1
        if c == '<':
            garbage = True
    else:
        if c == '>':
            garbage = False
        elif c == '!':
            i += 1
        else:
            collected += 1
    i += 1

print(score)
print(collected)
