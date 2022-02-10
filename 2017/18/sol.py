#!/usr/bin/env python3

states = dict()
turing = []
pos = 0
cur = 'A'

class Action:
    def __init__(self, write: int, move: int, next_action: str):
        self.write = write
        self.move = move
        self.next_action = next_action
    def __repr__(self):
        return "(write: {}, move: {}, next: {})".format(self.write, self.move, self.next_action)

lines = [ line.strip() for line in open('input').readlines() if line != '\n' ][2:]

for i in range(len(lines) // 9):
    block = lines[i*9:i*9+9]
    a0 = Action(int(block[2][-2:-1]), 1 if "right" in block[3] else -1, block[4][-2:-1])
    a1 = Action(int(block[6][-2:-1]), 1 if "right" in block[7] else -1, block[8][-2:-1])
    states[block[0][-2:-1]] = [a0, a1]

for i in range(12_629_077):
    if pos >= len(turing): 
        pos = len(turing)
        turing.append(0)
    if pos < 0: 
        pos = 0
        turing.insert(0, 0)
    action = states[cur][turing[pos]]
    turing[pos] = action.write
    pos += action.move
    cur = action.next_action

print("P1:", turing.count(1))
