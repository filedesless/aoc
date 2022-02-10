#!/usr/bin/env python3
# 0 -> clean, 1 -> weak, 2 -> infected, 3 -> flagged

infected = dict() # (x,y) => int

class Carrier:
    def __init__(self, width, height):
        self.pos = ( width // 2, height // 2 )
        self.direction = 0
    def turnLeft(self):
        self.direction = (self.direction - 1) % 4
    def turnRight(self):
        self.direction = (self.direction + 1) % 4
    def turnBack(self):
        self.direction = (self.direction + 2) % 4
    def move(self):
        pos = list(self.pos)
        if self.direction == 0:
            pos[1] -= 1
        elif self.direction == 1:
            pos[0] += 1
        elif self.direction == 2:
            pos[1] += 1
        elif self.direction == 3:
            pos[0] -= 1
        self.pos = tuple(pos)

x, y = 0, 0
for line in open('input'):
    x = 0
    for c in line.strip():
        pos = (x, y)
        infected[pos] = 2 if c == '#' else 0
        x += 1
    y += 1

virus = Carrier(x, y)
count = 0
for i in range(10000000):
    if virus.pos not in infected:
        infected[virus.pos] = False

    if infected[virus.pos] == 0:
        virus.turnLeft()
    elif infected[virus.pos] == 2:
        virus.turnRight()
    elif infected[virus.pos] == 3:
        virus.turnBack()
    else:
        count += 1

    infected[virus.pos] = (infected[virus.pos] + 1) % 4
    virus.move()

print("P2:", count)
