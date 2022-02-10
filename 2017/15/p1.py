#!/usr/bin/env python3

infected = dict() # (x,y) => bool

class Carrier:
    def __init__(self, width, height):
        self.pos = ( width // 2, height // 2 )
        self.direction = 0
    def turnLeft(self):
        self.direction = (self.direction - 1) % 4
    def turnRight(self):
        self.direction = (self.direction + 1) % 4
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
        infected[pos] = c == '#'
        x += 1
    y += 1

virus = Carrier(x, y)
count = 0
for i in range(10000):
    if virus.pos not in infected:
        infected[virus.pos] = False
    if infected[virus.pos]:
        virus.turnRight()
    else:
        virus.turnLeft()
        count += 1
    infected[virus.pos] = not infected[virus.pos]
    virus.move()

print("P1:", count)
