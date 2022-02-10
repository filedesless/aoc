#!/usr/bin/env python3

nodes, plane = [], []

for line in open('input'):
    plane.append([])
    for c in line.strip('\n'):
        plane[len(plane)-1].append(c)

direction = 2
x, y = 0, 0
# start pos
for i in range(len(plane[0])):
    if plane[0][i] == '|':
        x = i

steps = 0
while x >= 0 and x < len(plane[0]) and y >= 0 and y < len(plane):
    if plane[y][x] == '+':
        if direction % 2 == 0:
            if x < len(plane[0]) - 1 and ( plane[y][x+1] == '-' or plane[y][x+1].isalpha() ):
                direction = 1
            elif x > 0 and ( plane[y][x-1] == ' ' or plane[y][x-1] ):
                direction = 3
        else:
            if y < len(plane) - 1 and ( plane[y+1][x] == '|' or plane[y+1][x].isalpha() ):
                direction = 2
            elif y > 0 and ( plane[y-1][x] == '|' or plane[y-1][x].isalpha() ):
                direction = 0

    if plane[y][x].isalpha():
        nodes.append(plane[y][x])

    if plane[y][x] == ' ':
        break

    if direction == 0: y -= 1
    elif direction == 1: x += 1
    elif direction == 2: y += 1
    elif direction == 3: x -= 1
    steps += 1


print("P1:", "".join(nodes))
print("P2:", steps)
