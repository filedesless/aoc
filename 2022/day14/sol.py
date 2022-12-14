from typing import Tuple
import os

# lines = """498,4 -> 498,6 -> 496,6
# 503,4 -> 502,4 -> 502,9 -> 494,9""".splitlines()
path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]


def point(s: str) -> Tuple[int, int]:
    l, r = s.split(',')
    return (int(l), int(r))


# build the grid
grid = set()
for line in lines:
    points = line.split(' -> ')
    for k in range(len(points) - 1):
        l, r = sorted([point(points[k]), point(points[k+1])])
        for i in range(l[0], r[0]+1):
            for j in range(l[1], r[1]+1):
                grid.add((i, j))

lowest = max(y for (_, y) in grid)
n = len(grid)
a = False

# simulate sand falling
x, y = 500, 0
while (500, 0) not in grid:
    if y == lowest + 1:
        if not a:
            print('Part a:', len(grid) - n)
            a = True
        grid.add((x, y))
        x, y = 500, 0
    elif (x, y + 1) not in grid:
        y += 1
    elif (x - 1, y + 1) not in grid:
        x -= 1
        y += 1
    elif (x + 1, y + 1) not in grid:
        x += 1
        y += 1
    else:
        grid.add((x, y))
        x, y = 500, 0

print('Part b:', len(grid) - n)
