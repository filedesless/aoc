import itertools
import math

lines = [line.strip() for line in open("input").readlines()]

# mapping numbers coordinates in the input
atlas: dict[(int, int), str] = dict()
for (row, line) in enumerate(lines):
    col = 0
    while col < len(line):
        if line[col].isdigit():
            s = "".join(list(itertools.takewhile(str.isdigit, line[col:])))
            atlas[(row, col)] = s
            col += len(s)
        else:
            col += 1

part_sum, ratio_sum = 0, 0
for (row, line) in enumerate(lines):
    for (col, c) in enumerate(line):
        if not c.isdigit() and c != '.':
            # assuming part numbers are unique around a given symbol
            parts = {int(s) for ((x, y), s) in atlas.items()
                    for k in range(y, y + len(s))
                    if 0 <= row - 1 <= x <= row + 1 < len(lines)
                    if 0 <= col - 1 <= k <= col + 1 < len(lines[0])}
            part_sum += sum(parts)
            if c == '*' and len(parts) == 2:
                ratio_sum += math.prod(parts)

print("part 01: ", part_sum)
print("part 02: ", ratio_sum)
