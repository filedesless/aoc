"""sol 06"""
from math import prod

FILENAME = "input"
lines = [line.strip().split()
         for line in open(FILENAME, encoding="utf8").readlines()]
races = [(int(lines[0][i]), int(lines[1][i])) for i in range(1, len(lines[0]))]


def first_win(time: int, dist: int) -> int:
    for t in range(time):
        rest = time - t
        d = rest * t
        if d > dist:
            return t


print("part 01:", prod(time - 2*first_win(time, dist) + 1 for (time, dist) in races))

lines = [int("".join(line.strip().split()[1:]))
         for line in open(FILENAME, encoding="utf8").readlines()]
time, dist = lines
print("part 02:", time - 2*first_win(time, dist) + 1)
