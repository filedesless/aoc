import os


path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]


def score(i):
    if i == i.lower():
        return ord(i) - ord('a') + 1
    else:
        return ord(i) - ord('A') + 27


total = 0
for line in lines:
    n = len(line) // 2
    l, r = set(line[:n]), set(line[n:])
    for i in l & r:
        total += score(i)

print(total)

n = len(lines)
total = 0
for i in range(0, n, 3):
    a, b, c = map(set, lines[i:i+3])
    for i in a & b & c:
        total += score(i)

print(total)
