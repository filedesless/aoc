import os

# from problem input
a = [
    ['V', 'C', 'D', 'R', 'Z', 'G', 'B', 'W'],
    ['G', 'W', 'F', 'C', 'B', 'S', 'T', 'V'],
    ['C', 'B', 'S', 'N', 'W'],
    ['Q', 'G', 'M', 'N', 'J', 'V', 'C', 'P'],
    ['T', 'S', 'L', 'F', 'D', 'H', 'B'],
    ['J', 'V', 'T', 'W', 'M', 'N'],
    ['P', 'F', 'L', 'C', 'S', 'T', 'G'],
    ['B', 'D', 'Z'],
    ['M', 'N', 'Z', 'W']
]
b = [stack[:] for stack in a]


path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]

for line in lines:
    n, s, d = [int(i) for i in line.split(' ') if i.isnumeric()]
    for i in b[s-1][-n:]:
        a[d-1].append(a[s-1].pop())
        b[d-1].append(i)
    b[s-1] = b[s-1][:-n]

print("".join([stack[-1] for stack in a]))
print("".join([stack[-1] for stack in b]))
