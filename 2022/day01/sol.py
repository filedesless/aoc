import os

path = os.path.dirname(__file__)
input = open(path + '/input.txt').read()

sums = [sum(map(int, chunk.splitlines()))
        for chunk in input.split('\n\n')]

print('01a:', max(sums))
print('01b:', sum(sorted(sums)[-3:]))
