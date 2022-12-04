import os


path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]

contained = overlap = 0
for line in lines:
    [a, b, c, d] = [int(d) for p in line.split(',') for d in p.split('-')]
    if (a >= c and b <= d) or (a <= c and b >= d):
        contained += 1
    elif (a <= d and a >= c) or (b >= c and b <= d):
        overlap += 1

print(contained, contained + overlap)
