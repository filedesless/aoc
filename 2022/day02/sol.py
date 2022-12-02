import os


path = os.path.dirname(__file__)
input = open(path + '/input.txt').readlines()

score = [3, 6, 0]
a = b = 0
for line in input:
    l, r = line.split()
    i = abs(ord('A') - ord(l))
    j = abs(ord('X') - ord(r))
    a += j + 1 + score[(j - i) % 3]
    b += score[(j + 2) % 3] + (((i + j) % 3) or 3)

print(a, b)
