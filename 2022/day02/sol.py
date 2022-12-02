import os


path = os.path.dirname(__file__)
input = open(path + '/input.txt').readlines()

a = b = 0
for line in input:
    l, r = line.split()
    i = abs(ord('A') - ord(l))
    j = abs(ord('X') - ord(r))
    a += j + 1 + ((j - i + 1) % 3) * 3
    b += (j * 3) + ((i + j - 1) % 3) + 1

print(a, b)
