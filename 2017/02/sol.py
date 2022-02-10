#!/usr/bin/env python3

l = list(range(256))
skip = pos = 0

lengths = [ord(i) for i in open('input').readline().strip()] 
lengths += [17, 31, 73, 47, 23]

for n in range(64):
    for length in lengths:
        s = []
        for i in range(length):
            index = (pos + i) % len(l)
            s.append(l[index])
        s.reverse()
        for i in range(length):
            index = (pos + i) % len(l)
            l[index] = s[i]
        pos += length + skip
        skip += 1

h = ""
for i in range(16):
    d = 0
    for a in l[i*16:i*16+16]:
        d ^= a
    h += "{:02x}".format(d)

print(h)
