#!/usr/bin/env python3

key = "wenycdww"
#key = "flqrgnkx" # test

def knot(key):
    l = list(range(256))
    skip = pos = 0

    lengths = [ord(c) for c in key] 
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

    return h

def flood_fill(disk, x, y, target, replacement):
    if disk[x][y] == replacement: return
    if disk[x][y] != target: return
    disk[x][y] = replacement
    if x < 127:
        flood_fill(disk, x + 1, y, target, replacement)
    if x > 0:
        flood_fill(disk, x - 1, y, target, replacement)
    if y < 127:
        flood_fill(disk, x, y + 1, target, replacement)
    if y > 0:
        flood_fill(disk, x, y - 1, target, replacement)
    return

used = groups = 0
disk = []
for row in range(128):
    ret = knot("{}-{}".format(key, row))
    bits = bin(int(ret, 16))[2:].zfill(128)
    disk.append([bit for bit in bits])
    used += bits.count("1")
print("P1: ", used)

for i in range(len(disk)):
    for j in range(len(disk[i])):
        if disk[i][j] == "1":
            flood_fill(disk, i, j, "1", "0")
            groups += 1

print("P2: ", groups)
