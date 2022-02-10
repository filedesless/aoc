#!/usr/bin/env python3

pos, vel, acc = {}, {}, {}

# reads input file into sets: index => (x, y, z)
def start():
    for i, line in enumerate(open('input')):
        arr = line.split(' ')
        pos[i] = tuple(map(int, arr[0][3:-2].split(',')))
        vel[i] = tuple(map(int, arr[1][3:-2].split(',')))
        acc[i] = tuple(map(int, arr[2][3:-2].split(',')))

# index of shortest sum of absolute position values
def nearest():
    distances = { i: sum(map(abs, p)) for i, p in pos.items() }
    return min(distances, key=distances.get)

# sets; (x, y, z) => [ indexes ]
def collide():
    col = {}
    for i in pos.keys():
        if pos[i] in col:
            col[pos[i]].append(i)
        else:
            col[pos[i]] = [i]
    for l in col.values():
        if len(l) > 1:
            for i in l:
                del pos[i]
                del vel[i]
                del acc[i]

# run a tick in the simulation
def tick(P2=False):
    for i in pos.keys():
        vel[i] = tuple(map(sum, zip(vel[i], acc[i])))
        pos[i] = tuple(map(sum, zip(pos[i], vel[i])))
    if P2: collide()

start()
for i in range(500):
    tick()
print("P1:", nearest())

start()
for i in range(500):
    tick(True)
print("P2:", len(pos))
