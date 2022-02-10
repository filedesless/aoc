#!/usr/bin/env python3

ids = set()
pipes = set()

def connected_to(node):
    return set([y for x, y in pipes if x == node])

def talk_to(node, friends=set()):
    newfriends = connected_to(node) - friends
    if newfriends == set():
        return set([node])
    friends |= newfriends
    for friend in newfriends:
        friends |= talk_to(friend, friends)
    return friends

for line in open('input'):
    line = line.split()
    ids.add(int(line[0]))
    for x in line[2:]:
        ids.add(int(x.strip(',')))
        pipes.add( (int(line[0]), int(x.strip(','))) )

print("P1: ", len(talk_to(0)))

rest = ids
i = 0
while rest != set():
    rest -= talk_to(rest.pop())
    i += 1
print("P2: ", i)
