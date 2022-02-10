#!/usr/bin/env python3

ports = set([ tuple(map(int, line.strip().split('/'))) for line in open('input') ])
used = []
strengths = dict()
lengths = dict()

def addPort(end:int = 0) -> list:
    for port in ports - set(used):
        if end in port:
            used.append(port)
            yield used
            s = set(port)
            next_end = s.pop() if len(s) == 1 else (s - {end}).pop()
            yield from addPort(next_end)
            used.remove(port)

for bridge in addPort():
    strength = sum(list(map(sum, bridge)))
    index = tuple(bridge)
    strengths[index] = strength
    lengths[index] = len(bridge)

greatest = max(strengths.values())
print("P1:", greatest)

longest = max(lengths.values())
l = [ strengths[bridge] for bridge, length in lengths.items() if length == longest ]
print("P2:", max(l))
