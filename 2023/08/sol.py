from itertools import cycle
from math import lcm


lines = [line.strip() for line in open("input", encoding="utf8")]

graph: dict[str, (str, str)] = {}
for line in lines[2:]:
    words = ["".join(filter(str.isalnum, word)) for word in line.split()]
    graph[words[0]] = (words[2], words[3])

currents = [node for node in graph if node.endswith('A')]
j = currents.index('AAA')
steps = [0 for _ in currents]
for (i, _) in enumerate(currents):
    for direction in cycle(lines[0]):
        currents[i] = graph[currents[i]][0 if direction == "L" else 1]
        steps[i] += 1
        if currents[i].endswith('Z'):
            break

print('part 01:', steps[j])
print('part 02:', lcm(*steps))
