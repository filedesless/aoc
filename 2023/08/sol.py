from itertools import cycle


lines = [line.strip() for line in open("input", encoding="utf8")]

graph = dict()
for line in lines[2:]:
    words = ["".join(filter(str.isalpha, word)) for word in line.split()]
    graph[words[0]] = (words[2], words[3])
print(graph)

current, i = 'AAA', 0
for direction in cycle(lines[0]):
    current = graph[current][0 if direction == "L" else 1]
    i += 1
    if current == 'ZZZ':
        break

print(i)