from collections import deque
import os


path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt').readlines()]

# heightmap
H = {(i, j): int(ord('a' if c == 'S' else 'z' if c == 'E' else c) - ord('a'))
     for (i, line) in enumerate(lines)
     for (j, c) in enumerate(line)}

n = len(lines)
m = len(lines[0])

# graph (as an adjacency list)
G = {(i, j): {(k, l) for (k, l) in [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
              if 0 <= k < n and 0 <= l < m and H[(k, l)] <= h + 1}
     for ((i, j), h) in H.items()}

# starting/ending node
s = next((i, j) for (i, line) in enumerate(lines)
         for (j, c) in enumerate(line) if c == 'S')
e = next((i, j) for (i, line) in enumerate(lines)
         for (j, c) in enumerate(line) if c == 'E')


def bfs(visited: set) -> deque:
    # shortest path from any node in visited to e
    q = deque(visited)
    pred = {v: None for v in visited}
    while v := q.pop():
        if v == e:
            break
        for w in filter(lambda w: w not in visited, G[v]):
            visited.add(w)
            pred[w] = v
            q.appendleft(w)

    # backtrack pred to get path from  to e
    path = deque([v])
    while pred[v]:
        v = pred[v]
        path.appendleft(v)
    return path


print('Part a)', len(bfs({s})) - 1)
print('Part b)', len(bfs({v for v in G if H[v] == 0})) - 1)
