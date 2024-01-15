import sys
import collections

sys.setrecursionlimit(1_000_000)

# edited input manually
lines = [line.strip() for line in open("input", encoding="utf8").readlines()]
m, n = len(lines), len(lines[0])
Slin, Scol = 94, 98

grid = [['O' for _ in line] for line in lines]


def mark(lin: int, col: int):
    if not (0 <= lin < m and 0 <= col < n):
        return
    if grid[lin][col] == 'O':
        grid[lin][col] = 'I'


def path_len(lin: int, col: int, prev: str, step: int, p2: bool) -> list[(int, int)]:
    # first pass builds the grid with only connected pipes
    # grid[lin][col] = lines[lin][col]
    grid[lin][col] = '-'
    if step > 0 and (lin, col) == (Slin, Scol):
        return step
    nlin, ncol, nprev = lin, col, prev
    match (prev, lines[lin][col]):
        case ('S', '|') | ('E', 'L') | ('W', 'J'):
            nlin, nprev = lin - 1, 'S'
        case ('W', '-') | ('N', 'L') | ('S', 'F'):
            ncol, nprev = col + 1, 'W'
        case ('N', '|') | ('W', '7') | ('E', 'F'):
            nlin, nprev = lin + 1, 'N'
        case ('E', '-') | ('N', 'J') | ('S', '7'):
            ncol, nprev = col - 1, 'E'
    if p2:  # on the second pass we mark the "left" tile as we traverse
        match (prev, lines[lin][col]):
            case ('S', '|'):
                mark(lin, col - 1)
            case ('N', '|'):
                mark(lin, col + 1)
            case ('W', '-'):
                mark(lin - 1, col)
            case ('E', '-'):
                mark(lin + 1, col)
            case ('E', 'L'):
                mark(lin + 1, col)
                mark(lin, col - 1)
            case ('S', 'F'):
                mark(lin, col - 1)
                mark(lin - 1, col)
            case ('W', '7'):
                mark(lin - 1, col)
                mark(lin, col + 1)
            case ('N', 'J'):
                mark(lin, col + 1)
                mark(lin + 1, col)
    return path_len(nlin, ncol, nprev, step + 1, p2)


print('part 01:', path_len(Slin, Scol, 'E', 0, False) // 2)
# Rerun while marking 'I'
path_len(Slin, Scol, 'E', 0, True)
# Fill
seen = [[False for _ in line] for line in grid]
for x in range(m):
    for y in range(n):
        if grid[x][y] == 'I':
            queue = collections.deque([(x, y)])
            while queue:
                px, py = queue.pop()
                neighboors = [(px + 1, py), (px - 1, py),
                              (px, py + 1), (px, py - 1)]
                for (nx, ny) in neighboors:
                    if not seen[nx][ny] and grid[nx][ny] == 'O':
                        grid[nx][ny] = 'I'
                        queue.appendleft((nx, ny))
                seen[px][py] = True

for line in grid:
    print("".join(line))
print('part 02:', sum(1 for line in grid for c in line if c == 'I'))
