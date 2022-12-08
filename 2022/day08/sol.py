from math import prod
import os


input = """30373
25512
65332
33549
35390""".splitlines()

path = os.path.dirname(__file__)
input = open(path + '/input.txt')
grid = [[int(c) for c in line.strip()] for line in input]
n = len(grid)


def los(row, col):
    l = grid[row][col-1::-1]
    r = grid[row][col+1:]
    u = [grid[i][col] for i in reversed(range(row))]
    d = [grid[i][col] for i in range(row+1, n)]
    return [l, r, u, d]


def visible(row: int, col: int) -> bool:
    def p(trees): return all([tree < grid[row][col] for tree in trees])
    return any([row == n - 1, col == n - 1, row == 0, col == 0]
               + [p(trees) for trees in los(row, col)])


print(sum([visible(i, j) for i in range(n) for j in range(n)]))


def score(row: int, col: int) -> int:
    def p(trees):
        i = 0
        for tree in trees:
            i += 1
            if tree >= grid[row][col]:
                break
        return i

    return prod(p(trees) for trees in los(row, col))


print(max([score(i, j) for i in range(n) for j in range(n)]))
