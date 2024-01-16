from itertools import combinations


lines = [line.strip() for line in open('input', encoding='utf8').readlines()]

# expand the universe
emptyrows = [i for i in range(len(lines)) if all(c == '.' for c in lines[i])]
emptycols = [i for i in range(len(lines[0])) if all(
    lines[j][i] == '.' for j in range(len(lines)))]

# find galaxies
points = {(lin, col) for lin in range(len(lines))
          for col in range(len(lines[lin])) if lines[lin][col] == '#'}


def dist_axis(a: int, b: int, steps: [int], skip: int) -> int:
    if a > b:
        return dist_axis(b, a, steps, skip)
    return abs(a - b) + sum(skip for step in steps if a <= step <= b)


def dist_p1(a: (int, int), b: (int, int)) -> int:
    return dist_axis(a[0], b[0], emptyrows, 1) + dist_axis(a[1], b[1], emptycols, 1)


print('part 01:', sum(dist_p1(a, b) for (a, b) in combinations(points, 2)))


def dist_p2(a: (int, int), b: (int, int)) -> int:
    return dist_axis(a[0], b[0], emptyrows, 1000000 - 1) + dist_axis(a[1], b[1], emptycols, 1000000 - 1)


print('part 02:', sum(dist_p2(a, b) for (a, b) in combinations(points, 2)))
