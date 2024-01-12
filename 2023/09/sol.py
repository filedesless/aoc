
lines = [[int(word) for word in line.split()]
         for line in open("input", encoding="UTF8").readlines()]


def solve(line: list[int], p1: bool) -> int:
    m = [line]
    while any(x != 0 for x in m[-1]):
        m.append([])
        for (a, b) in zip(m[-2], m[-2][1:]):
            m[-1].append(b - a)
    m[-1].append(0)
    for i in reversed(range(len(m) - 1)):
        if p1:
            m[i].append(m[i][-1] + m[i + 1][-1])
        else:
            m[i].insert(0, m[i][0] - m[i + 1][0])
    return m[0][-1] if p1 else m[0][0]

print("part 01:", sum(solve(line, True) for line in lines))
print("part 02:", sum(solve(line, False) for line in lines))
