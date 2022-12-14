from typing import List, Union, Optional
import os
import json
from functools import cmp_to_key

Num = Union[int, List['Num']]
lines = """[1, 1, 3, 1, 1]
[1, 1, 5, 1, 1]

[[1], [2, 3, 4]]
[[1], 4]

[9]
[[8, 7, 6]]

[[4, 4], 4, 4]
[[4, 4], 4, 4, 4]

[7, 7, 7, 7]
[7, 7, 7]

[]
[3]

[[[]]]
[[]]

[1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
[1, [2, [3, [4, [5, 6, 0]]]], 8, 9]""".splitlines()
path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]

packets = [json.loads(line) for line in lines if line.strip() != ""]
pairs = [(packets[i], packets[i+1])
         for i in range(0, len(packets), 2)]


def cmp(l: Num, r: Num) -> int:
    match l, r:
        case int(l), int(r) if l == r: return 0
        case int(l), int(r): return -1 if l < r else 1
        case int(l), _: return cmp([l], r)
        case _, int(r): return cmp(l, [r])
        case [x, *xs], [y, *ys]:
            t = cmp(x, y)
            if t == 0:
                return cmp(xs, ys)
            return t
        case [], []: return 0
        case [], _: return -1
        case _, []: return 1


assert cmp([1, 1, 3, 1, 1], [1, 1, 5, 1, 1]) == -1
assert cmp([[1], [2, 3, 4]], [[1], 4]) == -1
assert cmp([9], [[8, 7, 6]]) == 1
assert cmp([[4, 4], 4, 4], [[4, 4], 4, 4, 4]) == -1
assert cmp([7, 7, 7, 7], [7, 7, 7]) == 1
assert cmp([], [3]) == -1
assert cmp([[[]]], [[]]) == 1
assert cmp([1, [2, [3, [4, [5, 6, 7]]]], 8, 9],
           [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]) == 1

print(sum(i + 1 for (i, (l, r)) in enumerate(pairs) if cmp(l, r) == -1))
l = sorted(packets + [[[2]], [[6]]], key=cmp_to_key(cmp))
print((l.index([[2]])+1) * (l.index([[6]])+1))
