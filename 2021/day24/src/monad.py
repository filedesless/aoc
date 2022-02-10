# first solution draft in python

from typing import List, Optional

serial = [1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9]
k = [(1, 12, 4),
     (1, 11, 11),
     (1, 13, 5),
     (1, 11, 11),
     (1, 14, 14),
     (26, -10, 7),
     (1, 11, 11),
     (26, -9, 4),
     (26, -3, 6),
     (1, 13, 5),
     (26, -5, 9),
     (26, -10, 12),
     (26, -4, 14),
     (26, -5, 14)]


def digit(z: int, w: int, i: int) -> int:
    rem = z % 26
    z //= k[i][0]
    if rem + k[i][1] != w:
        z = (z * 26) + (k[i][2] + w)
    return z


def monad(serial: List[int]) -> int:
    z = 0
    for (i, w) in enumerate(serial):
        z = digit(z, w, i)
    return z


def search(i: int, z: int, path: List[int]) -> Optional[List[int]]:
    if i == 14:
        if z == 0:
            return path
        else:
            return None

    for w in range(9, 0, -1):
        if k[i][0] == 26:
            if (z % 26) + k[i][1] != w:
                continue
        p = search(i + 1, digit(z, w, i), path + [w])
        if p:
            return p
    return None


assert 2538195 == monad(serial)

print(search(0, 0, []))
