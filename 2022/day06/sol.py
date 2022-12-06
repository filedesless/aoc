from collections import deque
import os


path = os.path.dirname(__file__)
input = open(path + '/input.txt').read()


def solve(n: int) -> int:
    ringbuf = deque(input[:n - 1], maxlen=n)
    for i in range(n - 1, len(input)):
        ringbuf.append(input[i])
        if len(set(ringbuf)) == n:
            return i + 1


print(solve(4))
print(solve(14))
