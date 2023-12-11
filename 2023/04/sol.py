import functools


filename = "input"
lines = [line.strip() for line in open(filename).readlines()]

# how many winning numbers we have for a given card
score: list[int] = []
for line in lines:
    [l, r] = line.split('|')
    score.append(len([n for n in r.split() if n in l.split()[2:]]))

print('part 01:', sum(2 ** (n - 1) if n > 0 else 0 for n in score))


@functools.cache
def copies_won(i: int) -> int:
    """the copies i won and the copies they won"""
    return score[i] + sum(copies_won(i+1+j) for j in range(score[i]))


n = len(score)
print('part 02:', n + sum(map(copies_won, range(n))))
