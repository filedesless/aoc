
class Transform:
    dst: int
    src: int
    len: int

    def __init__(self, _dst, _src, _len) -> None:
        self.dst = _dst
        self.src = _src
        self.len = _len


class Map:
    transforms: list[Transform]

    def __init__(self, _transforms: list[Transform]):
        self.transforms = _transforms


filename = "input"
with open(filename, encoding='utf-8') as f:
    seeds = [int(word) for word in f.readline().split()[1:]]
    f.readline()
    maps = []
    for _ in range(7):
        f.readline()
        transforms = []
        while (line := f.readline().strip()) != '':
            transforms.append(Transform(*(map(int, line.split()))))
        maps.append(Map(transforms))


def location(seed: int) -> int:
    for _map in maps:
        for transform in _map.transforms:
            if transform.src <= seed < transform.src + transform.len:
                seed = transform.dst + (seed - transform.src)
                break
    return seed


print('part 01:', min(map(location, seeds)))


class Batch:
    start: int
    n: int

    def __init__(self, start, n):
        self.start = start
        self.n = n

    def __repr__(self) -> str:
        return f"{self.start, self.n}"


def transform_batch(transform: Transform, batch: Batch) -> (list[Batch], list[Batch]):
    l, r = transform.src, transform.src + transform.len
    if l <= batch.start < r:
        offset = batch.start - l
        if batch.start + batch.n <= r:  # proper subset
            return ([Batch(transform.dst + offset, batch.n)], [])
        # right overlap
        count = transform.len - offset
        return ([Batch(transform.dst + offset, count)], [Batch(r, batch.n - count)])
    if batch.start <= l < batch.start + batch.n:  # left overlap
        moved, untouched = transform_batch(
            transform, Batch(l, batch.start + batch.n - l))
        return (moved, untouched + [Batch(batch.start, l - batch.start)])
    return ([], [batch])  # disjoint


def map_batch(ts: list[Transform], batch: Batch) -> list[Batch]:
    if not ts:
        return [batch]
    moved, untouched = transform_batch(ts[0], batch)
    return moved + [t for b in untouched for t in map_batch(ts[1:], b)]


batches = [Batch(seeds[i], seeds[i+1]) for i in range(0, len(seeds), 2)]
for _map in maps:
    batches = [t for batch in batches for t in map_batch(
        _map.transforms, batch)]
print('part 02:', min(b.start for b in batches))
