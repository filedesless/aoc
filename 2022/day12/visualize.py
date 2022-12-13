
class Visualize:
    grid = []

    def __init__(self, H, s, e, n, m):
        self.grid = [[str(H[(i, j)]//3) for j in range(m)] for i in range(n)]
        self.grid[s[0]][s[1]] = 'S'
        self.grid[e[0]][e[1]] = 'E'

    def show(self, path):
        print('\033c')
        print('path len:', len(path))
        for (i, c) in enumerate(path):
            if 0 < i < len(path) - 1:
                x, y = path[i]
                match (x - path[i + 1][0], y - path[i + 1][1]):
                    case (1, 0): c = '^'
                    case (-1, 0): c = 'v'
                    case (0, 1): c = '<'
                    case (0, -1): c = '>'
                self.grid[x][y] = '\033[94m' + c + '\033[0m'

        print('\n'.join(''.join(line) for line in self.grid))
