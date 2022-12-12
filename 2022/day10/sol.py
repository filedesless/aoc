import os

lines = """noop
addx 3
addx -5""".splitlines()

path = os.path.dirname(__file__)
lines = [line.strip() for line in open(path + '/input.txt')]


class CPU:
    cycle = x = 1
    score = 0

    def clock(self):
        self.display()
        self.cycle += 1
        if self.cycle % 40 == 20:
            signal = self.x * self.cycle
            # print(self.cycle, self.x, signal)
            self.score += signal

    def display(self):
        if self.cycle % 40 == 0:
            print()
        if self.x - 1 <= self.cycle % 40 <= self.x + 1:
            print('#', end='')
        else:
            print('.', end='')

    def run(self, lines):
        self.display()
        for line in lines:
            # print(self.cycle, line, self.x)
            self.clock()
            if line == 'noop':
                continue
            if line.startswith('addx'):
                self.x += int(line.split()[1])
                self.clock()
        print(self.score)


cpu = CPU()
cpu.run(lines)
