#!/usr/bin/env python3

programs = [ chr(97+i) for i in range(16) ]
states = [ list(programs) ]
moves = open('input').readline().strip().split(',')

def swap(x: int, y: int) -> None:
    tmp = programs[x]
    programs[x] = programs[y]
    programs[y] = tmp

def spin(n: int) -> None:
    for i in range(n):
        val = programs.pop(-1)
        programs.insert(0, val)

def dance() -> None:
    for move in moves:
        if move[0] == 's':
            spin(int(move[1:]))
        elif move[0] == 'x':
            x, y = map(int, move[1:].split('/'))
            swap(x, y)
        elif move[0] == 'p':
            x, y = move[1:].split('/')
            swap(programs.index(x), programs.index(y))

dance()
print("P1: ", "".join(programs))

while list(programs) not in states:
    states.append(list(programs))
    dance()

n = 1_000_000_000 % len(states)
print("P2: ", "".join(states[n]))
