#!/usr/bin/env python3

#Generator A starts with 699
#Generator B starts with 124

a = 699
b = 124

def generateA() -> None:
    global a
    a = (a * 16807) % 2147483647

def generateB() -> None:
    global b
    b = (b * 48271) % 2147483647

def compare() -> bool:
    return bin(a).zfill(16)[-16:] == bin(b).zfill(16)[-16:]

def P1(count: int = 0) -> int:
    for i in range(40_000_000):
        generateA()
        generateB()
        if compare():
            count += 1
    return count

def P2(count: int = 0) -> int:
    for i in range(5_000_000):
        generateA()
        while a % 4 != 0:
            generateA()
        generateB()
        while b % 8 != 0:
            generateB()
        if compare():
            count += 1
    return count

print("P1: ", P1())
print("P2: ", P2())
