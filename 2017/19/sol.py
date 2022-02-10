#!/usr/bin/env python3

num = 312051

class Point:
    def __init__(self, x, y, val):
        self.x = x
        self.y = y
        self.value = val

    def __str__(self):
        return "[{}, {}]: {}".format(self.x, self.y, self.value)

def get_top_left(num):
    layer = 0
    while 4 * layer ** 2 + 1 < num:
        layer += 1
    return Point(-layer, layer, 4 * layer ** 2 + 1)

def walk_from_top_left(pt):
    layer = pt.y
    while pt.x < layer:
        if pt.value == num:
            return pt
        pt.x += 1
        pt.value -= 1
    while pt.y > -(layer - 1):
        if pt.value == num:
            return pt
        pt.y -= 1
        pt.value -= 1
    while pt.x > -(layer - 1):
        if pt.value == num:
            return pt
        pt.x -= 1
        pt.value -= 1
    while pt.y < -(layer - 1):
        if pt.value == num:
            return pt
        pt.y += 1
        pt.value -= 1

def get_dist(p1, p2):
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)

p = get_top_left(num)
p = walk_from_top_left(p)
print(p)
print(get_dist(p, Point(0, 0, 1)))

# P2: https://oeis.org/A141481/b141481.txt
