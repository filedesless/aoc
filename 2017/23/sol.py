#!/usr/bin/env python3

parents = set()
children = set()
parenthood = set()
weights = {}

def children_of(node):
    return set([child for parent, child in parenthood if parent == node])

memo = {}
def weight(node):
    total = weights[node]
    for child in children_of(node):
        if child not in memo:
            memo[child] = weight(child)
        total += memo[child]
    return total

def children_weights(node):
    return list(map(weight, children_of(node)))

def is_balanced(node):
    return len(set(children_weights(node))) <= 1

def find_unbalanced(node):
    for child in children_of(node):
        if not is_balanced(child):
            return find_unbalanced(child)
    return node

def find_supposed_weight(node):
    s = set(children_weights(node))
    m = max(s)
    diff = abs(s.pop() - s.pop())
    for child in children_of(node):
        if weight(child) == m:
            return weights[child] - diff

for line in open('input'):
    arr = line.split()
    parents.add(arr[0])
    weights[arr[0]] = int(arr[1][1:-1])
    if '->' in line:
        for child in arr[arr.index('->')+1:]:
            children.add(child.strip(","))
            parenthood.add( (arr[0], child.strip(",")) )

print("Bottom is {}".format(parents - children))
unbalanced = find_unbalanced("bpvhwhh")
print(find_supposed_weight(unbalanced))
