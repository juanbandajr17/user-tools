# Question:
# Write a function Brackets(int n) that prints all combinations of well-formed brackets. For Brackets(3) the output would be ((())) (()()) (())() ()(()) ()()()


def brackets(n):
    if n == 0:
        return ['']
    if n == 1:
        return ['()']
    combos = []
    bees = brackets(n-1)
    combos.extend(['('+b+')'for b in bees])
    combos.extend(['()'+b for b in bees])
    combos.extend([b+'()' for b in bees])
    return sorted(list(set(combos)))

print(0, brackets(0))
print(1, brackets(1))
print(2, brackets(2))
print(3, brackets(3))
print(3, brackets(4))
