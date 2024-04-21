from random import randint

def liste_random(N):
    return [randint(0, 100) for _ in range(N)]

def partition(l, p):
    a, b = [], []
    for i in l:
        if i < p:
            a.append(i)
        else:
            b.append(i)
    return a, b

def tri_rapide(l):
    a, b = partition(l, l[0])
    if len(a) < 1 or len(b) < 1:
        return a + b
    else:
        return tri_rapide(a) + tri_rapide(b)

l = liste_random(100)

print(l)
print(tri_rapide(l))
