from collections import deque
from math import inf
from exo_graphe import g, labyrinthe_lst

def est_vide_file(file):
    return len(file) == 0

def cloner(file):
    file2 = deque()
    for _ in range(len(file)):
        x = file.pop()
        file2.appendleft(x)
        file.appendleft(x)
    return file2

def miroir(file):
    file2 = deque()
    for _ in range(len(file)):
        x = file.pop()
        #TODO
        file.appendleft(x)

def parcours_largeur(g, s):
    file = deque()
    file.appendleft(s)
    n = len(g)
    deja_enfile = [False] * n
    deja_enfile[s] = True
    while not est_vide_file(file):
        x = file.pop()
        for y in g[x]:
            if not deja_enfile[y]:
                file.appendleft(y)
                deja_enfile[y] = True
    return deja_enfile

def est_accessible(g, s1, s2):
    return parcours_largeur(g, s1)[s2]

def est_connexe(g):
    for i in range(len(g)):
        if False in parcours_largeur(g, i):
            return False
    return True

def distance(g, s):
    file = deque()
    file.appendleft(s)
    n = len(g)

    distance = [inf] * n
    distance[s] = 0

    while not est_vide_file(file):
        x = file.pop()
        for y in g[x]:
            if distance[y] == inf:
                distance[y] = distance[x] + 1
                file.appendleft(y)
    return distance


