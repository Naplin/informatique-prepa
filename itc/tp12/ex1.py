g1 = [[1, 3], [0, 2, 3, 4], [1, 4], [0, 1], [1, 2]]
g2 = [[1, 3], [2, 4], [4], [1], []]

def degreNO(l_g, s):
    return len(l_g[s])

# sur un graphe orienté, degreNO renvoie le degre sortant

def degre_O_entrant(l_g, s):
    d = 0
    for i in l_g:
        if s in i:
            d += 1
    return d

def matrice_vers_liste(m):
    l = []
    for i in range(len(m)):
        l0 = []
        for j in range(len(m[i])):
            if m[i][j]:
                l0.append(j)
        l.append(l0)
    return l

def liste_vers_matrice(l):
    n = len(l)
    m = []
    for i in l:
        l0 = []
        for j in range(n):
            l0.append(j in i)
        m.append(l0)
    return m

# correction
def matrice_vers_liste_c(m):
    n = len(m)
    l = [[] for _ in range(n)]
    for i in range(len(l)):
        for j in range(len(m[i])):
            if m[i][j]:
                l[i].append(j)
    return l

def degreNO_mat(m, s):
    d = 0
    for i in m[s]:
        if i:
            d+=1
    return d

def degre_entrant_mat(m, s):
    d = 0
    for i in m:
        if i[s]:
            d += 1
    return d

print(liste_vers_matrice(g1))
