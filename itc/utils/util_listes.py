from random import randint

def liste_random(N):
    return [randint(0, 100) for _ in range(N)]

# Tri rapide
def partition(L, p):
    L1, L2 = [], []
    for k in range(len(L)):
        if L[k] < p:
            L1.append(L[k])
        else:
            L2.append(L[k])
    return L1, L2

def tri_rapide(L):
    if L == []:
        return []
    else:
        p = L[0]
        L1, L2 = partition(L[1:], p)
        L1_trie = tri_rapide(L1)
        L2_trie = tri_rapide(L2)
        return L1_trie + [p] + L2_trie

# Tri fusion
def diviser(L):
    mil = len(L) // 2
    return L[:mil], L[mil:]

def fusion(L1, L2):
    L = []
    i, j = 0, 0
    while i < len(L1) and j < len(L2):
        if L1[i] < L2[j]:
            L.append(L1[i])
            i = i + 1
        else:
            L.append(L2[j])
            j = j + 1
    # ici, il va rester des éléments soit dans L1 soit dans L2 que l'on n'a pas encore ajoutés à L
    for k in range(i, len(L1)):
        L.append(L1[k])
    for k in range(j, len(L2)):
        L.append(L2[k])
    return L

def tri_fusion(L):
    if len(L) <= 1:
        return L
    else:
        L1, L2 = diviser(L)
        L1_trie = tri_fusion(L1)
        L2_trie = tri_fusion(L2)
        return fusion(L1_trie, L2_trie)


