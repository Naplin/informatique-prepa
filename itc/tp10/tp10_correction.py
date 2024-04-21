import random as rd
import time
from matplotlib import pyplot as plt
import math


def generer_liste(nb_elements, lim_elements):
    """Génère une liste de taille nb_elements contenant des entiers distincts compris entre 0 et lim_elements-1"""
    assert nb_elements <= lim_elements
    return rd.sample(range(lim_elements), nb_elements)


# Par exemple, generer_liste(5, 10) peut renvoyer [3, 7, 2, 0, 9]

# Exercice 1


def indice_min(L, i):
    """Renvoie l'indice du minimum de la liste L à partir de l'indice i"""
    indice_min = i
    for j in range(i + 1, len(L)):
        if L[j] < L[indice_min]:
            indice_min = j
    return indice_min


def tri_selection(L):
    """Trie la liste L par sélection"""
    for i in range(len(L) - 1):
        j = indice_min(L, i)
        L[i], L[j] = L[j], L[i]


def remonter_bulle(L, i):
    """Effectue une itération de l'algorithme de tri bulle"""
    for j in range(0, len(L) - i - 1):
        if L[j] > L[j + 1]:
            L[j], L[j + 1] = L[j + 1], L[j]


def tri_bulle(L):
    """Tri bulle"""
    for i in range(0, len(L) - 1):
        remonter_bulle(L, i)


def est_trie(L):
    for k in range(len(L) - 1):
        if L[k] > L[k + 1]:
            return False
    return True


def insere(L, k):
    """Insère l'élément L[k] dans la partie triée de L"""
    i = k
    while i > 0 and L[i - 1] > L[i]:
        L[i - 1], L[i] = L[i], L[i - 1]
        i = i - 1


def tri_insertion(L):
    """Tri par insertion"""
    for k in range(1, len(L)):
        insere(L, k)


# Exercice 2


def temps_calcul(fonction_tri, taille):
    """Renvoie le temps de calcul de la fonction fonction_tri sur une liste de taille éléments"""
    L = generer_liste(taille, taille)
    t1 = time.perf_counter()
    fonction_tri(L)
    t2 = time.perf_counter()
    return t2 - t1


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


def test_tri_fusion():
    L = generer_liste(100, 1000)
    L_trie = tri_fusion(L)
    assert est_trie(L_trie)
    assert L_trie == sorted(L)


test_tri_fusion()

x = [k for k in range(100, 10000, 100)]
y_fusion = [temps_calcul(tri_fusion, k) for k in x]
plt.plot(x, y_fusion)
plt.savefig("tp8_tri_fusion.png")
# Cela ressemble vachement à du n, on va donc essayer de normaliser par n.

plt.clf()
z = [y_fusion[k] / (x[k]) for k in range(len(x))]
plt.plot(x, z)
plt.savefig("tp8_tri_fusion_normalise_n.png")

# On voit que cela continue à être relativement croissant, on va donc essayer de normaliser par n log(n)

plt.clf()
z = [y_fusion[k] / (x[k] * math.log(x[k])) for k in range(len(x))]
plt.plot(x, z)
plt.savefig("tp8_tri_fusion_normalise_n_log_n.png")

# Exercice 4

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


def test_tri_rapide():
    L = generer_liste(100, 1000)
    L_trie = tri_rapide(L)
    assert est_trie(L_trie)
    assert L_trie == sorted(L)


test_tri_rapide()

x = [k for k in range(100, 10000, 100)]
y_tri_rapide = [temps_calcul(tri_rapide, k) for k in x]
plt.clf()
plt.plot(x, y_tri_rapide)
plt.savefig("tp8_tri_rapide.png")
# Idem, on normalise par n log(n)

plt.clf()
z = [y_tri_rapide[k] / (x[k] * math.log(x[k])) for k in range(len(x))]
plt.plot(x, z)
plt.savefig("tp8_tri_rapide_normalise_n_log_n.png")

# On peut comparer les deux tris
plt.clf()
plt.plot(x, y_tri_rapide, label="tri rapide")
plt.plot(x, y_fusion, label="tri fusion")
plt.legend()
plt.savefig("tp8_tri_rapide_vs_tri_fusion.png")
