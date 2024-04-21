import random
import time
from matplotlib import pyplot as plt
from math import log

def remonter_bulle(L, i):
    """ Effectue une itération de l'algorithme de tri bulle """
    for j in range(0, len(L) - i - 1):
        if L[j] > L[j + 1]:
            L[j], L[j + 1] = L[j + 1], L[j]

def tri_bulle(L):
    """ Tri bulle """
    for i in range(0, len(L) - 1):
        remonter_bulle(L, i)

def liste_aleatoire(taille):
    return [random.randint(0, 100) for _ in range(taille)]

def test_tri(fonction_tri, taille):
    """ Teste la fonction de tri """
    L = liste_aleatoire(taille)
    print("Liste avant tri :", L)
    fonction_tri(L)
    print("Liste après tri :", L)
    assert L == sorted(L)

def temps_calcul(f, x):
    """ Renvoie le temps de calcul de f(x), en secondes. """
    t1 = time.perf_counter()
    f(x)
    t2 = time.perf_counter()
    return t2 - t1

def test_temps_calcul(taille):
    L = liste_aleatoire(taille)
    print("Temps de calcul du tri bulle :", temps_calcul(tri_bulle, L), "s")
    print("Temps de calcul du tri Python :", temps_calcul(sorted, L), "s")

test_temps_calcul(1000) # on voit déjà que le tri bulle est très lent

x = range(100, 2000, 10)
# y = [temps_calcul(tri_bulle, liste_aleatoire(taille)) for taille in x]
# plt.xlabel("Taille de la liste")
# plt.ylabel("Temps de calcul (s)")
# plt.title("Temps de calcul du tri bulle")
# plt.grid(True)
# plt.plot(x, y, label="Tri bulle")
# plt.legend()
# plt.show()

# y = [temps_calcul(tri_bulle, liste_aleatoire(taille)) / (taille**2) for taille in x]
# plt.xlabel("Taille de la liste")
# plt.ylabel("Temps de calcul (s) divisé par n²")
# plt.title("Temps de calcul du tri bulle, normalisé par n²")
# plt.grid(True)
# plt.plot(x, y, label="Tri Python")
# plt.legend()
# plt.show()

y = [temps_calcul(sorted, liste_aleatoire(taille)) for taille in x]
plt.xlabel("Taille de la liste")
plt.ylabel("Temps de calcul (s) divisé par n log n")
plt.title("Temps de calcul du tri Python, normalisé par n log n")
plt.grid(True)
plt.plot(x, y, label="Tri Python")
plt.legend()
plt.show()


# Exercice 3

def PGCD(a, b):
    """ Calcule le PGCD de a et b """
    while b > 0:
        print(a, b)
        a, b = b, a % b
    print('\n')
    return a

def test_PGCD():
    assert PGCD(12, 18) == 6
    assert PGCD(17, 13) == 1
    assert PGCD(125454, 561211564) == 2
    assert PGCD(12, 0) == 12
    assert PGCD(0, 12) == 12
