import time
from matplotlib import pyplot as plt
from util_listes import *

def temps_calcul(fonction, obj, *args):
    t0 = time.perf_counter()
    fonction(obj, *args)
    t1 = time.perf_counter()
    return t1-t0

def comparer_temps_fonctions(fonctions, *args, generate=liste_random, n=1000):
    sizes = list(range(0, n, int(n/100)))
    plt.figure(0)
    for k in range(len(fonctions)):
        times = []
        f = fonctions[k]
        for i in sizes:
            obj = generate(i)
            t = temps_calcul(f, obj, *args)
            times.append(t)
        plt.plot(sizes, times, label=f.__name__)
        plt.legend()
    plt.show()

comparer_temps_fonctions([tri_rapide, tri_fusion], n=10000)
