# -*- coding: utf-8 -*-
"""
Created on Tue Jan 30 21:39:15 2024

@author: Josh
"""

import numpy as np
from matplotlib import pyplot as plt
import timeit
from random import randint

def recherche_dicho_naive(l, x):
    # On suppose l triée
    mil = len(l)//2
    
    if l == []:
        return False
    
    if x == l[mil]:
        return True
    elif x > l[mil]:
        return recherche_dicho_naive(l[mil+1:], x)
    else:
        return recherche_dicho_naive(l[:mil], x)
    return False

def recherche_aux(l, x, a, b):
    if a >= b:
        return False
    mil = (a+b)//2
    if l[mil] == x:
        return True
    elif x > l[mil]:
        return recherche_aux(l, x, mil+1, b)
    else:
        return recherche_aux(l, x, a, mil)
def recherche_dicho(l, x):
    return recherche_aux(l, x, 0, len(l))


def tableaux_timeit(f, N, x):
    time=np.array([])
    size=np.array([])
    for i in range(0, N, int(N/100)):
        liste = sorted([randint(0, 100) for _ in range(i)])
        t = timeit.Timer(lambda: f(liste, x)).repeat(1, 1)[0]
        time = np.append(time, t)
        size = np.append(size, i)
    return size, time

plt.close('all')

l = [1,2,3,5,6,8,8,9,14]
x = 11

size1, time1 = tableaux_timeit(recherche_dicho_naive, 1000000, x)
size2, time2 = tableaux_timeit(recherche_dicho, 1000000, x)

plt.figure(1)
plt.plot(size1, time1)
plt.show()

plt.figure(2)
plt.plot(size2, time2)
plt.show()

print(recherche_dicho(l, x))