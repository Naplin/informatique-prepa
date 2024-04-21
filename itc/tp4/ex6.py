# -*- coding: utf-8 -*-
"""
Created on Sun Oct 22 22:26:23 2023

@author: Josh
"""

from math import sin, pi
from matplotlib import pyplot as plt

plt.close()

def subdivision(a, b, n):
    return[a+i*(b-a)/n for i in range(n+1)]

liste_x = subdivision(0, 2*pi, 50)
liste_y = [sin(i) for i in liste_x]

plt.plot(liste_x, liste_y)
plt.show()