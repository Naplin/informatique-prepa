# -*- coding: utf-8 -*-
"""
Created on Fri Feb  2 14:26:36 2024

@author: Josh
"""

from random import randint

import sys

sys.path.append( 'C:/Users/occasion/Documents/Python Scripts/ITC' )
import temps_calcul_fonctions as tps

def liste_random(n):
    return [randint(0, 100) for _ in range(n)]

def diviser(l):
    mil = len(l)//2
    return l[:mil], l[mil:]

def fusion(l1, l2):
    i, j = 0, 0
    t = len(l1) + len(l2)
    if l1 == []:
        return l2
    if l2 == []:
        return l1
    l = []
    while len(l)<t:
        a, b = l1[i:], l2[j:]
        if a == []:
            l.append(b[0])
            j+=1
        elif b == []:
            l.append(a[0])
            i+=1
        elif a[0] > b[0]:
            l.append(b[0])
            j+=1
        elif a[0] <= b[0]:
            l.append(a[0])
            i+=1
        
    return l

def tri_partition_fusion(l):
    a, b = diviser(l)
    if len(b) < 1 or len(a) < 1:
        return fusion(a, b)
    else:
        return fusion(tri_partition_fusion(a), tri_partition_fusion(b))

l = liste_random(100)

l0 = tri_partition_fusion(l)
print(l0)

tps.graphes([tri_partition_fusion], 10000)


# l1, l2 = diviser(l)
# l1, l2 = sorted(l1), sorted(l2)

# l3 = fusion(l1, l2)
# print(l3)

# assert l0 == l3