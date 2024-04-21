# -*- coding: utf-8 -*-
"""
Created on Sun Oct 22 22:38:31 2023

@author: Josh
"""

from math import ceil


def log10_entier(n):
    l=1
    
    while n/10**l>=1:
        l+=1
    return l


def liste_chiffres(n, b):
    l=1
    c=[]
    
    while n/b**(l-1)>=1:
        c.append((n%(b**l))//b**(l-1))
        l+=1
        
    return c


def est_premier(n):
    for i in range(2, ceil(n/2)):
        if n%i==0:
            return False
    return True


def somme_carres(n):
    s=0
    
    for i in liste_chiffres(n, 10):
        s+=i**2
    return s


def est_heureux(n):
    c=somme_carres(n)
    
    while log10_entier(c)>1:
        c=somme_carres(c)
        
    
    return c==1
        

n=19
b=2

print(est_heureux(n))

# 95689