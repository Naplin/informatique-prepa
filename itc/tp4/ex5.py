# -*- coding: utf-8 -*-
"""
Created on Sun Oct 22 22:21:54 2023

@author: Josh
"""

def produit_scalaire(x, y):
    """
        
        entrée: deux vecteurs de même taille
        
        sortie: leur produit scalaire
        
    """
    assert len(x)==len(y)
    
    l=len(x)
    s=0
    
    for i in range(l):
        s+=x[i]*y[i]
    return s

x=[1,5,6]
y=[-2,3,3]

print(produit_scalaire(x, y))
