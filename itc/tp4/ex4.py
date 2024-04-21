# -*- coding: utf-8 -*-
"""
Created on Fri Oct 13 14:50:54 2023

@author: jtamisier
"""

from math import ceil



def coupe_en_deux(l):
    
    l1=l[:len(l)//2]
    l2=l[len(l)//2+1:]
    
    return l1, l2



def coupe_en_k(l, k):
    
    coupe = []
    
    s = len(l)
    h = ceil(s/k)
    
    a = 0
    b = 0
    
    for i in range(k):        
        if i == s%k and  s%k != 0:
            h-=1
        
        a = b
        b = a+h
        
        l0 = l[a:b]
        
        print(a, b, h)
        
        coupe.append(l0)
    return coupe
    
    

def premiere_phrase(s):
    k=len(s)
    
    for i in range(k):
        if s[i] == '.':
            k=i+1
            break
    
    return s[:k]



liste = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]
ph = "Bonjour, je suis joshua. Je fais de la programmation. J'aime ça."


print(premiere_phrase(ph))

