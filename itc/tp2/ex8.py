# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
from math import log, floor

def log2_entier(n):
    p=floor(log(n,2))
    return p

def test():
    assert log2_entier(1)==0
    assert log2_entier(8)==3
    assert log2_entier(9)==3

print(log2_entier(1000))
print(log2_entier(10**10))
    
