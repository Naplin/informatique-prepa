from collections import deque

def creer_file():
    return deque()

def est_vide_file(file):
    return len(file) == 0

def enfiler(file, x):
   deque.appendleft(file, x) 

def defiler(file):
    return file.pop()
