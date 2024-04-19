import heapq

def creer_file():
    return []

def ajouter(file, priorite, element):
    heapq.heappush(file, (priorite, element))

def retirer(file):
    _, x = heapq.heappop(file)
    return x

def est_vide(file):
    return file == []

