from util_graphes import small, large
from math import inf
import util_files_priorite
from util_fonctions import temps_calcul

def dijkstra(g, s):
    n = len(g)
    dist = [inf] * n
    file = util_files_priorite.creer_file() 
    dist[s] = 0
    util_files_priorite.ajouter(file, 0, s)
    while not(util_files_priorite.est_vide(file)):
        x = util_files_priorite.retirer(file)
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                dist[y] = dist[x] + p
                util_files_priorite.ajouter(file, dist[y], y)
    return dist

def plus_court_chemin(g, a, b):
    n = len(g)
    dist = [inf] * n
    pred = [[] for _ in range(n)]
    file = util_files_priorite.creer_file()
    dist[a] = 0
    util_files_priorite.ajouter(file, 0, a)
    while not(util_files_priorite.est_vide(file)):
        x = util_files_priorite.retirer(file)
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                pred[y] = pred[x] + [x]
                dist[y] = dist[x] + p
                util_files_priorite.ajouter(file, dist[y], y)
    return pred[b]
