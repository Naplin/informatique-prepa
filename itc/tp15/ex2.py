from math import inf
import ex1

def dijkstra(g, s):
    n = len(g)
    dist = [inf] * n
    file = ex1.creer_file() 
    dist[s] = 0
    ex1.ajouter(file, 0, s)
    while not(ex1.est_vide(file)):
        x = ex1.retirer(file)
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                dist[y] = dist[x] + p
                ex1.ajouter(file, dist[y], y)
    return dist
