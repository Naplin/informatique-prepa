import heapq
from math import dist, inf, sqrt
from matplotlib import pyplot as plt
from util_graphes import dijkstra, large
import util_files_priorite
from ex2 import plus_court_chemin

# Rappel TP 14
def ligne_to_sommet_arete(ligne):
    """Transforme une ligne en un sommet et une arête"""
    s_sommet, s_arrete = ligne[:-1].split()
    return int(s_sommet), int(s_arrete)


def ligne_to_arc(ligne):
    """Transforme une ligne en un arc"""
    s_sommet1, s_sommet2, s_poids = ligne[:-1].split()
    return int(s_sommet1), int(s_sommet2), int(s_poids)


def lire_graphe(nom_fichier):
    """Lit un graphe depuis un fichier"""
    file = open(nom_fichier, "r")
    ligne_sommet_arete = file.readline()
    nb_sommets, nb_aretes = ligne_to_sommet_arete(ligne_sommet_arete)
    graphe = [[] for k in range(nb_sommets)]
    for k in range(nb_aretes):
        ligne_arc = file.readline()
        sommet1, sommet2, poids = ligne_to_arc(ligne_arc)
        graphe[sommet1].append((sommet2, poids))
    file.close()
    return graphe

def read_positions(filename):
    """Lit les positions des sommets dans un fichier"""
    file = open(filename, "r")
    line_nb = int(file.readline()[:-1])
    positions = []
    for _ in range(line_nb):
        line = file.readline()[:-1]
        _, x, y = line.split()
        positions.append((int(x), int(y)))
    file.close()
    return positions


def afficher_positions(positions):
    """Affiche les positions des sommets"""
    lst_x, lst_y = [], []
    for x, y in positions:
        lst_x.append(x)
        lst_y.append(y)
    plt.plot(lst_x, lst_y, marker='.', markersize=1, linestyle='', color='black')

def afficher_positions_gradient(positions, gradients):
    """
    Affiche les positions des sommets avec un dégradé de couleur spécifié.

    Args:
    - positions (list): Liste des positions des sommets, chaque position étant un tuple (x, y).
    - colors (list): Liste des couleurs des sommets, chaque couleur étant un réel entre 0 et 1.

    """
    lst_x, lst_y = [], []
    for (x, y), couleur in zip(positions, gradients):
        lst_x.append(x)
        lst_y.append(y)
    fig = plt.gcf()
    plt.scatter(lst_x, lst_y, marker=',', s=(72./fig.dpi)**2, c=gradients, cmap='summer')

# à vous !
positions = read_positions("position_large.co")

def normaliser_dist(distances):
    m = max(distances)
    for d in range(len(distances)):
        distances[d] /= m
    return distances

a = 100000
b = 200000

def chemin_to_pos(positions, c):
    pos_chemin = []
    for i in c:
        pos_chemin.append(positions[i])
    return pos_chemin

"""
gradient = normaliser_dist(dijkstra(large, a))
afficher_positions_gradient(positions, gradient)
chemin = plus_court_chemin(large, a, b)

pos_chemin = chemin_to_pos(positions, chemin)

afficher_positions(pos_chemin)

plt.gca().set_aspect('equal')
plt.show()
"""

# Deuxième partie
def dijkstra_lim(g, s, l):
    n = len(g)
    k = l
    dist = [inf] * n
    file = util_files_priorite.creer_file() 
    dist[s] = 0
    util_files_priorite.ajouter(file, 0, s)

    ouvert = []
    ferme = []

    while not(util_files_priorite.est_vide(file)):
        if k == 0:
            ouvert = file_to_list(file)
            break
        k -= 1
        x = util_files_priorite.retirer(file)
        ferme.append(x)
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                dist[y] = dist[x] + p
                util_files_priorite.ajouter(file, dist[y], y)
    return dist, ferme, ouvert

def file_to_list(file):
    l = []
    while not util_files_priorite.est_vide(file):
        l.append(util_files_priorite.retirer(file))
    return l

def normaliser_lim(distances, a, b):
    m = 0
    for i in distances:
        if i != inf:
            if i > m:
                m = i
    for d in range(len(distances)):
        distances[d] = a + distances[d]/m*(b-a)
        if distances[d] == inf:
            distances[d] = 1
    return distances

l = 50000

"""

plt.figure()

distances, ferme, ouvert = dijkstra_lim(large, a, l)
gradient = normaliser_lim(distances, 0, 0.4)
afficher_positions_gradient(positions, gradient)

pos_ouvert = chemin_to_pos(positions, ouvert)

afficher_positions(pos_ouvert)

plt.gca().set_aspect('equal')
plt.show()
"""

# Troidième partie
def plus_court_chemin_opti(g, a, b):
    n = len(g)
    dist = [inf] * n
    pred = [[] for _ in range(n)]
    file = util_files_priorite.creer_file()
    dist[a] = 0
    util_files_priorite.ajouter(file, 0, a)
    while not(util_files_priorite.est_vide(file)):
        x = util_files_priorite.retirer(file)
        if x == b:
            break
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                pred[y] = pred[x] + [x]
                dist[y] = dist[x] + p
                util_files_priorite.ajouter(file, dist[y], y)
    return dist, pred[b]

print("testtttttttttttttttttttttttttttttttttttttttttttt")
_, chemin_test = plus_court_chemin_opti(large, 0, 2)
print(chemin_test)

from util_fonctions import temps_calcul

#print("Temps plus_court_chemin: ")
#print(temps_calcul(plus_court_chemin, large, a, b))
#
#print("Temps plus_court_chemin_opti: ")
#print(temps_calcul(plus_court_chemin_opti, large, a, b))

def distance_vol_oiseau(positions, s1, s2):
    x1, y1 = positions[s1]
    x2, y2 = positions[s2]
    return sqrt((x1-x2)**2 + (y1-y2)**2)

def a_star(g, positions, a, b):
    n = len(g)
    dist = [inf] * n
    pred = [[] for _ in range(n)]
    file = util_files_priorite.creer_file()
    dist[a] = 0
    util_files_priorite.ajouter(file, 0, a)

    while not(util_files_priorite.est_vide(file)):
        x = util_files_priorite.retirer(file)
        if x == b:
            break
        for y, p in g[x]:
            if dist[x] + p < dist[y]:
                pred[y] = pred[x] + [x]
                dist[y] = dist[x] + p
                distance_finale = distance_vol_oiseau(positions, y, b)
                util_files_priorite.ajouter(file, dist[y] + distance_finale, y)
    return dist, pred[b]

distance_dijkstra, chemin_dijkstra = plus_court_chemin_opti(large, a, b)
distance_a_star, chemin_a_star = a_star(large, positions, a, b)

print("Temps calcul de Dijsktra: ")
print(temps_calcul(plus_court_chemin_opti, large, a, b))

print("Temps calcul de A*")
print(temps_calcul(a_star, large, positions, a, b))

plt.figure(0)

gradient_dijkstra = normaliser_lim(distance_dijkstra, 0, 0.4)
gradient_a_star = normaliser_lim(distance_a_star, 0, 0.4)

afficher_positions_gradient(positions, gradient_dijkstra)
afficher_positions_gradient(positions, gradient_a_star)

afficher_positions(chemin_to_pos(positions, chemin_a_star))
afficher_positions(chemin_to_pos(positions, chemin_dijkstra))

plt.show()
