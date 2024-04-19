small = open('small.gr', 'r')
medium = open('medium.gr', 'r')
large = open('large.gr', 'r')

def ligne_to_nb_sommets_aretes(l):
    s = ""
    n = 0
    p = 0
    for i in l:
        if i == " ":
            n = int(s)
            s = ""
        elif i == "\n":
            p = int(s)
        else:
            s += i
    return n, p

def correction_nb(l):
    a = 0
    for i in range(len(l)):
        if l[i] == " ":
            a = i
    return int(l[:a]), int(l[a:])

def ligne_to_arc(l):
    s = ""
    x = -1
    y = -1
    poids = 0
    for i in l:
        if i == " ":
            if x == -1:
                x = int(s)
            else:
                y = int(s)
            s = ""
        elif i == "\n":
            poids = float(s)
        else:
            s += i
    return x, y, poids

def correction_arc(l):
    a, b = 0, 0
    for i in range(len(l)):
        if l[i] == " ":
            if a == 0:
                a = i
            else:
                b = i
    return int(l[:a]), int(l[a:b]), float(l[b:])

def lire_graphe(filename):
    file = open(filename, 'r')
    l0 = file.readline()
    n, _ = correction_nb(l0)
    g = [[] for _ in range(n)]
    for l in file.readlines():
        x, y, p = correction_arc(l)
        g[x].append((y, p))
    return g

print(lire_graphe("small.gr"))
