g1 = [[1, 3], [0, 2, 3, 4], [1, 4], [0, 1], [1, 2]]
g2 = [[1, 3], [2, 4], [4], [1], []]

def parcours_profondeur(g, s):
    t = [False for _ in range(len(g))]
    def visiter(g, s, t):
        t[s] = True
        print(s)
        for i in g[s]:
            if not(t[i]):
                visiter(g, i, t)
    visiter(g, s, t)
