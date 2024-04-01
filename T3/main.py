from z3 import *



# -1 -- пустая ячейка, -2 -- мина
def check(n, k):
    result = None

    #cells = [[Int(f'cell_{r}_{c}') for c in range(len(m[0]))] for r in range(len(m))]

    edges = {}
    s = Solver()

    for i in range(0, n - 1):
        for j in range(i + 1, n):
            # Значение переменной -- это идентификатор цвета
            # Цвета нумеруем от 0 до k - 1
            edges[(i, j)] = Int(f"edge_{i}_{j}")
            s.add(edges[(i, j)] > -1)
            s.add(edges[(i, j)] < k)

    for i in range(0, n - 2):
        for j in range(i + 1, n - 1):
            for o in range(j + 1, n):
                s.add( Or( Not(edges[(i, j)] == edges[(j, o)]), Not(edges[(i, j)] == edges[(i, o)])   ))
    
    res = s.check() == sat
    return res

def get_max_N(k):
    N = 1
    res = True
    while (res):
        N += 1
        res = check(N, k)
    
    return N - 1

get_max_N(2)