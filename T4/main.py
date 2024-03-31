from z3 import *

def get_neighb(x, y, xlen, ylen):
    result = []
    for i in [-1, 0, 1]:
        for j in [-1, 0, 1]:
            newx = x + i
            newy = y + j
            if (i != 0 or j != 0) and newx > -1 and newx < xlen and newy > -1 and newy < ylen:
                result.append((x + i, y + j))
    
    return result

# -1 -- пустая ячейка, -2 -- мина
def check(m, row, col):
    result = None

    #cells = [[Int(f'cell_{r}_{c}') for c in range(len(m[0]))] for r in range(len(m))]

    cells = {}
    s = Solver()

    # условия:  
    # 1 -- мина есть, 0 -- мины нет
    for r in range(len(m)):
        for c in range(len(m[0])):
            cells[(r, c)] = Int(f"M_{r}_{c}")
            s.add(cells[(r, c)] > -1)
            s.add(cells[(r, c)] < 2)

    for r in range(len(m)):
        for c in range(len(m[0])):
            if m[r][c] > -1:
                s.add(cells[(r, c)] == 0)
                neig = get_neighb(r, c, len(m), len(m[0]))
                s.add(Sum([cells[(nr, nc)] for nr, nc in neig]) == minefield[r][c])

    # Если мина -- значит значение ячейки точно 1
    for r in range(len(m)):
        for c in range(len(m[0])):
            if m[r][c] == -2:
                s.add(cells[(r, c)] == 1)


    s.add(cells[(row, col)] == 1)    
    result = s.check() == sat
    if result:
        RR = s.model()
    return result

E = -1
Mi = -2
minefield = [
    [2, E, E, E],
    [Mi, E, E, E],
    [E, E, E, E],
    [E, E, E, E]
]

check(minefield, 1, 1)