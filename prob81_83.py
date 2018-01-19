'''
https://projecteuler.net/problem=81 - DONE
https://projecteuler.net/problem=83 - DONE
'''

import sys

# Read the file.
def load_matrix(fname):
    f = open(fname, 'r')
    matrix = list(map(lambda l : list(map(lambda s : int(s), l.split(','))), f.readlines()))
    f.close()
    return matrix


def pretty_print(matrix):
    for l in matrix:
        print('\t'.join(map(lambda v : str(v), l)))


def solve_81(matrix):
    sums = [ [0] * len(matrix[0]) for _ in range(len(matrix)) ]
    visited = [ [False] * len(matrix[0]) for _ in range(len(matrix)) ]
    sums[0][0] = matrix[0][0]
    vqueue = [(0, 1), (1, 0)]
    while len(vqueue) > 0:
        x, y = vqueue.pop(0)
        if x >= len(matrix) or y >= len(matrix[0]) or visited[x][y]:
            continue
        visited[x][y] = True
        up = sums[x][y-1] if y > 0 else sys.maxsize
        left = sums[x-1][y] if x > 0 else sys.maxsize
        sums[x][y] = matrix[x][y] + min(left, up)
        #print(x, y, matrix[x][y], left, up)
        #pretty_print(sums)
        vqueue += [(x+1, y), (x, y+1)]
    return(sums[-1][-1])


def solve_83(matrix):
    sums = [ [sys.maxsize] * len(matrix[0]) for _ in range(len(matrix)) ]
    sums[0][0] = matrix[0][0]
    vqueue = [(0, 0)]
    while len(vqueue) > 0:
        x, y = vqueue.pop(0)
        if x >= len(matrix) or y >= len(matrix[0]) or x < 0 or y < 0:
            continue
        neighbours = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
        for (nx, ny) in neighbours:
            if nx >= len(matrix) or ny >= len(matrix[0]) or nx < 0 or ny < 0:
                continue
            newval = sums[x][y] + matrix[nx][ny]
            if newval < sums[nx][ny]:
                sums[nx][ny] = newval
                vqueue.append((nx, ny))
    return(sums[-1][-1])

if __name__ == '__main__':
    matrix = load_matrix('data/prob81.txt')
    #pretty_print(matrix)
    print(solve_81(matrix))
    print(solve_83(matrix))
