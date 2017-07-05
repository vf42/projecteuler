'''
Created on Oct 2, 2012

@author: vadim

Idea: use dot product of unit vectors to determine if the origin lies inside.
'''

import math

vlen = lambda v : math.sqrt(sum(map(lambda x : x * x, v)))

norm = lambda v : tuple(map(lambda x : x / vlen(v), v))

vect = lambda v0, v1 : tuple(map(lambda a : a[0] - a[1], zip(v1, v0))) 

dot = lambda v0, v1 : sum(map(lambda a : a[0] * a[1], zip(v0, v1)))

result = 0 

if __name__ == '__main__':
    f = open('data/triangles.txt', 'r')

    while True:
        line = f.readline()
        if not line:
            break
        spl = list(map(lambda x: int(x), line.split(',')))
        trg = []
        for i in range(0, len(spl), 2):
            trg.append((spl[i], spl[i+1]))
            
        pairs = [( vect(trg[0], trg[1]), vect(trg[0], trg[2]) ),
                 ( vect(trg[1], trg[2]), vect(trg[1], trg[0]) ),
                 ( vect(trg[2], trg[0]), vect(trg[2], trg[1]) )
                 ]
        value = 0
        for i in range(0, 3):
            (t0, t1) = pairs[i]
            tc = vect(trg[i], (0, 0))
            a0 = round(math.acos(dot(norm(t0), norm(t1))), 8)
            a1 = round(math.acos(dot(norm(t0), norm(tc))), 8)
            a2 = round(math.acos(dot(norm(t1), norm(tc))), 8)
            if a1 + a2 - a0 <= 0.0000001:
                value += 1
        if value == 3:
            result += 1
    print("Result:", result)
    f.close()