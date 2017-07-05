'''
Created on Oct 2, 2012

@author: vadim

Todo: need to learn string algorithms.
'''

f = open('data/keylog.txt')

mx = [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

while True:
    line = f.readline()
    if not line:
        break
    line = line[:-1]
    mx[int(line[0])][int(line[1])] += 1
    mx[int(line[1])][int(line[2])] += 1

for l in mx:
    print(l)
f.close()

if __name__ == '__main__':
    pass
