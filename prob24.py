#/usr/bin/env python3


def next_perm(s):
    k = -1
    for i in range(0, len(s) - 1):
        if s[i] < s[i + 1]:
            k = i
    if k == -1: return s
    l = -1
    for i in range(k + 1, len(s)):
        if s[i] <= s[k] and (l == -1 or s[i] > s[l]):
            l = i
    print(k, l)
    tmp, s[k] = s[k], s[l]
    s[l] = tmp
    return s[0:k+1] + s[:k:-1]
