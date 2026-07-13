# -*- coding: utf-8 -*-
"""
Created on Fri Feb 13 19:17:46 2026

@author: getsh
"""

import numpy as np 

def func(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10): 
    return pow(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 2)
N=100000
r = np.zeros(10)
I = 0
for i in range(N):
    for k in range(10):
        r[k] = np.random.uniform()
    I += func(r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[0])/N 
    
print(I)
    
    