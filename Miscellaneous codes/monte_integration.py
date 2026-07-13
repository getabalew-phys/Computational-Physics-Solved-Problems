# -*- coding: utf-8 -*-
"""
Created on Fri Feb 13 18:48:29 2026

@author: getsh
"""

import numpy as np 
import matplotlib.pyplot as plt 

def func(x):
    return np.sin(10*x**x)

s=0.0
a=0; b=2
N=100
I = 0

x=np.linspace(0,2,N)

for i in range(N): 
    r = (b-a)*np.random.uniform() - a
    s += (func(r)/N)*(b-a)
    I += func(x[i])/N*(b-a)

print("The integration of f=x**2 from x={} to x={} is {}".format(a,b,s), I)
    
