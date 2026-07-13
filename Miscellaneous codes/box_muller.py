# -*- coding: utf-8 -*-
"""
Created on Fri May 29 08:09:49 2026

@author: getsh
"""

import numpy as np
import matplotlib.pyplot as plt

N=1000
u1 = np.random.uniform(size=N)
u2 = np.random.uniform(size=N) 

Z1 = np.sqrt(-2*np.log(u1))*np.cos(2*np.pi*u2)
std = 6.4e-3
X1 = Z1*std
plt.hist(X1)