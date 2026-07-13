# -*- coding: utf-8 -*-
"""
Created on Fri Feb 27 18:34:05 2026

@author: getsh
"""

import numpy as np # numpy module for creating and manuplating arrays
import matplotlib.pyplot as plt # module for graphical outputs



N=200 # Number of steps
a=1; b=2; h=(b-a)/N # defining boundary values and increment h

#defining a numpy array to store values
x = np.zeros(N)
y = np.zeros(N)

# setting initial conditions
x[0] = a 
y[0] = 1 

#defining a function to compute slope at each point
def f(x,y): 
    return 2*x 

#bellows loop implement the euler algorithm
for i in range(N-1):
    
    slope = f(x[i],y[i])
    x[i+1] = x[i] + h 
    y[i+1] = y[i] + h*slope 
    
    
# belows segment of code added to plot the results up on running the code
plt.plot(x,y, label= "Euler, N=200", marker="^")
plt.plot(x, x**2,label= "Exact")
plt.title("Euler Method")
plt.legend()
plt.show()

    