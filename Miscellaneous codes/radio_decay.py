# -*- coding: utf-8 -*-
"""
Created on Wed Feb 11 09:54:40 2026

A radioactive decay of strontium-90 source simulation, while it is being detected 
with GM counter. 

@author: getsh
"""

import numpy as np 
import matplotlib.pyplot as plt 
import winsound


N0=int(1e2) # number of particles at t=0
lamda = 0.076 # decay constant of Strontium-90 source beta decay
dt = 0.1 # increment of time is measured in 10 nano seconds

t = np.zeros(10*N0) # time in nanosecond
N = np.zeros(10*N0) # number of particles in time
N[0] = N0; t[0] = 0 # setting initial conditions
delta = np.zeros(10*N0)

n = N0 
i=1; count=0; deltaN=0

while (n > 0): # this loop breaks when all the particles decay
    count +=1
    deltaN=0
    
    for i in range(n): 
        
        r = np.random.uniform() # generate a random number for each particles
        if (r < lamda): # the given particle decays probablistically 
            n = n - 1
            deltaN += 1
            if (n==0):
                tf = t[i-1] + dt
                break
            # winsound.Beep(1000, 100) # this creates a bip sound when a particle decay
    N[count] = N[count-1] - deltaN
    delta[count] = deltaN
    t[count] = t[count-1] + dt 
    
        
print("Time taken to decay in nano second:\n", "T =", tf)
plt.plot(t[:count], np.log(N[:count])) # this is a semilog plot
    
    
  
    


