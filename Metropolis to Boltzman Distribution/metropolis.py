# -*- coding: utf-8 -*-
"""
Created on Tue May  5 19:05:54 2026

@author: getsh
"""

import numpy as np 
import matplotlib.pyplot as plt 

v_delta = 4
v0 = 2 
beta = 1/4
v_max = 10*np.sqrt(1/beta)

def boltz_fun(x): 
    return np.exp(-beta*x**2/2)

N=10000
vel = np.zeros(N)
accept = 0
n_used = 0
i = 0



while(n_used<N):
    
    v_trial = v0 + v_delta*(2*np.random.uniform() - 1)
    ratio = boltz_fun(v_trial)/boltz_fun(v0) 
    r = np.random.uniform() 
    
    if (v_trial**2 <= v_max**2): # the square used for computational effeciency/ square root function can be used
        n_used +=1
        
        if (ratio >= r): # accept the step 
            vel[i] = v_trial
            v0 = v_trial 
            accept +=1
        else: # rejecting the step and stay there
            vel[i] = v0
        i +=1


            
E = 1/2*vel**2 # the energy of the distribution at each configuration as an array
E_mean = np.mean(E) # the mean value of the energy 
v_mean = np.mean(vel) # the mean value of the velocity distribution

v = np.linspace(-v_max, v_max, n_used) # this creates values of velocity in [-v_max, v_max] range
pdf = boltz_fun(v)*np.sqrt(beta/(2*np.pi)) # this mimic the actual theoretical distribution function



print("Acceptance rate:", accept/n_used, E_mean, v_mean)
  


   
plt.hist(vel, bins=50, density = "True", label="Simulation") # this is to mimic theoretical curve of MBD
plt.plot(v, pdf, label="Theoretical curve")
plt.xlabel("Velocity")
plt.ylabel("Probablity Density")
plt.title("Metropolis Implementation of 1D MBD")
plt.legend()

"""
pdf = boltz_fun(vel)*np.sqrt(beta/(2*np.pi))
plt.semilogy(E, pdf)
plt.xlabel("Energy")
plt.ylabel(r"$\ln(P(E))$")
plt.title("Semi-log plot of PDF with E")
plt.grid()

"""