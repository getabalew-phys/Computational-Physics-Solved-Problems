# -*- coding: utf-8 -*-
"""
Created on Tue May 26 13:20:34 2026
@author: getsh
"""

import numpy as np
import matplotlib.pyplot as plt

# --- Simulation parameters ---
N = 150                          # number of spins
N_trials = 20 * N**2             # total Monte Carlo steps
beta = 10                        # inverse temperature (1/kT)
J = 0.1                          # coupling constant

# --- State arrays ---
Spin_state = np.zeros(N)         # spin configuration
Magnetization = np.zeros(N_trials)
E = np.zeros(N_trials)

# --- Boltzmann factor ---
def boltz_factor(E):
    """Return Boltzmann weight for energy difference E."""
    return np.exp(-beta * E)

# --- Initialize unbiased spin state ---
for i in range(N):
    r = np.random.uniform()
    Spin_state[i] = -1 if r < 0.5 else 1

Magnetization[0] = sum(Spin_state)

# --- Compute initial energy ---
Nm = N - 1
for m in range(N):
    E[0] += -J * Spin_state[m] * Spin_state[Nm]
    Nm = m

# --- Snapshot storage setup ---
snapshot_every = N
n_snapshots = N_trials // snapshot_every
snapshots = np.zeros((n_snapshots, N))

# --- Main Monte Carlo loop ---
for j in range(N_trials):
    # pick random spin site
    n = int(N * np.random.uniform())

    # store snapshot every N steps
    if j % snapshot_every == 0:
        snapshots[j // snapshot_every] = Spin_state.copy()

    # periodic boundary conditions
    if n == 0:
        nl, nr = N - 1, n + 1
    elif n == N - 1:
        nl, nr = n - 1, 0
    else:
        nl, nr = n - 1, n + 1

    # energy difference for flipping spin
    dE = 2 * J * Spin_state[n] * (Spin_state[nl] + Spin_state[nr])
    w = boltz_factor(dE)
    r = np.random.uniform()

    # Metropolis acceptance rule
    if dE < 0 or w > r:
        Spin_state[n] = -Spin_state[n]                 # flip spin
        Magnetization[j] = Magnetization[j] + 2 * Spin_state[n]
        E[j] = E[j-1] + dE
    else:
        Magnetization[j] = Magnetization[j-1]          # carry forward
        E[j] = E[j-1]

# --- Burn-in cutoff ---
burnin = 10 * N**2

# --- Post burn-in averages ---
E_mean = np.mean(E[burnin:])
E_exact = -N * J * np.tanh(beta * J)
M_mean = np.mean(Magnetization[burnin:])

# specific heat from fluctuations
C = beta**2 * (np.mean(E[burnin:]**2) - np.mean(E[burnin:])**2) / N
C_exact = (beta * J / np.cosh(beta * J))**2

print(M_mean, E_mean, E_exact)

# --- Visualization ---

# Spin states in time (heatmap)
plt.figure(figsize=(12, 8))
plt.imshow(snapshots, aspect='auto', cmap='RdBu',
           vmin=-1, vmax=1,
           extent=[0, N, N_trials, 0])
plt.colorbar(label='Spin')
plt.xlabel('Site index')
plt.ylabel('MC step')
plt.title('Spin configuration evolution')

# Magnetization and Energy plots
fig, axes = plt.subplots(2, 1, figsize=(10, 8), sharex=True)

# Magnetization vs MC steps
axes[0].plot(range(N_trials), Magnetization, color='royalblue',
             linewidth=0.5, alpha=0.8)
axes[0].axvline(x=5*N**2, color='red', linestyle='--',
                linewidth=1.5, label='Burn-in cutoff')
axes[0].axhline(y=np.mean(Magnetization[5*N**2:]), color='black',
                linestyle='-', linewidth=1.5,
                label=f'$\\langle M \\rangle$ = {np.mean(Magnetization[5*N**2:]):.2f}')
axes[0].set_ylabel('Magnetization $M$', fontsize=12)
axes[0].set_title('Magnetization vs MC Steps', fontsize=13)
axes[0].legend(fontsize=10)
axes[0].grid(True, alpha=0.3)

# Energy vs MC steps
axes[1].plot(range(N_trials), E, color='darkorange',
             linewidth=0.5, alpha=0.8)
axes[1].axvline(x=5*N**2, color='red', linestyle='--',
                linewidth=1.5, label='Burn-in cutoff')
axes[1].axhline(y=np.mean(E[5*N**2:]), color='black',
                linestyle='-', linewidth=1.5,
                label=f'$\\langle E \\rangle$ sim = {np.mean(E[5*N**2:]):.2f}')
axes[1].axhline(y=E_exact, color='green', linestyle=':',
                linewidth=1.5, label=f'$\\langle E \\rangle$ exact = {E_exact:.2f}')
axes[1].set_ylabel('Energy $E$', fontsize=12)
axes[1].set_xlabel('MC Steps', fontsize=12)
axes[1].set_title('Energy vs MC Steps', fontsize=13)
axes[1].legend(fontsize=10)
axes[1].grid(True, alpha=0.3)
