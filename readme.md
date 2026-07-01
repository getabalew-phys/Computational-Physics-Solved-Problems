# Ising Model in 1D

This project implements the 1D classical Ising model using the Metropolis-Hastings algorithm.  
It was developed as part of Computational Physics coursework (Phys 4624) at Addis Ababa University.

## Features
- Monte Carlo simulation of spin chains (N = 150)
- Calculates mean energy, magnetization, and specific heat
- Compares simulation results with analytical expectations
- Visualizations: magnetization/energy vs MC steps, spin evolution heatmap

## Requirements
- Python 3.10+
- NumPy
- Matplotlib

## Usage
```bash
git clone https://github.com/username/ising-1d.git
cd ising-1d
python ising_1d.py

## Simulation results show close agreement with analytical values:

- Mean magnetization ≈ 0

- Mean energy ≈ -12.40 (analytical: -11.42)

- Specific heat ≈ 0.415 (analytical: 0.420)

## Author
Getabalew Manegerew
Addis Ababa University, Department of Physics

## License
MIT License