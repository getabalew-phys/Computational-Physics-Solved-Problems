# Computational Physics Solved Problems

Solutions to assignments, quizzes, and projects from Computational Physics courses at Addis Ababa University. Most code is written in Fortran, with Gnuplot used for plotting. The statistical/Monte Carlo projects (Ising Model, Metropolis-Boltzmann) are written in Python and belong to a **second, follow-up Computational Physics course** — the rest are from the first course.

## Contents

### First Course (Fortran)

- **Assignment** – five problems: quadratic equation solver, exponential growth/decay, Simpson's rule integration, bisection root finding, and an ODE predictor-corrector solver. Includes the original question sheet and solution write-up as PDFs.
- **Home_take** – take-home exam solutions: Monte Carlo estimation of an area (circle-in-square sampling) and an ODE driver using a predictor-corrector scheme.
- **Quiz-2** – curve fitting and least-squares problems, plus additional numerical exercises.
- **Quiz-3** – ODE problems solved with Euler's method via a custom ODE driver module.
- **Miscellaneous codes** – standalone numerical methods practice: Euler and Runge-Kutta methods, Newton-Raphson, bisection, Simpson's rule, Monte Carlo integration, linear interpolation, Box-Muller sampling, radioactive decay, projectile motion, and simple harmonic motion.

### Second Course (Python)

- **Free Fall** – simulates a freely falling object using Euler's method, with and without air resistance.
- **Pendulm Problem** – investigation of linear and non-linear simple pendulum; comparing Euler's method and RK4, including period estimation and energy conservation checks.
- **Ising Model 1D** – 1D Ising model simulated with the Metropolis-Hastings algorithm; computes magnetization, energy, and specific heat and compares them with analytical results.
- **Metropolis to Boltzman Distribution** – Metropolis Monte Carlo sampling used to recover a Boltzmann/Maxwell-type velocity distribution.

Most folders include a PDF write-up (questions and/or solutions) alongside the source code.

## Requirements

- A Fortran compiler (e.g. `gfortran`) for the first-course folders
- [Gnuplot](http://www.gnuplot.info/) for plotting Fortran output
- Python 3 with NumPy and Matplotlib for the Ising Model and Metropolis folders

## Usage

Clone the repo and enter the folder you're interested in:

```bash
git clone https://github.com/getabalew-phys/Computational-Physics-Solved-Problems.git
cd Computational-Physics-Solved-Problems/"Ising Model 1D"
```

For a Fortran problem:

```bash
gfortran ass_problem1.f90 -o problem1
./problem1
```

For a Python problem:

```bash
python Ising_model_1D.py
```

## Author

Getabalew Manegerew
Department of Physics, Addis Ababa University

## License

MIT License
