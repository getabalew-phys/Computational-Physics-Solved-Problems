MODULE ODE_DRIVER
    CONTAINS

SUBROUTINE corrector(t, x, v, dt)
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: f
    REAL(KIND=16), DIMENSION(0:) :: t, x, v
    REAL(KIND=16) :: dt, k1, k2, k3, p1, p2, p3
    INTEGER :: i, N 
    N = size(t) - 1 

     DO i=1, N - 1
        t(i) = t(i-1) + dt
        x(i) = x(i-1) + dt*v(i-1)
        v(i) = v(i-1) + dt*f(x(i-1))

        k1 = dt*v(i-1)
        k2 = dt*v(i)
        p1 = dt*f(x(i-1))

        x(i+1) = x(i-1) + 2*k2
        p2 = dt*f(x(i))
        v(i+1) = v(i-1) + 2*p2

        k3 = dt*v(i+1)
        p3 = dt*f(x(i+1))
        x(i+1) = x(i-1) + (k1 + 4*k2 + k3)/3
        v(i+1) = v(i-1) + (p1 + 4*p2 + p3)/3

    END DO

END SUBROUTINE corrector


END MODULE ODE_DRIVER

PROGRAM exam2
    USE ODE_DRIVER
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=10000
    REAL(KIND=16) :: t(0:N), x(0:N), v(0:N), dt, a=0, c=4e-14
    REAL(KIND=16) :: E_t(0:N), E_k(0:N), E_p(0:N), xeq=1.235e-10, k=72.7, m=35e-27
    INTEGER :: p, i

    ! let dx/dt = v, and v0 & x0 the initial conditions
    ! we also define external function " f " to be used as dv/dt = f

    dt = (c-a)/(0.1*N)
    t(0) = 0
  
    x(0) = 2e-11
    v(0) = 0
     
    CALL corrector(t, x, v, dt)

    E_t = 0.5*m*v**2 + 0.5*k*(x-xeq)**2
    E_k = 0.5*m*v**2
    E_p = 0.5*k*(x-xeq)**2
    OPEN(UNIT=10, FILE="home_exam2.csv", POSITION="APPEND", ACCESS="SEQUENTIAL", STATUS="REPLACE")
    DO p=0, N-2
        WRITE(UNIT=10, FMT="(6f32.20)") t(p), x(p), v(p), E_t(p), E_k(p), E_p(p)
    END DO
    CLOSE(UNIT=10)
    PRINT*, x(10), dt
    
END PROGRAM exam2

FUNCTION f(x) RESULT(s) 
    IMPLICIT NONE  
    REAL(KIND=16) :: x, xeq=1.235e-10, s, k=72.7, m=35e-27, r 
    CALL RANDOM_NUMBER(r)
    
    s = -k*(x - xeq)/m + ((2*r - 1)*1e-7)/m
END FUNCTION f

