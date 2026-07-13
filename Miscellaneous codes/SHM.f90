PROGRAM harmonic_oscillator
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: fx, fv
    INTEGER, PARAMETER :: N=100
    REAL(KIND=16) :: dt, t0=0, tf=2*3.14, v0=0, x0=1, x(0:2*N+1), v(0:2*N+1), t(0:2*N+1), k1, k2, p1, p2, E(0:2*N+1) 
    INTEGER :: i, k
    t(0) = t0
    x(0) = x0 
    v(0) = v0
    dt = (tf - t0)/N 


    DO i=1, 2*N+1 
        t(i) = t(i-1) + dt
        x(i) = x(i-1) + dt*fx(v(i-1))
        k1 = dt*fx(v(i-1)) 
        v(i) = v(i-1) - dt*fv(x(i-1),v(i-1), t(i-1))

        k2 = dt*fx(v(i)) 
        x(i) = x(i-1) + (k1 + k2)/2
        p1 = dt*fv(x(i-1),v(i-1), t(i-1))
        p2 = dt*fv(x(i),v(i), t(i))

        v(i) = v(i-1) - (p1 + p2)/2

    END DO

    E = x0**2 - (x**2 + v**2)

    OPEN(UNIT=10, FILE="shm.csv", POSITION="APPEND", ACCESS="SEQUENTIAL", STATUS="REPLACE")
    DO k=0, 2*N+1 
        WRITE(UNIT=10, fmt="(4f14.10)") t(k), x(k), v(k), E(k)
    END DO
    CLOSE(UNIT=10)
    PRINT*, x

END PROGRAM harmonic_oscillator 

FUNCTION fx(x) RESULT(y)
    IMPLICIT NONE 
    REAL(KIND=16) :: x,y 
    y = x 
END FUNCTION fx

FUNCTION fv(x,y,t) RESULT(z) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y, t, z 
    z = x + 0.4*y 
END FUNCTION fv  
