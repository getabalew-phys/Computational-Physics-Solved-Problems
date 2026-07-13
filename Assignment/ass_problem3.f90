MODULE integrator 
    IMPLICIT NONE 
    

    CONTAINS
    SUBROUTINE simpson(t, simp) 
        REAL, EXTERNAL :: f 
        REAL :: v0=10, a=3, t(0:), simp, t0=0, tf=3, dt
        INTEGER :: i, k, N
        N = size(t) - 1
        dt = (tf - t0)/N
        t(0) = t0
        simp = 0

        DO k=1, N
              t(k) = t(k-1) + dt 
        END DO

    IF (MOD(N,2)==0) THEN

        DO i=1, N-1, 2
        simp = simp + dt*(4*f(t(i)) + f(t(i-1)) + f(t(i+1)))/3 
        END DO
    ELSE  

        DO i=1, N-2, 2
        simp = simp + dt*(4*f(t(i)) + f(t(i-1)) + f(t(i+1)))/3 
        END DO
        simp = simp + dt*(-f(t(N-2)) + 8*f(t(N-1)) + 5*f(t(N)))/12
    END IF
    END SUBROUTINE simpson

    SUBROUTINE trapziod(t, trap) 
        REAL, EXTERNAL :: f 
        REAL :: v0=10, a=3, t(0:), trap, t0=0, tf=3, dt
        INTEGER :: i, k, N
        N = size(t) - 1
        trap = 0

        dt = (tf - t0)/N
        t(0) = t0
        DO k=1, N
              t(k) = t(k-1) + dt 
        END DO

        DO i=1, N 
        trap = trap + dt*(f(t(i)) + f(t(i-1)))/2 
        END DO
    END SUBROUTINE trapziod

END MODULE integrator


PROGRAM problem3
    USE integrator
    IMPLICIT NONE 
    REAL, EXTERNAL :: f 
    INTEGER, PARAMETER :: N1=1, N3=3, N5=5, N10=10
    REAL :: v0=10, a=3, dt, t0=0, tf=3 
    REAL:: x1(0:N1), x3(0:N3), x5(0:N5), x10(0:N10), trap1, trap3, trap5, trap10, simp1, simp3, simp5, simp10
  
    
     CALL trapziod(x1, trap1)
     CALL trapziod(x3, trap3)
     CALL trapziod(x5, trap5)
     CALL trapziod(x10, trap10)
     CALL simpson(x1, simp1)
     CALL simpson(x3, simp3)
     CALL simpson(x5, simp5)
     CALL simpson(x10, simp10)

     WRITE(*,FMT="(A16, 4X, A20, 4X,  A20)") "Number of Slices", "Result from Trapziod", "Result from Simpson"
     WRITE(UNIT=6, FMT="(I3, 20X, F6.3, 20X, F6.3)")  N1,  trap1,  simp1 
     WRITE(UNIT=6, FMT="(I3, 20X, F6.3, 20X, F6.3)")  N3,  trap3,  simp3
     WRITE(UNIT=6, FMT="(I3, 20X, F6.3, 20X, F6.3)")  N5,  trap5,  simp5 
     WRITE(UNIT=6, FMT="(I3, 20X, F6.3, 20X, F6.3)")  N10,  trap10,  simp10

     PRINT*, simp5
     

    
    
END PROGRAM problem3

FUNCTION f(t) RESULT(y)
    IMPLICIT NONE 
    REAL :: t, y, v0=10, a=3
    y = v0 + a*t 
END FUNCTION f