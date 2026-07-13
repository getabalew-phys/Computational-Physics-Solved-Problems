MODULE DRIVER 
    IMPLICIT NONE 
    CONTAINS 
    SUBROUTINE bisection(t0, tb, tol) 
        REAL, EXTERNAL :: f
        REAL :: t0, tb, t, tol
        INTEGER :: m
        t = (t0+tb)/2

    DO WHILE(abs(f(t))>tol) 
        t = (t0+tb)/2
        IF ((f(t0)*f(t))<0) THEN 
            tb=t 
        ELSE 
            t0=t
        END IF 
        m=m+1
    END DO
    END SUBROUTINE bisection


    SUBROUTINE simpson(t, simp, N) 
        REAL, EXTERNAL :: v 
        REAL :: v0=10, t(0:), simp, t0=0, tf=1, dt
        INTEGER :: i, k, N

        dt = (tf - t0)/N
        t(0) = t0
        DO k=1, N
              t(k) = t(k-1) + dt 
        END DO

        DO i=1, N-1, 2
        simp = simp + dt*(4*v(t(i)) + v(t(i-1)) + v(t(i+1)))/3 
        END DO
    END SUBROUTINE simpson


END MODULE DRIVER


PROGRAM problem3_2
    USE DRIVER
    IMPLICIT NONE 
    REAL, EXTERNAL :: f 
    INTEGER, PARAMETER :: N=10000
    REAL :: t0=1, tb=5, tol=2e-5, dt=1e-5, simp, t(0:N)
    
    
   CALL bisection(t0,tb, tol)
   CALL simpson(t, simp, N)

!  When the particle hit the ground again, 
!  it reached again to the origin of our frame of reference 
!  thus we can use bisection method at what time this happend
!  Appart that in physics term the displacement of the particle is 0 when it hits the ground
!  however, we can determine the total distance by using simpson rule for one half of the flight and multiply it by 2

WRITE(UNIT=6, FMT="(A40, X, F4.2, X, A8)") "The particle hit the ground again after", t0, "seconds." 
WRITE(UNIT=6, FMT="(A36, X, I2,X, A14)") "The displacement of the particle is", 0, "m from origin."
WRITE(UNIT=6, FMT="(A26, X, F7.5,X, A22)") "The distance it covers is", 2*simp, "m in its entire flight."


END PROGRAM problem3_2

FUNCTION f(t) RESULT(s) 
    IMPLICIT NONE 
    REAL :: t, s, v0=10, g=10
    s = v0*t - g/2*t**2
END FUNCTION f

FUNCTION v(t) RESULT(vt) 
    IMPLICIT NONE 
    REAL :: t, vt, v0=10, g=10
    vt = v0 - g*t
END FUNCTION v