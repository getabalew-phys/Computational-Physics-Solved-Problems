PROGRAM predictor
    IMPLICIT NONE 
    REAL, EXTERNAL :: f1, A, B, C, f
    INTEGER, PARAMETER :: N=100
    REAL :: x(0:N), dx = 0.01, y(0:N), dt=0.01, k1, k2,k3
    INTEGER :: k, i

    x(0) = 0
    DO k=1, N
        x(k) = x(k-1) + dx 
    END DO
 
    y(0) = 1
    DO k=1, N 
        k1 = f(x(k-1), y(k-1))
        y(k) = y(k-1) + dx*f(x(k-1), y(k-1))
        k2 = f(x(k), y(k))
        y(k) = y(k-1) + dx*(k1 + k2)/2
       
        
       PRINT*, y(k), x(k)
    END DO

    PRINT*, y(k-1), x(k-1)

    OPEN(UNIT=10, FILE="predictor.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO i=0,N 
        WRITE(UNIT=10, FMT="(2f14.8)") x(i), y(i)
    END DO
    CLOSE(UNIT=10)
END PROGRAM predictor

FUNCTION f1(x) RESULT(y) 
    IMPLICIT NONE 
    REAL :: x, y 
    y = 3 
END FUNCTION f1

FUNCTION A(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL :: x, y, z 
    z = 1
END FUNCTION A 

FUNCTION B(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL :: x, y, z
    z = 0
END FUNCTION B 

FUNCTION C(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL :: x, y, z
    z = -x**3 + cos(x)
END FUNCTION

FUNCTION f(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL, EXTERNAL :: f1, A, B, C
    REAL :: x, y, z 
    z = - ((B(x,y)*y+C(x,Y))/A(x,y))
END FUNCTION
