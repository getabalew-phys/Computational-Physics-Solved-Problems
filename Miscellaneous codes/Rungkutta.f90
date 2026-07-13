PROGRAM predictor
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: f1, A, B, C, f
    INTEGER, PARAMETER :: N=1000
    REAL(KIND=16) :: x(0:N), dx = 0.1, y(0:N), dt=0.01, k1, k2, k3, k4
    INTEGER :: k, i

    x(0) = 0
    DO k=1, N
        x(k) = x(k-1) + dx 
    END DO
 
    y(0) = 1
    DO k=1, N 
        k1 = dx*f(x(k-1), y(k-1))
        y(k) = y(k-1) + dx*f(x(k-1), y(k-1))
        k2 = dx*f(x(k-1)+dx/2, y(k-1)+k1/2)
        k3 = dx*f(x(k-1)+dx/2, y(k-1)+k2/2)
        k4 = dx*f(x(k-1)+dx, y(k-1)+k3)
        y(k) = y(k-1) + (k1 + 2*k2 + 2*k3 + k4)/6
        
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
    REAL(KIND=16) :: x, y 
    y = 3 
END FUNCTION f1

FUNCTION A(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y, z 
    z = 1
END FUNCTION A 

FUNCTION B(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y, z 
    z = 0
END FUNCTION B 

FUNCTION C(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y, z 
    z = -x**3 + cos(x)
END FUNCTION

FUNCTION f(x,y) RESULT(z) 
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: f1, A, B, C
    REAL(KIND=16) :: x, y, z  
    z = - ((B(x,y)*y+C(x,Y))/A(x,y))
END FUNCTION
