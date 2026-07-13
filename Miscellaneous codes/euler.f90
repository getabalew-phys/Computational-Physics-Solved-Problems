PROGRAM euler 
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: f1, A, B, C
    INTEGER, PARAMETER :: N=10
    REAL(KIND=16) :: x(0:N), dx = 0.01, y(0:N)
    INTEGER :: k, i

    x(0) = 0
    DO k=1, N
        x(k) = x(k-1) + dx 
    END DO
 
    y(0) = 1
    DO k=1, N 
        y(k) = y(k-1) - dx*B(x(k-1))/A(x(k-1))*y(k-1) - dx*C(x(k-1))/A(x(k-1))
       PRINT*, y(k)
    END DO

    PRINT*, y(k-1), x(k-1)

    OPEN(UNIT=10, FILE="euler.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO i=0,N 
        WRITE(UNIT=10, FMT="(2f20.13)") x(i), y(i)
    END DO
    CLOSE(UNIT=10)
END PROGRAM euler

FUNCTION f1(x) RESULT(y) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y 
    y = 3 
END FUNCTION f1

FUNCTION A(x) RESULT(y) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y 
    y = 1
END FUNCTION A 

FUNCTION B(x) RESULT(y) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y 
    y = cos(x)
END FUNCTION B 

FUNCTION C(x) RESULT(y) 
    IMPLICIT NONE 
    REAL(KIND=16) :: x, y 
    y = 0
END FUNCTION