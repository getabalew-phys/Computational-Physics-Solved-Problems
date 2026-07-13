PROGRAM problem3b
    IMPLICIT NONE 
    REAL(KIND=16), PARAMETER :: dx=0.001, a=2.5, b=3.5
    INTEGER(KIND=16), PARAMETER :: N=(b-a)/dx
    REAL(KIND=16) :: x(0:N), fx(0:N), fm(N), ff(N), fb(N)
    INTEGER(KIND=16) :: i, k

    x(0) = 0
    DO i=1, N 
        x(i) = x(i-1) + dx
    END DO 

    fx = x**2 -2*x + 1

    DO k=1, N 
        fb(k) = (fx(k) - fx(k-1))/dx 
    END DO 


    DO k=0, N-1
        ff(k+1) = (fx(k+1) - fx(k))/dx 
    END DO 

    DO k=1, N-1
        fm(k) = (fx(k+1) - fx(k-1))/(2*dx) 
    END DO 

    OPEN(UNIT=10, FILE="problem3b.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=0, N-1
        WRITE(10,FMT="(4f14.8)") x(k),  ff(k), fb(k), fm(k)
    END DO 
    CLOSE(UNIT=10)

    print*, ff(15), fm(15)


END PROGRAM problem3b