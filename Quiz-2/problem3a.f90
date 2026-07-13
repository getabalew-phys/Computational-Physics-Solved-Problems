PROGRAM problem3a
    IMPLICIT NONE 
    REAL, PARAMETER :: dx=0.001, a=0, b=(3./4.)*3.14
    INTEGER, PARAMETER :: N=(b-a)/dx
    REAL :: x(0:N), fx(0:N), fm(N), ff(N), fb(N)
    INTEGER :: i, k

    x(0) = 0
    DO i=1, N 
        x(i) = x(i-1) + dx
    END DO 

    fx = 1 + x + sin(x) 

    DO k=1, N 
        fb(k) = (fx(k) - fx(k-1))/dx 
    END DO 


    DO k=0, N-1
        ff(k+1) = (fx(k+1) - fx(k))/dx 
    END DO 

    DO k=1, N-1
        fm(k) = (fx(k+1) - fx(k-1))/(2*dx) 
    END DO 

    OPEN(UNIT=10, FILE="problem3a.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=0, N-1
        WRITE(10,FMT="(4f14.8)") x(k),  ff(k), fb(k), fm(k)
    END DO 
    CLOSE(UNIT=10)

    print*, ff(15), fm(15)


END PROGRAM problem3a 