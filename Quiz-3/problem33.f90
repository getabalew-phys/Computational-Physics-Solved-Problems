MODULE DIFFERENTIATION_DRIVER
    IMPLICIT NONE 
    
    CONTAINS
    SUBROUTINE backward(x,bd,dx) 
        IMPLICIT NONE 
        REAL, EXTERNAL :: f 
        REAL, DIMENSION(0:) :: x, bd
        REAL :: dx
        INTEGER :: i, N 
        N = size(x) 

        DO i=1, N 
            x(i) = x(i-1) + dx 
            bd(i) = (f(x(i)) - f(x(i-1)))/dx 
        END DO

    END SUBROUTINE backward

    SUBROUTINE forward(x, fd, dx) 
        IMPLICIT NONE 
        REAL, EXTERNAL :: f 
        REAL :: x(0:), fd(:), dx 
        INTEGER :: i, N 
        N = size(fd)

        DO i=1, N 
        x(i) = x(i-1) + dx 
        END DO 

        DO i=1, N
            fd(i) = (f(x(i+1)) - f(x(i)))/dx 
        END DO

    END SUBROUTINE forward

    SUBROUTINE midpoint(x,md,dx) 
        IMPLICIT NONE 
        REAl, EXTERNAL :: f 
        REAL :: x(0:), md(:), dx 
        INTEGER :: i, N 
        N = size(md) 
        
        DO i=1, N 
            x(i) = x(i-1) + dx 
        END DO 

        DO i=1, N-1
            md(i) = (f(x(i+1)) - f(x(i-1)))/(2*dx) 
        END DO

    END SUBROUTINE midpoint


END MODULE DIFFERENTIATION_DRIVER

PROGRAM problem33
    USE DIFFERENTIATION_DRIVER
    IMPLICIT NONE 
    REAL, EXTERNAL :: f
    INTEGER, PARAMETER :: N=10000
    REAL :: x(0:N), fd(1:N), bd(1:N-1), md(1:N-1), a=-5, b=5, dx
    INTEGER :: i, k 
    dx = (b-a)/N
    x(0) = a 

    


    CALL backward(x, bd, dx)
    CALL forward(x, fd, dx)
    CALL forward(x, md, dx) 


    OPEN(UNIT=10, FILE="problem33.csv", POSITION="APPEND", ACCESS="SEQUENTIAL", STATUS="REPLACE")
    DO k=1, N-1
    WRITE(UNIT=10, FMT="(4f10.5)") x(k), bd(k), fd(k), md(k)
    END DO
    CLOSE(UNIT=10)

    

END PROGRAM problem33

FUNCTION f(x) RESULT(y) 
    IMPLICIT NONE 
    REAL :: x, y
    y = x**3 + 2*x**2 + 2*x + 2 
END FUNCTION f