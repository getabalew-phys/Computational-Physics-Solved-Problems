MODULE ODE_DRIVER
    CONTAINS
    SUBROUTINE euler(x, y, z, dx) 
    IMPLICIT NONE 
    REAL, EXTERNAL :: f
    REAL, DIMENSION(0:) :: x, y, z
    REAL :: dx
    INTEGER :: i, k

    
    k = size(x) - 1
    DO i=1, k
        x(i) = x(i-1) + dx
        y(i) = y(i-1) + dx*z(i-1)
        z(i) = z(i-1) - dx*f(y(i-1), z(i-1))
    END DO
    
END SUBROUTINE euler

SUBROUTINE picard(x, y, z, dx)
    IMPLICIT NONE 
    REAL, EXTERNAL :: f
    REAL, DIMENSION(0:) :: x, y, z
    REAL :: dx, k1, k2, p1, p2
    INTEGER :: i, N 
    N = size(x) - 1 

    DO i=1, N 
        x(i) = x(i-1) + dx
        y(i) = y(i-1) + dx*z(i-1)
        z(i) = z(i-1) + dx*f(y(i-1), z(i-1))

        k1 = dx*z(i-1)
        k2 = dx*z(i)
        p1 = dx*f(y(i-1), z(i-1))

        y(i) = y(i-1) + (k1 + k2)/2
        p2 = dx*f(y(i), z(i))
        z(i) = z(i-1) + (p1 + p2)/2
    END DO

END SUBROUTINE picard

SUBROUTINE corrector(x, y, z, dx)
    IMPLICIT NONE 
    REAL, EXTERNAL :: f
    REAL, DIMENSION(0:) :: x, y, z
    REAL :: dx, k1, k2, k3, p1, p2, p3
    INTEGER :: i, N 
    N = size(x) - 1 

     DO i=1, N - 1
        x(i) = x(i-1) + dx
        y(i) = y(i-1) + dx*z(i-1)
        z(i) = z(i-1) + dx*f(y(i-1), z(i-1))

        k1 = dx*z(i-1)
        k2 = dx*z(i)
        p1 = dx*f(y(i-1), z(i-1))

        y(i+1) = y(i-1) + 2*k2
        p2 = dx*f(y(i), z(i))
        z(i+1) = z(i-1) + 2*p2

        k3 = dx*z(i+1)
        p3 = dx*f(y(i+1), z(i+1))
        y(i+1) = y(i-1) + (k1 + 4*k2 + k3)/3
        z(i+1) = z(i-1) + (p1 + 4*p2 + p3)/3

    END DO

END SUBROUTINE corrector


END MODULE ODE_DRIVER

PROGRAM problem3_1a
    USE ODE_DRIVER
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=1000
    REAL :: x(0:N), y_e(0:N), z_e(0:N),x_p(0:N), y_p(0:N), z_p(0:N), x_c(0:N), y_c(0:N), z_c(0:N), dx, a=0, c=1
    INTEGER :: k, i

    ! let dy/dx = z, and a & c the range of the solution
    ! we also define external function " f " to be used as dz/dx = f

    dx = (c-a)/(N)
    x(0) = 0
    y_e(0) = 0.5
    z_e(0) = 1
    y_p(0) = 0.5
    z_p(0) = 1
    y_c(0) = 0.5
    z_c(0) = 1
     
    CALL euler(x, y_e, z_e, dx)
    CALL picard(x, y_p, z_p, dx)
    CALL corrector(x, y_c, z_c, dx)

    OPEN(UNIT=10, FILE="problem31a.csv", POSITION="APPEND", ACCESS="SEQUENTIAL", STATUS="REPLACE")
    DO k=0, N-2
        WRITE(UNIT=10, FMT="(7f9.5)") x(k),y_e(k), z_e(k), y_p(k), z_p(k), y_c(k), z_c(k)
    END DO
    CLOSE(UNIT=10)
    
    
END PROGRAM problem3_1a 

FUNCTION f(y,z) RESULT(s) 
    IMPLICIT NONE  
    REAL :: z, y, s, b=0.5, w0=1, r 
    CALL RANDOM_NUMBER(r)
    
    s = -(b*z + w0**2*y) + 2*r - 1
END FUNCTION f

