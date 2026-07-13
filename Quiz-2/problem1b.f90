PROGRAM problem1B
    IMPLICIT NONE 
    INTEGER , PARAMETER :: N=4
    REAL :: x0(0:N), y0(0:N), x(0:2*N), fx(0:2*N), dx=1, step=0.5 
    REAL :: c1, c2, c3, c4, fxl(0:2*N), a0, a1
    INTEGER :: i, k
   
    x0 = (/0, 1, 2, 3, 4/)
    y0 = (/1.0, 1.0, 9.2, 100.0, 169.0/) 

    CALL range(x0,x)
    CALL Linear_fit(y0,fx)
    CALL least_square(x0, y0, x, fxl)


    OPEN(UNIT=10, FILE="problem1b.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=0, 2*N 
        WRITE(10,FMT="(5f8.3)") x(k),  fx(k), fxl(k)
    END DO 
    CLOSE(UNIT=10)

    

    PRINT*,x
    WRITE(*, FMT="(f6.2)") fx
        

END PROGRAM problem1B


SUBROUTINE range(x0,x)
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=4
    REAL, INTENT(IN) :: x0(0:N)
    REAL :: x(0:2*N), step=0.5, dx=1
    INTEGER :: i
    DO i=0, 2*N 
            IF (MOD(i,2) == 0) THEN 
                x(i) = x0(i/2)
            ELSE 
                x(i) = x0((i-1)/2) + step 
            END IF
        END DO 
END SUBROUTINE range

SUBROUTINE Linear_fit(y0,fx)
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=4
    REAL, INTENT(IN) :: y0(0:N)
    REAL :: fx(0:2*N), step=0.5, dx=1
    INTEGER :: k
    DO k=0, 2*N 
            IF (MOD(k,2) == 0) THEN 
                fx(k) = y0(k/2)
            ELSE 
                fx(k) = y0((k-1)/2) + step/dx*(y0((k+1)/2) - y0((k-1)/2)) 
            END IF
    END DO 
    fx(0) = y0(0)
END SUBROUTINE Linear_fit

SUBROUTINE least_square(x0,y0,x,fls)
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=4
    REAL, INTENT(IN) :: x0(0:N), y0(0:N), x(0:2*N) 
    REAL, INTENT(INOUT) :: fls(0:2*N) 
    REAL :: c1, c2, c3, c4, a0, a1
    INTEGER :: i
        c1 = SUM(x0)
        c2 = DOT_PRODUCT(x0,x0) 
        c3 = SUM(y0)
        c4 = DOT_PRODUCT(x0,y0)

        a0 =  (c1*c4 - c2*c3)/(c1**2 - (N+1)*c2)
        a1 =  (c1*c3 - (N +1)*c4)/(c1**2 - (N+1)*c2)


        DO i=0, 2*N 
            fls(i) = a0 + a1*x(i)
        END DO

END SUBROUTINE least_square