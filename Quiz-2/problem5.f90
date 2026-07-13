PROGRAM problem5 
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=5
    REAL :: x0(1:N), y0(1:N), x(1:2*N), fx(1:2*N)
    INTEGER :: i,k
    x0 = (/0.17, 0.24, 0.38, 0.46, 0.89/)
    y0 = (/500, 1000, 1200, 1600, 1800/) 
 DO i=1, 2*N 
        IF(MOD(i,2).ne.0) THEN 
            x(i) = x0((i+1)/2)
        ELSE 
            x(i) = (x0((i)/2) + x0((i+2)/2))/2
        END IF
    END DO 

    PRINT*, 1600 + (0.69 - 0.46)/(0.89 - 0.46)*(1800-1600)

    DO k=1, 2*N 
        IF(MOD(k,2).ne.0) THEN 
            fx(k) = y0((k+1)/2)
        ELSE 
            fx(k) = fx((k-1)) + (x(k) - x((k-1)))/(x0((k+2)/2) - x0((k)/2))*(y0((k+2)/2)-fx(k-1)) 
        END IF
    END DO

     OPEN(UNIT=10, FILE="problem5.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=1, 2*N - 1
        WRITE(10,FMT="(2f14.5)") x(k),  fx(k)
    END DO 

    OPEN(UNIT=10, FILE="problem5r.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=1, N
        WRITE(10,FMT="(2f14.5)") x0(k),  y0(k)
    END DO 
    CLOSE(UNIT=10)

    PRINT*, fx
    
END PROGRAM problem5