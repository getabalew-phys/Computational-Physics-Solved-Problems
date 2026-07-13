PROGRAM problem4 
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=10
    REAL :: x0(1:N), y0(1:N), x(1:2*N), fx(1:2*N)
    INTEGER :: i,k
    x0 = (/-0.83836, -0.383775, 0.0358423, 0.525396, 1.01495, 1.18979, 1.43457, 1.81921, 2.37870, 2.97316/)
    y0 = (/8.54662, 22.7661, 39.2307, 50.0075, 39.5300, 31.1480, 19.1738, 7.79823, 1.66143, 0.164646/) 
    
    DO i=1, 2*N 
        IF(MOD(i,2).ne.0) THEN 
            x(i) = x0((i+1)/2)
        ELSE 
            x(i) = (x0((i)/2) + x0((i+2)/2))/2
        END IF
    END DO 

    DO k=1, 2*N 
        IF(MOD(k,2).ne.0) THEN 
            fx(k) = y0((k+1)/2)
        ELSE 
            fx(k) = fx((k-1)) + (x(k) - x((k-1)))/(x0((k+2)/2) - x0((k)/2))*(y0((k+2)/2)-fx(k-1)) 
        END IF
    END DO

     OPEN(UNIT=10, FILE="problem4.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=1, 2*N - 1
        WRITE(10,FMT="(2f8.4)") x(k),  fx(k)
    END DO 

    OPEN(UNIT=10, FILE="problem4r.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=1, N
        WRITE(10,FMT="(2f8.4)") x0(k),  y0(k)
    END DO 
    CLOSE(UNIT=10)

    PRINT*, fx
END PROGRAM problem4 