PROGRAM newton 
    IMPLICIT NONE 
    REAL, EXTERNAL :: f1, fd
    REAL :: x(1:20), fun(1:20)
    INTEGER :: k=1,i

    x(1) = 1
    fun(1) = f1(x(1))

    DO WHILE(abs(f1(x(k)))>0.0001)
        x(k+1) = x(k) - f1(x(k))/fd(x(k))
        k = k + 1
        fun(k+1) = f1(real(k+1))
        
    END DO
    
    PRINT*, f1(x(k)), x(k), k

    OPEN(UNIT=10, FILE="newton.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO i=1,20 
        WRITE(UNIT=10, FMT="(2f8.3)") x(i), fun(i)
    END DO
    CLOSE(UNIT=10)
    

END PROGRAM newton

FUNCTION f1(x) RESULT(y) 
    IMPLICIT NONE 
    REAL :: x, y 
    y = cos(x) - x
END FUNCTION f1 

FUNCTION fd(x) RESULT(y) 
    IMPLICIT NONE 
    REAL :: x, y 
    y = -sin(x) - 1
END FUNCTION fd

