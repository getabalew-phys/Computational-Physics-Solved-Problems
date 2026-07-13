PROGRAM bisection 
    IMPLICIT NONE 
    REAL, EXTERNAL :: f1 
    REAL :: a=1, b=5, dx=0.1, x0 
    INTEGER :: k=0
    x0 = (a+b)/2
    
    DO WHILE(abs(f1(x0))>1) 
        x0 = (a+b)/2
        IF (f1(a)*f1(x0) < 0) THEN
            b = x0 
        ELSE IF (f1(b)*f1(x0)<0) THEN 
            a = x0 
        END IF 
        PRINT*, a, b, x0
        k = k + 1
    END DO 

    PRINT*, f1(x0), x0, k

END PROGRAm bisection

FUNCTION f1(x) RESULT(y) 
    IMPLICIT NONE 
    REAL :: x , y 
    y = 8*x**5 - 20
END FUNCTION