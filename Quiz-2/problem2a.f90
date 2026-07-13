FUNCTION f1(x) RESULT(y) 
        IMPLICIT NONE 
        
        REAL, INTENT(IN) :: x(:)
        REAL ::  y(size(x)) 
        
        y = 1 + x + sin(x)

    END FUNCTION



PROGRAM problem2a
    
    IMPLICIT NONE 
    EXTERNAL :: f1
    INTEGER, PARAMETER :: N=10000
    REAL :: x(0:N), fx1(0:N), h, a=0, b=(3./4.)*3.14, S, Ss
    INTEGER :: i, k, m
    h = (b-a)/N
    x(0) = a
    DO i=1,N 
        x(i) = x(i-1) + h 
    END DO 

    fx1 =  f1(x)
    PRINT*, x
    !PRINT*, fx1 

    S = 0 

    DO k=1, N 
        S = S + h/2*(fx1(k-1) + fx1(k))
    END DO 

    PRINT*, S 

    Ss=0
    DO k=1, N, 2
      Ss = Ss + h/3*(fx1(k-1) + 4*fx1(k) + fx1(k+1))
    END DO 

    PRINT*, Ss



END PROGRAM problem2a

    