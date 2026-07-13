PROGRAM integration
    IMPLICIT NONE 
    EXTERNAL :: func
    INTEGER :: k, i, N, m
    PARAMETER(N=1000)
    REAL :: x(N+1), f(N+1), s=0, h, a, b
   
     
    a=0
    b=10 
    !N=100
    h = (b-a)/N
    x(1) = 0
      
    CALL range(N, x, h)
    f = x**2
    
   ! PRINT*, f
    
    DO i=1,N
        s = s + (f(i) + f(i+1))/(2)*h

    END DO
PRINT*, s

END PROGRAM integration

SUBROUTINE range(N,x, h) 
    IMPLICIT NONE 
    INTEGER, INTENT(IN) :: N
    INTEGER :: k 
    REAL, INTENT(IN) :: h 
    REAL :: x(N+1)

    DO k=1,N
        x(k+1) = x(k) + h

    END DO
END SUBROUTINE range

