PROGRAM problem2b
    
    IMPLICIT NONE
    REAL(KIND=16), EXTERNAL :: f1, f2
    INTEGER, PARAMETER :: N=500
    REAL(KIND=16) :: x(0:N), h, a=0, b=3, trap(0:N), Simp(0:N)
    INTEGER :: i, k, m
    h = (b-a)/N
    x(0) = a
    DO i=1,N 
        x(i) = x(i-1) + h 
    END DO 

    !fx1 =  f1(x)
    !PRINT*, x
    !PRINT*, fx1 

    trap(0) = 0 

    DO k=1, N 
        trap(k) = trap(k-1) + h/2*(f2(x(k-1)) + f2(x(k)))
    END DO 

  
    Simp(0)=0
    DO k=1, N-1
      Simp((k)) = Simp((k-1)) + (h/6)*(f2(x((k-1))) + 4*f2(x(k)) + f2(x((k+1))))
    END DO 
    Simp(N) = Simp(N-1) + (h/3)*(f2(x((N-1))) + f2(x(k))) 
    

    


    OPEN(UNIT=10, FILE="problem2b.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE")
    DO k=0, N 
        WRITE(10,FMT="(3f14.8)") x(k),  trap(k), Simp(k)
    END DO 
    CLOSE(UNIT=10)

    PRINT*, simp(N-1), trap(N-1)


END PROGRAM problem2b

SUBROUTINE trapziod(x,fx,a,b,S)
    IMPLICIT NONE 
    REAL(KIND=16), EXTERNAL :: f1
    REAL(KIND=16), INTENT(IN) :: x(:), a, b, fx
    REAL(KIND=16) :: h
    REAL(KIND=16), INTENT(INOUT) :: s
    INTEGER :: k , N 
    N = size(x)
    h = (b-a)/N
    s = 0 
    DO k=2, N
        s = s + h/2*(f1(x(k-1)) + f1(x(k)))
    END DO
END SUBROUTINE trapziod


    FUNCTION f1(x) RESULT(y) 
        IMPLICIT NONE 
        
        REAL(KIND=16), INTENT(IN) :: x
        REAL(KIND=16) ::  y 
        
        y = 1 + x + sin(x)

    END FUNCTION f1

    FUNCTION f2(x) RESULT(y) 
        IMPLICIT NONE 
        
        REAL(KIND=16), INTENT(IN) :: x
        REAL(KIND=16) ::  y 
        
        y = x**2

    END FUNCTION f2