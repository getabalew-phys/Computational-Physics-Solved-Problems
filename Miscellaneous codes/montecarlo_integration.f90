PROGRAM montecarlo
    IMPLICIT NONE 
    REAL, EXTERNAL :: f, w, g
    INTEGER, parameter :: p=1
    REAL :: x, s, a=0, b=1, h=0.4, I(p), del, sig(p), r, r0, t
    INTEGER :: N=100, k, j
    
    CALL random_number(r0)
    r=0

            
       DO j=1,P
            s=0
            del=0
            DO k=1,N 
                CALL random_number(t)
                CALL random_number(x) 
                r0 = r
                r = r0 + h*(2*t - 1)
                IF ((w(r)/w(r0))>1) THEN
                    s = s + g(r)*w(r)
                    del = del + (g(r)*w(r))**2
                
                ELSE IF ((w(r)/w(r0))>x) THEN
                    s = s + g(r)*w(r)
                    del = del + (f(r))!*w(r))**2
                ELSE
                s = s + g(r0)*w(r0)
                del = del + (g(r0)*w(r0))**2
                END IF

            END DO 
            I(j) = s/N
            sig(j) = (del/N - I(j)**2)
        END DO
            
            
            PRINT*, I
            PRINT*, sum(I)/(p), sqrt(sig(1)/p)
END PROGRAM montecarlo

FUNCTION f(x) RESULT(y) 
    IMPLICIT NONE  
    REAL :: x, y 
    y = x**2
END FUNCTION f

FUNCTION w(x) RESULT(y)
    IMPLICIT NONE  
    REAL :: x, y 
    y = (exp(-x) - 1)
END FUNCTION w

FUNCTION g(x) RESULT(y)
    IMPLICIT NONE 
    REAL,EXTERNAL :: f, w
    REAL :: x, y 
    y = f(x)/w(x)
END FUNCTION g 

