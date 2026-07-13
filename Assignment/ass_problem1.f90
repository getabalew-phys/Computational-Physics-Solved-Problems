PROGRAM problem1 
    IMPLICIT NONE
    REAL :: a=1, b=2, c=3
    COMPLEX :: x1, x2 


    CALL quadratic(a,b,c,x1,x2)
    PRINT*, "_________________ Solutions in complex number format __________________" 
    PRINT*, "The two roots are", x1 
    PRINT*, "And ", x2

END PROGRAM problem1

SUBROUTINE quadratic(a,b,c,x1,x2)
    IMPLICIT NONE 
    REAL, INTENT(IN) :: a, b, c
    COMPLEX, INTENT(INOUT) :: x1, x2 

    IF ((b**2-4*a*c)>=0) THEN 
        x1 = (-b - sqrt(b**2-4*a*c))/(2*a)
        x2 = (-b + sqrt(b**2-4*a*c))/(2*a)
    ELSE 
        x1 = cmplx(-b/(2*a), - sqrt(4*a*c-b**2)/(2*a))
        x2 = cmplx(-b/(2*a),  sqrt(4*a*c-b**2)/(2*a)) 
    END IF 

END SUBROUTINE quadratic