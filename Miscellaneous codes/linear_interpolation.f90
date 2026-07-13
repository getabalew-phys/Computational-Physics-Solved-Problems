PROGRAM prime 
    IMPLICIT NONE
    REAL :: x_in(20), y_in(20), h_int(19),m
    INTEGER :: k, i 
    LOGICAL :: b
    b = (.True.).neqv.(.True.)
    m = log10(4.)
    DO i=1,20
        IF (MOD(i,2)==1) THEN
        x_in(i) = i 
        ELSE IF (MOD(i,2)==0) THEN 
        x_in(i) = 0
        END IF
    END DO 

    y_in = 5*x_in**3

    DO k=1, 20
        IF (MOD(k,2) == 1) THEN 
            h_int(k) = y_in(k)
        ELSE IF (MOD(k,2) == 0) THEN 
            y_in(k) = y_in(k) + (y_in(k+1) - y_in(k))*(k - x_in(k))/(x_in(k+1) - x_in(k))
        END IF 
    END DO

    PRINT*, h_int
    PRINT*, (y_in)

    
    

END PROGRAM 