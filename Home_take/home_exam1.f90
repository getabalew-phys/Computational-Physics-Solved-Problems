PROGRAM exam1 
    IMPLICIT NONE 
    INTEGER, PARAMETER :: N=10000
    REAL :: x, y, A0=9, A, Ac
    INTEGER :: k, i, acc=0
    k=0
    !let A0 be the area of the square, Ac the area of the half circles, A the area of the grooved rectangle

        DO i=0, N 
                CALL random_number(x)
                CALL random_number(y)
            IF (sqrt(x**2 + y**2) <= 1) THEN 
                k = K + 1
                !PRINT*, x**2 + y**2, k
            END IF 
        END DO
    Ac = 0.5*k/real(N)
    A = A0 - 4*Ac
PRINT*, "The area of the grooved rectangle (Razor Blade) is:", A ,"mm square", k

END PROGRAM exam1