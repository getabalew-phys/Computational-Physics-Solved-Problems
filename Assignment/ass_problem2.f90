PROGRAM problem2
    IMPLICIT NONE
    INTEGER, PARAMETER :: N=500
    REAL :: h0=50, h(0:N), r=0.5 
    INTEGER :: k 
    h(0)=h0 

    DO k=1, N 
        h(k) = r*h(k-1)
        
    END DO 

   

    OPEN(UNIT=10, FILE="ass_problem2.csv", POSITION="APPEND", ACCESS="SEQUENTIAL", STATUS="REPLACE")
     DO k=0, N 
        WRITE(UNIT=10, FMT="(I6, f11.8)") k, h(k)
     END DO
     CLOSE(UNIT=10)

Print*, 2*sum(h)-h0

END PROGRAM problem2