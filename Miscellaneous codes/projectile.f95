PROGRAM projectile 
    IMPLICIT NONE 
    REAL :: theta(90), range(90), g, ang_rad, v0
    PARAMETER(g=9.8, ang_rad=3.14/180)
    INTEGER :: k, i
    v0 = 10

    DO k=1,90
        theta(k) = ang_rad*(k)
    END DO 
    
    range = (v0**2)*sin(2*theta)/g

  

    
    OPEN(UNIT=10, FILE="projectile.txt", ACCESS="sequential", ACTION="WRITE", POSITION="APPEND", STATUS="UNKNOWN")
    DO i=1, 90

    WRITE(10,'(F6.2,1X,F6.2)') theta(i), range(i)
    END DO
    CLOSE(UNIT=10, STATUS="KEEP") 
    
END PROGRAM projectile