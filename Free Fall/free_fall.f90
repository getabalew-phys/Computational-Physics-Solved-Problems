PROGRAM free_fall
IMPLICIT NONE 
REAL, EXTERNAL :: a  !this calls a function "a" to our main program which is an acceleration of the particle
REAL, PARAMETER :: dt=0.1, t0=0, tf=20, u0=0, r  !here the parameters and initial conditions are sat
INTEGER, PARAMETER :: N=(tf-t0)/dt   !this is used to declare the array size of the speed and the time
REAL :: u(N), t(N) !here speed and time are declared an array of N size
INTEGER :: i, k

!below initial conditions are explicitly sat
t(1) = t0 
u(1) = u0 

!below loop is to updates the time and the speed of the particle using Euler's method
    DO i=1, N - 1
        t(i+1) = t(i) + dt
        u(i+1) = u(i) + a(u(i), t(i))*dt 
        
        IF (Mod(i, 5)==0) THEN 
            WRITE(*, FMT="(2F10.5)") t(i+1), u(i+1)
        END IF
    END DO

!this part of the code is to export the data for visualizations
    OPEN(UNIT=8, FILE="free_fall.csv", ACCESS="SEQUENTIAL", POSITION="APPEND", STATUS="REPLACE" )
    DO k=1, N 
        WRITE(UNIT=8, FMT="(2F10.5)") t(k), u(k) 
    END DO 
    CLOSE(UNIT=8)

    call random_number(r)
    print*, r


END PROGRAM free_fall

!this function used to define the acceleration
FUNCTION a(u, t) RESULT(y) 
    IMPLICIT NONE 
    REAL :: u, t, g=9.8, m=5.5, c=1, y ! g=gravity [kg m /s^2], m=mass [kg], and c=drag constant [kg/s], as given in the problem
    y = g - c*u/m
END FUNCTION a