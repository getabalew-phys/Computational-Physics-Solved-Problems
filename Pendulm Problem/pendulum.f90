PROGRAM pendulum
    IMPLICIT NONE 
    REAL, EXTERNAL :: alpha
    REAL, PARAMETER :: dt=0.01, t0=0, tf=20, theta0=1, omega0=0  !here the parameters and initial conditions are sat
    INTEGER, PARAMETER :: N=(tf-t0)/dt  !this is used to declare the array size of the angular velocity and the time
    REAL :: theta(N), omega(N), t(N), theta_k(N), omega_k(N), t_k(N) !here angular displacement, angular velocity and time are declared an array of N size
    REAL :: period_e, period_k, dl_e, dl_k
    INTEGER :: i, k
    LOGICAL :: cond1=.False., cond2=.False.

    !below initial conditions are explicitly sat for EULER method
    t(1) = t0
    theta(1) = theta0
    omega(1) = omega0 

    !below initial conditions are explicitly sat for RK4 method
    t_k(1) = t0
    theta_k(1) = theta0
    omega_k(1) = omega0 

    !this loop calls EULER and RK4 subroutines and updates the arrays value  
        DO i=1, N-1 
            CALL EULER(t(i), t(i+1), dt, theta(i), theta(i+1), omega(i), omega(i+1))
            CALL RK(t_k(i), t_k(i+1), dt, theta_k(i), theta_k(i+1), omega_k(i), omega_k(i+1))

            !belows parameters are useful to find a match for initial condition of the pendulum for each method
            dl_e = theta0 - theta(2)
            dl_k = theta0 - theta_k(2)

            !this conditional statement check when the EULER meet initial setup
            IF (Abs(theta(i+1)-theta0)<dl_e.and.(.not.cond1)) THEN 
                period_e = t(i+1)
                cond1 = .True.
            END IF

            !this conditional statement check when the RK4 meet initial setup
            IF (Abs(theta_k(i+1)-theta0)<dl_k.and.(.not.cond2)) THEN 
                period_k = t_k(i+1)
                cond2 = .True.
            END IF

            !this is to exit the loop when the period for both methods reached
            IF (cond1.and.cond2) EXIT
            
        END DO


    WRITE(*,*) "The period in EULER method: p_e =", period_e
    WRITE(*,*) "The period in RK4 method: p_k =", period_k
    WRITE(*,*) "The the difference is: dT=", period_k - period_e

END PROGRAM pendulum

!this function is for angular acceleration
FUNCTION alpha(omega, theta, t) RESULT(y)
    IMPLICIT NONE 
    REAL :: omega, theta, t, y, g=9.8, l=9.8/9
    y = -g*(theta)/l
END FUNCTION alpha

!defining subrouting for EULER method
SUBROUTINE euler(t_, t, dt, theta_, theta, omega_, omega) 
    IMPLICIT NONE
    REAL, EXTERNAL :: alpha
    REAL :: t_, t, dt, theta_, theta, omega_, omega, a  !underscore "_" is used for the ith element while the rest are for i+1 th
    
    t = t_ + dt
    a = alpha(omega_, theta_, t_)
    omega = omega_ + a*dt 
    theta = theta_ + omega*dt
    
END SUBROUTINE euler

!defining subrouting for RK4 method
SUBROUTINE rk(t_, t, dt, theta_, theta, omega_, omega)
    IMPLICIT NONE 
    REAL, EXTERNAL :: alpha 
    REAL :: t_, t, dt, theta_, theta, omega_, omega !underscore "_" is used for the ith element while the rest are for i+1 th
    REAL :: k1, k2, k3, k4, p1, p2, p3, p4 !k's are small increments in angular velocity (omega), while p's are for angular displacement (theta)

            t = t_ + dt
            p1 = omega_*dt
            k1 = alpha(omega_, theta_, t_)*dt

            k2 = alpha(omega_+k1/2, theta_+p1/2, t_+dt/2)*dt
            p2 = (omega_ + k1/2)*dt

            k3 = alpha(omega_+k2/2, theta_+p2/2, t_+dt/2)*dt
            p3 = (omega_ + k2/2)*dt
            
            k4 = alpha(omega_+k3, theta_+p3, t_+dt)*dt
            p4 = (omega_ + k3)*dt

            theta = theta_ + (p1 + 2*(p2 + p3) + p4)/6
            omega = omega_ + (k1 + 2*(k2 + k3) + k4)/6 
    
END SUBROUTINE rk
