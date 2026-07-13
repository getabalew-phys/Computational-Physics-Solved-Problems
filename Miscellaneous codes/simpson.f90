PROGRAM simp 
    IMPLICIT NONE 
    INTEGER :: N 
    PARAMETER(N=11100) 
    REAL(KIND=16) :: x(0:N), f(0:N), s , a, b
    REAL(KIND=16) :: h
    INTEGER :: k, i 

    IF (MOD(N,3).NE.0) STOP "N must be divisible by 3 for simpson algorithm to be applied."
    
    a=0 
    b=1
    h = (b-a)/N
    x(0) = a
    DO i=1, N 
        x(i) = x(i-1) + h
    END DO

    f = exp(x**3)
    
    s = 0
    k=0
    DO WHILE(k<=N)  
        s = s + (h/2)*(f(k)+4*f(k+1) + f(k+2))
        k = k+3
    END DO


    WRITE(*, FMT='(F15.5)') s
END PROGRAM simp 