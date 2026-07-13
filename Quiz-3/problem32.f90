PROGRAM problem3
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=2000
  REAL :: x(0:N), fd(N), bd(N), md(N), dx
  INTEGER :: i

  dx=10.0/N
  x(0)=-5.0
  DO i=1,N
    x(i)=x(i-1)+dx
  END DO

  DO i=1,N-1
    fd(i)=(f(x(i+1))-f(x(i)))/dx
    bd(i)=(f(x(i))-f(x(i-1)))/dx
    md(i)=(f(x(i+1))-f(x(i-1)))/(2.0*dx)
  END DO

  OPEN(10,FILE="diff.dat",STATUS="REPLACE")
  DO i=1,N-1
    WRITE(10,'(5f12.6)') x(i),fd(i),bd(i),md(i),df(x(i))
  END DO
  CLOSE(10)

CONTAINS
  REAL FUNCTION f(x)
    REAL :: x
    f=x**3+2*x**2+2*x+2
  END FUNCTION f

  REAL FUNCTION df(x)
    REAL :: x
    df=3*x**2+4*x+2
  END FUNCTION df
END PROGRAM problem3
