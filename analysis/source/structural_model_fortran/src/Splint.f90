!     Last change:  JFC   6 Jan 2000    2:56 pm
SUBROUTINE splint(xa,ya,y2a,n,x,nrow,ncol,y)
INTEGER, INTENT(IN) :: n, nrow, ncol
REAL, INTENT(IN) :: x(nrow,ncol)
REAL, INTENT(IN) :: xa(n,1), ya(n,1), y2a(n,1)
REAL, INTENT(OUT) :: y(nrow,ncol)
INTEGER :: k, khi, klo, indr, indc
REAL :: a, b, h

DO indr=1,nrow
  DO indc=1,ncol
    klo = 1
    khi = n
    DO WHILE (khi-klo>1)
      k=(khi+klo)/2
      IF (xa(k,1)>x(indr,indc)) THEN
        khi=k
      ELSE
        klo=k
      END IF
    END do
    h = xa(khi,1)-xa(klo,1)
    a = (xa(khi,1)-x(indr,indc))/h
    b = (x(indr,indc)-xa(klo,1))/h
    y(indr,indc) = a*ya(klo,1)+b*ya(khi,1)+((a**3-a)*y2a(klo,1)+(b**3-b)*y2a(khi,1))*(h**2)/6.0
  END DO
END DO

END SUBROUTINE
