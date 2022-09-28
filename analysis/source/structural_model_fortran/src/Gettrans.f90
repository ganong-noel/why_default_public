!     Last change:  JFC  11 Aug 1999    4:42 pm
SUBROUTINE gettrans(ym,wm,n,m,r,s,p)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, INTENT(IN) :: m, r, s
REAL, INTENT(IN) :: ym(n,1), wm(n,1)
REAL, INTENT(OUT) :: p(n,n)
REAL :: f
INTEGER :: ind1, ind2
REAL, ALLOCATABLE :: x(:,:), y(:,:), w(:,:), c1(:,:), c2(:,:), sm(:,:)
ALLOCATE(x(n,n), y(n,n), w(n,n), c1(n,n), c2(n,n), sm(n,1))

DO ind1= 1,n
  x(:,ind1)= ym(:,1)
  w(:,ind1)= wm(:,1)
END DO
y= TRANSPOSE(x)
w= TRANSPOSE(w)

DO ind1=1,n
  DO ind2=1,n
    f= y(ind1,ind2)-m*(1-r)-x(ind1,ind2)*r
    c1(ind1,ind2)= EXP(-(f*f)/(2*s*s))/(SQRT(2*3.141592654)*s)
    f= y(ind1,ind2)-m*(1-r)-m*r
    c2(ind1,ind2)= EXP(-(f*f)/(2*s*s))/(SQRT(2*3.141592654)*s)
    p(ind1,ind2)= (c1(ind1,ind2)*w(ind1,ind2))/c2(ind1,ind2)
  END DO
END DO

sm= 0.0
DO ind1=1,n
  DO ind2=1,n
    sm(ind1,1)= sm(ind1,1)+p(ind1,ind2)
  END DO
END DO

DO ind2=1,n
  DO ind1=1,n
    p(ind1,ind2)= p(ind1,ind2)/sm(ind1,1)
  END DO
END DO

DEALLOCATE(x,y,w,c1,c2,sm)
END SUBROUTINE
