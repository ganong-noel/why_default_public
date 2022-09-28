!     Last change:  JFC  19 Sep 2000    4:41 pm
SUBROUTINE irutil2(cons,ncolsc,gamma,util)
INTEGER, INTENT(IN) :: ncolsc
REAL, INTENT(IN) :: cons(ncolsc,1), gamma
REAL, INTENT(OUT) :: util(1,ncolsc)
INTEGER :: ind2
DO ind2=1,ncolsc
  util(1,ind2)= (cons(ind2,1))**(1.0-gamma)/(1.0-gamma)
END DO

END SUBROUTINE
