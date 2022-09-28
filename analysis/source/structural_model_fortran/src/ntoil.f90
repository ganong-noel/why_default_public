!     Last change:  VP   10 Nov 2004    2:02 pm
SUBROUTINE  NTOIL(value,grid,n,ind)
implicit none
INTEGER, INTENT(IN) :: n
REAL, INTENT(IN) :: value, grid(n,1)
INTEGER, INTENT(OUT) :: ind

if (value .GE. grid(n,1)) then
        ind=n-1
ELSEIF (value < grid(1,1)) then
        ind=1
else
	ind = 1+INT((value-grid(1,1))/(grid(2,1)-grid(1,1)))
end if

RETURN
END subroutine
