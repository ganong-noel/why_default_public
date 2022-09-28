!     Last change:  JFC  26 May 2011    3:59 pm
SUBROUTINE irevrenti(sav,nc,ndebt,v,ncash,nr,secd2,gcash,p,fy,nqpy,wmy,anaux,gdebt,utbdaux,gi,tax,gr,pl,pl1,pyh,prh,tpi,evout)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nc, ndebt, ncash, nr, nqpy
REAL, INTENT(IN) :: anaux, utbdaux,tax,pl,pl1
REAL, INTENT(IN) :: gi(1,1), gr(1,1)
REAL, INTENT(IN) :: sav(nc,ndebt), v(ncash,nr,nr,nr,nr), secd2(ncash,nr,nr,nr,nr)
REAL, INTENT(IN) :: gcash(ncash,1), p(1,nr), tpi(1,nr)
REAL, INTENT(IN) :: fy(nqpy,nr), wmy(nqpy,1)
REAL, INTENT(IN) :: pyh(2,2), prh(2,2)
REAL, INTENT(IN) :: gdebt(ndebt,1)
REAL, INTENT(OUT) :: evout(nc,ndebt)
INTEGER :: ind1, ind2, ind3, indy,ind8, ind9, indpy
REAL, ALLOCATABLE :: nw(:,:), nw2(:,:), aux(:,:), evout1(:,:), evout2(:,:), evout3(:,:), evout4(:,:)
ALLOCATE(nw(nc,ndebt),nw2(nc,ndebt),aux(nc,ndebt))
ALLOCATE(evout1(nc,ndebt),evout2(nc,ndebt),evout3(nc,ndebt),evout4(nc,ndebt))

evout=0.0
DO ind1=1,nr  ! Cycle for inflation rate next period
 evout1=0.0
   DO ind9=1,2    ! Cycle for real interest rates next period
    evout2=0.0
     DO indy=1,nqpy  ! Cycle for temporary labor income shocks
      evout3=0.0
      DO ind8=1,nr     ! Cycle for house prices next period
       evout4=0.0
        DO indpy=1,nr    ! Cycle for permanent labor income next period

            
           DO ind2=1,nc
            DO ind3=1,ndebt
              nw(ind2,ind3)= sav(ind2,ind3)*(1.0+(EXP(gr(1,1)+gi(1,1))-1.0)*(1.0-tax))*pl/pl1 &
                     -anaux+(1.0-tax)*fy(indy,indpy)
            END DO
           END DO

           nw2(:,:)=MAX(nw(:,:),gcash(1,1))
           nw2(:,:)=MIN(nw2(:,:),gcash(ncash,1))
     
           CALL splint(gcash,v(:,ind1,indpy,ind8,ind9),secd2(:,ind1,indpy,ind8,ind9),ncash,nw2(:,:),nc,ndebt,aux)
          
           ! ind8=1 same number of times of low house prices ==> high house price innovation
            evout4(:,:)=evout4(:,:)+ pyh(indpy,ind8)*aux(:,:)
         END DO      ! Close cycle for permanent income next period, depends on house prices
         evout3=evout3+prh(ind9,ind8)*evout4
        END DO      ! Close cycle for house prices next period, depends on real rates
     evout2=evout2+evout3*wmy(indy,1)
    END DO          ! Close cycle for temporary income shocks
    evout1(:,:)=evout1(:,:)+ tpi(1,ind9)*evout2(:,:)
  END DO            ! Close cycle for real rate next period
  evout=evout+p(1,ind1)*evout1
END DO              ! Close cycle for inflation next period

DEALLOCATE(nw,nw2,aux,evout1,evout2,evout3,evout4)
END SUBROUTINE
