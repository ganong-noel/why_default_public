!     Last change:  JFC  28 May 2011    2:34 pm
SUBROUTINE irevi(sav,nc,ndebt,v,vin,ncash,nr,secd2,gcash,lgcash,p,fy,nqpy,wmy,anaux,mortint,gdebt,&
  utbdaux,gi,tax,gr,pl,pl1,ptax,pm,pyh,house,remdebt1,ph1,prh,tpi,evout)
IMPLICIT NONE
INTEGER, INTENT(IN) :: nc, ndebt, ncash, nr, nqpy
REAL, INTENT(IN) :: anaux,mortint,utbdaux,tax, pl, pl1, ptax,pm,house,remdebt1
REAL, INTENT(IN) :: gi(1,1), gr(1,1)
REAL, INTENT(IN) :: sav(nc,ndebt), v(ncash,nr,nr,nr,nr), secd2(ncash,nr,nr,nr,nr), vin(ncash,nr,nr,nr,nr)
REAL, INTENT(IN) :: gcash(ncash,1), lgcash(ncash,1), p(1,nr), ph1(nr,1), tpi(1,nr)
REAL, INTENT(IN) :: fy(nqpy,nr), wmy(nqpy,1)
REAL, INTENT(IN) :: pyh(2,2), prh(2,2)
REAL, INTENT(IN) :: gdebt(ndebt,1)
REAL, INTENT(OUT) :: evout(nc,ndebt)
INTEGER :: ind1, ind2, ind3, indy,ind8,ind9, indpy, icashrent, icashrentaux
REAL :: cashrent, lcashrent, wrent, vrent
REAL, ALLOCATABLE :: nw(:,:), nw2(:,:), aux(:,:), evout1(:,:), evout2(:,:), evout3(:,:), evout4(:,:)
ALLOCATE(nw(nc,ndebt),nw2(nc,ndebt),aux(nc,ndebt))
ALLOCATE(evout1(nc,ndebt),evout2(nc,ndebt),evout3(nc,ndebt),evout4(nc,ndebt))


evout=0.0
 DO ind9=1,2    ! Cycle for real interest rates next period
    evout1=0.0
    DO indy=1,nqpy  ! Cycle for temporary labor income shocks
      evout2=0.0
      DO ind8=1,nr    ! Cycle for house prices next period
        evout3=0.0
        DO indpy=1,nr  ! Cycle for permanent labor income shocks


           DO ind2=1,nc
            DO ind3=1,ndebt
              nw(ind2,ind3)= sav(ind2,ind3)*&
              (1.0+(EXP(gr(1,1)+gi(1,1))-1.0)*(1.0-tax))*pl/pl1-gdebt(ind3,1)*(EXP(gi(1,1))) &
                    -anaux-ptax-pm+(1.0-tax)*fy(indy,indpy)+tax*mortint+tax*ptax
             END DO
           END DO

           nw2(:,:)=MAX(nw(:,:),gcash(1,1))
           nw2(:,:)=MIN(nw2(:,:),gcash(ncash,1))

           evout4=0.0
           DO ind1=1,nr  ! Cycle for inflation rate next period

           CALL splint(gcash,v(:,ind1,indpy,ind8,ind9),secd2(:,ind1,indpy,ind8,ind9),ncash,nw2(:,:),nc,ndebt,aux)

           DO ind2=1,nc
            DO ind3=1,ndebt
                IF (nw(ind2,ind3)<0.0) THEN
                  aux(ind2,ind3)= vin(1,ind1,indpy,ind8,ind9)
                  IF ((0.94*ph1(ind8,1)*pl1*house) > remdebt1) THEN
                    cashrent  = nw(ind2,ind3)+0.94*ph1(ind8,1)*house-remdebt1/pl1
                    cashrent= MAX(MIN(cashrent,gcash(ncash,1)),gcash(1,1))
                    lcashrent= LOG(cashrent)
                    CALL ntoil(lcashrent,lgcash,ncash,icashrent)
                    icashrentaux= icashrent+1
                    wrent= (cashrent-gcash(icashrent,1))/(gcash(icashrentaux,1)-gcash(icashrent,1))
                    wrent= MAX(MIN(1.0,wrent),0.0)
                    vrent=  (1.0-wrent)*(vin(icashrent,ind1,indpy,ind8,ind9))+wrent*(vin(icashrentaux,ind1,indpy,ind8,ind9))
                    aux(ind2,ind3)= vrent
                  END IF
                END IF
             END DO
           END DO



            evout4(:,:)=evout4(:,:)+p(1,ind1)*aux(:,:)
          END DO         ! Close cycle for inflation ! Close cycle for permanent income next period, depends on house prices
         ! ind8=1 same number of times of low house prices ==> high house price innovation
          evout3=evout3+pyh(indpy,ind8)*evout4
      END DO            ! Close cycle for permanent income next period, depends on house prices
      evout2=evout2+prh(ind9,ind8)*evout3
    END DO              ! Close cycle for house prices next period, depends on real rates
   evout1=evout1+wmy(indy,1)*evout2
  END DO                ! Close cycle for temporary income shocks
  evout=evout+tpi(1,ind9)*evout1
END DO                  ! Close cycle for real interest rates

DEALLOCATE(nw,nw2,aux,evout1,evout2,evout3, evout4)
END SUBROUTINE
