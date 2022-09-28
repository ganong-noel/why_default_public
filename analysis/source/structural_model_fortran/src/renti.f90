  !     Last change:  JFC  27 May 2011   10:38 am
PROGRAM renti
IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!
! DEFINE VARIABLES !
!!!!!!!!!!!!!!!!!!!!

CHARACTER(LEN=6), DIMENSION(21,1) :: filename

INTEGER :: td=20, t=20
REAL :: beta=0.98, gamma= 2.0, infinity=-1e+10, bequest=400.0

REAL :: tax=0.25, tax_p= 0.015, m_p= 0.025

REAL :: lti=4.5, dw=0.10, rentalpre=0.0     ! lti is the loan to income, rentalpre is the rental premium

REAL :: loan                                ! loan amount = LTI*levely
REAL :: house                               ! house value at the initial date
REAL :: nomhousval, reahousval              ! nominal and real house value

REAL :: mur=0.0291, sigr= 0.009, phir= 0.891     ! This is for the inflation process
REAL :: mui, sigi, phii, betai, sigrealrate      ! This is for the real interest rates

REAL :: mudph=0.0033,  sigdph= 0.162            ! This is for the real log house price process
REAL :: sigpy= 0.062976, sigty                  ! sigpy: stdev of permanent inc shocks
REAL :: growthy= 0.007963, levely= 46.353       ! growth rate of income and income level at the initial date
REAL, DIMENSION(21,1) :: labinc

REAL :: correl= 0.0, regcoef, sigres, sigres2  ! reg parameter of temp inc shocks on real rate
REAL :: ppyhouse, corrpyhouse= 0.191
REAL :: ppratehouse1, ppratehouse2, sigini, corrratehouse= 0.30
REAL, DIMENSION(2,2,2) :: prh

INTEGER :: nqpy=2, ncash=185, nr=2, ndebt=1, nc=160

INTEGER :: auxmax, ic, idebt, iref
INTEGER :: ind1, ind2, ind3, ind4, ind5, ind6, ind7, indt, ind8, ind9

INTEGER:: taux, ind4aux, ind8aux, taux2, ind5aux
INTEGER, DIMENSION(1) :: pt
REAL :: utbdaux, anaux, anaux2, pricelevel, pricelevel1, tprice
REAL :: sv=0.0

! dimension nr*1
REAL, DIMENSION(2,1) :: gr, giinov
REAL, DIMENSION(2,2) :: gi
! dimension nqpy*1
REAL, DIMENSION(2,1) :: ymr, wmr, ymy, wmy
! dimension nr*nr
REAL, DIMENSION(2,2) :: p, pyh, tpi
REAL, DIMENSION(2,2) :: inr, ini
! dimensions nr*1
REAL, DIMENSION(2,1) :: dlph

! dimension nr*nr*nqpy
REAL, DIMENSION(2,2,2) :: eyt
REAL, DIMENSION(2,2,2) :: expeyt
! dimensions nr*1
REAL, DIMENSION(2,1) :: lyp, yp
! dimensions: nr*nr*nqpy*21*(td+1)
REAL, DIMENSION(2,2,2,21,21) :: fy

! dimensions ncash*1
REAL, DIMENSION(185,1) :: gcash, lgcash
! dimensions ndebt*1
REAL, DIMENSION(1,1) :: gdebt
! dimensions nc*1
REAL, DIMENSION(160,1) :: gcons, lgcons

! dimensions 1*nc
REAL, DIMENSION(1,160) :: util
! dimension (td+1,1)
REAL, DIMENSION(21,1) :: utbd

! dimensions ncash*1
REAL, DIMENSION(185,1) :: sec
! dimensions nr*ncash*nr*nra*lref*nia*2
! nr: dimension = 1 inflation rate in period 1: no longer a state variable / ncash / nr: current inflation rate
! nra: number of times of past high/low inflation / lref=1 / nia: number of times of past high/low perm income/house prices
! 2: current real interest rate
REAL, DIMENSION(1,185,2,21,21,21,2) :: secd2
! dimensions 1*ncash*nr*nra*lref*nia*2
REAL, DIMENSION(1,185,2,21,21,21,2) :: cons=0.0, debt=0.0
REAL, DIMENSION(1,185,2,21,21,21,2) :: vr1=0.0, vr2=0.0
REAL, ALLOCATABLE :: v1(:,:,:), vv(:,:,:), auxv(:,:)
REAL, ALLOCATABLE :: auxv1(:,:)
REAL, ALLOCATABLE :: sav(:,:,:), u2(:,:,:)

INTEGER :: indcase
CHARACTER(len=255) :: cwd
REAL :: stigma

NAMELIST /config_nml/ mui, sigi, phii, betai, sigty, indcase, stigma
OPEN(4, FILE="config_nml.nml")
  READ(4, nml=config_nml)
  WRITE(*,nml=config_nml)
CLOSE(4)


CALL getcwd(cwd)
WRITE(*,*) TRIM(cwd)

loan = lti*levely
house = loan/(1.0-dw)


filename(1,1) = 'rear01'
filename(2,1) = 'rear02'
filename(3,1) = 'rear03'
filename(4,1) = 'rear04'
filename(5,1) = 'rear05'
filename(6,1) = 'rear06'
filename(7,1) = 'rear07'
filename(8,1) = 'rear08'
filename(9,1) = 'rear09'
filename(10,1) = 'rear10'
filename(11,1) = 'rear11'
filename(12,1) = 'rear12'
filename(13,1) = 'rear13'
filename(14,1) = 'rear14'
filename(15,1) = 'rear15'
filename(16,1) = 'rear16'
filename(17,1) = 'rear17'
filename(18,1) = 'rear18'
filename(19,1) = 'rear19'
filename(20,1) = 'rear20'
filename(21,1) = 'rear21'

!!!!!!!!!!!!!!
! QUADRATURE !
!!!!!!!!!!!!!!

ymy(1,1)= -1.0
ymy(2,1)=  1.0
wmy(1,1)= 0.5
wmy(2,1)= 0.5


ymr(1,1)= -1.0
ymr(2,1)= 1.0
wmr(1,1)= 0.5
wmr(2,1)= 0.5


!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! One-period log inflation !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DO ind1=1,nr
   gr(ind1,1)= ymr(ind1,1)*sigr+mur
END DO
WRITE(*,*) gr(1,1)
WRITE(*,*) gr(2,1)


CALL gettrans(gr,wmr,nr,mur,phir,sigr,p)

DO ind1=1,nr
  DO ind2=1,nr
    WRITE(*,*) p(ind1,ind2)
  END DO

END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! One-period log real yield !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DO ind1=1,nr
   giinov(ind1,1)= ymr(ind1,1)*sigi+mui
END DO

CALL gettrans(giinov,wmr,nr,mui,phii,sigi,tpi)

DO ind1=1,nr
   DO ind2=1,nr
       gi(ind2,ind1)= giinov(ind2,1)+betai*gr(ind1,1)
   END DO
END DO


WRITE(*,*) gi(1,1)
WRITE(*,*) gi(2,1)
WRITE(*,*) gi(1,2)
WRITE(*,*) gi(2,2)


DO ind1=1,nr
  DO ind2=1,nr
    WRITE(*,*) tpi(ind1,ind2)
  END DO
END DO



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Innovations to log inflation !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DO ind1=1,nr
  DO ind2=1,nr
    inr(ind1,ind2)= gr(ind2,1)-(p(ind1,1)*gr(1,1)+p(ind1,2)*gr(2,1))
  END DO
END DO

WRITE(*,*) inr(1,1)
WRITE(*,*) inr(1,2)
WRITE(*,*) inr(2,1)
WRITE(*,*) inr(2,2)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Innovations to real rate that are uncorrelated with inflation  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DO ind1=1,nr
  DO ind2=1,nr
    ini(ind1,ind2)= giinov(ind2,1)-(tpi(ind1,1)*giinov(1,1)+tpi(ind1,2)*giinov(2,1))
  END DO
END DO

WRITE(*,*) ini(1,1)
WRITE(*,*) ini(1,2)
WRITE(*,*) ini(2,1)
WRITE(*,*) ini(2,2)







!!!!!!!!!!!!!!!!
! House prices !
!!!!!!!!!!!!!!!!

DO ind1=1,nr
   dlph(ind1,1)= ymr(nr-ind1+1,1)*sigdph+mudph
END DO

WRITE(*,*) dlph(1,1)               ! High house price returns
WRITE(*,*) dlph(2,1)               ! Low house price returns

WRITE(*,*) EXP(dlph(1,1))               ! High house price returns
WRITE(*,*) EXP(dlph(2,1))               ! Low house price returns


!!!!!!!!!!
! Income !
!!!!!!!!!!
DO ind1=1,nr
   lyp(ind1,1)= ymr(nr-ind1+1,1)*sigpy
   yp(ind1,1)= EXP(lyp(ind1,1))
END DO

WRITE(*,*) lyp(1,1)            ! High permanent income
WRITE(*,*) lyp(2,1)            ! Low permanent income


WRITE(*,*) yp(1,1)
WRITE(*,*) yp(2,1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Correlation between innovations to permanent income and house prices !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ppyhouse = (corrpyhouse*sigpy*sigdph-0.5*(dlph(1,1)-mudph)*lyp(2,1)-0.5*(dlph(2,1)-mudph)*lyp(1,1))/ &
 (0.5*(dlph(1,1)-mudph)*lyp(1,1)-0.5*(dlph(1,1)-mudph)*lyp(2,1)-0.5*(dlph(2,1)-mudph)*lyp(1,1)+0.5*(dlph(2,1)-mudph)*lyp(2,1))

WRITE(*,*) ppyhouse

pyh(1,1) = ppyhouse
pyh(2,1) = (1.0-ppyhouse)
pyh(1,2) = (1.0-ppyhouse)
pyh(2,2) = ppyhouse

WRITE(*,*) pyh(1,1)
WRITE(*,*) pyh(2,1)
WRITE(*,*) pyh(1,2)
WRITE(*,*) pyh(2,2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Correlation between innovations to the real rate (that are uncorrelated with inflation) and real house prices !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

sigini = (tpi(1,1)*(ini(1,1)**2.0)+tpi(1,2)*(ini(1,2)**2))**0.5
WRITE(*,*) sigini



ppratehouse2 = (corrratehouse*sigini*sigdph + ( (-tpi(1,1)*(dlph(1,1)-mudph)*ini(1,1)) + &
               (-tpi(1,2)*(dlph(1,1)-mudph)*ini(1,2))+(0.5/tpi(1,1)*(-2*tpi(1,1)*ini(1,1)*(dlph(1,1)-mudph)))))/ &
               (2.0*(dlph(1,1)-mudph)*ini(1,2)*tpi(1,2) + (-tpi(1,2)/tpi(1,1))*(-2.0*tpi(1,1)*ini(1,1)*(dlph(2,1)-mudph)))
ppratehouse1 = 0.5/tpi(1,1)-tpi(1,2)/tpi(1,1)*ppratehouse2



WRITE(*,*) ppratehouse1
WRITE(*,*) ppratehouse2



prh(1,1,1) = ppratehouse1
prh(1,1,2) = (1.0-ppratehouse1)
prh(1,2,1) = ppratehouse2
prh(1,2,2) = (1.0-ppratehouse2)

prh(2,1,1) = (1.0-ppratehouse2)
prh(2,1,2) = ppratehouse2
prh(2,2,1) = (1.0-ppratehouse1)
prh(2,2,2) = ppratehouse1



WRITE(*,*) prh(1,1,1)
WRITE(*,*) prh(1,1,2)
WRITE(*,*) prh(1,2,1)
WRITE(*,*) prh(1,2,2)

WRITE(*,*) prh(2,1,1)
WRITE(*,*) prh(2,1,2)
WRITE(*,*) prh(2,2,1)
WRITE(*,*) prh(2,2,2)






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Correlation between temporary income shocks and the level of real rates !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

sigrealrate = (sigi*sigi+betai*betai*sigr*sigr)**0.5
WRITE(*,*) sigrealrate


regcoef = correl*sigty/sigrealrate
sigres2= sigty*sigty - regcoef*regcoef*sigrealrate*sigrealrate
sigres= sigres2**(0.5)

WRITE (*,*) regcoef


DO ind1=1,nr
   DO ind2=1,nr
     eyt(ind1,ind2,1)= regcoef*(gi(ind1,ind2)-mui-betai*mur)+ymy(1,1)*sigres
     eyt(ind1,ind2,2)= regcoef*(gi(ind1,ind2)-mui-betai*mur)+ymy(2,1)*sigres
   END DO
END DO

DO ind1=1,nr
   DO ind2=1,nr
     WRITE(*,*) eyt(ind1,ind2,1)
     WRITE(*,*) eyt(ind1,ind2,2)

   END DO
END DO
expeyt= EXP(eyt)


labinc(1,1)= levely
DO ind1=2,td+1
   labinc(ind1,1)= levely*(1+growthy)**(ind1-1)
END DO

DO ind1=1,td+1
  taux=ind1
  DO ind2=1,taux   ! ind2 = number times of low permanent income shocks in the past
    fy(:,:,:,ind2,ind1)=labinc(ind1,1)*expeyt(:,:,:)*(yp(2,1)**(ind2-1)*yp(1,1)**(taux-ind2))
  END DO
END DO

WRITE(*,*) labinc(1,1)
WRITE(*,*) labinc(3,1)
WRITE(*,*) fy(1,1,1,1,1)
WRITE(*,*) fy(1,1,1,1,3)
WRITE(*,*) fy(1,1,1,2,3)
WRITE(*,*) fy(1,1,1,3,3)





!!!!!!!!!!!!!!!!!!!
! CONSTRUCT GRIDS !
!!!!!!!!!!!!!!!!!!!

DO ind1=1,ncash
    lgcash(ind1,1)= 0.0+(ind1-1.0)*0.035
END DO
gcash= EXP(lgcash)

WRITE (*,*) gcash(ncash,1)




DO ind1=1,nc
    lgcons(ind1,1)= 0.00+(ind1-1.0)*0.036
END DO
gcons= EXP(lgcons)

WRITE (*,*) gcons(nc,1)




DO ind1=1,ndebt
!   gdebt(ind1,1)= 0.0+(ind1-1.0)*2.0
    gdebt(ind1,1)= 0.0
END DO


!!!!!!!!!!!!!!!!!!!
! TERMINAL PERIOD !
!!!!!!!!!!!!!!!!!!!

t=21
WRITE(*,*) t


DO ind1=1,1
  DO ind2=1,ncash
    DO ind3=1,nr
     DO ind4=1,21
      DO ind5=1,21
       DO ind8=1,21
        DO ind9=1,2
         pricelevel = EXP(gr(2,1))**(ind4-1)*EXP(gr(1,1))**(t-ind4)
         reahousval= ((EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(t-ind8))
         tprice = ((pricelevel**0.5)+(0.3**0.5)*((pricelevel*reahousval)**0.5))**2
         sv= (gcash(ind2,1)*pricelevel)/tprice
         sv= MAX(sv,gcash(1,1))
         vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= bequest*(sv**(1.0-gamma))/(1.0-gamma)
        END DO
       END DO
      END DO
     END DO
    END DO
  END DO
END DO



! OPEN(UNIT=25,FILE=filename(t,1),STATUS='replace',ACTION='write')

! DO ind1= 1,nr
! DO ind3= 1,nr
!   DO ind4=1,16
!    DO ind5=1,1
!     DO ind8=1,16
!      DO ind9=1,2
!      DO ind2= 1,ncash
!        WRITE(25,*) vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
!      END DO
!      END DO
!     END DO
!    END DO
!   END DO
! END DO
! END DO


! CLOSE(UNIT=25)


vr1(:,:,:,:,:,:,:)= vr2(:,:,:,:,:,:,:)


CALL irutil2(gcons,nc,gamma,util)


!!!!!!!!!!!!!!!!!!!!!!!!!!
! LOWER BOUND ON UTILITY !
!!!!!!!!!!!!!!!!!!!!!!!!!!


DO ind1=1,td+1
  IF (ind1.eq.1) THEN
    utbd(td+1,1)= bequest*(gcons(1,1))**(1.0-gamma)/(1.0-gamma)    ! cons = 1
  ELSE
     utbd(td-ind1+2,1)= (gcons(1,1))**(1.0-gamma)/(1.0-gamma)+beta*utbd(td-ind1+3,1)
   END IF
END DO


!!!!!!!!!!!!!!!!!
! OTHER PERIODS !
!!!!!!!!!!!!!!!!!



DO indt= 1,td

  t= td-indt+1
  taux=t+1
  taux2=t+1
  WRITE(*,*) t

! Spline for the value function at t+1: taux (no of past inflation) = t+1 and taux2 (number of past perm inc) = t+1

secd2= 0.0
DO ind1= 1,1
  DO ind2= 1,nr
   DO ind3=1,taux
     DO ind4=1,taux
      DO ind8=1,taux2
       DO ind9=1,2
        CALL spline(gcash(:,1),vr1(ind1,:,ind2,ind3,ind4,ind8,ind9),ncash,gamma,sec)
        secd2(ind1,:,ind2,ind3,ind4,ind8,ind9) = sec(:,1)
       END DO
      END DO
     END DO
   END DO
  END DO
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CYCLE FOR THE STATE VARIABLES !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


taux=t

ind1= 1
  DO ind2= 1,ncash        ! Cycle for cash-on-hand
  WRITE(*,*) ind2
    DO ind3= 1,nr         ! Current inflation rate
     DO ind4=1,t          ! Number of times of high inflation in the past
      DO ind5=1,t         ! Number of times of low permanent income in the past: ind5=1 is zero
       DO ind8=1,t        ! Number of times of low house prices in the past: ind8=1 is zero
        DO ind9=1,2       ! Current real interest rate



ALLOCATE(sav(nc,ndebt,2),u2(nc,ndebt,2))

sav=infinity
u2= infinity

DO ind6=1,nc
  DO ind7=1,ndebt
     sav(ind6,ind7,1)= gcash(ind2,1)-gcons(ind6,1)+gdebt(ind7,1)
  END DO
END DO


DO ind6=1,nc
   DO ind7=1,ndebt
      IF (sav(ind6,ind7,1)<0.0) THEN
         u2(ind6,ind7,1)= infinity
      ELSE
         u2(ind6,ind7,1)=util(1,ind6)
      END IF
   END DO
END DO


ALLOCATE(v1(nc,ndebt,2))
v1=0.0

utbdaux= utbd(t+1,1)
sav=MAX(sav,0.0)


ALLOCATE(auxv1(nc,ndebt))
auxv1=0.0

! USER COST = NOMINAL INTEREST RATE - EXPECTED NOMINAL HOUSE PRICE GROWTH + PROP TAX + MAINT. + RENTAL PREMIUM
anaux=(EXP(gr(ind3,1)+gi(ind9,ind3))-1.0)-(EXP(mudph+sigdph*sigdph/2)*EXP(gr(ind3,1))-1.0)+tax_p+m_p+rentalpre
! PRICE LEVEL
pricelevel = EXP(gr(2,1))**(ind4-1)*EXP(gr(1,1))**(t-ind4)
! REAL AND NOMINAL HOUSE VALUE
reahousval= ((EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(t-ind8))
nomhousval= house*pricelevel*reahousval
! REAL RENTAL PAYMENT
anaux2=anaux*nomhousval/pricelevel


IF (ind3.eq.2) THEN
    ind4aux = ind4+1
ELSE
    ind4aux = ind4
END IF

pricelevel1 = EXP(gr(2,1))**(ind4aux-1)*EXP(gr(1,1))**(t+1-ind4aux)


ind8aux=ind8+1
ind5aux=ind5+1

CALL irevrenti(sav(:,:,1),nc,ndebt,vr1(ind1,:,:,ind4aux,ind5:ind5aux,ind8:ind8aux,:),ncash,nr, &
            secd2(ind1,:,:,ind4aux,ind5:ind5aux,ind8:ind8aux,:), &
            gcash,p(ind3,:),fy(ind9,ind3,:,ind5:ind5aux,t+1),nqpy,wmy,anaux2,gdebt,utbdaux,gi(ind9,ind3),tax, &
            gr(ind3,1), pricelevel, pricelevel1, pyh, prh(ind9,:,:), tpi(ind9,:), auxv1)


v1(:,:,1)=auxv1(:,:)
DEALLOCATE(auxv1)


DEALLOCATE(sav)
ALLOCATE(vv(nc,ndebt,2))
vv=0.0
vv=u2+beta*v1
ALLOCATE(auxv(nc*ndebt*2,1))
auxv=RESHAPE((vv),(/nc*ndebt*2,1/))
vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= MAXVAL(auxv(:,1))

pt= MAXLOC(auxv(:,1))
auxmax= FLOOR((REAL(pt(1))-1.0)/REAL(nc*ndebt))
iref= auxmax+1
idebt= (pt(1)-1-(iref-1)*nc*ndebt)/nc+1
ic= (pt(1)-1-(iref-1)*nc*ndebt-(idebt-1)*nc)+1
cons(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gcons(ic,1)
debt(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gdebt(idebt,1)

DEALLOCATE(auxv,vv,v1,u2)

IF (vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)<-10000) THEN


  cons(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gcons(1,1)
  debt(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gdebt(1,1)
  vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=utbd(t,1)
END IF

        END DO
       END DO
     END DO
    END DO
  END DO
END DO


WRITE(*,*) gcash(1,1)


! cons(2,:,:,:,:,:,:)= cons(1,:,:,:,:,:,:)
! debt(2,:,:,:,:,:,:)= debt(1,:,:,:,:,:,:)
! vr2(2,:,:,:,:,:,:)= vr2(1,:,:,:,:,:,:)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WRITE CONS CHOICES AND VALUE FUNCTION !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

WRITE(*,*) gcash(1,1)


OPEN(UNIT=25,FILE=filename(t,1),STATUS='replace',ACTION='write')

DO ind1= 1,1
  DO ind3= 1,nr
    DO ind4=1,taux
     DO ind5=1,t
      DO ind8=1,t
       DO ind9=1,2
       DO ind2= 1,ncash
         WRITE(25,*) vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
       END DO
       END DO
      END DO
     END DO
    END DO
  END DO
END DO

DO ind1= 1,1
  DO ind3= 1,nr
    DO ind4=1,taux
     DO ind5=1,t
      DO ind8=1,t
       DO ind9=1,2
       DO ind2= 1,ncash
           WRITE(25,*) cons(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
       END DO
       END DO
      END DO
     END DO
    END DO
  END DO
END DO

! DO ind1= 1,nr
!  DO ind3= 1,nr
!    DO ind4=1,taux
!     DO ind5=1,1
!       DO ind8=1,t
!       DO ind9=1,2
!        DO ind2= 1,ncash
!         WRITE(25,*) debt(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
!        END DO
!       END DO
!       END DO
!      END DO
!     END DO
!  END DO
! END DO


CLOSE(UNIT=25)

vr1(:,:,:,:,:,:,:)= vr2(:,:,:,:,:,:,:)

END DO ! FOR THE t CYCLE


END PROGRAM
