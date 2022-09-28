!     Last change:  JFC  24 Feb 2012    9:38 am
PROGRAM armi
IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!
! DEFINE VARIABLES !
!!!!!!!!!!!!!!!!!!!!
CHARACTER(LEN=6), DIMENSION(20,1) :: filein
REAL, ALLOCATABLE :: aux_in(:,:)
REAL, DIMENSION(1,185,2,21,21,21,2) :: v_in, v_in_1
INTEGER:: nrec, auxnrecin1, auxnrecin2
INTEGER, DIMENSION(1,185,2,21,21,21,2) :: def=0

INTEGER :: icashrent, icashrentaux
REAL :: cashrent, lcashrent, wrent, vrent, vstayaux

CHARACTER(LEN=6), DIMENSION(1,1) :: filemortchoice
CHARACTER(LEN=6), DIMENSION(21,1) :: filename

INTEGER :: td=20, t=20
REAL :: beta=0.98, gamma= 2.0, infinity=-1e+10, bequest=400.0

REAL :: tax=0.25, tax_p= 0.015, m_p= 0.025
REAL :: reahousval, nomhousval, proptax, propmaint


REAL :: theta=0.017
INTEGER ::   indcase     ! Case: 1 low inf low real; 2 low inf high real; 3 high inf low real; 4 high inf high real

REAL :: lti=4.5, dw=0.10
REAL :: loan                              ! loan amount = LTI*levely
REAL :: house                             ! house value at the initial date

REAL :: mur=0.0291, sigr= 0.009, phir= 0.891      ! This is for the inflation process
REAL :: mui, sigi, phii, betai, sigrealrate       ! This is for the real rate process

REAL :: mudph=0.0033,  sigdph= 0.162        ! This is for the real house price process
REAL :: sigpy= 0.062976, sigty              ! sigpy: stdev of permanent inc shocks
REAL :: growthy= 0.007963, levely= 46.353   ! growth rate of income and income level at the initial date
REAL, DIMENSION(21,1) :: labinc

REAL :: correl= 0.00, regcoef, sigres, sigres2  ! reg parameter of temp inc shocks on real rate
REAL :: ppyhouse, corrpyhouse= 0.191

REAL :: ppratehouse1, ppratehouse2, sigini, corrratehouse= 0.30
REAL, DIMENSION(2,2,2) :: prh

REAL :: vstay, probmove=0.04, ratiolock= 0.20

INTEGER :: nqpy=2, ncash=185, nr=2, ndebt=1, nc=160

INTEGER :: auxmax, ic, idebt, iref
INTEGER :: ind1, ind2, ind3, ind4, ind5, ind6, ind7, indt, indaux, ind8, ind9

INTEGER:: ind1aux, taux, ind4aux, taux2, ind8aux, ind5aux
INTEGER, DIMENSION(1) :: pt
REAL :: utbdaux, anaux, anaux2, pricelevel, pricelevel1, tprice, remdebt1
REAL :: mortint, mortint2
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
REAL, DIMENSION(2,1) :: dlph, ph1


! dimension nr*td
REAL, DIMENSION(4,20) :: lyield, yield
REAL, DIMENSION(4,20) :: anfactor, anuity, loanrepaid
REAL, DIMENSION(4,21) :: remdebt
! dimension nr*nr*td
REAL, DIMENSION(4,4,20) :: prob
REAL, DIMENSION(4,4) :: probinfr

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
! dimensions nr*ncash*nr*nra*lref*nia*2 ---- nra= number of times with high interest ---- lref= period when last refinance
REAL, DIMENSION(1,185,2,21,21,21,2) :: secd2
! dimensions nr*ncash*nr*nra*lref*nia*2
REAL, DIMENSION(1,185,2,21,21,21,2) :: cons=0.0, debt=0.0
REAL, DIMENSION(1,185,2,21,21,21,2) :: vr1=0.0, vr2=0.0, vstayout=0.0, vrentout=0.0
REAL, ALLOCATABLE :: v1(:,:,:), vv(:,:,:), auxv(:,:)
REAL, ALLOCATABLE :: auxv1(:,:)
REAL, ALLOCATABLE :: sav(:,:,:), u2(:,:,:)
REAL :: stigma

NAMELIST /config_nml/ mui, sigi, phii, betai, sigty, indcase, stigma
OPEN(4, FILE="config_nml.nml")
  READ(4, nml=config_nml)
  WRITE(*, nml=config_nml)
CLOSE(4)

loan = lti*levely
house = loan/(1.0-dw)

filemortchoice(1,1) = 'v01arm'

filename(1,1) = 'year01'
filename(2,1) = 'year02'
filename(3,1) = 'year03'
filename(4,1) = 'year04'
filename(5,1) = 'year05'
filename(6,1) = 'year06'
filename(7,1) = 'year07'
filename(8,1) = 'year08'
filename(9,1) = 'year09'
filename(10,1) = 'year10'
filename(11,1) = 'year11'
filename(12,1) = 'year12'
filename(13,1) = 'year13'
filename(14,1) = 'year14'
filename(15,1) = 'year15'
filename(16,1) = 'year16'
filename(17,1) = 'year17'
filename(18,1) = 'year18'
filename(19,1) = 'year19'
filename(20,1) = 'year20'
filename(21,1) = 'year21'

filein(1,1) = 'rear01'
filein(2,1) = 'rear02'
filein(3,1) = 'rear03'
filein(4,1) = 'rear04'
filein(5,1) = 'rear05'
filein(6,1) = 'rear06'
filein(7,1) = 'rear07'
filein(8,1) = 'rear08'
filein(9,1) = 'rear09'
filein(10,1) = 'rear10'
filein(11,1) = 'rear11'
filein(12,1) = 'rear12'
filein(13,1) = 'rear13'
filein(14,1) = 'rear14'
filein(15,1) = 'rear15'
filein(16,1) = 'rear16'
filein(17,1) = 'rear17'
filein(18,1) = 'rear18'
filein(19,1) = 'rear19'
filein(20,1) = 'rear20'


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

WRITE(*,*) EXP(gr(1,1))
WRITE(*,*) EXP(gr(2,1))


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


WRITE(*,*) EXP(dlph(1,1))
WRITE(*,*) EXP(dlph(2,1))

!!!!!!!!!!
! Income !
!!!!!!!!!!

DO ind1=1,nr
   lyp(ind1,1)= ymr(nr-ind1+1,1)*sigpy
   yp(ind1,1)= EXP(lyp(ind1,1))
END DO


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

!!!!!!!!!!!!!!!!!!!
! CONSTRUCT GRIDS !
!!!!!!!!!!!!!!!!!!!
DO ind1=1,ncash
   lgcash(ind1,1)= 0.0+(ind1-1.0)*0.035
END DO
gcash= EXP(lgcash)

WRITE (*,*) gcash(1,1)
WRITE (*,*) gcash(ncash,1)

DO ind1=1,nc
   lgcons(ind1,1)= 0.00+(ind1-1.0)*0.036
END DO
gcons= EXP(lgcons)

WRITE (*,*) gcons(1,1)
WRITE (*,*) gcons(nc,1)

! Grid for when allow for additional loan
DO ind1=1,ndebt
!   gdebt(ind1,1)= 0.0+(ind1-1.0)*1.0
    gdebt(ind1,1)= 0.0
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CONSTRUCT TERM STRUCTURE !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
lyield=0.0
yield=0.0

lyield(1,1)=gr(1,1)+giinov(1,1)+betai*gr(1,1)
lyield(2,1)=gr(1,1)+giinov(2,1)+betai*gr(1,1)
lyield(3,1)=gr(2,1)+giinov(1,1)+betai*gr(2,1)
lyield(4,1)=gr(2,1)+giinov(2,1)+betai*gr(2,1)

WRITE (*,*) lyield(1,1)
WRITE (*,*) lyield(2,1)
WRITE (*,*) lyield(3,1)
WRITE (*,*) lyield(4,1)

yield(1,1)=EXP(lyield(1,1))-1
yield(2,1)=EXP(lyield(2,1))-1
yield(3,1)=EXP(lyield(3,1))-1
yield(4,1)=EXP(lyield(4,1))-1

WRITE (*,*) yield(1,1)
WRITE (*,*) yield(2,1)
WRITE (*,*) yield(3,1)
WRITE (*,*) yield(4,1)

! Joint probability
probinfr= 0.0
probinfr(1,1)= p(1,1)*tpi(1,1)
probinfr(1,2)= p(1,1)*tpi(1,2)
probinfr(1,3)= p(1,2)*tpi(1,1)
probinfr(1,4)= p(1,2)*tpi(1,2)

probinfr(2,1)= p(1,1)*tpi(2,1)
probinfr(2,2)= p(1,1)*tpi(2,2)
probinfr(2,3)= p(1,2)*tpi(2,1)
probinfr(2,4)= p(1,2)*tpi(2,2)

probinfr(3,1)= p(2,1)*tpi(1,1)
probinfr(3,2)= p(2,1)*tpi(1,2)
probinfr(3,3)= p(2,2)*tpi(1,1)
probinfr(3,4)= p(2,2)*tpi(1,2)

probinfr(4,1)= p(2,1)*tpi(2,1)
probinfr(4,2)= p(2,1)*tpi(2,2)
probinfr(4,3)= p(2,2)*tpi(2,1)
probinfr(4,4)= p(2,2)*tpi(2,2)

prob= 0.0
prob(1,1,2)=probinfr(1,1)
prob(1,2,2)=probinfr(1,2)
prob(1,3,2)=probinfr(1,3)
prob(1,4,2)=probinfr(1,4)

prob(2,1,2)=probinfr(2,1)
prob(2,2,2)=probinfr(2,2)
prob(2,3,2)=probinfr(2,3)
prob(2,4,2)=probinfr(2,4)

prob(3,1,2)=probinfr(3,1)
prob(3,2,2)=probinfr(3,2)
prob(3,3,2)=probinfr(3,3)
prob(3,4,2)=probinfr(3,4)

prob(4,1,2)=probinfr(4,1)
prob(4,2,2)=probinfr(4,2)
prob(4,3,2)=probinfr(4,3)
prob(4,4,2)=probinfr(4,4)

DO ind1=3,20
   ind1aux=ind1-1
   prob(1,1,ind1)= prob(1,1,ind1aux)*probinfr(1,1)+prob(1,2,ind1aux)*probinfr(2,1)&
                   +prob(1,3,ind1aux)*probinfr(3,1)+prob(1,4,ind1aux)*probinfr(4,1)
   prob(1,2,ind1)= prob(1,1,ind1aux)*probinfr(1,2)+prob(1,2,ind1aux)*probinfr(2,2)&
                   +prob(1,3,ind1aux)*probinfr(3,2)+prob(1,4,ind1aux)*probinfr(4,2)
   prob(1,3,ind1)= prob(1,1,ind1aux)*probinfr(1,3)+prob(1,2,ind1aux)*probinfr(2,3)&
                   +prob(1,3,ind1aux)*probinfr(3,3)+prob(1,4,ind1aux)*probinfr(4,3)
   prob(1,4,ind1)= prob(1,1,ind1aux)*probinfr(1,4)+prob(1,2,ind1aux)&
                   *probinfr(2,4)+prob(1,3,ind1aux)*probinfr(3,4)+prob(1,4,ind1aux)*probinfr(4,4)
   prob(2,1,ind1)= prob(2,1,ind1aux)*probinfr(1,1)+prob(2,2,ind1aux)*probinfr(2,1)&
                   +prob(2,3,ind1aux)*probinfr(3,1)+prob(2,4,ind1aux)*probinfr(4,1)
   prob(2,2,ind1)= prob(2,1,ind1aux)*probinfr(1,2)+prob(2,2,ind1aux)*probinfr(2,2)&
                   +prob(2,3,ind1aux)*probinfr(3,2)+prob(2,4,ind1aux)*probinfr(4,2)
   prob(2,3,ind1)= prob(2,1,ind1aux)*probinfr(1,3)+prob(2,2,ind1aux)*probinfr(2,3)&
                   +prob(2,3,ind1aux)*probinfr(3,3)+prob(2,4,ind1aux)*probinfr(4,3)
   prob(2,4,ind1)= prob(2,1,ind1aux)*probinfr(1,4)+prob(2,2,ind1aux)*probinfr(2,4)&
                   +prob(2,3,ind1aux)*probinfr(3,4)+prob(2,4,ind1aux)*probinfr(4,4)
   prob(3,1,ind1)= prob(3,1,ind1aux)*probinfr(1,1)+prob(3,2,ind1aux)*probinfr(2,1)&
                   +prob(3,3,ind1aux)*probinfr(3,1)+prob(3,4,ind1aux)*probinfr(4,1)
   prob(3,2,ind1)= prob(3,1,ind1aux)*probinfr(1,2)+prob(3,2,ind1aux)*probinfr(2,2)&
                   +prob(3,3,ind1aux)*probinfr(3,2)+prob(3,4,ind1aux)*probinfr(4,2)
   prob(3,3,ind1)= prob(3,1,ind1aux)*probinfr(1,3)+prob(3,2,ind1aux)*probinfr(2,3)&
                   +prob(3,3,ind1aux)*probinfr(3,3)+prob(3,4,ind1aux)*probinfr(4,3)
   prob(3,4,ind1)= prob(3,1,ind1aux)*probinfr(1,4)+prob(3,2,ind1aux)*probinfr(2,4)&
                  +prob(3,3,ind1aux)*probinfr(3,4)+prob(3,4,ind1aux)*probinfr(4,4)
   prob(4,1,ind1)= prob(4,1,ind1aux)*probinfr(1,1)+prob(4,2,ind1aux)*probinfr(2,1)+&
                   prob(4,3,ind1aux)*probinfr(3,1)+prob(4,4,ind1aux)*probinfr(4,1)
   prob(4,2,ind1)= prob(4,1,ind1aux)*probinfr(1,2)+prob(4,2,ind1aux)*probinfr(2,2)+&
                   prob(4,3,ind1aux)*probinfr(3,2)+prob(4,4,ind1aux)*probinfr(4,2)
   prob(4,3,ind1)= prob(4,1,ind1aux)*probinfr(1,3)+prob(4,2,ind1aux)*probinfr(2,3)+&
                   prob(4,3,ind1aux)*probinfr(3,3)+prob(4,4,ind1aux)*probinfr(4,3)
   prob(4,4,ind1)= prob(4,1,ind1aux)*probinfr(1,4)+prob(4,2,ind1aux)*probinfr(2,4)+&
                   prob(4,3,ind1aux)*probinfr(3,4)+prob(4,4,ind1aux)*probinfr(4,4)
END DO

DO ind1=2,20
   ind1aux=ind1-1
   lyield(1,ind1)=1.0/ind1*(lyield(1,ind1aux)*(ind1aux)+prob(1,1,ind1)*lyield(1,1)+&
                  prob(1,2,ind1)*lyield(2,1)+prob(1,3,ind1)*lyield(3,1)+prob(1,4,ind1)*lyield(4,1))
   lyield(2,ind1)=1.0/ind1*(lyield(2,ind1aux)*(ind1aux)+prob(2,1,ind1)*lyield(1,1)+&
                   prob(2,2,ind1)*lyield(2,1)+prob(2,3,ind1)*lyield(3,1)+prob(2,4,ind1)*lyield(4,1))
   lyield(3,ind1)=1.0/ind1*(lyield(3,ind1aux)*(ind1aux)+prob(3,1,ind1)*lyield(1,1)+&
                   prob(3,2,ind1)*lyield(2,1)+prob(3,3,ind1)*lyield(3,1)+prob(3,4,ind1)*lyield(4,1))
   lyield(4,ind1)=1.0/ind1*(lyield(4,ind1aux)*(ind1aux)+prob(4,1,ind1)*lyield(1,1)+&
                   prob(4,2,ind1)*lyield(2,1)+prob(4,3,ind1)*lyield(3,1)+prob(4,4,ind1)*lyield(4,1))
END DO

WRITE (*,*) lyield(1,20)
WRITE (*,*) lyield(2,20)
WRITE (*,*) lyield(3,20)
WRITE (*,*) lyield(4,20)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ANUITY FACTOR AND ANUITY !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
anfactor=0.0

DO ind1=1,20
  anfactor(1,1)=anfactor(1,1)+1/(EXP(lyield(1,20))+theta)**ind1
  anfactor(2,1)=anfactor(2,1)+1/(EXP(lyield(2,20))+theta)**ind1
  anfactor(3,1)=anfactor(3,1)+1/(EXP(lyield(3,20))+theta)**ind1
  anfactor(4,1)=anfactor(4,1)+1/(EXP(lyield(4,20))+theta)**ind1
END DO

WRITE(*,*) anfactor(1,1)
WRITE(*,*) anfactor(2,1)
WRITE(*,*) anfactor(3,1)
WRITE(*,*) anfactor(4,1)

anuity=0.0
anuity(1,1)=loan/anfactor(1,1)
anuity(2,1)=loan/anfactor(2,1)
anuity(3,1)=loan/anfactor(3,1)
anuity(4,1)=loan/anfactor(4,1)

remdebt=0.0
remdebt(1,1)=loan
remdebt(2,1)=loan
remdebt(3,1)=loan
remdebt(4,1)=loan

DO ind1=2,20
   ind1aux=ind1-1
   remdebt(1,ind1)=remdebt(1,ind1aux)-(anuity(1,1)-remdebt(1,ind1aux)*(EXP(lyield(1,20))+theta-1.0))
   remdebt(2,ind1)=remdebt(2,ind1aux)-(anuity(2,1)-remdebt(2,ind1aux)*(EXP(lyield(2,20))+theta-1.0))
   remdebt(3,ind1)=remdebt(3,ind1aux)-(anuity(3,1)-remdebt(3,ind1aux)*(EXP(lyield(3,20))+theta-1.0))
   remdebt(4,ind1)=remdebt(4,ind1aux)-(anuity(4,1)-remdebt(4,ind1aux)*(EXP(lyield(4,20))+theta-1.0))
END DO

WRITE(*,*) anuity(1,1)
WRITE(*,*) anuity(2,1)
WRITE(*,*) anuity(3,1)
WRITE(*,*) anuity(4,1)

DO ind1=1,20
   WRITE(*,*) remdebt(1,ind1)
END DO

DO ind1=2,20
   ind1aux=ind1-1
   loanrepaid(1,ind1aux)=remdebt(1,ind1aux)-remdebt(1,ind1)
   loanrepaid(2,ind1aux)=remdebt(2,ind1aux)-remdebt(2,ind1)
   loanrepaid(3,ind1aux)=remdebt(3,ind1aux)-remdebt(3,ind1)
   loanrepaid(4,ind1aux)=remdebt(4,ind1aux)-remdebt(4,ind1)
END DO
loanrepaid(1,20)=remdebt(1,20)-0.0
loanrepaid(2,20)=remdebt(2,20)-0.0
loanrepaid(3,20)=remdebt(3,20)-0.0
loanrepaid(4,20)=remdebt(4,20)-0.0

DO ind1=1,20
   WRITE(*,*) loanrepaid(1,ind1)
END DO

DO ind1=1,20
   WRITE(*,*) loanrepaid(2,ind1)
END DO

DO ind1=1,20
   WRITE(*,*) loanrepaid(3,ind1)
END DO

DO ind1=1,20
   WRITE(*,*) loanrepaid(4,ind1)
END DO

IF (indcase .eq. 2) THEN
    anuity(1,:)= anuity(2,:)
    loanrepaid(1,:)= loanrepaid(2,:)
    remdebt(1,:)= remdebt(2,:)
    WRITE(*,*) indcase
    WRITE(*,*) loanrepaid(1,20)

    ELSE IF (indcase .eq. 3) THEN
    anuity(1,:)= anuity(3,:)
    loanrepaid(1,:)= loanrepaid(3,:)
    remdebt(1,:)= remdebt(3,:)
    WRITE(*,*) indcase
    WRITE(*,*) loanrepaid(1,20)

    ELSE IF (indcase .eq. 4) THEN
    anuity(1,:)= anuity(4,:)
    loanrepaid(1,:)= loanrepaid(4,:)
    remdebt(1,:)= remdebt(4,:)
    WRITE(*,*) indcase
    WRITE(*,*) loanrepaid(1,20)
END IF

!!!!!!!!!!!!!!!!!!!
! TERMINAL PERIOD !
!!!!!!!!!!!!!!!!!!!
! At the terminal date derive utility from accumulated wealth including housing wealth
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
         sv= (gcash(ind2,1)+house*(EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(21-ind8))*pricelevel/tprice
         sv= MAX(sv,gcash(1,1))
         vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= bequest*(sv**(1.0-gamma))/(1.0-gamma)
        END DO
       END DO
      END DO
     END DO
    END DO
  END DO
END DO

vr1(:,:,:,:,:,:,:)= vr2(:,:,:,:,:,:,:)

CALL irutil2(gcons,nc,gamma,util)

!!!!!!!!!!!!!!!!!!!!!!!!!!
! LOWER BOUND ON UTILITY !
!!!!!!!!!!!!!!!!!!!!!!!!!!
DO ind1=1,td+1
  IF (ind1.eq.1) THEN
     utbd(td+1,1)= bequest*(gcons(1,1))**(1.0-gamma)/(1.0-gamma)
  ELSE
     utbd(td-ind1+2,1)= (gcons(1,1))**(1.0-gamma)/(1.0-gamma)+beta*utbd(td-ind1+3,1)
  END IF
END DO

!!!!!!!!!!!!!!!!!
! OTHER PERIODS !
!!!!!!!!!!!!!!!!!
DO indt= 1,td

  t= td-indt+1
  WRITE(*,*) t

! Spline for the value function at t+1: taux has to be equal to t+2
taux=t+1
taux2= t+1

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

IF (t.eq.20) THEN

DO ind1=1,1
 DO ind2=1,nr
  DO ind3=1,taux
   DO ind4=1,taux
    DO ind5=1,taux2
     DO ind6=1,2
      DO ind8=1,ncash
         pricelevel = EXP(gr(2,1))**(ind4-1)*EXP(gr(1,1))**(t+1-ind4)
         reahousval= ((EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(t+1-ind8))
         tprice = ((pricelevel**0.5)+(0.3**0.5)*((pricelevel*reahousval)**0.5))**2
         sv= gcash(ind8,1)*pricelevel/tprice
         sv= MAX(sv,gcash(1,1))
         v_in_1(ind1,ind8,ind2,ind3,ind4,ind5,ind6)= bequest*(sv**(1.0-gamma))/(1.0-gamma)
      END DO
     END DO
    END DO
   END DO
  END DO
 END DO
END DO


ELSE

nrec=1*ncash*nr*taux*taux*taux2*2*2
auxnrecin1=1*ncash*nr*taux*taux*taux2*2
ALLOCATE(aux_in(nrec,1))
OPEN(UNIT=25,FILE=filein(taux,1),STATUS='old',ACTION='read')
READ(25,*) aux_in
CLOSE(UNIT=25)
DO ind1=1,1
 DO ind2=1,nr
  DO ind3=1,taux
   DO ind4=1,taux
    DO ind5=1,taux2
     DO ind6=1,2
        auxnrecin2=(ind1-1)*nr*taux*taux*taux2*2*ncash+(ind2-1)*taux*taux*taux2*2*ncash &
           +(ind3-1)*taux*taux2*2*ncash+(ind4-1)*taux2*2*ncash+(ind5-1)*2*ncash+(ind6-1)*ncash
        v_in_1(ind1,:,ind2,ind3,ind4,ind5,ind6)= aux_in(0*auxnrecin1+auxnrecin2+1:0*auxnrecin1+auxnrecin2+ncash,1)
     END DO
    END DO
   END DO
  END DO
 END DO
END DO
DEALLOCATE(aux_in)

END IF


!!!!!!!!!!!!!!!!!!!!!!!!!
! Continue the program
!!!!!!!!!!!!!!!!!!!!!!!!!
taux=t
taux2=t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ VALUE FUNCTION IN THE DEFAULT STATE !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
nrec=1*ncash*nr*taux*taux*taux2*2*2
auxnrecin1=1*ncash*nr*taux*taux*taux2*2
def=0

ALLOCATE(aux_in(nrec,1))
OPEN(UNIT=25,FILE=filein(t,1),STATUS='old',ACTION='read')
READ(25,*) aux_in
CLOSE(UNIT=25)
DO ind1=1,1
 DO ind2=1,nr
  DO ind3=1,taux
   DO ind4=1,taux
    DO ind5=1,taux2
     DO ind6=1,2
        auxnrecin2=(ind1-1)*nr*taux*taux*taux2*2*ncash+(ind2-1)*taux*taux*taux2*2*ncash &
           +(ind3-1)*taux*taux2*2*ncash+(ind4-1)*taux2*2*ncash+(ind5-1)*2*ncash+(ind6-1)*ncash
        v_in(ind1,:,ind2,ind3,ind4,ind5,ind6)= aux_in(0*auxnrecin1+auxnrecin2+1:0*auxnrecin1+auxnrecin2+ncash,1)
     END DO
    END DO
   END DO
  END DO
 END DO
END DO
DEALLOCATE(aux_in)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CYCLE FOR THE STATE VARIABLES !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ind1= 1                   ! The inflation in period 1 is not a state variable
  DO ind2= 1,ncash

  WRITE(*,*) ind2

    DO ind3= 1,nr         ! Current inflation rate
     DO ind4=1,t          ! Number of times of high inflation in the past: 1 stands for zero
      DO ind5=1,t         ! Number of times of low permanent income in the past
       DO ind8=1,t        ! Number of times of low house prices in the past
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

! NOMINAL MORTGAGE PAYMENT = LOAN REPAYMENT + INTEREST PAYMENT ON REMAINING DEBT
anaux=loanrepaid(ind1,t)+(EXP(gr(ind3,1)+gi(ind9,ind3))-1.0+theta)*remdebt(ind1,t)
mortint=(EXP(gr(ind3,1)+gi(ind9,ind3))-1.0+theta)*remdebt(ind1,t)
! PRICE LEVEL
pricelevel = EXP(gr(2,1))**(ind4-1)*EXP(gr(1,1))**(t-ind4)
! REAL MORTGAGE PAYMENT
anaux2=anaux/pricelevel
mortint2= mortint/pricelevel

! REAL AND NOMINAL HOUSE VALUE
reahousval= ((EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(t-ind8))
nomhousval= house*pricelevel*reahousval
! PROPERTY TAXES AND MAINTENANCE
proptax   = tax_p*reahousval
propmaint = m_p*reahousval

IF (ind3.eq.2) THEN
    ind4aux = ind4+1
ELSE
    ind4aux = ind4
END IF

pricelevel1 = EXP(gr(2,1))**(ind4aux-1)*EXP(gr(1,1))**(t+1-ind4aux)

ind8aux=ind8+1
ind5aux=ind5+1

ph1(1,1)= ((EXP(dlph(2,1)))**(ind8-1)*(EXP(dlph(1,1)))**(t+1-ind8))
ph1(2,1)= ((EXP(dlph(2,1)))**(ind8aux-1)*(EXP(dlph(1,1)))**(t+1-ind8aux))
remdebt1= remdebt(ind1,t+1)

CALL irevi(sav(:,:,1),nc,ndebt,vr1(ind1,:,:,ind4aux,ind5:ind5aux,ind8:ind8aux,:), &
            v_in_1(ind1,:,:,ind4aux,ind5:ind5aux,ind8:ind8aux,:), &
             ncash,nr, secd2(ind1,:,:,ind4aux,ind5:ind5aux,ind8:ind8aux,:),gcash,lgcash,p(ind3,:), &
             fy(ind9,ind3,:,ind5:ind5aux,t+1),nqpy,wmy,anaux2,mortint2,gdebt,utbdaux,gi(ind9,ind3),tax, &
             gr(ind3,1), pricelevel, pricelevel1, proptax, propmaint,pyh,house,remdebt1,ph1,prh(ind9,:,:),tpi(ind9,:),auxv1)

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

vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9) = vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9) = v_in(ind1,ind2,ind3,ind4,ind5,ind8,ind9) - stigma

IF (vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9) + stigma < v_in(ind1,ind2,ind3,ind4,ind5,ind8,ind9)) THEN
  IF (((0.94*nomhousval) .le. remdebt(ind1,t))) THEN
    vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=v_in(ind1,ind2,ind3,ind4,ind5,ind8,ind9) - stigma
    def(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=1
   END IF
END IF

! IF ((def(ind1,ind2,ind3,ind4,ind5,ind8,ind9) .eq. 1) .and. ((0.94*nomhousval)>remdebt(ind1,t))) THEN
  !  cashrent= gcash(ind2,1)+(0.94*nomhousval-remdebt(ind1,t))/pricelevel
  !  cashrent= MAX(MIN(cashrent,gcash(ncash,1)),gcash(1,1))
  !  lcashrent= LOG(cashrent)
  !  CALL ntoil(lcashrent,lgcash,ncash,icashrent)
  !  icashrentaux= icashrent+1
  !  wrent= (cashrent-gcash(icashrent,1))/(gcash(icashrentaux,1)-gcash(icashrent,1))
  !  wrent= MAX(MIN(1.0,wrent),0.0)
  !  vrent=  (1.0-wrent)*(v_in(ind1,icashrent,ind3,ind4,ind5,ind8,ind9))+wrent*(v_in(ind1,icashrentaux,ind3,ind4,ind5,ind8,ind9))
  !  vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= vrent
  !  def(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= 2
  !  vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= vrent
!END IF

IF ((def(ind1,ind2,ind3,ind4,ind5,ind8,ind9) .eq. 0) .and. ((0.94*nomhousval)>remdebt(ind1,t))) THEN
    cashrent= gcash(ind2,1)+(0.94*nomhousval-remdebt(ind1,t))/pricelevel
    cashrent= MAX(MIN(cashrent,gcash(ncash,1)),gcash(1,1))
    lcashrent= LOG(cashrent)
    CALL ntoil(lcashrent,lgcash,ncash,icashrent)
    icashrentaux= icashrent+1
    wrent= (cashrent-gcash(icashrent,1))/(gcash(icashrentaux,1)-gcash(icashrent,1))
    wrent= MAX(MIN(1.0,wrent),0.0)
    vrent=  (1.0-wrent)*(v_in(ind1,icashrent,ind3,ind4,ind5,ind8,ind9))+wrent*(v_in(ind1,icashrentaux,ind3,ind4,ind5,ind8,ind9))
    vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= vrent
    IF (vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)<vrent) THEN
        vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=vrent
        def(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=2
     END IF
END IF

IF (t>1)  THEN
    cashrent= gcash(ind2,1)+(0.94*nomhousval-remdebt(ind1,t))/pricelevel
    cashrent= MAX(gcash(ind2,1),cashrent)
    cashrent= MAX(MIN(cashrent,gcash(ncash,1)),gcash(1,1))
    lcashrent= LOG(cashrent)
    CALL ntoil(lcashrent,lgcash,ncash,icashrent)
    icashrentaux= icashrent+1
    wrent= (cashrent-gcash(icashrent,1))/(gcash(icashrentaux,1)-gcash(icashrent,1))
    wrent= MAX(MIN(1.0,wrent),0.0)
    vrent= (1.0-wrent)*(v_in(ind1,icashrent,ind3,ind4,ind5,ind8,ind9))+wrent*(v_in(ind1,icashrentaux,ind3,ind4,ind5,ind8,ind9))
    vstay= vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
    vstayaux = vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
    IF ((0.94*nomhousval)>remdebt(ind1,t))  THEN
       vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= probmove*vrent+(1.0-probmove)*vstay
       vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9) = probmove*vrent+(1.0-probmove)*vstayaux

    ELSE
       vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= probmove*ratiolock*(vrent - stigma)+(1.0-probmove*ratiolock)*vstay
       vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= probmove*ratiolock*(vrent - stigma)+(1.0-probmove*ratiolock)*vstayaux
       vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9) = vrent - stigma

    END IF
END IF

! LOWER BOUND ON UTILITY
IF (vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)<-10000) THEN
  cons(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gcons(1,1)
  debt(ind1,ind2,ind3,ind4,ind5,ind8,ind9)= gdebt(1,1)
  vr2(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=utbd(t,1)
END IF

IF (vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)<-10000) THEN
  vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=utbd(t,1)
END IF

IF (vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)<-10000) THEN
  vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)=utbd(t,1)
END IF

        END DO
       END DO
     END DO
   END DO
  END DO
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WRITE CONS CHOICES AND VALUE FUNCTION !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OPEN(UNIT=25,FILE=filename(t,1),STATUS='replace',ACTION='write')
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

DO ind1= 1,1
  DO ind3= 1,nr
    DO ind4=1,taux
     DO ind5=1,t
      DO ind8=1,t
       DO ind9=1,2
         DO ind2= 1,ncash
          WRITE(25,*) def(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
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
         WRITE(25,*) vstayout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
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
         WRITE(25,*) vrentout(ind1,ind2,ind3,ind4,ind5,ind8,ind9)
       END DO
       END DO
      END DO
     END DO
    END DO
  END DO
END DO

CLOSE(UNIT=25)

vr1(:,:,:,:,:,:,:)= vr2(:,:,:,:,:,:,:)

END DO ! FOR THE t CYCLE

! Write V01 for mortgage choice
WRITE(*,*) t

OPEN(UNIT=25,FILE=filemortchoice(1,1),STATUS='replace',ACTION='write')
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
CLOSE(UNIT=25)

END PROGRAM
