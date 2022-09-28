% May 4 2011
% This program simulates the data for arm income

clear
rng('shuffle')
%randn('state',0) % rand state from Cocco
%rand('state',0)

%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize Parameters %
%%%%%%%%%%%%%%%%%%%%%%%%%
A = read_namelist("config_nml.nml")

boost_prob_bad_house_shock = A.ag_shock_config.boost_prob_bad_house_shock;

nsim=40000;

% gam= 3;
gam= 2;
beta= 0.98;
bequest= 400.0;

tb= 1;
td= 20;
t= td-tb+1;

tax=0.25;
tax_p= 0.015;
m_p= 0.025;


indcase = A.config_nml.indcase;
theta= 0.017;
mur=0.0291;
sigr= 0.009;
phir= 0.891;


mui = A.config_nml.mui;
sigi = A.config_nml.sigi;
phii=A.config_nml.phii;
betai=A.config_nml.betai;
sigrealrate = (sigi*sigi+betai*betai*sigr*sigr)^0.5;

mudph=0.0033;
sigdph=0.162;

labinc= zeros(21,1);
sigpy= 0.062976;
sigty=A.config_nml.sigty;
levely= 46.353;
growthy= 0.007963;
corrpyhouse = 0.191;
corrratehouse = 0.30;
probmove= 0.04;
ratiolock= 0.20;

correl= 0.0;

lti= 4.5;
dw= 0.10;
loan= lti*levely;
house= loan/(1.0-dw);
rentalpre= 0.00;

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set variables for grids %
%%%%%%%%%%%%%%%%%%%%%%%%%%%


ncash= 185;
nr= 2;
ni= 2;
ndebt= 1;
nc= 160;



%%%%%%%%%%%%%%
% Quadrature %
%%%%%%%%%%%%%%

ymr= zeros(2,1);
wmr= zeros(2,1);

ymr(1,1)= -1.0;
ymr(2,1)=  1.0;
wmr(1,1)= 0.5;
wmr(2,1)= 0.5;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% One-period log inflation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gr= zeros(2,1);
p= zeros(2,2);

gr = ymr*sigr+mur;
p= gettrans(gr,wmr,mur,phir,sigr);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% One-period log real yield %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

giinov=zeros(2,1);
gi=zeros(2,2);
tpi= zeros(2,2);


giinov= ymr*sigi+mui;
tpi=gettrans(giinov,wmr,mui,phii,sigi);



for i=1:2;
   for j=1:2;
       gi(j,i)= giinov(j,1)+betai*gr(i,1);
   end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Innovations to log inflation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inr= zeros(2,2);

for i=1:nr;
   for j=1:nr;
      inr(i,j)=gr(j,1)-(p(i,1)*gr(1,1)+p(i,2)*gr(2,1))
   end
end

acum=zeros(nr,nr);

for i=1:nr,
  for j=1:nr,
    if j==1;
      acum(i,j)= p(i,j);
    else
      acum(i,j)= acum(i,j-1)+p(i,j);
    end
  end
end

acum1=zeros(nr,1);
acum1(1,1)= 0.5;
acum1(2,1)= 1.0;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Innovations to real rates that are uncorrelated with inflation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ini= zeros(2,2);

for i=1:nr;
   for j=1:nr;
      ini(i,j)= giinov(j,1)-(tpi(i,1)*giinov(1,1)+tpi(i,2)*giinov(2,1))
   end
end


%%%%%%%%%%%%%%%%
% House prices %
%%%%%%%%%%%%%%%%

dlgph= zeros(2,1);
gph= zeros(2,1);

for i=1:nr;
   dlgph(i,1) = ymr(nr-i+1,1)*sigdph+mudph;
end

gph= exp(dlgph);

%%%%%%%%%%
% Income %
%%%%%%%%%%

lyp= zeros(2,1);
yp= zeros(2,1);

for i=1:nr;
   lyp(i,1) = ymr(nr-i+1,1)*sigpy;
end
yp= exp(lyp);

nqpy= 2;
weig= zeros(2,1);
grid= zeros(2,1);
weig(1,1)= 0.5;
weig(2,1)= 0.5;
grid(1,1)= -1.0;
grid(2,1)=  1.0;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Correlation between temporary income shocks and the level of real rates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regcoef = correl*sigty/sigrealrate;
sigres2= sigty*sigty - regcoef*regcoef*sigrealrate*sigrealrate;
sigres= sigres2^(0.5);


eyt= zeros(2,2,2);
expeyt= zeros(2,2,2);

% Innovations to the inflation process correlated with temporary income
% shocks

for i=1:nr;
   for j=1:nr;
     eyt(i,j,1)= regcoef*(gi(i,j)-mui-betai*mur)+grid(1,1)*sigres;
     eyt(i,j,2)= regcoef*(gi(i,j)-mui-betai*mur)+grid(2,1)*sigres;
   end
end

expeyt= exp(eyt);

labinc(1,1)=  levely;
for i=2:td+1;
   labinc(i,1)= levely*(1+growthy)^(i-1);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Correlation permanent income house prices %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ppyhouse =(corrpyhouse*sigpy*sigdph-0.5*(dlgph(1,1)-mudph)*lyp(2,1)-0.5*(dlgph(2,1)-mudph)*lyp(1,1))/(0.5*(dlgph(1,1)-mudph)*lyp(1,1)-0.5*(dlgph(1,1)-mudph)*lyp(2,1)-0.5*(dlgph(2,1)-mudph)*lyp(1,1)+0.5*(dlgph(2,1)-mudph)*lyp(2,1));

ppyhouse

pyh= zeros(2,2);
pyh(1,1) = ppyhouse;
pyh(2,1) = (1.0-ppyhouse);
pyh(1,2) = (1.0-ppyhouse);
pyh(2,2) = ppyhouse;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Correlation real rates house prices %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sigini = (tpi(1,1)*(ini(1,1)^2)+tpi(1,2)*(ini(1,2)^2))^0.5;

ppratehouse2 = (corrratehouse*sigini*sigdph+((-tpi(1,1)*(dlgph(1,1)-mudph)*ini(1,1))+(-tpi(1,2)*(dlgph(1,1)-mudph)*ini(1,2))+(0.5/tpi(1,1)*(-2*tpi(1,1)*ini(1,1)*(dlgph(1,1)-mudph)))))/(2.0*(dlgph(1,1)-mudph)*ini(1,2)*tpi(1,2) + (-tpi(1,2)/tpi(1,1))*(-2.0*tpi(1,1)*ini(1,1)*(dlgph(2,1)-mudph)));
ppratehouse1 = 0.5/tpi(1,1)-tpi(1,2)/tpi(1,1)*ppratehouse2;

ppratehouse1
ppratehouse2

prh= zeros(2,2,2);
prh(1,1,1) = ppratehouse1;
prh(1,1,2) = (1.0-ppratehouse1);
prh(1,2,1) = ppratehouse2;
prh(1,2,2) = (1.0-ppratehouse2);

prh(2,1,1) = (1.0-ppratehouse2);
prh(2,1,2) = ppratehouse2;
prh(2,2,1) = (1.0-ppratehouse1);
prh(2,2,2) = ppratehouse1;



%%%%%%%%%%%%%%%%%%%
% Construct grids %
%%%%%%%%%%%%%%%%%%%

lgcash= zeros(ncash,1);
lgcons= zeros(nc,1);

gcash= zeros(ncash,1);
gcons= zeros(nc,1);
gdebt= zeros(ndebt,1);

for i=1:ncash,
   lgcash(i,1)= 0.0+(i-1)*0.035;
   gcash(i,1)= exp(lgcash(i,1));
end

for i=1:nc,
    lgcons(i,1)= 0.00+(i-1)*0.036;
    gcons(i,1)= exp(lgcons(i,1));
end

for i=1:ndebt,
   gdebt(i,1)= 0.00;
end



%%%%%%%%%%%%%%%%%%%%%%
% Joint probability  %
%%%%%%%%%%%%%%%%%%%%%%

probinfr= zeros(4,4);

probinfr(1,1)= p(1,1)*tpi(1,1);
probinfr(1,2)= p(1,1)*tpi(1,2);
probinfr(1,3)= p(1,2)*tpi(1,1);
probinfr(1,4)= p(1,2)*tpi(1,2);

probinfr(2,1)= p(1,1)*tpi(2,1);
probinfr(2,2)= p(1,1)*tpi(2,2);
probinfr(2,3)= p(1,2)*tpi(2,1);
probinfr(2,4)= p(1,2)*tpi(2,2);

probinfr(3,1)= p(2,1)*tpi(1,1);
probinfr(3,2)= p(2,1)*tpi(1,2);
probinfr(3,3)= p(2,2)*tpi(1,1);
probinfr(3,4)= p(2,2)*tpi(1,2);

probinfr(4,1)= p(2,1)*tpi(2,1);
probinfr(4,2)= p(2,1)*tpi(2,2);
probinfr(4,3)= p(2,2)*tpi(2,1);
probinfr(4,4)= p(2,2)*tpi(2,2);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRUCT TERM STRUCTURE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lyield=zeros(4,20);
yield=zeros(4,20);

lyield(1,1)=gr(1,1)+giinov(1,1)+betai*gr(1,1);
lyield(2,1)=gr(1,1)+giinov(2,1)+betai*gr(1,1);
lyield(3,1)=gr(2,1)+giinov(1,1)+betai*gr(2,1);
lyield(4,1)=gr(2,1)+giinov(2,1)+betai*gr(2,1);

yield(1,1)=exp(lyield(1,1))-1;
yield(2,1)=exp(lyield(2,1))-1;
yield(3,1)=exp(lyield(3,1))-1;
yield(4,1)=exp(lyield(4,1))-1;

prob=zeros(4,4,20);
prob(1,1,2)=probinfr(1,1);
prob(1,2,2)=probinfr(1,2);
prob(1,3,2)=probinfr(1,3);
prob(1,4,2)=probinfr(1,4);

prob(2,1,2)=probinfr(2,1);
prob(2,2,2)=probinfr(2,2);
prob(2,3,2)=probinfr(2,3);
prob(2,4,2)=probinfr(2,4);

prob(3,1,2)=probinfr(3,1);
prob(3,2,2)=probinfr(3,2);
prob(3,3,2)=probinfr(3,3);
prob(3,4,2)=probinfr(3,4);

prob(4,1,2)=probinfr(4,1);
prob(4,2,2)=probinfr(4,2);
prob(4,3,2)=probinfr(4,3);
prob(4,4,2)=probinfr(4,4);



for j=3:20;
 prob(1,1,j)=prob(1,1,j-1)*probinfr(1,1)+prob(1,2,j-1)*probinfr(2,1)+prob(1,3,j-1)*probinfr(3,1)+prob(1,4,j-1)*probinfr(4,1);
 prob(1,2,j)=prob(1,1,j-1)*probinfr(1,2)+prob(1,2,j-1)*probinfr(2,2)+prob(1,3,j-1)*probinfr(3,2)+prob(1,4,j-1)*probinfr(4,2);
 prob(1,3,j)=prob(1,1,j-1)*probinfr(1,3)+prob(1,2,j-1)*probinfr(2,3)+prob(1,3,j-1)*probinfr(3,3)+prob(1,4,j-1)*probinfr(4,3);
 prob(1,4,j)=prob(1,1,j-1)*probinfr(1,4)+prob(1,2,j-1)*probinfr(2,4)+prob(1,3,j-1)*probinfr(3,4)+prob(1,4,j-1)*probinfr(4,4);

 prob(2,1,j)=prob(2,1,j-1)*probinfr(1,1)+prob(2,2,j-1)*probinfr(2,1)+prob(2,3,j-1)*probinfr(3,1)+prob(2,4,j-1)*probinfr(4,1);
 prob(2,2,j)=prob(2,1,j-1)*probinfr(1,2)+prob(2,2,j-1)*probinfr(2,2)+prob(2,3,j-1)*probinfr(3,2)+prob(2,4,j-1)*probinfr(4,2);
 prob(2,3,j)=prob(2,1,j-1)*probinfr(1,3)+prob(2,2,j-1)*probinfr(2,3)+prob(2,3,j-1)*probinfr(3,3)+prob(2,4,j-1)*probinfr(4,3);
 prob(2,4,j)=prob(2,1,j-1)*probinfr(1,4)+prob(2,2,j-1)*probinfr(2,4)+prob(2,3,j-1)*probinfr(3,4)+prob(2,4,j-1)*probinfr(4,4);

 prob(3,1,j)=prob(3,1,j-1)*probinfr(1,1)+prob(3,2,j-1)*probinfr(2,1)+prob(3,3,j-1)*probinfr(3,1)+prob(3,4,j-1)*probinfr(4,1);
 prob(3,2,j)=prob(3,1,j-1)*probinfr(1,2)+prob(3,2,j-1)*probinfr(2,2)+prob(3,3,j-1)*probinfr(3,2)+prob(3,4,j-1)*probinfr(4,2);
 prob(3,3,j)=prob(3,1,j-1)*probinfr(1,3)+prob(3,2,j-1)*probinfr(2,3)+prob(3,3,j-1)*probinfr(3,3)+prob(3,4,j-1)*probinfr(4,3);
 prob(3,4,j)=prob(3,1,j-1)*probinfr(1,4)+prob(3,2,j-1)*probinfr(2,4)+prob(3,3,j-1)*probinfr(3,4)+prob(3,4,j-1)*probinfr(4,4);

 prob(4,1,j)=prob(4,1,j-1)*probinfr(1,1)+prob(4,2,j-1)*probinfr(2,1)+prob(4,3,j-1)*probinfr(3,1)+prob(4,4,j-1)*probinfr(4,1);
 prob(4,2,j)=prob(4,1,j-1)*probinfr(1,2)+prob(4,2,j-1)*probinfr(2,2)+prob(4,3,j-1)*probinfr(3,2)+prob(4,4,j-1)*probinfr(4,2);
 prob(4,3,j)=prob(4,1,j-1)*probinfr(1,3)+prob(4,2,j-1)*probinfr(2,3)+prob(4,3,j-1)*probinfr(3,3)+prob(4,4,j-1)*probinfr(4,3);
 prob(4,4,j)=prob(4,1,j-1)*probinfr(1,4)+prob(4,2,j-1)*probinfr(2,4)+prob(4,3,j-1)*probinfr(3,4)+prob(4,4,j-1)*probinfr(4,4);

end


for j=2:20;
 lyield(1,j)=1/j*(lyield(1,j-1)*(j-1)+prob(1,1,j)*lyield(1,1)+prob(1,2,j)*lyield(2,1)+prob(1,3,j)*lyield(3,1)+prob(1,4,j)*lyield(4,1));
 lyield(2,j)=1/j*(lyield(2,j-1)*(j-1)+prob(2,1,j)*lyield(1,1)+prob(2,2,j)*lyield(2,1)+prob(2,3,j)*lyield(3,1)+prob(2,4,j)*lyield(4,1));
 lyield(3,j)=1/j*(lyield(3,j-1)*(j-1)+prob(3,1,j)*lyield(1,1)+prob(3,2,j)*lyield(2,1)+prob(3,3,j)*lyield(3,1)+prob(3,4,j)*lyield(4,1));
 lyield(4,j)=1/j*(lyield(4,j-1)*(j-1)+prob(4,1,j)*lyield(1,1)+prob(4,2,j)*lyield(2,1)+prob(4,3,j)*lyield(3,1)+prob(4,4,j)*lyield(4,1));
end
yield=exp(lyield)-1;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANUITY FACTOR AND ANUITY %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

anfactor=zeros(4,20);

for j=1:20;
   anfactor(1,1)=anfactor(1,1)+1/(exp(lyield(1,20))+theta)^j;
   anfactor(2,1)=anfactor(2,1)+1/(exp(lyield(2,20))+theta)^j;
   anfactor(3,1)=anfactor(3,1)+1/(exp(lyield(3,20))+theta)^j;
   anfactor(4,1)=anfactor(4,1)+1/(exp(lyield(4,20))+theta)^j;
end

anuity=zeros(4,20);
anuity(1,1)=loan/anfactor(1,1);
anuity(2,1)=loan/anfactor(2,1);
anuity(3,1)=loan/anfactor(3,1);
anuity(4,1)=loan/anfactor(4,1);

remdebt=zeros(4,21);
remdebt(1,1)=loan;
remdebt(2,1)=loan;
remdebt(3,1)=loan;
remdebt(4,1)=loan;


loanrepaid=zeros(4,20);

for i=2:20;
   remdebt(1,i)=remdebt(1,i-1)-(anuity(1,1)-remdebt(1,i-1)*(exp(lyield(1,20))+theta-1));
   remdebt(2,i)=remdebt(2,i-1)-(anuity(2,1)-remdebt(2,i-1)*(exp(lyield(2,20))+theta-1));
   remdebt(3,i)=remdebt(3,i-1)-(anuity(3,1)-remdebt(3,i-1)*(exp(lyield(3,20))+theta-1));
   remdebt(4,i)=remdebt(4,i-1)-(anuity(4,1)-remdebt(4,i-1)*(exp(lyield(4,20))+theta-1));
end

for i=2:20
   loanrepaid(1,i-1)=remdebt(1,i-1)-remdebt(1,i);
   loanrepaid(2,i-1)=remdebt(2,i-1)-remdebt(2,i);
   loanrepaid(3,i-1)=remdebt(3,i-1)-remdebt(3,i);
   loanrepaid(4,i-1)=remdebt(4,i-1)-remdebt(4,i);
end

loanrepaid(1,20)=remdebt(1,20)-0.0;
loanrepaid(2,20)=remdebt(2,20)-0.0;
loanrepaid(3,20)=remdebt(3,20)-0.0;
loanrepaid(4,20)=remdebt(4,20)-0.0;

for i=1:20
   loanrepaid(1,i)
end



%%%%%%%%%%%%%%%
% Select case %
%%%%%%%%%%%%%%%


if indcase==2;
    anuity(1,:)= anuity(2,:);
    loanrepaid(1,:)= loanrepaid(2,:);
    remdebt(1,:)= remdebt(2,:);

    elseif indcase==3;
    anuity(1,:)= anuity(3,:);
    loanrepaid(1,:)= loanrepaid(3,:);
    remdebt(1,:)= remdebt(3,:);

    elseif indcase==4;
    anuity(1,:)= anuity(4,:);
    loanrepaid(1,:)= loanrepaid(4,:);
    remdebt(1,:)= remdebt(4,:);
end

%%%%%%%%%%%%%%%%%%%%%%%%%
% Load Policy Functions %
%%%%%%%%%%%%%%%%%%%%%%%%%


nra=1;
lref=1;
nia=1;
cons1= zeros(1,ncash,nr,nra,lref,nia,2);
def1= zeros(1,ncash,nr,nra,lref,nia,2);
vstay1= zeros(1,ncash,nr,nra,lref,nia,2);
vrent1= zeros(1,ncash,nr,nra,lref,nia,2);
consdef1= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year01
load rear01
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
             for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons1(i,:,j,m,n,h,q)= year01(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def1(i,:,j,m,n,h,q)= year01(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay1(i,:,j,m,n,h,q)= year01(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent1(i,:,j,m,n,h,q)= year01(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef1(i,:,j,m,n,h,q)= rear01(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
         end
      end
	end
end

1


clear year01
clear rear01

nra=2;
lref=2;
nia=2;
cons2= zeros(1,ncash,nr,nra,lref,nia,2);
def2= zeros(1,ncash,nr,nra,lref,nia,2);
vstay2= zeros(1,ncash,nr,nra,lref,nia,2);
vrent2= zeros(1,ncash,nr,nra,lref,nia,2);
consdef2= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year02
load rear02
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons2(i,:,j,m,n,h,q)= year02(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def2(i,:,j,m,n,h,q)= year02(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay2(i,:,j,m,n,h,q)= year02(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent2(i,:,j,m,n,h,q)= year02(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef2(i,:,j,m,n,h,q)= rear02(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

2

clear year02
clear rear02

nra=3;
lref=3;
nia=3;
cons3= zeros(1,ncash,nr,nra,lref,nia,2);
def3= zeros(1,ncash,nr,nra,lref,nia,2);
vstay3= zeros(1,ncash,nr,nra,lref,nia,2);
vrent3= zeros(1,ncash,nr,nra,lref,nia,2);
consdef3= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year03
load rear03
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons3(i,:,j,m,n,h,q)= year03(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def3(i,:,j,m,n,h,q)= year03(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay3(i,:,j,m,n,h,q)= year03(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent3(i,:,j,m,n,h,q)= year03(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef3(i,:,j,m,n,h,q)= rear03(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

3

clear year03
clear rear03

nra=4;
lref=4;
nia=4;
cons4= zeros(1,ncash,nr,nra,lref,nia,2);
def4= zeros(1,ncash,nr,nra,lref,nia,2);
vstay4= zeros(1,ncash,nr,nra,lref,nia,2);
vrent4= zeros(1,ncash,nr,nra,lref,nia,2);
consdef4= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year04
load rear04
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons4(i,:,j,m,n,h,q)= year04(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def4(i,:,j,m,n,h,q)= year04(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay4(i,:,j,m,n,h,q)= year04(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent4(i,:,j,m,n,h,q)= year04(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef4(i,:,j,m,n,h,q)= rear04(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

4

clear year04
clear rear04

nra=5;
lref=5;
nia=5;
cons5= zeros(1,ncash,nr,nra,lref,nia,2);
def5= zeros(1,ncash,nr,nra,lref,nia,2);
vstay5= zeros(1,ncash,nr,nra,lref,nia,2);
vrent5= zeros(1,ncash,nr,nra,lref,nia,2);
consdef5= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year05
load rear05
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons5(i,:,j,m,n,h,q)= year05(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def5(i,:,j,m,n,h,q)= year05(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay5(i,:,j,m,n,h,q)= year05(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent5(i,:,j,m,n,h,q)= year05(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef5(i,:,j,m,n,h,q)= rear05(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

5

clear year05
clear rear05

nra=6;
lref=6;
nia=6;
cons6= zeros(1,ncash,nr,nra,lref,nia,2);
def6= zeros(1,ncash,nr,nra,lref,nia,2);
vstay6= zeros(1,ncash,nr,nra,lref,nia,2);
vrent6= zeros(1,ncash,nr,nra,lref,nia,2);
consdef6= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year06
load rear06
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons6(i,:,j,m,n,h,q)= year06(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def6(i,:,j,m,n,h,q)= year06(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay6(i,:,j,m,n,h,q)= year06(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent6(i,:,j,m,n,h,q)= year06(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef6(i,:,j,m,n,h,q)= rear06(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
         end
            end
			end
		end
   end
end

6

clear year06
clear rear06

nra=7;
lref=7;
nia=7;
cons7= zeros(1,ncash,nr,nra,lref,nia,2);
def7= zeros(1,ncash,nr,nra,lref,nia,2);
vstay7= zeros(1,ncash,nr,nra,lref,nia,2);
vrent7= zeros(1,ncash,nr,nra,lref,nia,2);
consdef7= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year07
load rear07
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons7(i,:,j,m,n,h,q)= year07(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def7(i,:,j,m,n,h,q)= year07(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay7(i,:,j,m,n,h,q)= year07(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent7(i,:,j,m,n,h,q)= year07(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef7(i,:,j,m,n,h,q)= rear07(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

7

clear year07
clear rear07

nra=8;
lref=8;
nia=8;
cons8= zeros(1,ncash,nr,nra,lref,nia,2);
def8= zeros(1,ncash,nr,nra,lref,nia,2);
vstay8= zeros(1,ncash,nr,nra,lref,nia,2);
vrent8= zeros(1,ncash,nr,nra,lref,nia,2);
consdef8= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year08
load rear08
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons8(i,:,j,m,n,h,q)= year08(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def8(i,:,j,m,n,h,q)= year08(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay8(i,:,j,m,n,h,q)= year08(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent8(i,:,j,m,n,h,q)= year08(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef8(i,:,j,m,n,h,q)= rear08(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

8

clear year08
clear rear08

nra=9;
lref=9;
nia=9;
cons9= zeros(1,ncash,nr,nra,lref,nia,2);
def9= zeros(1,ncash,nr,nra,lref,nia,2);
vstay9= zeros(1,ncash,nr,nra,lref,nia,2);
vrent9= zeros(1,ncash,nr,nra,lref,nia,2);
consdef9= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year09
load rear09
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons9(i,:,j,m,n,h,q)= year09(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def9(i,:,j,m,n,h,q)= year09(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay9(i,:,j,m,n,h,q)= year09(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent9(i,:,j,m,n,h,q)= year09(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef9(i,:,j,m,n,h,q)= rear09(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

9

clear year09
clear rear09

nra=10;
lref=10;
nia=10;
cons10= zeros(1,ncash,nr,nra,lref,nia,2);
def10= zeros(1,ncash,nr,nra,lref,nia,2);
vstay10= zeros(1,ncash,nr,nra,lref,nia,2);
vrent10= zeros(1,ncash,nr,nra,lref,nia,2);
consdef10= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year10
load rear10
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons10(i,:,j,m,n,h,q)= year10(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def10(i,:,j,m,n,h,q)= year10(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay10(i,:,j,m,n,h,q)= year10(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent10(i,:,j,m,n,h,q)= year10(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef10(i,:,j,m,n,h,q)= rear10(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

10

clear year10
clear rear10

nra=11;
lref=11;
nia=11;
cons11= zeros(1,ncash,nr,nra,lref,nia,2);
def11= zeros(1,ncash,nr,nra,lref,nia,2);
vstay11= zeros(1,ncash,nr,nra,lref,nia,2);
vrent11= zeros(1,ncash,nr,nra,lref,nia,2);
consdef11= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year11
load rear11
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons11(i,:,j,m,n,h,q)= year11(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def11(i,:,j,m,n,h,q)= year11(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay11(i,:,j,m,n,h,q)= year11(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent11(i,:,j,m,n,h,q)= year11(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef11(i,:,j,m,n,h,q)= rear11(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

11

clear year11
clear rear11

nra=12;
lref=12;
nia=12;
cons12= zeros(1,ncash,nr,nra,lref,nia,2);
def12= zeros(1,ncash,nr,nra,lref,nia,2);
vstay12= zeros(1,ncash,nr,nra,lref,nia,2);
vrent12= zeros(1,ncash,nr,nra,lref,nia,2);
consdef12= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year12
load rear12
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons12(i,:,j,m,n,h,q)= year12(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def12(i,:,j,m,n,h,q)= year12(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay12(i,:,j,m,n,h,q)= year12(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent12(i,:,j,m,n,h,q)= year12(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef12(i,:,j,m,n,h,q)= rear12(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

12

clear year12
clear rear12

nra=13;
lref=13;
nia=13;
cons13= zeros(1,ncash,nr,nra,lref,nia,2);
def13= zeros(1,ncash,nr,nra,lref,nia,2);
vstay13= zeros(1,ncash,nr,nra,lref,nia,2);
vrent13= zeros(1,ncash,nr,nra,lref,nia,2);
consdef13= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year13
load rear13
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons13(i,:,j,m,n,h,q)= year13(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def13(i,:,j,m,n,h,q)= year13(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay13(i,:,j,m,n,h,q)= year13(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent13(i,:,j,m,n,h,q)= year13(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef13(i,:,j,m,n,h,q)= rear13(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

13

clear year13
clear rear13

nra=14;
lref=14;
nia=14;
cons14= zeros(1,ncash,nr,nra,lref,nia,2);
def14= zeros(1,ncash,nr,nra,lref,nia,2);
vstay14= zeros(1,ncash,nr,nra,lref,nia,2);
vrent14= zeros(1,ncash,nr,nra,lref,nia,2);
consdef14= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year14
load rear14
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons14(i,:,j,m,n,h,q)= year14(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def14(i,:,j,m,n,h,q)= year14(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay14(i,:,j,m,n,h,q)= year14(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent14(i,:,j,m,n,h,q)= year14(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef14(i,:,j,m,n,h,q)= rear14(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

14

clear year14
clear rear14

nra=15;
lref=15;
nia=15;
cons15= zeros(1,ncash,nr,nra,lref,nia,2);
def15= zeros(1,ncash,nr,nra,lref,nia,2);
vstay15= zeros(1,ncash,nr,nra,lref,nia,2);
vrent15= zeros(1,ncash,nr,nra,lref,nia,2);
consdef15= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year15
load rear15
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons15(i,:,j,m,n,h,q)= year15(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def15(i,:,j,m,n,h,q)= year15(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay15(i,:,j,m,n,h,q)= year15(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent15(i,:,j,m,n,h,q)= year15(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef15(i,:,j,m,n,h,q)= rear15(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

15

clear year15
clear rear15


nra=16;
lref=16;
nia=16;
cons16= zeros(1,ncash,nr,nra,lref,nia,2);
def16= zeros(1,ncash,nr,nra,lref,nia,2);
vstay16= zeros(1,ncash,nr,nra,lref,nia,2);
vrent16= zeros(1,ncash,nr,nra,lref,nia,2);
consdef16= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year16
load rear16
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons16(i,:,j,m,n,h,q)= year16(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def16(i,:,j,m,n,h,q)= year16(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay16(i,:,j,m,n,h,q)= year16(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent16(i,:,j,m,n,h,q)= year16(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef16(i,:,j,m,n,h,q)= rear16(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

16

clear year16
clear rear16

nra=17;
lref=17;
nia=17;
cons17= zeros(1,ncash,nr,nra,lref,nia,2);
def17= zeros(1,ncash,nr,nra,lref,nia,2);
vstay17= zeros(1,ncash,nr,nra,lref,nia,2);
vrent17= zeros(1,ncash,nr,nra,lref,nia,2);
consdef17= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year17
load rear17
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons17(i,:,j,m,n,h,q)= year17(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def17(i,:,j,m,n,h,q)= year17(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay17(i,:,j,m,n,h,q)= year17(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent17(i,:,j,m,n,h,q)= year17(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef17(i,:,j,m,n,h,q)= rear17(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

17

clear year17
clear rear17


nra=18;
lref=18;
nia=18;
cons18= zeros(1,ncash,nr,nra,lref,nia,2);
def18= zeros(1,ncash,nr,nra,lref,nia,2);
vstay18= zeros(1,ncash,nr,nra,lref,nia,2);
vrent18= zeros(1,ncash,nr,nra,lref,nia,2);
consdef18= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year18
load rear18
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons18(i,:,j,m,n,h,q)= year18(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def18(i,:,j,m,n,h,q)= year18(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay18(i,:,j,m,n,h,q)= year18(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent18(i,:,j,m,n,h,q)= year18(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef18(i,:,j,m,n,h,q)= rear18(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

18

clear year18
clear rear18


nra=19;
lref=19;
nia=19;
cons19= zeros(1,ncash,nr,nra,lref,nia,2);
def19= zeros(1,ncash,nr,nra,lref,nia,2);
vstay19= zeros(1,ncash,nr,nra,lref,nia,2);
vrent19= zeros(1,ncash,nr,nra,lref,nia,2);
consdef19= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;

load year19
load rear19
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons19(i,:,j,m,n,h,q)= year19(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def19(i,:,j,m,n,h,q)= year19(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay19(i,:,j,m,n,h,q)= year19(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent19(i,:,j,m,n,h,q)= year19(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
consdef19(i,:,j,m,n,h,q)= rear19(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

19

clear year19
clear rear19

nra=20;
lref=20;
nia=20;
cons20= zeros(1,ncash,nr,nra,lref,nia,2);
def20= zeros(1,ncash,nr,nra,lref,nia,2);
vstay20= zeros(1,ncash,nr,nra,lref,nia,2);
vrent20= zeros(1,ncash,nr,nra,lref,nia,2);
consdef20= zeros(1,ncash,nr,nra,lref,nia,2);

aux1=1*ncash*nr*nra*lref*nia*2;
load year20
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
cons20(i,:,j,m,n,h,q)= year20(0*aux1+aux2+1:0*aux1+aux2+ncash,1);
def20(i,:,j,m,n,h,q)= year20(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
vstay20(i,:,j,m,n,h,q)= year20(2*aux1+aux2+1:2*aux1+aux2+ncash,1);
vrent20(i,:,j,m,n,h,q)= year20(3*aux1+aux2+1:3*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

clear year20

aux1=1*ncash*nr*nra*lref*nia*2;
load rear20
for i=1:1,
   for j=1:nr,
      for m=1:nra,
         for n=1:lref,
            for h=1:nia,
               for q=1:2,
aux2=(i-1)*ncash*nr*nra*lref*nia*2+(j-1)*ncash*nra*lref*nia*2+(m-1)*ncash*lref*nia*2+(n-1)*ncash*nia*2+(h-1)*ncash*2+(q-1)*ncash;
consdef20(i,:,j,m,n,h,q)= rear20(1*aux1+aux2+1:1*aux1+aux2+ncash,1);
               end
            end
			end
		end
   end
end

clear rear20

20




%%%%%%%%%%%%%%
% Simulation %
%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize aggregate variables %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simcons0= zeros(td+1,1);
simdebt0= zeros(td+1,1);
simdef0= zeros(td+1,1);
simw0= zeros(td+1,1);
simy0= zeros(td+1,1);
simr0= zeros(td+1,1);
simi0= zeros(td+1,1);
simref0= zeros(td+1,1);
simdefc0= zeros(td+1,1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize individual variables %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simcons= zeros(td+1,nsim);
simdebt= zeros(td+1,nsim);
simdefc= zeros(td+1,nsim);
simvdefc= zeros(td+1,nsim);
simdef= zeros(td+1,nsim);
simw= zeros(td+1,nsim);
simcoh= zeros(td+1,nsim);
sav= zeros(td+1,nsim);
simy= zeros(td+1,nsim);
simph= zeros(td+1,nsim);
simpl= zeros(td+1,nsim);
simltv= zeros(td+1,nsim);
simref= zeros(td+1,nsim);
mpreal= zeros(td+1,nsim);
rentpay= zeros(td+1,nsim);
mpnominal= zeros(td+1,nsim);
indcash= zeros(td+1,nsim);

simr= zeros(td+1,nsim);
indr= zeros(td+1,nsim);
indnr= zeros(td+1,nsim);

simi= zeros(td+1,nsim);
indi= zeros(td+1,nsim);
indni= zeros(td+1,nsim);

indir= zeros(td+1,nsim);
indlref= ones(td+1,nsim);
inr= zeros(td+1,nsim);

simmove=  zeros(td+1,nsim);
indmove= zeros(td+1,nsim);

indty = zeros(td+1, nsim);

simvrent = zeros(td+1,nsim);
simvstay = zeros(td+1,nsim);
simconsstay = zeros(td+1,nsim);
simconsrent = zeros(td+1,nsim);
stayindcash = zeros(td+1,nsim);
rentindcash = zeros(td+1,nsim);
staysimw = zeros(td+1,nsim);
rentsimw = zeros(td+1,nsim);
staysav = zeros(td+1,nsim);
rentsav = zeros(td+1,nsim);
staysimcoh = zeros(td+1,nsim);
rentsimcoh = zeros(td+1,nsim);
simtwealthstay = zeros(td+1,nsim);
simtwealthrent = zeros(td+1,nsim);
indcashout = zeros(1, nsim);
indcashoutrent = zeros(1,nsim);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulate for each individual  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for j=1:nsim,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate path for inflation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rn= rand(td+1,1);

for i=1:nr,
  if i==1,
    if (indcase==1 || indcase==2);
       indr(1,j)= 1;
       indnr(1,j)= 1;
    elseif (indcase==3 || indcase==4);
       indr(1,j)= 2;
       indnr(1,j)= 1;
    end
  end
end

for k=2:td+1,
  aux= indr(k-1,j);
  for i=1:nr,
     if i==1,
        if rn(k,1)<acum(aux,i);
             indr(k,j)= i;
        end
     else
        if rn(k,1)>acum(aux,i-1)& rn(k,1)<acum(aux,i);
             indr(k,j)= i;
        end
     end
  end
end


for i=2:td+1,
   if indr(i-1,j)==1,
      indnr(i,j)=indnr(i-1,j);
   else
      indnr(i,j)=indnr(i-1,j)+1;
   end
end


for i=1:td+1,
    simr(i,j)= gr(indr(i,j),1);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate real interest rate %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rnir= rand(td+1,1);

if (indcase==1 || indcase==3);
   indir(1,j)= 1;
elseif (indcase==2 || indcase==4);
   indir(1,j)= 2;
end

for i=2:td+1,
  if indir(i-1,j)==1;      % Low real rate in the previous period
    if rnir(i,1)<tpi(1,1);
      indir(i,j)= 1;
    else
      indir(i,j)= 2;
    end
  elseif indir(i-1,j)==2;  % High real rate in the previous period
    if rnir(i,1)<tpi(2,1);
      indir(i,j)= 1;
    else
      indir(i,j)= 2;
    end
  end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate real house prices  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rni= rand(td+1,1) + boost_prob_bad_house_shock;

indi(1,j)= 1;
indni(1,j)= 1;

for i=2:td+1,
  if indir(i,j)==1;       % Low real rates next period
    if rni(i,1)<prh(indir(i-1,j),1,1);
      indi(i,j)= 1;            % same number of low house price growth in the past, high innovation
      indni(i,j)=indni(i-1,j);
    else
      indi(i,j)= 2;            % one more observation of low house price growth in the past, low innovation
      indni(i,j)=indni(i-1,j)+1;
    end
  elseif indir(i,j)==2;  % High real rates next period
    if rni(i,1)<prh(indir(i-1,j),2,1);
      indi(i,j)= 1;
      indni(i,j)=indni(i-1,j);
    else
      indi(i,j)= 2;
      indni(i,j)=indni(i-1,j)+1;
    end
  end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAME AGGREGATE SHOCKS FOR 50 INDIVIDUALS  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if j<=50,
jag = floor((j-1)/50);
for i=1:td+1,
    indr(i,j) =   indr(i,jag*50+1);
    indnr(i,j) =  indnr(i,jag*50+1);
    simr(i,j)=    simr(i,jag*50+1);
    indi(i,j) =   indi(i,jag*50+1);
    indni(i,j) =  indni(i,jag*50+1);
    indir(i,j) =  indir(i,jag*50+1);
end
end

if j>=50,
jag = floor((j-1)/50);
for i=1:td+1,
    indr(i,j) =  indr(i,jag*50+1);
    indnr(i,j) = indnr(i,jag*50+1);
    simr(i,j)=   simr(i,jag*50+1);
    indi(i,j) =  indi(i,jag*50+1);
    indni(i,j) = indni(i,jag*50+1);
    indir(i,j) = indir(i,jag*50+1);
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate permanent income %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rnpy= rand(td+1,1);

indpy(1,j)= 1;
indlref(1,j)= 1;

for i=2:td+1,
 if indi(i,j)==1;
   if rnpy(i,1)<pyh(1,1);
     indpy(i,j)= 1;
     indlref(i,j)=indlref(i-1,j);
   else
     indpy(i,j)= 2;
     indlref(i,j)=indlref(i-1,j)+1;
   end
 end
 if indi(i,j)==2;
   if rnpy(i,1)<pyh(1,2);
     indpy(i,j)= 1;
     indlref(i,j)=indlref(i-1,j);
   else
     indpy(i,j)= 2;
     indlref(i,j)=indlref(i-1,j)+1;
   end
 end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate labor income/house prices %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rny= rand(td+1,1);

indty(:, j)= rny;

for i=1:td+1,
   taux=i;

if i==1,
   if rny(i,1)<0.5;
     simy(i,j)= labinc(i,1)*expeyt(indir(i,j),indr(i,j),1);
     simph(i,j)= 1;
	else
     simy(i,j)= labinc(i,1)*expeyt(indir(i,j),indr(i,j),2);
     simph(i,j)= 1;
   end
else
   if rny(i,1)<0.5;
     simy(i,j)= labinc(i,1)*expeyt(indir(i,j),indr(i,j),1)*(yp(2,1)^(indlref(i,j)-1)*yp(1,1)^(taux-indlref(i,j)));
     simph(i,j)= (gph(2,1)^(indni(i,j)-1)*gph(1,1)^(taux-indni(i,j)));
	else
     simy(i,j)= labinc(i,1)*expeyt(indir(i,j),indr(i,j),2)*(yp(2,1)^(indlref(i,j)-1)*yp(1,1)^(taux-indlref(i,j)));
     simph(i,j)= (gph(2,1)^(indni(i,j)-1)*gph(1,1)^(taux-indni(i,j)));
   end
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate ind forced move %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rnmove= rand(td+1,1);
for i=2:td+1,
    auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
    if (0.94*simph(i,j)*auxprice*house)>remdebt(1,i);
       if (rnmove(i,1)<probmove);
           indmove(i,j)= 1;
       end
    else
       if (rnmove(i,1)<(probmove*ratiolock));
           indmove(i,j)= 1;
       end
    end
end


%%%%%%%%%%%%%%%%%
% First period
%%%%%%%%%%%%%%%%%


simpl(1,j)=1.0;
simw(1,j)=0.0;
i= 1;
cash= simw(i,j)+(1-tax)*simy(i,j);
cash= min(cash,gcash(ncash,1));
simw(1,j)= cash;
simcoh(1,j)= cash;

lcash= log(cash);
indcash(i,j)= ntoi(lcash,lgcash);

simdefc(i,j)= def1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
if simdefc(i,j)==0;
    simcons(i,j)= cons1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
  else
    simcons(i,j)= consdef1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
    simdef(i,j)=1;
    simdefc(i+1,j)=1;
end
simvrent(i,j)= vrent1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
simvstay(i,j)= vstay1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
simconsstay(i,j)= cons1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
simconsrent(i,j)= consdef1(1,indcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
simdebt(i,j)=0;
sav(i,j)= cash-simcons(i,j)+simdebt(i,j);
auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
auxprice1=(exp(gr(2,1))^(indnr(i+1,j)-1)*exp(gr(1,1))^(i+1-indnr(i+1,j)));
simw(i+1,j)= sav(i,j)*(1+((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j))))-1)*(1-tax))*auxprice/auxprice1-simdebt(1,j)*exp(gi(indir(i,j),indr(i,j)));
simpl(i,j)= auxprice;
if simdefc(i,j)==0;
    anaux= loanrepaid(1,i)+(exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0+theta)*remdebt(1,i);
    simw(i+1,j)= simw(i+1,j)-anaux/auxprice-(tax_p+m_p)*house*simph(i,j)+simy(i+1,j)*(1-tax)+tax*(exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0+theta)*remdebt(1,i)/auxprice+tax*tax_p*simph(i,j)*house;
    mpreal(i,j)=anaux/auxprice;
    mpnominal(i,j)=anaux;
    rentpay(i,j)=((exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(1,j),1)))-1.0)+tax_p+m_p+rentalpre)*simph(i,j)*house;
else
    anaux=(exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(1,j),1)))-1.0)+tax_p+m_p+rentalpre;
    rentpay(i,j)=((exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(1,j),1)))-1.0)+tax_p+m_p+rentalpre)*simph(i,j)*house;
    simw(i+1,j)= simw(i+1,j)+simy(i+1,j)*(1-tax)-simph(i,j)*house*anaux;
    mpreal(i,j)=(loanrepaid(1,i)+(exp(gr(indr(1,j),1)+gi(indir(1,j),indr(i,j)))-1.0+theta)*remdebt(1,i))/auxprice;
end


if (simw(i+1,j)<0) & ((0.94*simph(i+1,j)*auxprice1*house)<remdebt(1,i+1));	% In case the agent has to default and negative equity
      simdefc(i+1,j)= 1;
      simdef(i+1,j)= 1;
      simw(i+1,j)= gcash(1,1);
end
if (simw(i+1,j)<0) & ((0.94*simph(i+1,j)*auxprice1*house)>remdebt(1,i+1));	% In case the agent has to default and negative equity
      simdefc(i+1,j)= 2;
      simdef(i+1,j)= 1;
      simw(i+1,j)= simw(i+1,j)+ 0.94*simph(i+1,j)*house-remdebt(1,i+1)/auxprice1;
end
if simdef(i,j)==1;
     simdef(i+1,j)=1;
     simdefc(i+1,j)=1;
end
if simw(i+1,j)<gcash(1,1);			% In case the agent has to default
     simw(i+1,j)= gcash(1,1);
end


simw(i+1,j)= min(simw(i+1,j),gcash(ncash,1));

staysimw(1,j) = simw(1,j);
rentsimw(1,j) = simw(1,j);
staysimw(2,j) = simw(2,j);
rentsimw(2,j) = simw(2,j);
stayindcash(1,j) = indcash(1, j);
rentindcash(1,j)  = indcash(1, j);
rentindcashaux = min(rentindcash(i,j) + 1,ncash);
rentcash = cash;
wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
wrent = max(min(1.0,wrent),0.0);
%simvrent(i,j)=(1.0-wrent)*vrent1(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent1(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
simvrent(i,j)=vrent1(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
simvstay(i,j)= vstay1(1,stayindcash(i,j),indr(1,j),indnr(1,j),indlref(1,j),indni(1,j),indir(1,j));
simconsstay(i,j)= cons1(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
simconsrent(i,j)= consdef1(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
staysav(1,j)= sav(1,j);
rentsav(1,j)= sav(1,j);
staysimcoh(1,j)= simcoh(1,j);
rentsimcoh(1,j)= simcoh(1,j);

% Remaining periods

for i=2:td,
   cash= simw(i,j);
   cash= min(cash,gcash(ncash,1));
   simcoh(i,j)= cash;
   lcash= log(cash);
   indcash(i,j)= ntoi(lcash,lgcash);

   staycash= staysimw(i,j);
   staycash= min(staycash,gcash(ncash,1));
   staysimcoh(i,j)= staycash;
   staylcash= log(staycash);
   stayindcash(i,j)= ntoi(staylcash,lgcash);

   rentcash= rentsimw(i,j);
   indcashoutrent(1,j)=indcashout(1,j);
   rentcash= min(rentcash,gcash(ncash,1));
   rentsimcoh(i,j)= rentcash;
   rentlcash= log(rentcash);
   rentindcash(i,j)= ntoi(rentlcash,lgcash);

   if i==2
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
     if simdefc(i,j)==0;
           simdefc(i,j)= def2(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons2(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef2(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef2(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent2(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent2(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent2(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay2(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons2(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef2(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==3
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
     if simdefc(i,j)==0;
           simdefc(i,j)= def3(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons3(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef3(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef3(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent3(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent3(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent3(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay3(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons3(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef3(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==4
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
     if simdefc(i,j)==0;
           simdefc(i,j)= def4(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons4(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef4(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef4(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent4(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent4(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent4(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay4(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons4(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef4(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==5
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def5(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons5(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef5(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef5(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent5(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent5(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent5(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay5(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons5(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef5(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==6
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def6(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons6(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef6(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef6(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent6(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent6(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent6(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay6(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons6(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef6(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==7
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def7(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons7(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef7(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef7(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent7(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent7(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent7(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay7(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons7(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef7(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==8
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def8(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons8(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef8(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef8(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent8(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent8(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent8(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay8(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons8(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef8(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==9
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def9(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons9(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef9(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef9(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent9(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent9(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent9(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay9(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons9(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef9(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==10
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def10(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons10(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef10(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef10(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent10(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent10(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent10(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay10(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons10(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef10(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==11
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def11(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons11(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef11(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef11(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent11(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent11(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent11(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay11(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons11(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef11(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==12
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def12(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons12(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef12(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef12(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent12(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent12(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent12(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay12(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons12(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef12(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==13
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def13(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons13(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef13(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef13(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent13(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent13(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent13(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay13(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons13(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef13(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==14
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def14(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons14(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef14(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
          elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef14(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent14(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent14(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent14(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay14(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons14(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef14(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==15
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def15(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons15(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef15(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef15(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent15(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent15(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent15(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay15(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons15(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef15(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==16
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def16(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons16(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef16(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef16(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent16(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent16(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent16(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay16(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons16(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef16(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==17
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def17(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons17(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef17(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef17(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent17(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent17(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent17(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay17(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons17(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef17(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==18
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def18(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons18(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef18(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef18(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent18(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent18(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent18(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay18(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons18(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef18(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
    elseif i==19
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def19(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons19(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef19(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;

     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef19(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
     rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent19(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent19(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent19(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay19(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons19(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef19(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   elseif i==20
     if simdefc(i,j)==0 & indmove(i,j)==1;
        simmove(i,j)=1;
        auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
        if (0.94*simph(i,j)*house)<(remdebt(1,i)/auxprice);
          simdefc(i,j)=1;
        else
          simdefc(i,j)=2;
        end
     end
       if simdefc(i,j)==0;
           simdefc(i,j)= def20(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     end
     if simdefc(i,j)==0;
           simcons(i,j)= cons20(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     elseif simdefc(i,j)==1;
           simcons(i,j)= consdef20(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     elseif simdefc(i,j)==2;       % Decide to sell house and move into renting
           auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
           simw(i,j) = simw(i,j)+ (0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashout(1,j));
           cash = simw(i,j);
           indcashout(1,j) = 1;
           simcoh(i,j)= cash;
           cash= max(cash,gcash(1,1));
           cash= min(cash,gcash(ncash,1));
           lcash= log(cash);
           indcash(i,j)= ntoi(lcash,lgcash);
           simcons(i,j)= consdef20(1,indcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
           simdef(i,j)=1;
           simdefc(i+1,j)=1;
     end
     auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
     if ((simdefc(i,j) == 0 & 0.94*simph(i,j)*house)>(remdebt(1,i)/auxprice)) || simdefc(i,j) == 2;
         rentsimw(i,j) = rentsimw(i,j)+(0.94*simph(i,j)*house-remdebt(1,i)/auxprice) * abs(1-indcashoutrent(1,j));
         rentcash = rentsimw(i,j);
         indcashoutrent(1,j) = 1;
         rentsimcoh(i,j)= rentcash;
         rentcash= max(rentcash,gcash(1,1));
         rentcash= min(rentcash,gcash(ncash,1));
         rentlcash= log(rentcash);
         rentindcash(i,j)= ntoi(rentlcash,lgcash);
     end
    rentindcashaux = min(rentindcash(i,j) + 1,ncash);
     wrent = (rentcash-gcash(rentindcash(i,j),1))/(gcash(rentindcashaux,1)-gcash(rentindcash(i,j),1));
     wrent = max(min(1.0,wrent),0.0);
     %simvrent(i,j)=(1.0-wrent)*vrent20(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j))+wrent*vrent20(1,rentindcashaux,indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvrent(i,j)= vrent20(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simvstay(i,j)= vstay20(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsstay(i,j)= cons20(1,stayindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simconsrent(i,j)= consdef20(1,rentindcash(i,j),indr(i,j),indnr(i,j),indlref(i,j),indni(i,j),indir(i,j));
     simdebt(i,j)= 0;
   end
   simcoh(i,j)= cash;
   sav(i,j)= cash-simcons(i,j)+simdebt(i,j);
   auxprice=(exp(gr(2,1))^(indnr(i,j)-1)*exp(gr(1,1))^(i-indnr(i,j)));
   auxprice1=(exp(gr(2,1))^(indnr(i+1,j)-1)*exp(gr(1,1))^(i+1-indnr(i+1,j)));
   simw(i+1,j)= sav(i,j)*(1+((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j))))-1)*(1-tax))*auxprice/auxprice1-simdebt(1,j)*exp(gi(indir(i,j),indr(i,j)));
   simpl(i,j)= auxprice;

   staysimcoh(i,j)= staycash;
   staysav(i,j)= staycash-simconsstay(i,j)+simdebt(i,j);
   staysimw(i+1,j)= staysav(i,j)*(1+((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j))))-1)*(1-tax))*auxprice/auxprice1-simdebt(1,j)*exp(gi(indir(i,j),indr(i,j)));

   rentsimcoh(i,j)= rentcash;
   rentsav(i,j)= rentcash-simconsrent(i,j)+simdebt(i,j);
   rentsimw(i+1,j)= rentsav(i,j)*(1+((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j))))-1)*(1-tax))*auxprice/auxprice1-simdebt(1,j)*exp(gi(indir(i,j),indr(i,j)));
  if simdefc(i,j)==0;
    anaux= loanrepaid(1,i)+(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0+theta)*remdebt(1,i);
    simw(i+1,j)= simw(i+1,j)-anaux/auxprice-(tax_p+m_p)*simph(i,j)*house+simy(i+1,j)*(1-tax)+tax*(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0+theta)*remdebt(1,i)/auxprice+tax*tax_p*simph(i,j)*house;
    simw(i+1,j)= min(simw(i+1,j),gcash(ncash,1));
    mpreal(i,j)=anaux/auxprice;
    mpnominal(i,j)=anaux;
    rentpay(i,j)=((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(i,j),1)))-1.0)+tax_p+m_p+rentalpre)*simph(i,j)*house;


  else
    anaux=(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0)-(((exp(mudph+sigdph*sigdph/2))*(exp(gr(indr(i,j),1))))-1.0)+tax_p+m_p+rentalpre;
    rentpay(i,j)=((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(i,j),1)))-1.0)+tax_p+m_p+rentalpre)*simph(i,j)*house;
    simw(i+1,j)= simw(i+1,j)+simy(i+1,j)*(1-tax)-simph(i,j)*house*anaux*auxprice/auxprice;
    simw(i+1,j)= min(simw(i+1,j),gcash(ncash,1));
    mpreal(i,j)=(loanrepaid(1,i)+(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0+theta)*remdebt(1,i))/auxprice;
    rentpay(i,j)=((exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0)-((exp(mudph+sigdph*sigdph/2)*exp(gr(indr(i,j),1)))-1.0)+tax_p+m_p+rentalpre)*simph(i,j)*house;
  end

  stayanaux= loanrepaid(1,i)+(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0+theta)*remdebt(1,i);
  staysimw(i+1,j)= staysimw(i+1,j)-stayanaux/auxprice-(tax_p+m_p)*simph(i,j)*house+simy(i+1,j)*(1-tax)+tax*(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0+theta)*remdebt(1,i)/auxprice+tax*tax_p*simph(i,j)*house;
  staysimw(i+1,j)= min(staysimw(i+1,j),gcash(ncash,1));
  rentanaux=(exp(gr(indr(i,j),1)+gi(indir(i,j),indr(i,j)))-1.0)-(((exp(mudph+sigdph*sigdph/2))*(exp(gr(indr(i,j),1))))-1.0)+tax_p+m_p+rentalpre;
  rentsimw(i+1,j)= rentsimw(i+1,j)+simy(i+1,j)*(1-tax)-simph(i,j)*house*rentanaux*auxprice/auxprice;
  rentsimw(i+1,j)= min(rentsimw(i+1,j),gcash(ncash,1));

 if (simw(i+1,j)<0) & (i<21) & ((0.94*simph(i+1,j)*auxprice1*house)<remdebt(1,i+1));	% In case the agent has to default and negative equity
      simdefc(i+1,j)= 1;
      simdef(i+1,j)= 1;
      simw(i+1,j)= gcash(1,1);
 end
 if (simw(i+1,j)<0) & (i<21) & ((0.94*simph(i+1,j)*auxprice1*house)>remdebt(1,i+1));	% In case the agent has to default and negative equity
      simdefc(i+1,j)= 2;
      simdef(i+1,j)= 1;
      simw(i+1,j)= simw(i+1,j)+ ((0.94*simph(i+1,j)*house-remdebt(1,i+1)/auxprice1)*abs(1-indcashout(1,j)));
      indcashout(1,j) = 1;
 end
  if simdef(i,j)==1;
      simdef(i+1,j)=1;
      simdefc(i+1,j)=1;
  end
  if simw(i+1,j)<gcash(1,1);			% In case the agent has to default
      simw(i+1,j)= gcash(1,1);
  end

   simltv(i,j)= remdebt(1,i)/(simph(i,j)*simpl(i,j)*house);
   simw(i+1,j)= min(simw(i+1,j),gcash(ncash,1));

   staysimw(i+1,j)= min(max(staysimw(i+1,j),gcash(1,1)), gcash(ncash,1));
   rentsimw(i+1,j)= simw(i+1,j);
end	% the t cycle
% Fill in consumption at td+1
if simdefc(21,j)== 1,
	    simcons(21,j)= simw(21,j);
else
    simcons(21,j)= simw(21,j)+simph(21,j)*house;
end

simtwealthstay(:,j)= staysimw(21,j)+simph(21,j)*house;
simtwealthrent(:,j)= rentsimw(21,j);
simcons0= simcons(:,j)/nsim+simcons0;
simdebt0= simdebt(:,j)/nsim+simdebt0;
simr0= simr(:,j)/nsim+simr0;
simi0= simi(:,j)/nsim+simi0;
simw0= simw(:,j)/nsim+simw0;
simy0= simy(:,j)/nsim+simy0;
simdef0= simdef(:,j)/nsim+simdef0;
simref0= simref(:,j)/nsim+simref0;
simdefc0= simdefc(:,j)/nsim+simdefc0;

end   % the j cycle



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CALCULATE PV OF PROFITS OF MORTGAGE PROVIDERS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loss= 0.3;
simpi= zeros(td+1,nsim);
pvsimpi= zeros(td+1,nsim);

df=zeros(2,2,2);
df(1,1,2)=	1.016612898;
df(1,1,1)=	0.968840207;
df(2,1,2)=	0.988246038;
df(2,1,1)= 	0.941806362;
df(1,2,2)=	0.977337163;
df(1,2,1)=	0.931410117;
df(2,2,2)=	0.949971223;
df(2,2,1)= 	0.905330158;



for j=1:nsim,
  for i=1:td,
      if i==1,
        if simdefc(i,j)==1,
            simpi(i,j)= simph(i,j)*simpl(i,j)*house*(1.0-loss);
        elseif simdefc(i,j)==0,
            simpi(i,j)= mpnominal(i,j)*df(indir(i,j),indr(i,j),indi(i+1,j));
        elseif simdefc(i,j)==2,                  % Chooses to rent in period i
            simpi(i,j)= remdebt(1,i);
        end
      elseif i>1,
        if simdefc(i,j)==1 & simdefc(i-1,j)==0,  % Chooses to default in period i
            simpi(i,j)= simph(i,j)*simpl(i,j)*house*(1.0-loss);
        elseif simdefc(i,j)==0,                  % Chooses not to default in period i
            simpi(i,j)= mpnominal(i,j)*df(indir(i,j),indr(i,j),indi(i+1,j));
        elseif simdefc(i,j)==2,                  % Chooses to rent in period i
            simpi(i,j)= remdebt(1,i);
        elseif simdefc(i,j)==1 & simdefc(i-1,j)==1;
            simpi(i,j)= 0;
        end
      end
  end
end




for j=1:nsim,
  for i=1:td,
     auxi= td-i+1;
     if i==1,
         pvsimpi(auxi,j)= simpi(auxi,j);
     elseif i>1
         pvsimpi(auxi,j)=pvsimpi(auxi+1,j)*df(indir(auxi,j),indr(auxi,j),indi(auxi+1,j))+simpi(auxi,j);
    end
  end

end

mean(pvsimpi(1,:)')/loan


age=ones(20,1);

for j=2:20;
	age(j,1)=j;
end


clear simdebt simvdef simdef simref mpnominal indcash simr indnr simi indi indni
out=zeros(nsim*20,19);

simexpgi= zeros(nsim,20);
for j=1:nsim;
    for i=1:20;
    simexpgi(i,j)=exp(gi(indir(i,j),indr(i,j)));
    end
end


for j=1:nsim,
   out((j-1)*20+1:(j-1)*20+20,1)= j*ones(20,1);
   out((j-1)*20+1:(j-1)*20+20,2)= age(1:20,1);
   out((j-1)*20+1:(j-1)*20+20,3)= simy(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,4)= simcons(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,5)= sav(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,6)= simcoh(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,7)= simdefc(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,8)= exp(gr(indr(1:20,j),1));
   out((j-1)*20+1:(j-1)*20+20,9)= simexpgi(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,10)= simph(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,11)= simpl(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,12)= simltv(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,13)= remdebt(1,1:20);
   out((j-1)*20+1:(j-1)*20+20,14)= mpreal(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,15)= rentpay(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,16)= pvsimpi(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,17)= simmove(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,18)= indlref(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,19)= indty(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,20)= simw(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,21)= simvrent(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,22)= simvstay(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,23)= simconsstay(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,24)= simconsrent(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,25)= rentsimw(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,26)= staysimw(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,27)= rentsimcoh(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,28)= staysimcoh(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,29)= rentsav(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,30)= staysav(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,31)= simtwealthrent(1:20,j);
   out((j-1)*20+1:(j-1)*20+20,32)= simtwealthstay(1:20,j);
end


save statsa800x50.raw out -ascii;
