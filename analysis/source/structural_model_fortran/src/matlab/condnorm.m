function f=condnorm(y,x,m,r,s) ;
f=(y-m*(1-r)-x*r) ;
f=exp(-(f.*f)./(2.0*s*s))./(sqrt(2.0*pi)*s) ;
