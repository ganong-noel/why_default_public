function p=gettrans(ym,wm,m,r,s) ;
% this function computes the transition matrix
                         % ym is the vector of quadrature points
                         % wm is the vector of weights
                         % m is the mean of the process
                         % r is the rho of the process
                         % s is the conditional std.dev. of the process                                                

[n,n0]=size(ym) ;        % used to get n the number of quadrature points

x=ym(:,ones(n,1)) ;      % an nxn matrix whose ji element is consumption
                         % growth in state j - so x is the value of
                         % consumption growth at time t, if the state is
                         % j at time t and i at time t+1 (notice it's
                         % constant across i)

y=x' ;                   % also an nxn matrix whose ji element is
                         % consumption growth in state i - so y is the
                         % value of consumption growth at time t+1, if
                         % the state is j at time t and i at time t+1
                         % (notice it's constant across j)

w=wm(:,ones(n,1))' ;     % converts the weight vector to a nxn matrix
                         % whose ji element is w(i) for all j

c1=condnorm(y,x,m,r,s) ; % computes an nxn matrix whose ji element is
                         % f[y(i)|x(j)] given the parameters m,r,s

c2=condnorm(y,m,m,r,s) ; % computes an nxn matrix whose ji element is
                         % f[y(i)|mu] given the parameters m,r,s

p=(c1.*w)./c2 ;          % builds the transition matrix with elemets
                         % p(j,i)=f[y(i)|x(j)]w(i)/f[y(i)|mu]

sm=sum(p')' ;            % creates column vector with elements
                         % s(j)=sum(i) p(j,i)

p=p./sm(:,ones(n,1)) ;   % gets final transition matrix as p(j,i)/s(j)
                         % now sum(i) p(j,i) = 1 for all j

