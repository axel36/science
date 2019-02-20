function f = f_1_1( u,p,epsilon )
%DF2U1 Summary of this function goes here
%   Detailed explanation goes here

%f = ((2/e*u0-(4*u0*u0*(u1-u0))/(e*e*(1+(2/e*u0*(u1-u0))^2)^1/2))/(1+(2/e*u0*(u1-u0))^2)) ; 
%f= (2*u0)/((e*((e*e+4*u0*u0*(u1-u0)*(u1-u0))/e*e)^3/2));
%f=u/(e*((4*u^2*(p - u)^2)/e^2 + 1)) - (4*u^3*(2*p - 2*u)*(p - u))/(e^3*((4*u^2*(p - u)^2)/e^2 + 1)^2);
f=(2*epsilon*u)/((1 + (4/epsilon^2)*u^2*(p - u)^2)^(1/2)*(epsilon^2 + 4*u^4 - 8*u^3*p + 4*u^2*p^2)) ;

end

