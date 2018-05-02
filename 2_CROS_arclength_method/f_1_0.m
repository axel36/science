function f = f_1_0( u, p,epsilon )
%DF2U0 Summary of this function goes here
%   Detailed explanation goes here
%f=(2*e*(u1-2*u0))/((e*e+4*u0*u0*(u1-u0)*(u1-u0))*(((4*u0*u0*(u1-u0)*(u1-u0))/e*e+1)^1/2));

%f =((2*u1/e-4*u0/e-(u0*(2*u0-2*u1)*(4*u0-2*u1))/(e*e*((1+(2/e*u0*(u1-u0))^2)^1/2)))/(1+(2/e*u0*(u1-u0))^2)) ;
% matlab f= (p - u)/(e*((4*u^2*(p - u)^2)/e^2 + 1)) - u/(e*((4*u^2*(p - u)^2)/e^2 + 1)) - (u*(p - u)*((8*u*(p - u)^2)/e^2 - (4*u^2*(2*p - 2*u))/e^2))/(e*((4*u^2*(p - u)^2)/e^2 + 1)^2);
f=(2*epsilon*(-2*u + p))/((1 + (4/epsilon^2)*u^2*(p - u)^2)^(1/2)*(epsilon^2 + 4*u^4 - 8*u^3*p + 4*u^2*p^2));

end

