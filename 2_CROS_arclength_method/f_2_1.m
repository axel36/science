function f = f_2_1( u,p,epsilon )
%DFU1 Summary of this function goes here
%   Detailed explanation goes here

%f=-(2*u^2*(2*p - 2*u))/(e^2*((4*u^2*(p - u)^2)/e^2 + 1)^2);
f= -(4*u*(-u + p))/(epsilon^2*(1 + (4/epsilon^2)*u^2*(p - u)^2)^(3/2));
end

