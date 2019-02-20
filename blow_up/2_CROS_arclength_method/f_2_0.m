function f = f_2_0(u,p,epsilon )
%DFU0 Summary of this function goes here
%   Detailed explanation goes here

%f= -((8*u*(p - u)^2)/e^2 - (4*u^2*(2*p - 2*u))/e^2)/(2*((4*u^2*(p - u)^2)/e^2 + 1)^2);
f=-(4*u*(2*u^2 - 3*u*p + p^2))/(epsilon^2*(1 + (4/epsilon^2)*u^2*(p - u)^2)^(3/2));

end

