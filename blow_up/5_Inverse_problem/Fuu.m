function f = Fuu(u,Q)
%FUU Summary of this function goes here
%   Vector U -> F
global N;
global ep;
global h;
left=-8;
right=4;
f(1)=(u(2)-2*u(1)+left)*ep/(h*h)+ u(1)*(u(2)-left)/(2*h)-u(1)*Q(1);
% c=[u(1) u(2) u(3)]
f(N-1)=(right-2*u(N-1)+u(N-2))*ep/(h*h) + u(N-1)*(right-u(N-2))/(2*h) - u(N-1)*Q(N-1);
for n= 2:1:N-2
    f(n)=(u(n+1)-2*u(n)+u(n-1))*ep/(h*h)+ u(n)*(u(n+1)-u(n-1))/(2*h) - u(n)*Q(n);
end
f=f';
end

