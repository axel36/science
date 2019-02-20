function f = Fuu(u,t)
%FUU Summary of this function goes here
%   Vector U -> F
global N;
global ep;
global h;
b= 2+cos(4*3.14159265359*t);
left=-6 + sin(4*3.14159265359*t);
right= 6-2*sin(4*3.14159265359*t);
f(1)=(u(2)-2*u(1)+left)/(h*h)+ u(1)*(u(2)-left)/(ep*2*h)-u(1)*b/ep;
% c=[u(1) u(2) u(3)]
f(N-1)=(right-2*u(N-1)+u(N-2))/(h*h) + u(N-1)*(right-u(N-2))/(ep*2*h) - u(N-1)*b/ep;
for n= 2:1:N-2
    f(n)=(u(n+1)-2*u(n)+u(n-1))/(h*h)+ u(n)*(u(n+1)-u(n-1))/(ep*2*h) - u(n)*b/ep;
end
f=f';
end

