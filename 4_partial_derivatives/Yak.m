function f = Yak( u,t )
%YAK Summary of this function goes here
%   Detailed explanation goes here

global N;
global ep;
global h;
f=zeros(N-1);
left=-6 + sin(4*3.14159265359*t);
right= 6-2*sin(4*3.14159265359*t);
b= 2+cos(4*3.14159265359*t);
f(1,1)=-2/(h*h)+(u(2)-left)/(ep*h*2) - b/ep;
f(1,2)=1/(h*h) + u(1)/(ep*h*2);
f(N-1,N-2)=1/(h*h) - u(N-1)/(ep*h*2);
f(N-1,N-1)=-2/(h*h)+(right-u(N-2))/(ep*h*2) - b/ep;

for n= 2:1:N-2
    f(n,n)= -2/(h*h)+(u(n+1)-u(n-1))/(ep*h*2) - b/ep;
    f(n,n-1)=1/(h*h) - u(n)/(ep*h*2);
    f(n,n+1)=1/(h*h) + u(n)/(ep*h*2);
end
end
