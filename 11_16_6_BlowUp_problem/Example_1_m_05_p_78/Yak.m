function f = Yak( u )
%YAK Summary of this function goes here
%   Detailed explanation goes here

global N;
global m;
global p;
global h;
f=zeros(N);
f(1,1)=(-2*m*u(1)^(m-1))/(3*h*h);
f(1,2)=(2*m*u(2)^(m-1))/(3*h*h);
% f(N,N)=(2*p*u(N)^(p-1))/(h)- (2*m*u(N)^(m-1))/(h*h);
% f(N,N-1)=(2*m*u(N-1)^(m-1))/(h*h);
f(N,N)=(p*u(N)^(p-1))- (3*m*u(N)^(m-1))/(2*h);
f(N,N-1)=(2*m*u(N-1)^(m-1))/(h);
f(N,N-2)=(-1)*(m*u(N-2)^(m-1))/(2*h);

for n= 2:1:N-1
    f(n,n)= (-m*u(n)^(m-1)*2)/(h*h);
    f(n,n-1)=(m*u(n-1)^(m-1))/(h*h);
    f(n,n+1)=(m*u(n+1)^(m-1))/(h*h);
end
end
