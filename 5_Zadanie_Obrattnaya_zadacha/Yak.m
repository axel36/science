function f = Yak( u,q )
%YAK Summary of this function goes here
%   Detailed explanation goes here

global N;
global ep;
global h;
f=zeros(N-1);
left=-8;
right= 4;

f(1,1)=-2*ep/(h*h)+(u(2)-left)/(h*2) - q(1);
f(1,2)=1*ep/(h*h) + u(1)/(h*2);
f(N-1,N-2)=1*ep/(h*h) - u(N-1)/(h*2);
f(N-1,N-1)=-2*ep/(h*h)+(right-u(N-2))/(h*2) - q(N-1);

for n= 2:1:N-2
    f(n,n)= -2*ep/(h*h)+(u(n+1)-u(n-1))/(h*2) - q(n);
    f(n,n-1)=ep*1/(h*h) - u(n)/(h*2);
    f(n,n+1)=ep*1/(h*h) + u(n)/(h*2);
end
end
