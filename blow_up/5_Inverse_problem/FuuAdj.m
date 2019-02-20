function f = FuuAdj(u,psi,q)
%FUU Summary of this function goes here
%   Vector U -> F
global N;
global ep;
global h;
f(1)=(-2*ep*(psi(2)-2*psi(1)))/(2*h*h)+(u(1)*psi(2))/(2*h)+psi(1)*q(1);
f(N-1)=(-2*ep*(psi(N-2)-2*psi(N-1)))/(2*h*h)+(u(N-1)*psi(N-2)*(-1))/(2*h)+psi(N-1)*q(N-1);
for n= 2:N-2
    f(n)=(-2*ep*(psi(n+1)-2*psi(n)+psi(n-1)))/(2*h*h)+(u(n)*(psi(n+1)-psi(n-1)))/(2*h)+psi(n)*q(n);
end
end

