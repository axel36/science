function u=direct_solution(N,M,tau,q,h,eps,ksi,x)

for i=1:N+1 % нужно задать как появится условие
    u0t(i)=(((x(i))^2)-x(i)-2)-6*tanh(-3*ksi(i));
end

for j=1:M+1 % нужно задать как появится условие
    u0x(j)=-8;
end

for j=1:M+1 % нужно задать как появится условие
    uNx(j)=4;
end

u=direct_problem(N,M,tau,u0x,uNx,u0t,q,h,eps);

end