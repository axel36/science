function psi=backward_solution(N,M,tau,q,h,eps,u,f_obs)

for i=1:N+1 % f добавить
    psiMt(i)=-2*(u(i,M+1)-f_obs(i,M+1));
end

for j=1:M+1 % 
    psi0x(j)=0;
end

for j=1:M+1 % нужно задать как появится условие
    psiNx(j)=0;
end

psi=backward_problem(N,M,tau,psi0x,psiNx,psiMt,q,h,eps,u);


end