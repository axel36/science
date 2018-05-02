function J=gradient_new(u,f_obs,q,alp,N,h)


for i=1:N+1
    F(i)=(u(i)-f_obs(i))^2;
end

for i=1:N+1
    Q(i)=(q(i))^2;
end

J=(sum(F)-F(1)/2-F(N+1)/2)*h+alp*(sum(Q)-Q(1)/2-Q(N+1)/2)*h;

end