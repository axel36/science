function dJ=gradient_proizv(u,psi,q,alp,N,M,h)


for i=1:N+1
    for j=1:M+1
        F(i,j)=u(i,j)*psi(i,j);
    end
end

ddJ=(sum(F,2)-F(:,1)/2-F(:,M+1)/2)*h+alp*q';

dJ=ddJ';

end