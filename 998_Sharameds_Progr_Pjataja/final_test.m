% зададим сетку
N=50; M=100;
x_left=0; x_right=1;
h=(x_right-x_left)/(N);
t_left=0; t_right=0.3;
tau=(t_right-t_left)/(M);

t=t_left:tau:t_right;
x=x_left:h:x_right;

eps=0.5;

for i=1:N+1 % нужно задать как появится условие
    ksi(i)=(x(i)-0.25)/eps;
end

for i=1:N+1 % нужно задать как появится условие
    q(i)=0;
end

u=direct_solution(N,M,tau,q,h,eps,ksi,x);
[ T_mesh , X_mesh] = meshgrid( t, x );
meshc(X_mesh,T_mesh,u(:,:))

% for m = 1:M+1
%     m
%     plot(x,u(:,m),'-sk','MarkerSize',3);
%     axis([0 1 -9 9]);
%     hold off;
%     drawnow;
%     pause(0.1);
% end


for i=1:N+1 % нужно задать как появится условие
    q_obs(i)=1;
end

f_obs=direct_solution(N,M,tau,q_obs,h,eps,ksi,x);

psi=backward_solution(N,M,tau,q,h,eps,u,f_obs);


for m = M+1:-1:1
    m
    plot(x,psi(:,m),'-sk','MarkerSize',3);
    axis([0 1 -9 9]);
    hold off;
    drawnow;
    pause(0.1);
end


% meshc(X_mesh,T_mesh,psi(:,:))

for i=1:N+1
    for j=1:M+1
        func(i,j)=u(i,j)*psi(i,j);
    end
end

J=itegr_trapezija(func,h,M+1);

alp=1;

dJ=J'+alp*q;

betta=0.05;

qnew=q-betta*dJ;

for i=1:N+1
    func123(i)=(u(i,M+1)-f_obs(i,M+1))^2;
end


J_prov=itegr(func123,h,N+1);

u_prov=direct_solution(N,M,tau,qnew,h,eps,ksi,x);

for i=1:N+1
    func12(i)=(u_prov(i,M+1)-f_obs(i,M+1))^2;
end


J_prov2=itegr(func12,h,N+1);





