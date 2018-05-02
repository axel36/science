function f = Fuu(u,N,m,p,h)
%
%  from Vector U -> makes rigt part of equasion F
% global N;
% global m; % глобальные переменные , кол-во итераций, вводные параметры задачи, шаг по сетке
% global p;
% global h;
f = zeros(N ,1);
 
f(1,1)=(u(2)^m-2*u(1)^m+((4*u(1)^m-u(2)^m))/3)/(h*h); %задаем в 1-ом и N-ом узле
f(N,1)=u(N)^p-(3*u(N)^m-4*u(N-1)^m+u(N-2)^m)/(2*h);
for n= 2:1:N-1
    f(n,1)=(u(n+1)^m-2*u(n)^m+u(n-1)^m)/(h*h); %задаем во всех остальных
end
%f=f';
end

