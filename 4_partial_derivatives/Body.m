clear;
clc;
close('all');
%Задача №4 сингулярно возмущенное ур-е;
global ep; 
ep=0.05;%Эпсилон
global N; 
N=300;%Число ОТРЕЗКОВ по Х
M=300;% Число ОТРЕЗКОВ по времени
global h;
h=2/N;%Разбиение по Х 
tau=1.5/M;%Изменение времени за итерацию
aa=(1+1i)/2;% переменная Схемы Розенброка
n=0;%переменная цикла
%Цикл Для получения начального приближения
zeros(M,N);
for k=-1:h:1
   
    n=n+1;
    X(n)=k;%Вектор Х координаты
    U(1,n)=2*tanh(k/ep)+2;
end
% plot(X,U(1,:),'-*r','MarkerSize',3,'LineWidth',1);
n=0;%переменная цикла счетчик
%Сама Схема
n=0;
 for k=0:tau:1.5
     n=n+1;
     t(n)=k;
 end

 for n= 1:M+1  
        n
     Y=Yak(U(n,2:N),t(n));%Считаем матрицу Якоби
     F=Fuu(U(n,2:N),t(n)+tau/2);%Считаем вектор значений функции 
     w=TridiagonalMatrixAlgorithm(eye(N-1)-aa*tau*Y,F);
     w=real(w);
     w=w';%считаем w
     U(n+1,2:N)=U(n,2:N)+tau*w;%Записываем новые значения в U
     U(n+1,1) = -6 + sin(4*3.14159265359*t(n)) ;
   end
% plot(X,U(1:201,1)','-*r','MarkerSize',3,'LineWidth',1);
% figure;
% plot(X,U(2,:),'-*r','MarkerSize',3,'LineWidth',1);
% g=inputdlg('Grafiki? Y? N?','graf');
% z=g{1};
% if z ==  'Y'| z=='y'
% 
% end
graf;

