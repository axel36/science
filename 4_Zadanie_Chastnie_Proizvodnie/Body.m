clear;
clc;
close('all');
%������ �4 ���������� ����������� ��-�;
global ep; 
ep=0.05;%�������
global N; 
N=300;%����� �������� �� �
M=300;% ����� �������� �� �������
global h;
h=2/N;%��������� �� � 
tau=1.5/M;%��������� ������� �� ��������
aa=(1+1i)/2;% ���������� ����� ����������
n=0;%���������� �����
%���� ��� ��������� ���������� �����������
zeros(M,N);
for k=-1:h:1
   
    n=n+1;
    X(n)=k;%������ � ����������
    U(1,n)=2*tanh(k/ep)+2;
end
% plot(X,U(1,:),'-*r','MarkerSize',3,'LineWidth',1);
n=0;%���������� ����� �������
%���� �����
n=0;
 for k=0:tau:1.5
     n=n+1;
     t(n)=k;
 end

 for n= 1:M+1  
        n
     Y=Yak(U(n,2:N),t(n));%������� ������� �����
     F=Fuu(U(n,2:N),t(n)+tau/2);%������� ������ �������� ������� 
     w=TridiagonalMatrixAlgorithm(eye(N-1)-aa*tau*Y,F);
     w=real(w);
     w=w';%������� w
     U(n+1,2:N)=U(n,2:N)+tau*w;%���������� ����� �������� � U
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

