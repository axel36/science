clear;
clc;

%�������� ������, ���������� ����������� ��-�;
global ep; 
ep=0.5;%�������
global N; 
N=100;%����� �������� �� �
M=200;% ����� �������� �� �������
global h;
h=1/N;%��������� �� � 
tau=0.2/M;%��������� ������� �� ��������
aa=(1+1i)/2;% ���������� ����� ����������
S_max=100001;%����� �������� ��� ���������� q(x)
axi=[0 1 -10 10];

%
%                            ���������������� ����, ��������� f_obs
%

n=0;%���������� �����
%���� ��� ��������� ���������� �����������
U=zeros(N+1,M+1);
n=0;
for k=0:h:1
    n=n+1;
    X(n)=k;%������ � ����������
    U(n,1)= (k^2 - k - 2)-6*tanh(-3*(k-0.25)/ep);
       
end
% plot(X,U(:,1),'-*r','MarkerSize',3,'LineWidth',1);
U_init=U(:,1);

%������ ������ ������� q(x)
for n = 1:N+1
Q(n) = sin(3*pi*X(n));
Line(n)=0;
end
% figure;
% plot(X,Q,'-*r','MarkerSize',3,'LineWidth',1);

n=0;%���������� ����� �������
 for k=1:M+1
     t(k)=n;
     n=n+tau;
 end
 
%���� �����
 for n= 1:M  
        n;
     Y=Yak(U(2:N,n),Q);%������� ������� �����
     F=Fuu(U(2:N,n),Q);%������� ������ �������� ������� 
     
     w=TridiagonalMatrixAlgorithm(eye(N-1)-aa*tau*Y,F);
     w=real(w);
     %w=w';%������� w
     U(2:N,n+1)=U(2:N,n)+tau*w;%���������� ����� �������� � U
     U(1,n+1) = -8 ;
     U(N+1,n+1) =4 ;
 end
%graf;

%�������� F_obs  
F_obs=U(:,M+1);
sum=0;
for n= 1:N+1
    F_obs_err(n,1)=F_obs(n)*(1+0.001*(rand-rand));
    sum= sum+(F_obs_err(n)- F_obs(n))^2;
    
end
  Delta=sqrt(sum);

% figure;
% plot(X',F_obs,'-g');
% hold on;
% plot(X',F_obs_err,'--r');
% 
% Delta^2
% 
% contiue;
%
                %���� ����������������� ������� �������� ������
%


%������ ��������� ����������� ������� q(x)
for n= 1:N+1
q(n,1)= 0;
end

%�������� ���� 
k=0;
s=0;
sum=0;
res2= 2*Delta^2;
while res2 > Delta^2
     
     s=s+1;
     U_s=zeros(N+1,M+1);
        %������� ������ ������
     U_s(:,1)=U_init; 
     for n= 1:M  
        n;
        Y=Yak(U_s(2:N,n),q(:,s));%������� ������� �����
        F=Fuu(U_s(2:N,n),q(:,s));%������� ������ �������� ������� 
     
        w=TridiagonalMatrixAlgorithm(eye(N-1)-aa*tau*Y,F);
        w=real(w);
        %w=w';%������� w
        U_s(2:N,n+1)=U_s(2:N,n)+tau*w;%���������� ����� �������� � U
        U_s(1,n+1) = -8 ;
        U_s(N+1,n+1) = 4 ;
     end
     
     
     %      ������� ����������� ������
     
     Psi_s=zeros(N+1,M+1);
     Psi_s(:,M+1)=-2*(U_s(:,M+1)-F_obs_err);
     for m= M+1:-1:2
         
        Y=YakAdj(U_s(2:N,m),q(:,s));%������� ������� �����
        F=FuuAdj(U_s(2:N,m),Psi_s(2:N,m),q(:,s));%������� ������ �������� ������� 
     
        w=TridiagonalMatrixAlgorithm(eye(N-1)+aa*tau*Y,F);
        w=real(w);
        %w=w';%������� w
        Psi_s(2:N,m-1)=Psi_s(2:N,m)-tau*w;%���������� ����� �������� � Psi
        Psi_s(1,m-1) = 0 ;
        Psi_s(N+1,m-1) =0 ;
     end
%      graf(X, Psi_s, axi,M, 3);
%      graf(X, U_s, axi,M, 3);
% 
%                           %  ����� �������� ��� ������ ���������
%                          % �����������
     for n =2:N+1
         sum=0;
         for m =2:M
            sum = sum +((U_s(n,m)*Psi_s(n,m)+U_s(n,m-1)*Psi_s(n,m-1))*tau)/2;
         end
         Grad(n)=sum;
     end
     Grad=Grad';
     q(:,s+1)=q(:,s)-0.03*Grad;
     
       %������� ����������� �-�� (20)
       sum=0;
      for n= 2:N+1
          sum = sum+(((U_s(n,M+1)-F_obs_err(n))^2 + (U_s(n-1,M+1)-F_obs_err(n-1))^2)*h)/2;
      end
      J(s)=sum;
      
     txt=num2str(J(s));
     txt1=num2str(s);
     plot(X,Q,'--','MarkerSize',1,'LineWidth',1);
     hold on;
     plot(X,Line,'--.k','MarkerSize',1,'LineWidth',1);
     plot(X,q(:,s+1),'-*r','MarkerSize',3,'LineWidth',2);
      hT = text(0.7, 1.65,txt);
      hT = text(0.15, 1.65,txt1);
     hold off;
      if rem(s,150) == 0
          k=k+1;
%           mov(k) = getframe;
      end
     axis([0 1 -2 2]);
     xlabel('x'); 
     ylabel('q'); 
      drawnow;
%       pause(1);
    
      Grad=[];
      U_s=[];
      Psi_s=[];
      
      res2 = J(s);
 end
% movie2avi(mov, '100kvid.avi', 'compression', 'None');