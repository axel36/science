clear;
clc;

%�������� ������, ���������� ����������� ��-�;
global ep; 

aa=(1+1i)/2;% ���������� ����� ����������
S_max=100001;%����� �������� ��� ���������� q(x)
axi=[0 1 -10 10];
S_for_Rich=2;
a=1;% �������� ��� �������� �����
r=1.2;
pp=2;%��������� ������� ��������
pogr=0.001;
ep=0.095905;%�������

koe=1;
N1=30;
for s_mst= 1:S_for_Rich
    
    
global N; 
N=N1*a*koe;%����� �������� �� �
M=20*a*koe;% ����� �������� �� �������
global h;
h=1/N;%��������� �� � 
tau=0.2/M;%��������� ������� �� ��������
    
%
%                            ���������������� ����, ��������� f_obs
%

n=0;%���������� �����
%���� ��� ��������� ���������� �����������
U(N+1,M+1,s_mst)=0;
U(:,:,s_mst)=zeros(N+1,M+1);
n=0;
for k=0:h:1
    n=n+1;
    X(n,s_mst)=k;%������ � ����������
    U(n,1,s_mst)= (k^2 - k - 2)-6*tanh(-3*(k-0.25)/ep);
       
end
 %plot(X,U(:,11),'-*r','MarkerSize',3,'LineWidth',1);
U_init=U(:,1,s_mst);

%������ ������ ������� q(x)
for n = 1:N+1
Q(n) = sin(3*pi*X(n,s_mst));
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
        n
     Y=Yak(U(2:N,n,s_mst),Q);%������� ������� �����
     F=Fuu(U(2:N,n,s_mst),Q);%������� ������ �������� ������� 
     
     w=TridiagonalMatrixAlgorithm(eye(N-1)-aa*tau*Y,F);
     w=real(w);
     %w=w';%������� w
     U(2:N,n+1,s_mst)=U(2:N,n,s_mst)+tau*w;%���������� ����� �������� � U
     U(1,n+1,s_mst) = -8 ;
     U(N+1,n+1,s_mst) =4 ;
     TT(n+1,s_mst)=t(n+1); %��� �������

 end,
 
                                            %End of schem
 
 n=0;
 for n=1:M+1
u(:,n,s_mst)= interp1(X(:,s_mst),U(:,1,s_mst),X(1:N1,1));
 end
% for i = 1:a:N+1  % ����� �� �
%         n=n+1;
%         k=0;
%         for j =1:a:M+1 % ����� �� �������
%             k=k+1;
%             u(n,k,s_mst)=U(i,j,s_mst);
%             TTT(k,s_mst)=TT(j,s_mst); %����������� ������� �������
%         end
%     end
%  
graf(X(:,s_mst), U(:,:,s_mst), axi,M, 3);
a=a*r;

end
 
    R(:,:)=(u(:,:,2)-U(1:30,1:25,1))/(r^pp-1);%��������� R
    max(max(R))
DeltaR = sqrt(sum(sum(R.*R)));
%�������� F_obs  
F_obs_end(N+1)=U(N+1,M+1,s_mst);  % !!!!!!!!!!!!!!!!!!!!!!!!!!����� s_mst ����� 1 ��� 2???
F_obs_end(: )=U(:,M+1,s_mst);
sum=0;
for n= 1:N+1
    F_obs_err(n )=F_obs_end(n )*(1+pogr*(rand-rand));
    sum= sum+(F_obs_err(n )- F_obs_end(n ))^2;
    
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
q(n,1 )= 0;
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
     Psi_s(:,M+1)=-2*(U_s(:,M+1)-F_obs_err(: ));
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
     q(:,s+1 )=q(:,s )-0.03*Grad;
     
       %������� ����������� �-�� (20)
       sum=0;
      for n= 2:N+1
          sum = sum+(((U_s(n,M+1)-F_obs_err(n))^2 + (U_s(n-1,M+1)-F_obs_err(n-1))^2)*h)/2;
      end
      J(s )=sum;
      
     txt=num2str(J(s));
     txt0=num2str(Delta *Delta );
     txt1=num2str(s);
   %  txt3=num2str(s_mst);
     plot(X(:,s_mst),Q,'--','MarkerSize',1,'LineWidth',1);
     hold on;
     plot(X(:,s_mst),Line,'--.k','MarkerSize',1,'LineWidth',1);
     plot(X(:,s_mst),q(:,s+1 ),'-*r','MarkerSize',3,'LineWidth',2);
      hT = text(0.7, 1.65,txt);
       hT = text(0.7, 1.85,txt0);
    %  hT = text(0.9, 1.8,txt3);
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