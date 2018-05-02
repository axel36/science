clear;
aa=(1+1i)/2;%����� �����
e=10^-2;%����� �����
 E=eye(2);%������� � �����
 s=12;%����� ��������
 uu0=3;%���. ������� U
 uu1=-1;%���. ������� �   
 dl=0.01;%��������� dL
 n=1; %������� ����� 
 
 %������ ����, �������� ������. �����
 while uu1 < 2    
    U(1,n)=uu0;%��� �����
    F=[((2/e)*uu0*(uu1 - uu0))/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2); 1/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2)];
    Yak=[f_1_0(uu0,uu1,e) f_1_1(uu0,uu1,e);f_2_0(uu0,uu1,e) f_2_1(uu0,uu1,e)];
    A=E-dl*aa*Yak;
    w=inv(A)*F; 
    W=real(w);         %����� , ������������������ ������� ����� ����
    uu0=uu0+dl*W(1);
    uu1=uu1+dl*W(2);
    n=n+1; 
 end
  X(1)=n;%�������� ������ � ��� ������� R(N)
  a=1;%���������� ��������
  
for m = 1:1:s
    m%�����  
    z=1;%���������� ��� ������ ���� �����
    a=a*2;%���������� �����. ��� ����.
    dl=0.01/a;%���������� dL
    n=n*2;%����������� 
    uu0=3;%��� �������� U
    uu1=-1;%��� �������� l
    %���� ������ ����. �����
    for t= 1:1:n
         if rem(t,a)== 1 %������� �� ����� �� ����������� =1(�������� �� ��������� ������ ������ ��������)
            U(m+1,z)=uu0;% ��������� ������� ��������  
            N(m,z)=t;%��������
          	z=z+1;%������� ��� ���� ��������
         end
        UUU(m,t)=uu0; 
        F=[((2/e)*uu0*(uu1 - uu0))/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2); 1/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2)];
        Yak=[f_1_0(uu0,uu1,e) f_1_1(uu0,uu1,e);f_2_0(uu0,uu1,e) f_2_1(uu0,uu1,e)];
        A=E-dl*aa*Yak;
        w=inv(A)*F;       %���� �����
        W=real(w);
        uu0=uu0+dl*W(1);
        uu1=uu1+dl*W(2);   
    end
 end
r=2;%���������� ��������
ff=1;%������ � ������� �������� �
pp=1;%��������� ������� ��������
v=1;%������ � ������ R, ���������� �� 3-� ��������(��� ������� � ������ �������)
    
%���� ��� ���������� U � ������ � R   1 ���� ( 1-� �������)
 for k= ff:1:s-1
    R(k,:)=(U(k+1,:)-U(k,:))/(r^pp-1);%��������� R
    Uu(k,:)=U(k+1,:)+R(k,:);%��������� ������ ���������� ��������
    dD(k)=sqrt(sum(R(k,:).*R(k,:)))/sqrt(sum(Uu(k,:).*Uu(k,:)))*100;%������������� �����������
    Y(1,k)=sqrt(sum(R(k,:).*R(k,:)));%�������� ��� ���������� ������� ( ����� ������� ������)
 end
       %���� ��� P���   1 ����                                                    
 for k=ff:1:s-2
     h=sqrt(sum(R(k+1,:).*R(k+1,:)))/sqrt(sum(R(k,:).*R(k,:)));%��������� Rn � Rn+1
     p(k,1)=log(h)/log(r)*(-1);     %�������������� ���� �                                     
     
 end
 
 % ������ ���� ��� P��� , U, R (��������� �������)
 
  while ff<s-1
     v=v+1;
    ff=ff+1;
    pp=pp+1;%���������� �������� ��� ������ ��������
     
    for k= ff:1:s-1
        R(k,:,v)= (Uu(k+1-ff+1,:,v-1)-Uu(k-ff+1,:,v-1))/(r^pp-1);%��������� R
        Uu(k,:,v)=Uu(k-ff+2,:,v-1)+R(k,:,v);%��������� ������ ���������� ��������
        dD(k,v)=sqrt(sum(R(k,:,v).*R(k,:,v)))/sqrt(sum(Uu(k,:,v).*Uu(k,:,v)))*100;%������������� �����������
        Y(v,k)=sqrt(sum(R(k,:,v).*R(k,:,v)));%�������� ��� ���������� ������� ( ����� ������� R)
    end
    
    for k=ff:1:s-2
         h=sqrt(sum(R(k+1,:,v).*R(k+1,:,v)))/sqrt(sum(R(k,:,v).*R(k,:,v)));
         p(k,v)=log(h)/log(r)*(-1);      %��������� ������� � �                                  
    end
   
  end
%  
%    for k= 1:1:s-1
%    
%     
% t=rem(k,2)+1;% ��� ����� ������
% 
% 
%  VarX=X(k+1:s);
%  VarY=Y(k,k:s-1);1
%  loglog(VarX,VarY) ;%������
% 
%  hold on  %���� �� ������
%  end


 p
%  dD