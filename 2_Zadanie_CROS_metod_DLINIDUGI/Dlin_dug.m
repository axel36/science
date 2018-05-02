 clear;
 close ('all')
 aa=(1+1i)/2;
 n=1;
 uu0=3;
 uu1=-1;
 e=10^-2;
 E=eye(2);

 dl=0.00711;
  while uu1 < 2
    X(n)=uu1;     
    Y(n)=uu0;
    F=[((2/e)*uu0*(uu1 - uu0))/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2) ; 1/sqrt(1 + (4/e^2)*uu0^2*(uu1 - uu0)^2)];
    Yak=[f_1_0(uu0,uu1,e) f_1_1(uu0,uu1,e);f_2_0(uu0,uu1,e) f_2_1(uu0,uu1,e)];
    A=E-dl*aa*Yak;
    w=inv(A)*F; 
    W=real(w);
    uu0=uu0+dl*W(1);
    uu1=uu1+dl*W(2);
    n=n+1; 
 end
 

plot(X,Y,'-*b','MarkerSize',3,'LineWidth',1);
axis([-1 2 0 3]);