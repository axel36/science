%dU/dt= e0*U*(t-U)
 clear;
 close ('all')
 N=800;
 T=3/N;
 aa=(1+1i)/2;% переменная Схемы Розенброка
 e0=200;
 e00=1/200;
 U=3;
 n=1;
 
   
    for t= -1:T:2  
  
        Y(n)=U;
        X(n)=t;    
        w=(e0*U*(t+T/2-U))/(1-aa*T*e0*(t-2*U));%Сама схема
        W=real(w);
        U=U+T*W;                
        
        n=n+1;
    end
figure;
        plot(X,Y,'LineWidth',2);