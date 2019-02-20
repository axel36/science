%  a=2; %Нужные переменные
%  ff=1;
%  v=1;
%  m=1;
%  pp=2;%Начальный порядок точности
% 
% % второй этап для Pэфф , U, R
%  while ff<s-1
%      v=v+1;
%     % a=a+1;
%     ff=ff+1;
%     pp=pp+1;
%      
%     for k= ff:1:s-1
%         R(k,:,v)= (Uu(k+1-ff+1,:,v-1)-Uu(k-ff+1,:,v-1))/(a^pp-1);
%         Uu(k,:,v)=Uu(k-ff+2,:,v-1)+R(k,:,v);
%         dD(k,v)=sqrt(sum(R(k,:,v).*R(k,:,v)))/sqrt(sum(Uu(k,:,v).*Uu(k,:,v)))*100;
%         Y(v,k)=sqrt(sum(R(k,:,v).*R(k,:,v)));
%     end
%     
%     for k=ff:1:s-2
%          h=sqrt(sum(R(k+1,:,v).*R(k+1,:,v)))/sqrt(sum(R(k,:,v).*R(k,:,v)));
%          p(k,v)=log(h)/log(a)*(-1);                                        
%     end
%    
%  end
%   color = ['b';'r'];
%  i=1;
% VarX=[];
% VarY=[];

 Y(v,k)=sqrt(sum(R(k,:,v).*R(k,:,v)));

for k=1:1:24
    for l=1:1:24
    YY(k,l)=sqrt(sum(R(l,:,k).*R(l,:,k)));
    end
end
