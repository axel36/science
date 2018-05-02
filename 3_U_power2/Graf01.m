
%График для U^2
a=10-2;
b=3;

 for k= 1:1:501
     P(k)=log(R(a,k)/R(a+1,k))/log(r);
 end
plot(X,P,'-*r','MarkerSize',3,'LineWidth',1);
figure;
plot(X,UU(b,:),'-*b','MarkerSize',3,'LineWidth',1);
axis([0 2 -0 1.2]);