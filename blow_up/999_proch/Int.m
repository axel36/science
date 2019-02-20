clear;
h=0.05;
k=0;
%start=-2*pi;
for n =0:h:3
    k=k+1;
    Y(k)=5;
    X(k)=n;
end
    plot(X,Y,'-or','MarkerSize',3,'LineWidth',1);
J=0;
k=1;
for n= h:h:3   
    k=k+1;
    J=J+(Y(k)+Y(k-1))*h/2
end