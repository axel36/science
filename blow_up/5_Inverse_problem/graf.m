function gg = graf( X,Y,axi,M,inf )
%GRA Summary of this function goes here
%   Detailed explanation goes here

figure;
gg=5;
if inf == 1
    plot(X,Y,'-or','MarkerSize',3,'LineWidth',1);
    axis([axi(1) axi(2) axi(3) axi(4)]);
end
if inf == 3
   
for n= 1:1:M+1
    n
    pause(0.01);
%     txt2=num2str(t(n));
%     txt1='t =';
%     txt=[txt1 ' ' txt2] ;
    plot(X,Y(:,n),'-or','MarkerSize',3,'LineWidth',1);
    axis([axi(1) axi(2) axi(3) axi(4)]);
%     hT = text(-0.6, 7.5,txt); 
%     hT = text(-0.6, 6.5,'epsilon = 0.05'); 
    xlabel('x'); 
    ylabel('u'); 
    drawnow;
%     pause(0.00001);
   % mov(n) = getframe; 
    end

% Сохраняем видеофайл с презентацией результатов вычислений 
%movie2avi(mov, 'tz.avi', 'compression', 'None');
end

end