figure;

for n= 1:1:M
    n
    txt2=num2str(t(n));
    txt1='t =';
    txt=[txt1 ' ' txt2] ;
    plot(X(1:N),U(n,1:N),'-or','MarkerSize',3,'LineWidth',1);
    axis([-1 1 -9 9]);
    hT = text(-0.6, 7.5,txt); 
    hT = text(-0.6, 6.5,'epsilon = 0.05'); 
    xlabel('x \in (-1,1)'); 
    ylabel('u'); 
    drawnow;
%     pause(0.00001);
   % mov(n) = getframe; 
    
    
end

% Сохраняем видеофайл с презентацией результатов вычислений 
%movie2avi(mov, 'tz.avi', 'compression', 'None');

