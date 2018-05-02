figure;

for n= 1:1:M_0+1
% for n = 15;
    n
    txt2=num2str(t_0(n));
    txt1='t =';
    txt=[txt1 ' ' txt2] ;
    plot(X_0,u(n,:,9),'-or','MarkerSize',3,'LineWidth',1);
    axis([0 5 -9 90000000]);
    hT = text(1, 7.5,txt); 
    %hT = text(-0.6, 6.5,'epsilon = 0.05'); 
    xlabel('x \in (0,5)'); 
    ylabel('u'); 
    drawnow;
    pause(0.00001);
   % mov(n) = getframe; 
    
    
end

% Сохраняем видеофайл с презентацией результатов вычислений 
%movie2avi(mov, 'tz.avi', 'compression', 'None');

