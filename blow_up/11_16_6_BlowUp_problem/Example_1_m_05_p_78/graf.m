
% colormap jet;
% %colormap hsv;
% surf(X_0(1:50),t_0(1:50),u(1:50,1:50,8));
% axis([0 4.99 0 3 1 2000]);
% xlabel('x');
% ylabel('time');
% zlabel('U');
%for n= 1:1:M_0+1
i=0;
for k = 1:51
    Line(k)= 2.12690914688488221246486374692134;
end
figure;
 for n = 1:M_0+1;
     %
    n
    i=i+1;
    txt2=num2str(t_0(n));
    txt1='t =';
    txt=[txt1 ' ' txt2] ;
    plot(X_0,Z,'--black','LineWidth',2);
    hold on;
    plot(X_0,u(n,:,8),'-or','MarkerSize',3,'LineWidth',1);
    axis([0 5 0 10]);
    hT = text(2, 5,txt); 
    %hT = text(-0.6, 6.5,'epsilon = 0.05'); 
    xlabel('x \in (0,5)'); 
    ylabel('u'); 
    drawnow;
   hold off;
   pause(0.05);
    mov(n) = getframe;
    
end

% Сохраняем видеофайл с презентацией результатов вычислений 
movie2avi(mov, 'tz.avi', 'compression', 'None');

