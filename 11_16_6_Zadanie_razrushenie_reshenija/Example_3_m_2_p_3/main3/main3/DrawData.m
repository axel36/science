figure;
close('all')
%% «ависимость Peff от времени
%–исуем зависимость теоретического пор€дка точности от номера узла
plot(t_0(2:M_0+1),t_0(2:M_0+1)*0 + p,'-*k','MarkerSize',3);
hold on;
plot(t_0(2:M_0+1),p_eff_ForEveryLayer(S-2,:),'-sk','MarkerSize',5,'LineWidth',1);
hold off;
xlabel('t');
ylabel('p^{eff}');
title('p^{eff} for each time layer');
axis([t_0(2) t_0(length(t_0)) -3 (p+2)]);

%% «ависимость Peff от х на каждом временном слое
h = figure;
Ma=max(max(p_eff_ForParticularLayer(S-2,:,:)));
Mi=min(min(p_eff_ForParticularLayer(S-2,:,:)));
%-------------------------------------------------------------------------------
for m = 1:M_0+1
    %m=20;
    plot(x_0(2:N_0),x_0(2:N_0)*0 + p,'-*k','MarkerSize',3);
    hold on;
    % –исуем зависимость эффективного пор€дка точности от номера узла
    plot(x_0(1:N_0),p_eff_ForParticularLayer(S-2,:,m),'-sk','MarkerSize',5,'LineWidth',1);
    hold off;
    xlabel('x');
    ylabel('p^{eff}');
    title('p^{eff} for particular time layer');
    axis([x_0(2) x_0(length(x_0)-1) Mi-2 Ma]);
    txt2=num2str(m-1);
    txt1='layer number =';
    txt=[txt1 ' ' txt2] ;
    hT = text(1, 3,txt); 
    drawnow;
    t_0(m)
    pause(0.05);
end 
    % Capture the plot as an image 
%     frame = getframe(gcf);
%     img =  frame2im(frame);
%     [img,cmap] = rgb2ind(img,256);
%     if m == 1
%         imwrite(img,cmap,'animation2.gif','gif','LoopCount',Inf,'DelayTime',0.1);
%     else
%         imwrite(img,cmap,'animation2.gif','gif','WriteMode','append','DelayTime',0.1);
%     end
%end
%-------------------------------------------------------------------------------
 %% построение u от N_0,M_0 и S    
% for s=1:S
% figure;
% for n=1:M
%  
%     txt2=num2str(n);
%     txt1='t =';
%     txt=[txt1 ' ' txt2] ;
%     plot(X,Line,'--black','LineWidth',2);
%     hold on;
%     plot(X,u(n,:,s),'-or','MarkerSize',3,'LineWidth',1);
%     axis([0 5 0 10]);
%     hT = text(2.5, 3,txt); 
%     %hT = text(-0.6, 6.5,'epsilon = 0.05'); 
%     xlabel('x \in (0,5)'); 
%     ylabel('u'); 
%     drawnow;
%    hold off;
%    pause(0.07);
%     %mov(n) = getframe;
%     
% end
% close('all');
% end
