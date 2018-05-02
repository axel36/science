load('p_eff_ForParticularLayer_NEW.mat')
p_eff_New = p_eff_ForParticularLayer;
load('p_eff_ForParticularLayer_OLD.mat')
figure
close('all')
S=12;
S_new=6;
for m = 1:M_0+1
    %m=20;
    plot(x_0(2:N_0),x_0(2:N_0)*0 + 2,'-*k','MarkerSize',3);
    hold on;
    % Рисуем зависимость эффективного порядка точности от номера узла
    plot(x_0(1:N_0-1),p_eff_ForParticularLayer(S-2,1:N_0-1,m),'-sk','MarkerSize',5,'LineWidth',1);
    hold on;
    plot(x_0(1:N_0-1),p_eff_New(S_new-2,1:N_0-1,m),'-*r','MarkerSize',5,'LineWidth',1);
    
    hold off;
    xlabel('x');
    ylabel('p^{eff}');
    title('p^{eff} for particular time layer');
    axis([x_0(2) x_0(length(x_0)-1) -2 6]);
    txt2=num2str(t_0(m));
    txt3=num2str(S);
    txt1='time =';
    txt=[txt1 ' ' txt2 ] ;
    hT = text(0.3, 5,txt); 
    drawnow;
    t_0(m)
    pause(1);
end
%     %%Capture the plot as an image 
%     frame = getframe(gcf);
%     img =  frame2im(frame);
%     [img,cmap] = rgb2ind(img,256);
%     if m == 1
%         imwrite(img,cmap,'2met.gif','gif','LoopCount',Inf,'DelayTime',0.9);
%     else
%         imwrite(img,cmap,'2met.gif','gif','WriteMode','append','DelayTime',0.9);
%     end
% end