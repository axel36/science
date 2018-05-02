figure
%S=6;
for n=1:45
 
    txt2=num2str(t_0(n));
    txt1='t =';
    txt=[txt1 ' ' txt2] ;
    plot(X,Line,'--black','LineWidth',2);
    hold on;
    plot(X,u(n,:,s),'-or','MarkerSize',3,'LineWidth',1);
    hold on;
    %plot(x_0(1:N_0-1),p_eff_ForParticularLayer(S-2,1:N_0-1,n),'-sk','MarkerSize',5,'LineWidth',1);
    hold off
    axis([0 5 -2 8]);
   % hT = text(1, 6,txt); 
    %hT = text(-0.6, 6.5,'epsilon = 0.05'); 
    xlabel('x \in (0,5)'); 
    ylabel('u'); 
    drawnow;
    hold off;
    pause(0.1);
    mov(n) = getframe;
    
end
%movie2avi(mov,'myavifile.avi','Compression','Cinepak')
%figure
