figure
for S = 8:12

hold on;
plot(x_0(2:N_0),x_0(2:N_0)*0 + p,'-*k','MarkerSize',3);

plot(t_0(2:M_0+1),p_eff_ForEveryLayer(S-2,:),'-*r','MarkerSize',5,'LineWidth',1);
hold off;
xlabel('t');
txt3=num2str(S);
hT = text(1, 3,txt3);
ylabel('p^{eff}');
title('p^{eff} for each time layer');
axis([t_0(2) t_0(length(t_0)) -3 (p+2)]);

end