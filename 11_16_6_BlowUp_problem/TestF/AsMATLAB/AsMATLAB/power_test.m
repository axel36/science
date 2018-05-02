ter=zeros(S-2,2);
for j=1:S-2
    n=0;
    for i=1:50
        if (p_eff_ForEveryLayer(j,i) < 0)
            ter(j,1)=ter(j)+ p_eff_ForEveryLayer(j,i);
            n=n+1;
        end
    end
    ter(j,1)=ter(j)/n;
    ter(j,2)=j+2;
end
ter