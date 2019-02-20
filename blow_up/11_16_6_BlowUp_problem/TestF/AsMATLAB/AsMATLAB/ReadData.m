clear;
clc;
%close('all')
Z=csvread('Panin_3_12u.dat');
PP=csvread('Panin_3_12p.dat');
%% Константы
L=5;
tt=0.1;%Время
%-----------------------------------------------------------------------------
N=Z(1);
M=Z(2);
S=Z(3);
%-----------------------------------------------------------------------------
M_0=M-1;
N_0=N-1;
p = 2;
%-----------------------------------------------------------------------------
h=L/(N-1);%Разбиение по Х 
tau=tt/(M_0);%Разбиение по времени 
N
%% Сетки
for k = 1:N
    Line(k)= 1.355;
end
n=0;
for k=0:tau:tt
    n=n+1;
    t_0(n)=k;
end
n=0;
for k=0:h:L
    n=n+1;
    X(n)=k;
end
x_0=X;
%% Считывание данных
n=4;
u=zeros(N,M,S);
for s= 1:S
    for j=1:M
        for i=1:N
            u(j,i,s)=Z(n);
            n=n+1;
        end
    end
end
n=S*M*N+4;
for s=1:S-2
	for i=1:M-1
		p_eff_ForEveryLayer(s,i)=Z(n);
		n=n+1;
	end
end
n=1;
for s=1:S-2
	for i=1:N-2
		for j=1:M
			p_eff_ForParticularLayer(s,i,j)=PP(n);
			n=n+1;
		end
	end
end
%S=10;
%save('p_eff_ForParticularLayer_OLD','p_eff_ForParticularLayer','x_0','t_0','M_0',)
DrawData;