Z=csvread('Panin_2_2__Results_ERRR_11.dat');
S=11;
N=51;
M=51;
RRR=zeros(S,N,M);
n=1;
S=11;
for s= 1:S-2
    for i=1:N
        for j=1:M
            RRR(s,i,j)=Z(n);
            n=n+1;
        end
    end
end