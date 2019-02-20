function [ a,b,c,d  ] = tet( q,w,e )
a(1)=0;
b(1)=0;
c(1)=0;
for i = 2:10
    a(i)=a(i-1)+q;
end
for i = 2:10
    b(i)=b(i-1)+w;
end
for i = 2:10
    c(i)=c(i-1)+e;
end
d=80;
end

