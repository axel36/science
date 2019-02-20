function [X] = TridiagonalMatrixAlgorithm(a,b,c,B)

% �������, ������� ��������� ����� �������� ��� �������
% �������� ������� A X = B

% �������������� ����� �� ����������� ����� MatLab'� �� ������
% http://www.mathworks.com/matlabcentral/fileexchange/40722-tridiag-m/content/tridiag.m
    
% ������� ���������:
% B - ����� ������ ����� ����� n (������� ��� ������)
% a, b, c - ������� ����� n, ���������� ������������ ���������� (b(1) � c(n) �� ������������)
%
%  [ a(1)  c(1)                                  ] [  X(1)  ]   [  B(1)  ]
%  [ b(2)  a(2)  c(2)                            ] [  X(2)  ]   [  B(2)  ]
%  [       b(3)  a(3)  c(3)                      ] [        ]   [        ]
%  [            ...   ...   ...                  ] [  ...   ] = [  ...   ]
%  [                    ...    ...    ...        ] [        ]   [        ]
%  [                        b(n-1) a(n-1) c(n-1) ] [ X(n-1) ]   [ B(n-1) ]
%  [                                 b(n)  a(n)  ] [  X(n)  ]   [  B(n)  ]
%
    
    n = length(B);
    v = zeros(n,1);   
    X = zeros(n,1);
    
    w = a(1);
    X(1) = B(1)/w;
    for i = 2:n
        v(i - 1) = c(i - 1)/w;
        w = a(i) - b(i)*v(i - 1);
        X(i) = (B(i) - b(i)*X(i - 1))/w;
    end
    for j = n-1:-1:1
        X(j) = X(j) - v(j)*X(j + 1);
    end

end