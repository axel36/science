function [X] = TridiagonalMatrixAlgorithm(A,B)

% Функция, которая реализует метод прогонки для решения
% линейной системы A X = B

    % Надо обратить внимание на то, что массивы трёхдиагональных
    % коэффициентов выбираются из матрицы А, что не есть экономично
    % При наличии возможности необходимо в качестве входных параметров
    % передовать не матрицу А, а вектора a, b и c
    
    n = length(B);
    v = zeros(n,1);   
    X = zeros(n,1);
    
    a = zeros(1,n);
    b = zeros(1,n);
    c = zeros(1,n);
    
    for i = 1:n
        a(i) = A(i,i);
        if i > 1
            b(i) = A(i,i - 1);
        end
        if i < n
            c(i) = A(i,i + 1);
        end
    end
    
    % Подрпрограммка взята на официальном сайте MatLab'а по ссылке
    % http://www.mathworks.com/matlabcentral/fileexchange/40722-tridiag-m/content/tridiag.m
    
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