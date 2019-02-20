! Программа решения СЛАУ в общем виде
! Реализована последняя версия метода сопряжённых градиентов (от Калиткина Н.Н.)


program main

	integer N,M
	integer i,j
	real(16),allocatable :: A(:,:)
	real(16),allocatable :: b(:)
	real(16),allocatable :: x_inv(:)

	open(1,file = 'in.dat')
        ! Считываем число неизвестных N и уравнений M
		read(1,*)N
		read(1,*)M
	close(1)
	
	allocate(b(M))
	! Нулевой процесс считывает правую часть уравнения Ax=b
    open(2,file = 'bData.dat')
	    do j = 1,M
			read(2,*) b(j)
		enddo
	close(2)	
	
	! Задаём начальное приближение для x
	allocate(x_inv(N))
	do i = 1,N
		x_inv(i)=0.0q0
	enddo
	
	allocate(A(M,N))
	open(3,file = 'AData.dat')
	    do j = 1,M
	        do i = 1,N
		        read(3,*) A(j,i)
		    enddo
	    enddo
	close(3)
   	
    ! Вызов подпрограммы решения СЛАУ Ax=b
    ! (метод сопряжённых градиентов от Н.Н. Калиткина)
    call ConjugateGradientMethod(A,M,N,b,x_inv)
    
    open(4,file='Results.dat')
        do i = 1,N
	        write(4,*) x_inv(i)
		enddo
	close(4)

end


! Подпрограмма реализует метод сопряжённых градиентов для решения СЛАУ AZ=U
! Версия метода от Н.Н. Калиткина)
! Н.Н. Калиткин, Л.В. Кузьмина "Улучшенная форма метода сопряжённых градиентов" //
! Математическое моделирование, 2011, т. 23, н. 7, стр. 33-51
subroutine ConjugateGradientMethod(A,M,N,b,x)
    
    integer N,M
	integer i,j
	integer iter
	real(16) b(M)
	real(16) x(N)
	real(16),allocatable :: r(:),p(:),q(:),r_previous(:),p_previous(:),q_previous(:)
	real(16),allocatable :: r_temp(:),q_temp(:)
	real(16) A(M,N)
	real(16) ScalP
	
	allocate(r(N),p(N),q(N),r_previous(N),p_previous(N),q_previous(N))
	allocate(r_temp(M),q_temp(M))
	
	iter = 0
	do i = 1,N
	    p_previous(i) = 0.0q0
	enddo
	
	do while (iter < N)
	    iter = iter + 1
	    
	    ! Вычисление невязки r
	    if (iter == 1) then 
	        call MatrixVectorMultiplication(A,M,N,x,r_temp)
	        do j = 1,M
	            r_temp(j) = r_temp(j) - b(j)
	        enddo
	        call TransposedMatrixVectorMultiplication(A,M,N,r_temp,r)
	    else
	        ! При iter>=2 невязка вычисляется по рекурентной формуле
	        call ScalarProduct(ScalP,p_previous,q_previous,N)
	        do i = 1,N
	            r(i) = r_previous(i) - (1.0q0/ScalP)*q_previous(i)
	        enddo
	    endif
	    
	    ! Вычисление вспомогательного вектора p
	    call ScalarProduct(ScalP,r,r,N)
	    do i = 1,N
	        p(i) = p_previous(i) + (1.0q0/ScalP)*r(i)
	    enddo
	    
	    ! Вычисление вектора q (основная операция)
	    call MatrixVectorMultiplication(A,M,N,p,q_temp)
	    call TransposedMatrixVectorMultiplication(A,M,N,q_temp,q)
	    
	    ! Вычисление нового приближения для решения
	    call ScalarProduct(ScalP,p,q,N)
	    do i = 1,N
	        x(i) = x(i) - (1.0q0/ScalP)*p(i)
	    enddo
	
	    do i = 1,N
	        r_previous(i) = r(i)
	        p_previous(i) = p(i)
	        q_previous(i) = q(i)
	    enddo
	    
	enddo
	
	deallocate(r,p,q,r_previous,p_previous,q_previous)
	
end

! Подпрограмма, реализующая умножение матрицы A на вектор X (AX=B)
! (реализует самую вычислительно затратную часть метода)
subroutine MatrixVectorMultiplication(A,M,N,x,b)
    
    integer N,M
	integer i,j
	real(16) x(N),b(M)
	real(16) A(M,N)
        
    do j = 1,M
		b(j) = 0.0q0
		do i = 1,N
	        b(j) = b(j) + A(j,i)*x(i)
		enddo
	enddo
	        
end


! Подпрограмма, реализующая умножение транспонированной матрицы A (т.е. A') на вектор B (A'B=X)
! (реализует вторую из двух самых вычислительно затратных частей метода)
subroutine TransposedMatrixVectorMultiplication(A,M,N,b,x)
    
    integer N
	integer i,j
	real(16) x(N),b(M)
	real(16) A(M,N)
        
    do i = 1,N
		x(i) = 0.0q0
		do j = 1,M
	        x(i) = x(i) + A(j,i)*b(j)
		enddo
	enddo
	        
end


! Подпрограмма, реализующая скалярное произведение
subroutine ScalarProduct(SP,a,b,N)
    
    integer N
	integer i
	real(16) a(N),b(N)
	real(16) SP

    SP=0.0q0
    do i = 1,N
		SP = SP + a(i)*b(i)
	enddo	
	    
end