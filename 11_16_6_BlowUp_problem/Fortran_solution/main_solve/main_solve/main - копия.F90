program main

	implicit none
	
	real(16) :: eps,k
	real(16) :: m_pov,p_pov ,L ,h ,tt ,tau ,a ,Aa ,pi ,var_x ,var_t
	integer :: s,M,N,INKAPS,INDICATE
	integer :: N_X, M_T, SGU,r_x,r_t,M_0 ,N_0
    INTEGER , allocatable :: I(:),J(:)
	real(16), allocatable :: X(:), T(:), u(:,:,:),X_0(:),T_0(:)
	real(16), allocatable :: f(:),MAT(:),U1(:)
	complex(16) i_unit, a_11
	!complex(16), allocatable :: a(:), b(:), c(:), w(:)
    
	!-----------------ЗАДАНИЕ НЕОБХОДИМЫХ КОНСТАНТ-----------------------------
	pi= 3.141592653589793238462643383279502q0
    i_unit = (0.0dq, 1.0dq)     ! Параметр, отвечающий за свойства схемы:
	a_11 = (1.0dq+i_unit)/2.0dq	! CROS1 - схема Розенброка с комплексным коэффициентом (p_t = 2, L2-устойчивость, монотонность ЕСТЬ)
	!a_11 = (0.5dq,0.dq)		! KN - схема Крэнака-Николсона - она же "схема с полусуммой" (p_t = 2, A-устойчивость, монотонности НЕТ)
	!a_11 = (1.0dq,0.dq)		! DIRK1 - обратная схема Эйлера - она же "чисто неявная" (p_t = 1, L1-устойчивость, монотонности ЕСТЬ)
	!a_11 = (0.0dq,0.dq)	    ! ERK1 - схема Эйлера (p_t = 1, устойчивости НЕТ, монотонности НЕТ)
    
	
    N_X = 5q0				    ! Задаём число интервалов сетки по пространству N_X
	M_T = 10q0					! Задаём число интервалов сетки по времени M_T
    tt=3q0
    L=5q0
    
    m_pov=1.0q0/2.0q0           !Степени нелинейной задачи
	p_pov=7.0q0/8.0q0
    a=3q0*pi/3.5q0             
	Aa=sin(3q0*a)**5q0+((-5q0)*m_pov*a*cos(3q0*a)*(sin(3*a)**4q0))**(1q0/(p_pov-m_pov+1q0))
    M_0=M_T
	N_0=N_X
    !------------------------------СГУЩЕНИЕ------------------------------------
    r_x=2q0    !ПАРАМЕТР СГУЩЕНИЯ по Х
    r_t=2q0    ! По t
    SGU=6q0      !       $$$$__КОЛИЧЕСТВО СГУЩЕНИЙ__$$$$
    var_x=1q0  !
    var_t=1q0  !
    !--------------------------------------------------------------------------
    !----------------------------САМА ПРОГРАММА--------------------------------
    ALLOCATE(u(0:M_T,0:N_X,0:SGU+1)) ! Первая компонента - число временных слоёв, вторая число узлов по координате
    ALLOCATE(X_0(0:N_0))
    ALLOCATE(T_0(0:M_0))
    INDICATE=0
    DO s = 0,SGU
    
        ALLOCATE(MAT(0:N_X-1))!ИЗМЕНЕНИЕ МАТРИЦЫ ДЛЯ СХЕМЫ РОЗЕНБРОКА
        Mat(N_X-1)=0q0
        tau=tt/M_T 
        H=L/N_X !Разбиение по Х 
        ALLOCATE(U1(0:N_X))
        ALLOCATE(X(0:N_X))
        U1 = 0q0!обнуляем вектор с решениями
        X = 0q0
        !Цикл Для получения начального приближения
        n=0q0 !счетчик
        DO k= 0,L,H
            X(n)=k!Вектор Х координаты
            IF (k<2q0) THEN
                U1(n)=Aa  
            ELSE
            U1(n)=Aa-(sin(a*(k-1q0))**5q0)!начальное приближение
            ENDIF
            n=n+1
        ENDDO
        
        N=0
        ALLOCATE(T(0:M_T))
        DO k=0,TT,TAU
            t(n)=k
            n=n+1
        ENDDO
        
        IF (0.eqv.INDICATE) THEN
        X_0 = X !Сохраняем базовую сетку по Х
        T_0=t   !Сохраняем базовую сетку по Т
        INDICATE=INDICATE+1
        ENDIF
 !--------------Помощь для отбора конечной сетки N_0 X M_0-----------------
        N=0 !счетчик
        ALLOCATE(I(0:N_0)) 
        ALLOCATE(J(0:M_0))
        DO M = 0,N_X,var_x  ! отбоР по Х
            I(n)=M 
            n=n+1
        ENDDO
        N=0 !счетчик
        DO M = 0,M_T,var_t  ! отбоР по t
            J(n)=M 
            n=n+1
        ENDDO
 !-------------------------------------------------------------------------
        DO M = 1,N_0+1 
            u(1,M,s) = U1(I(M))  !Задаю первый временной слой
        ENDDO 
        INKAPS=1 
 !-----------------------------Сама Схема-----------------------------------
        DO M = 0,M_T-1
               write(*,*)M
               write(*,*)s
!           F=Fuu(U1(1:N_X),N_X,m_pov,p_pov,h)
!           call Prepares_beforeNew_tridalgorithm(U1(1:N_X),a_11,tau,Mat,N_X,m_pov,p_pov,h)
!           call TridiagonalMatrixAlgorithm(w,N_X,a,b,c,f) ??
!           U1(1:N_X) = U1(1:N_X) + TAU*real(w)
            U1(1:N_X) = U1(1:N_X)+1
            


            U1(0) = ((4q0*U1(1)**m_pov-U1(2)**m_pov)/3q0)**(1.0q0/m_pov)
             
            
            N=M+1
            if (N.eq.J(INKAPS)) THEN
                DO N = 0,N_0 
                    u(INKAPS,N,s) = U1(I(N))  
                ENDDO 
            INKAPS=INKAPS+1
            ENDIF
        ENDDO    
!------------------------------конец схемы---------------------------------       
        
        var_x=var_x*r_x  !приращение для следующей итерации
        var_t=var_t*r_t 
        N_X=N_X*r_x  !Cгущение сеток Х и времени
        M_T=M_T*r_t 
        DEALLOCATE(MAT)
        DEALLOCATE(U1)
        DEALLOCATE(X)
        DEALLOCATE(T)
        DEALLOCATE(I)
        DEALLOCATE(J)
        !DEALLOCATE()
        !DEALLOCATE()
        
    ENDDO
    
    
    write(*,*)s
   pause
    
    
    
    
END
    