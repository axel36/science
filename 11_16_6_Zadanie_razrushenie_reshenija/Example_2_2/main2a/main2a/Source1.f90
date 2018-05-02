
program main
 use ieee_arithmetic
	implicit none

	real(16) :: eps,k,coef,coefF,VAR
	real(16) :: m_pov,p_pov ,L ,h ,tt ,tau ,a ,Aa ,pi ,var_x ,var_t
	integer :: s,M,N,INKAPS,INDICATE
	integer :: N_X, M_T, SGU, r_x,r_t,M_0 ,N_0,I,J,R
    INTEGER , allocatable :: II(:),JJ(:)
	real(16), allocatable :: X(:), T(:), u(:,:,:),X_0(:),T_0(:)
	real(16), allocatable :: F(:),MAT(:),U1(:),U_ARRAY(:,:,:,:)
	complex(16) :: i_unit, a_11
	complex(16), allocatable :: A_MAT(:), B_MAT(:), C_MAT(:), w_MAT(:),W(:)
	external pow
    real(16), allocatable:: p_eff_ForEveryLayer(:,:),p_eff_ForParticularLayer(:,:,:)
	!-----------------ЗАДАНИЕ НЕОБХОДИМЫХ КОНСТАНТ-----------------------------
	pi= 3.141592653589793238462643383279502q0
    i_unit = (0.0q0, 1.0q0)     ! Параметр, отвечающий за свойства схемы:
	a_11 = (1.0q0+i_unit)/2.0q0	! CROS1 - схема Розенброка с комплексным коэффициентом (p_t = 2, L2-устойчивость, монотонность ЕСТЬ)
	!a_11 = (0.5dq,0.dq)		! KN - схема Крэнака-Николсона - она же "схема с полусуммой" (p_t = 2, A-устойчивость, монотонности НЕТ)
	!a_11 = (1.0dq,0.dq)		! DIRK1 - обратная схема Эйлера - она же "чисто неявная" (p_t = 1, L1-устойчивость, монотонности ЕСТЬ)
	!a_11 = (0.0dq,0.dq)	    ! ERK1 - схема Эйлера (p_t = 1, устойчивости НЕТ, монотонности НЕТ)

    !N_X=N   M_X=M
    N_X = 50 !число интервалов по Х, которое меняется при сгущениях
	M_T = 50 !число интервалов по T, которое меняется при сгущениях
    tt=3q0
    L=5q0

    m_pov=3.0q0           !Степени нелинейной задачи
	p_pov=3.0q0           !константы иходного примера
    a=pi/4.0q0
	Aa=Qsin(3q0*a)**5q0+((-5q0)*m_pov*a*Qcos(3q0*a)*(Qsin(3*a)**4q0))**(1q0/(p_pov-m_pov+1q0))!!!!!!!!!!!!!!!!!!! QSIN
    M_0=M_T!кол-во отрезков несгущенной задачи                             
	N_0=N_X
    !------------------------------СГУЩЕНИЕ------------------------------------
    r_x=2q0    !ПАРАМЕТР СГУЩЕНИЯ по Х
    r_t=2q0    ! По t
    SGU=8      !       $$$$__КОЛИЧЕСТВО СГУЩЕНИЙ__$$$$
    var_x=1q0  !переменные для сгущения
    var_t=1q0  !
    !--------------------------------------------------------------------------
    !----------------------------САМА ПРОГРАММА--------------------------------
    ALLOCATE(u(1:M_0+1,1:N_0+1,1:SGU)) ! Первая компонента - число временных слоёв, вторая число узлов по координате, 3- число сгущений
    u=0.0q0
    ALLOCATE(X_0(1:N_0))!сетки несгущенной задачи   
    ALLOCATE(T_0(1:M_0))
    
    INDICATE=0
!Начало цикла    
    DO s = 1,SGU
        ALLOCATE(MAT(1:N_X))!ИЗМЕНЕНИЕ МАТРИЦЫ ДЛЯ СХЕМЫ РОЗЕНБРОКА(N-1 дифф уравнений и одно алгебраическое)
        DO n=1,N_X,1
            MAT(n)=1q0
        enddo
        MAT(N_X)=0.0q0
        tau=tt/M_T !изменение шага по времени
        H=L/N_X !Разбиение по Х
        ALLOCATE(U1(1:N_X+1))!массив с решениями на каждом сгущении
        ALLOCATE(X(1:N_X+1))
        U1 = 0.0q0!обнуляем массив с решениями
        X = 0.0q0
        !Цикл Для получения начального приближения
        n=0 !счетчик
        X(1)=0
        
        DO k= 1,N_x
	        X(k+1)=X(k)+H
        ENDDO



        DO k= 1,N_x+1
            
            IF (X(k)<2q0) THEN
                U1(k)=Aa
            ELSE
                U1(k)=Aa-(Qsin(a*(X(k)-1q0))**5q0)!начальное приближение
            ENDIF
        ENDDO
        
        ALLOCATE(T(1:M_T+1))
        N=0
        DO k=0,TT,TAU
            n=n+1
            t(n)=k
        ENDDO

        IF (0.eqv.INDICATE) THEN
        X_0 = X !Сохраняем базовую сетку по Х
        T_0=t   !Сохраняем базовую сетку по Т
        INDICATE=INDICATE+1
        ENDIF
 !--------------Помощь для отбора конечной сетки N_0 X M_0-----------------
        N=0 !счетчик
        ALLOCATE(II(1:N_X+1))
        ALLOCATE(JJ(1:M_T+1))
        DO M = 1,N_X+1,var_x  ! отбоР по Х
            n=n+1
            II(n)=M
        ENDDO
        N=0 !счетчик
        DO M = 1,M_T+1,var_t  ! отбоР по t
            n=n+1
            JJ(n)=M
        ENDDO
 !-------------------------------------------------------------------------
        DO M = 1,N_0+1 
            u(1,M,s) = U1(II(M))  !Задаю первый временной слой
        ENDDO 
        INKAPS=2 
 !-----------------------------Сама Схема-----------------------------------
        ALLOCATE(W(1:N_X))
        ALLOCATE(a_MAT(1:N_X))
        a_MAT=(0.0q0,0.0q0)
        ALLOCATE(B_MAT(1:N_X))
        b_MAT=(0.0q0,0.0q0)
        ALLOCATE(C_MAT(1:N_X))
        c_MAT=(0.0q0,0.0q0)
        ALLOCATE(F(1:N_X)) 
        DO M = 1,M_T
            write(*,*)M
            write(*,*)s
            
            CALL PREPARES_BEFORENEW_TRIDALGORITHM(a_MAT,b_MAT,c_MAT,F,U1(2:N_X+1),a_11,TAU,Mat,N_X,m_pov,p_pov,h)
            CALL TridiagonalMatrixAlgorithm(W,N_X+1,a_MAT,b_MAT,c_MAT,F)
            U1(2:N_X+1) = U1(2:N_X+1) + TAU*real(W)
            U1(1) = ((4q0*U1(2)**m_pov-U1(3)**m_pov)/3q0)**(1.0q0/m_pov)
            !N=M+1
            if ((M+1) .eq. JJ(INKAPS)) THEN
                DO N = 1,N_0+1
                    u(INKAPS,N,s) = U1(II(N))
                ENDDO
                INKAPS=INKAPS+1
            ENDIF
            
        ENDDO
!------------------------------конец схемы---------------------------------

        var_x = var_x*r_x  !приращение для следующей итерации
        var_t=var_t*r_t
        N_X=N_X*r_x  !Cгущение сеток Х и времени
        M_T=M_T*r_t
        DEALLOCATE(MAT)
        DEALLOCATE(U1)
        DEALLOCATE(X)
        DEALLOCATE(T)
        DEALLOCATE(II)
        DEALLOCATE(JJ)
        DEALLOCATE(W)
        DEALLOCATE(a_MAT)
        DEALLOCATE(B_MAT)
        DEALLOCATE(C_MAT)
        DEALLOCATE(F) 
             !DEALLOCATE()

    ENDDO
Allocate(p_eff_ForEveryLayer(1:SGU-2,1:M_0),p_eff_ForParticularLayer(1:SGU-2,1:N_0,1:M_0+1))

p_eff_ForEveryLayer=0.0q0
p_eff_ForParticularLayer=0.0q0
CALL TemplateForBlowUpDiagnostics(p_eff_ForEveryLayer,p_eff_ForParticularLayer,u,M_0,N_0,SGU)


    open(1,file = 'Results.dat')
    open(2,file = 'Results_P.dat')
    open(3,file = 'Results_testtestU.dat')
		write(1,*) N_0+1
		write(1,*) M_0+1
		write(1,*) SGU
		!write(1,*) x_left
		!write(1,*) x_right
		!write(1,*) T_calc
	    do s = 1,SGU
			do j = 1,M_0+1
				do i = 1,N_0+1
					write(1,*) u(j,i,s)
					
				enddo
			enddo
		enddo
		
		!write(1,*) SGU-2
		!write(1,*) N_0
		do s= 1,SGU-2
		    do i = 1,M_0
		        write(1,*) p_eff_ForEveryLayer(s,i)
		    enddo
	    enddo
	    
	    !write(1,*) SGU-2
		!write(1,*) M_0
		do s= 1,SGU-2
		    do i = 1,N_0
				DO j = 1,M_0+1
				    if ( ieee_is_finite((p_eff_ForParticularLayer(s,i,j))))  then
					write(2,*) p_eff_ForParticularLayer(s,i,j)
					else
					write(2,*) "Inf"
					endif
					
				enddo
		    enddo
	    enddo
		
		do s= 1,SGU-2
		    do i = 1,N_0
				DO j = 1,M_0+1
				    write(3,*) p_eff_ForParticularLayer(s,i,j)
					
				enddo
		    enddo
	    enddo
		
		
	close(1)
	close(3)
    close(2)
    !pause
    




END  !ДЛЯ Всей программы

!CALL TridiagonalMatrixAlgorithm(W,n,F,G,H,d)

subroutine TridiagonalMatrixAlgorithm(x,N,a,b,c,d)

	implicit none

	real(16) :: d(N-1)
	complex(16)  :: a(N-1), b(N-1), c(N-1), v(N-1), x(N-1)
	complex(16)  :: w
	integer :: n
	integer :: i,j

	! Функция, которая реализует метод прогонки для решения линейной системы A X = B

	! Входные параметры:
	! d - ветор правой части длины n (столбец или строка)
	! a, b, c - вектора длины n, содержащие коэффициенты диагоналей (b(1) и c(n) не используются)
	!
	!  [ a(1)  c(1)                                  ] [  x(1)  ]   [  d(1)  ]
	!  [ b(2)  a(2)  c(2)                            ] [  x(2)  ]   [  d(2)  ]
	!  [       b(3)  a(3)  c(3)                      ] [        ]   [        ]
	!  [            ...   ...   ...                  ] [  ...   ] = [  ...   ]
	!  [                    ...    ...    ...        ] [        ]   [        ]
	!  [                        b(N-2) a(N-2) c(N-2) ] [ x(N-2) ]   [ d(N-2) ]
	!  [                               b(N-1) a(N-1) ] [ x(N-1) ]   [ d(N-1) ]

    v = (0.d0,0.d0)
    X = (0.d0,0.d0)

    w = a(1)
    x(1) = d(1)/w
    do i = 2,N-1
        v(i - 1) = c(i - 1)/w
        w = a(i) - b(i)*v(i - 1)
        x(i) = (d(i) - b(i)*x(i - 1))/w
    enddo
    do j = N-2,1,-1
        x(j) = x(j) - v(j)*x(j + 1)
    enddo

end
!-----------------------------END_OF_TMA---------------------------------------
subroutine PREPARES_BEFOReNEW_TRIDALGORITHM(A,B,C,F,U,a_11,TAU,Mat,N_X,m_pov,p_pov,h)

    COMPLEX(16) :: A(1:N_X),B(1:N_X),C(1:N_X)
    COMPLEX(16) :: coef,coeff,a_11
    REAL(16) :: TAU,MAT(1:N_X),U(1:N_X),m_pov,P_pov,h,F(1:N_X)
    integer :: N_X
    COMPLEX(16) :: N_Xx,U_I_PLS,U_I,U_I_MNS,U_one,U_two, U_Nx, U_Nx_mn1,U_Nx_mn2
    integer :: i,j
    external pow

    
    
!------------------PREPARES_BEFORE_NEW_TRIDALGORITHM---------------------------
!PREPARES_BEFORENEW_TRIDALGORITHM Summary of this function goes here
! Функция, которая подготавливает массивы, которые содержат диагонали матрицы решаемой системы ОДУ
! Данная матрица имеет вид [M - (1+1i)/2*tau*f_u] и является трёхдиагональной

! Входными параметрами являются:
! U1 - решение системы ОДУ в текущий момент времени (кроме граничных узлов)
!  tau - текущий шаг по времени
! a_11 - комплексный параметр схемы Розенброка -(1+1i)/2
! Маt - вспомогательный параметр (вектор аналог матрице М)
! Выходные параметры:
! a, b и c - соответствующие вектора
!
!  [ a(1)  c(1)                           ]
!  [ b(2)  a(2)  c(2)                     ]
!  [       b(3)  a(3)  c(3)               ]
!  [            ...   ...   ...           ]
!  [                    ...    ...  ...   ]
!  [                          b(N)  a(N)  ]
!-------------------------------------------------------------------------!


			!ALLOCATE(A(1:N_X-1))
            !ALLOCATE(B(1:N_X-1))
            !ALLOCATE(C(1:N_X-1))
            !ALLOCATE(F(1:N_X-1))
            
            U_one=CMPLX(U(1) , 0.0q0)
            U_two=CMPLX(U(2) , 0.0q0) 
            U_Nx=CMPLX(U(N_X) , 0.0q0)
            U_Nx_mn1=CMPLX(U(N_X-1) , 0.0q0)          
            U_Nx_mn2=CMPLX(U(N_X-2) , 0.0q0)
            


            A(1) =Mat(1)-a_11*TAU*((-2q0*m_pov*pow( U_one,m_pov-1q0)) /(3q0*h*h)) 
			c(1)=0-a_11*TAU*((2*m_pov*pow(U_two,(m_pov-1)))/(3*h*h)) 
            a(N_X)=Mat(N_X)-a_11*TAU*((p_pov*pow(U_Nx,(p_pov-1q0)))-(3q0*m_pov*pow(U_Nx,(m_pov-1q0)))/(2q0*h)) 
            b(N_X)=0q0-a_11*TAU*((2q0*m_pov*pow(U_Nx_mn1,(m_pov-1q0)))/(h)) 
                    
            f(1)=(pow(U_two,m_pov)-2*pow(U_one,m_pov)+((4*pow(U_one,m_pov)-pow(U_two,m_pov)))/3)/(h*h) 
            f(N_X)=pow(U_Nx,p_pov)-(3*pow(U_Nx,m_pov)-4*pow(U_Nx_mn1,m_pov)+pow(U_Nx_mn2,m_pov))/(2*h) 
            
            
            f(1)=f(1)
            
            !A(1) =Mat(1)-a_11*TAU*((-2q0*m_pov*(1/cqsqrt((1.0,0.0)*U(1))))/(3q0*h*h)) 
			!c(1)=0-a_11*TAU*((2*m_pov*U(2)**(m_pov-1))/(3*h*h)) 
            !a(N_X)=Mat(N_X)-a_11*TAU*((p_pov*U(N_X)**(p_pov-1q0))-(3q0*m_pov*U(N_X)**(m_pov-1q0))/(2q0*h)) 
            !b(N_X)=0q0-a_11*TAU*((2q0*m_pov*U(N_X-1q0)**(m_pov-1q0))/(h)) 
                    
            !f(1)=(U(2)**m_pov-2*U(1)**m_pov+((4*U(1)**m_pov-U(2)**m_pov))/3)/(h*h) 
            !f(N_X)=U(N_X)**p_pov-(3*U(N_X)**m_pov-4*U(N_X-1)**m_pov+U(N_X-2)**m_pov)/(2*h) 
            
            
                   
            
            !print *,"N_Xx" 
            !print *, N_Xx
            !print *,"end"
                        
            
           
            DO I= 2,N_X-1,1
                U_I_PLS=CMPLX(U(i+1) , 0.0q0)
                U_I=CMPLX(U(i) , 0.0q0)
                U_I_MNS=CMPLX(U(i-1) , 0.0q0)
                
                !a(i)= Mat(i)-a_11*TAU*((-m_pov*2q0*(1/cqsqrt((1.0,0.0)*U(i))))/(h*h))
                a(i)= Mat(i)-a_11*TAU*((-m_pov*2q0*pow(U_I,(m_pov-1q0)))/(h*h))
                
                !b(i)= 0q0-a_11*TAU*((m_pov*(1/cqsqrt((1.0,0.0)*U(i-1))))/(h*h))
                b(i)= 0q0-a_11*TAU*((m_pov*pow(U_I_MNS,(m_pov-1q0)))/(h*h)) 
                
                !c(i)= 0q0-a_11*TAU*((m_pov*(1/cqsqrt((1.0,0.0)*U(i+1))))/(h*h))                
                c(i)= 0q0-a_11*TAU*((m_pov*pow(U_I_PLS,(m_pov-1q0)))/(h*h)) 
                
                !f(i)=((cqsqrt((1.0,0.0)*U(i+1)))-2*(cqsqrt((1.0,0.0)*U(i)))+(cqsqrt((1.0,0.0)*U(i-1))))/(h*h)
                f(i)=(pow(U_I_PLS,m_pov)-2*pow(U_I,m_pov)+pow(U_I_MNS,m_pov))/(h*h)
                
            ENDDO
           
            !coef =0-a_11*TAU*((-1)*(m_pov*(1/cqsqrt((1.0,0.0)*U_Nx_mn2)))/(2*h))
            coef =0-a_11*TAU*((-1)*(m_pov*pow(U_Nx_mn2,(m_pov-1)))/(2*h))
            coeff= b(N_X-1)/coef 

            b(N_X)=a(N_X-1)-coeff*b(N_X) 
            a(N_X)=c(N_X-1)-coeff*a(N_X) 
            f(N_X)=f(N_X-1)-coeff*f(N_X) 


END
!------------END OF PREPARES_BEFORENEW_TRIDALGORITHM-----------


!------------------ТЕПЕРЬ TemplateForBlowUpDiagnostics------------------------------

subroutine TemplateForBlowUpDiagnostics(p_eff_ForEveryLayer,p_eff_ForParticularLayer,U,M_0,N_0,SGU)

!implicit none

integer:: P,M,M_0,S,SGU,N_0,N,L,I,J,Q
real(16):: U_ARRAY(1:SGU,1:SGU,1:M_0 + 1,1:N_0 + 1)
real(16):: u(1:M_0+1,1:N_0+1,1:SGU),R_rN,R_N,R
real(16):: p_eff_ForEveryLayer(1:SGU-2,1:M_0),p_eff_ForParticularLayer(1:SGU-2,1:N_0,1:M_0+1)


!-------------------------ВВОДНАЯ ЧАСТЬ---------------------------------
    !ALLOCATE(U_ARRAY(1:SGU,1:SGU,1:M_0 + 1,1:N_0 + 1))
    P = 2; ! Теоретический порядок точности схемы
    Q = 1;
    R = 2; ! Коэффициент сгущения сетки
    DO S = 1,SGU
        DO M = 1,M_0+1
            DO N = 1,N_0+1
                U_ARRAY(S,1,M,N) = u(M,N,S)
            ENDDO
        ENDDO
    ENDDO        
!---------------Рекурентно уточняем решение по Ричардсону-------------------
    L=0
    DO L = 2,SGU
        DO S = L,SGU
            DO I = 1,M_0 + 1
                DO J = 1,N_0 + 1
                    U_ARRAY(s,l,i,j) = U_ARRAY(s,l-1,i,j) + (U_ARRAY(s,l-1,i,j) - U_ARRAY(s-1,l-1,i,j))/(r**(p + q*((l - 1) - 1)) - 1);
                ENDDO
            ENDDO
        ENDDO
    ENDDO
! Вычисляем эффективный порядок точности вычисления решения на каждом временном слое
! m >= 2 (кроме первого, так как на нём решение задано точно)
    !ALLOCATE(p_eff_ForEveryLayer(1:SGU-2,1:M_0))
    
    DO M = 2,(M_0 + 1)
        DO s = 3,SGU
            !TEMP_RN=(u_array(s,2,m,:)-u_array(s,1,m,:)
            !TEMP_N=(u_array(s-1,2,m,:)-u_array(s-1,1,m,:)
            R_rN = Qsqrt(sum((u_array(s,2,m,:)-u_array(s,1,m,:))*(u_array(s,2,m,:)-u_array(s,1,m,:))));
            R_N = Qsqrt(sum((u_array(s-1,2,m,:)-u_array(s-1,1,m,:))*(u_array(s-1,2,m,:)-u_array(s-1,1,m,:))));
            p_eff_ForEveryLayer(s-2,m - 1)= ((Qlog(Qabs(R_N)))-(Qlog(Qabs(R_rN))))/(Qlog(r));
        ENDDO
    ENDDO

! Вычисляем эффективный порядок точности (во всех пространственных точках кроме граничных) 
! Вычисляем решение на определённом временном слое с номером m
    !ALLOCATE(p_eff_ForParticularLayer(1:SGU-2,1:N_0)) 


	
	Do m = 1,M_0+1
    
    ! Вычисляем эффективный порядок точности (во всех пространственных точках кроме граничных) 
    ! на определённом временном слое с номером m
		!p_eff_ForParticularLayer = zeros(S-2,N_0 - 1);
		DO n = 2,N_0
			DO s = 3,SGU
				R_rN = Qabs(u_array(s,2,m,n)-u_array(s,1,m,n));
				R_N = Qabs(u_array(s-1,2,m,n)-u_array(s-1,1,m,n));
                p_eff_ForParticularLayer(s-2,n - 1,M)= ((Qlog(R_N))-(Qlog(R_rN)))/(Qlog(r));
			ENDDO
		ENDDO
	ENDDO
	
	
	
END

!---------------Complex Power function--------------------------

 complex (16) FUNCTION pow(Z, x) 
    
    REAL(16):: b,a,x,a_new,b_new
    complex (16):: Z
    A=Qreal(Z)
    B=QIMAG(Z)
    pi= 3.141592653589793238462643383279502q0
    a_new=0
    b_new=0
    if ((a<0).and.(b>=0)) then
        a_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qcos(x*(Qatan(b/a)+pi))
        b_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qsin(x*(Qatan(b/a)+pi))    
    elseif ((a<0).and.(b<=0))then
        a_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qcos(x*(Qatan(b/a)-pi))
        b_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qsin(x*(Qatan(b/a)-pi))
    elseif ((a==0).and.(b==0))then
        a_new=0
        b_new=0
    else
        a_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qcos(x*Qatan(b/a))
        b_new=Qexp(x*Qlog(Qsqrt(a**2+b**2)))*Qsin(x*Qatan(b/a))
    

    endif
   !f=-(b_new)
        !pow = CMPLX(b_new,a_new)
        pow = qCMPLX(a_new,b_new)
        
    RETURN
    
 END FUNCTION
