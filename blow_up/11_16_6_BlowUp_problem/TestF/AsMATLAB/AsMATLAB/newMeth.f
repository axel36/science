
program main
 use ieee_arithmetic
	implicit none

	REAL*16 :: eps,k,coef,coefF,VAR,m_real
	REAL*16 :: m_pov,p_pov ,L ,H ,TIME ,TAU ,a ,Aa ,pi ,var_x ,var_t
	INTEGER :: s,M,N,INKAPS,INDICATE
	INTEGER :: N_X, M_T, SGU, r_x,r_t,M_0 ,N_0,I,J,R
    INTEGER , allocatable :: II(:),JJ(:)
	REAL*16, allocatable :: X(:), T(:), U(:,:,:),X_0(:),T_0(:),R_R(:,:,:)
	REAL*16, allocatable :: F(:),MAT(:),U1(:),U_ARRAY(:,:,:,:)
	COMPLEX*32  :: i_unit, a_11
	COMPLEX*32 , allocatable :: A_MAT(:), B_MAT(:), C_MAT(:), w_MAT(:),W(:)
    REAL*16, allocatable:: p_eff_ForEveryLayer(:,:),p_eff_ForParticularLayer(:,:,:)
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
    TIME=0.05q0 !да, так нужно ._. 
    !Panin 3 : m=3, p = 5, L =5, TIME = 0.05
    L=5.0q0
    m_pov=3.0q0           !Степени нелинейной задачи
	p_pov=5.0q0           !константы иходного примера
    a=pi/4.0q0
	Aa=qsin(3q0*a)**5q0+((-5q0)*m_pov*a*qcos(3q0*a)*(qsin(3*a)**4q0))**(1q0/(p_pov-m_pov+1q0))
    M_0=M_T !кол-во отрезков несгущенной задачи                             
	N_0=N_X
    !------------------------------СГУЩЕНИЕ------------------------------------
    r_x=2q0    !ПАРАМЕТР СГУЩЕНИЯ по Х
    r_t=2q0    ! По T
    SGU=10      !       $$$$__КОЛИЧЕСТВО СГУЩЕНИЙ__$$$$
    var_x=1q0  !переменные для сгущения
    var_t=1q0  !
    !--------------------------------------------------------------------------
    !----------------------------САМА ПРОГРАММА--------------------------------
    ALLOCATE(U(1:M_0+1,1:N_0+1,1:SGU)) ! Первая компонента - число временных слоёв, вторая число узлов по координате, 3- число сгущений
    U=0.0q0
	ALLOCATE(X_0(1:N_0))!сетки несгущенной задачи   
    ALLOCATE(T_0(1:M_0))
    INDICATE=0
	
    !------------------------------Начало цикла    
    DO s = 1,SGU
        ALLOCATE(MAT(1:N_X))!ИЗМЕНЕНИЕ МАТРИЦЫ ДЛЯ СХЕМЫ РОЗЕНБРОКА(N-1 дифф уравнений и одно алгебраическое)
        DO N=1,N_X,1
            MAT(N)=1q0
        ENDDO
        MAT(N_X)=0.0q0
		
        TAU=TIME/M_T !Разбиение по времени
        H=L/N_X !Разбиение по Х
		
        ALLOCATE(U1(1:N_X+1))!массив с решениями на каждом сгущении
		U1 = 0.0q0!обнуляем массив с решениями
        ALLOCATE(X(1:N_X+1))
		X = 0.0q0
        
        N=0 !счетчик
        X(1)=0.0q0
        DO N= 1,N_x
	        X(N+1)=X(N)+H     
        ENDDO

        DO N= 1,N_x+1
            IF (X(N)<2q0) THEN
                U1(N)=Aa
            ELSE
                U1(N)=Aa-(qsin(a*(X(N)-1q0))**5q0)!начальное приближение
            ENDIF
        ENDDO
        
        ALLOCATE(T(1:M_T+1))
        T(1)=0
        DO N=2,M_T+1
            T(N)=T(N-1)+TAU
        ENDDO

        IF (0.eqv.INDICATE) THEN
        X_0 = X !Сохраняем базовую сетку по Х
        T_0=T   !Сохраняем базовую сетку по Т
        INDICATE=INDICATE+1
        ENDIF
 !--------------Помощь для отбора конечной сетки N_0 X M_0-----------------
        N=0 !счетчик
        ALLOCATE(II(1:N_X+1))
        ALLOCATE(JJ(1:M_T+1))
        DO M = 1,N_X+1,var_x  ! отбоР по Х
            N=N+1
            II(N)=M
        ENDDO
        N=0 !счетчик
        DO M = 1,M_T+1,var_t  ! отбоР по T
            N=N+1
            JJ(N)=M
        ENDDO
 !-------------------------------------------------------------------------
        DO N = 1,N_0+1 
            U(1,N,s) = U1((N - 1)*(r_x**(s - 1)) + 1)  !Задаю первый временной слой
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
            M_real=real(M)/real(M_t)
            write(*,* )M_real
            write(*,*)" "
            write(*,*)s
        !110 format (F16.3)

            CALL PREPARES_BEFORENEW_TRIDALGORITHM(a_MAT,b_MAT,c_MAT,F,U1(2:N_X+1),a_11,TAU,Mat,N_X,m_pov,p_pov,H)
            CALL TridiagonalMatrixAlgorithm(W,N_X+1,a_MAT,b_MAT,c_MAT,F)
			
            U1(2:N_X+1) = U1(2:N_X+1) + TAU*REAL(W)
            U1(1) = ((4q0*U1(2)**m_pov-U1(3)**m_pov)/3q0)**(1.0q0/m_pov)

            
            if ((M+1) .eq. ((INKAPS - 1)*(r_t**(s - 1)) + 1)) THEN
                DO N = 1,N_0+1
                    U(INKAPS,N,s) = U1((n - 1)*(r_x**(s - 1)) + 1)
                    !write(*,*)(n - 1)*(r_x**(s - 1)) + 1
                ENDDO
                !write(*,*)"time"
                INKAPS=INKAPS+1
                !write(*,*)M+1
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
     ENDDO
	 
	ALLOCATE(p_eff_ForEveryLayer(1:SGU-2,1:M_0),p_eff_ForParticularLayer(1:SGU-2,1:N_0-1,1:M_0+1),R_R(1:SGU-2,1:N_0+1,1:M_0+1))
	p_eff_ForEveryLayer=0.0q0
	p_eff_ForParticularLayer=0.0q0
	
	CALL TemplateForBlowUpDiagnostics(p_eff_ForEveryLayer,p_eff_ForParticularLayer,U(1:M_0+1,1:N_0+1,1:SGU),M_0,N_0,SGU,R_R)

    open(1,file = 'PaninNM_next_05_3_5_10u.dat')
    open(2,file = 'PaninNM_next_05_3_5_10p.dat')
	open(3,file = 'Panin_3_Results_ERRR_12.dat')
	
   	write(1,*) N_0+1
	write(1,*) M_0+1
	write(1,*) SGU
	
	DO s = 1,SGU
		DO j = 1,M_0+1
			DO i = 1,N_0+1
				write(1,*) U(j,i,s)
			ENDDO
		ENDDO
	ENDDO
		
	DO s= 1,SGU-2
	    DO i = 1,M_0
	        write(1,*) p_eff_ForEveryLayer(s,i)
	    ENDDO
	ENDDO
	    
	DO s= 1,SGU-2
	    DO i = 1,N_0-1
			DO j = 1,M_0+1
			    write(2,*) p_eff_ForParticularLayer(s,i,j)
			ENDDO
	    ENDDO
	ENDDO
	DO s= 1,SGU-2
	    DO i = 1,N_0+1
			DO j = 1,M_0+1
			    write(3,*) R_R(s,i,j)
			ENDDO
	    ENDDO
	ENDDO
	close(1)
	close(2)
    close(3)
END  !ДЛЯ Всей программы

subroutine TridiagonalMatrixAlgorithm(x,N,a,b,c,d)
	implicit none
	REAL*16 :: d(N-1)
	COMPLEX*32   :: a(N-1), b(N-1), c(N-1), v(N-1), x(N-1)
	COMPLEX*32   :: w
	INTEGER :: N
	INTEGER :: i,j
	! Функция, которая реализует метод прогонки для решения линейной системы A X = B
	! Входные параметры:
	! d - ветор правой части длины N (столбец или строка)
	! a, b, c - вектора длины N, содержащие коэффициенты диагоналей (b(1) и c(N) не используются)
	!
	!  [ a(1)  c(1)                                  ] [  x(1)  ]   [  d(1)  ]
	!  [ b(2)  a(2)  c(2)                            ] [  x(2)  ]   [  d(2)  ]
	!  [       b(3)  a(3)  c(3)                      ] [        ]   [        ]
	!  [            ...   ...   ...                  ] [  ...   ] = [  ...   ]
	!  [                    ...    ...    ...        ] [        ]   [        ]
	!  [                        b(N-2) a(N-2) c(N-2) ] [ x(N-2) ]   [ d(N-2) ]
	!  [                               b(N-1) a(N-1) ] [ x(N-1) ]   [ d(N-1) ]
    v = (0.q0,0.q0)
    X = (0.q0,0.q0)
    w = a(1)
    x(1) = d(1)/w
    DO i = 2,N-1
        v(i - 1) = c(i - 1)/w
        w = a(i) - b(i)*v(i - 1)
        x(i) = (d(i) - b(i)*x(i - 1))/w
    ENDDO
    DO j = N-2,1,-1
        x(j) = x(j) - v(j)*x(j + 1)
    ENDDO
END
!-----------------------------END_OF_TMA---------------------------------------
subroutine PREPARES_BEFOReNEW_TRIDALGORITHM(A,B,C,F,U,a_11,TAU,Mat,N_X,m_pov,p_pov,H)
    COMPLEX*32  :: A(1:N_X),B(1:N_X),C(1:N_X)
    COMPLEX*32  :: coef,coeff,a_11
    REAL*16 :: TAU,MAT(1:N_X),U(1:N_X),m_pov,p_pov,H,F(1:N_X)
    INTEGER :: N_X
    COMPLEX*32  :: N_Xx,U_I_PLS,U_I,U_I_MNS,U_one,U_two, U_Nx, U_Nx_mn1,U_Nx_mn2
    INTEGER :: i,j
!------------------PREPARES_BEFORE_NEW_TRIDALGORITHM---------------------------
!PREPARES_BEFORENEW_TRIDALGORITHM Summary of this function goes here
! Функция, которая подготавливает массивы, которые содержат диагонали матрицы решаемой системы ОДУ
! Данная матрица имеет вид [M - (1+1i)/2*TAU*f_u] и является трёхдиагональной

! Входными параметрами являются:
! U1 - решение системы ОДУ в текущий момент времени (кроме граничных узлов)
!  TAU - текущий шаг по времени
! a_11 - комплексный параметр схемы Розенброка -(1+1i)/2
! Маt - вспомогательный параметр (вектор аналог матрице М)
! Выходные параметры:
! a, B и c - соответствующие вектора
!
!  [ a(1)  c(1)                           ]
!  [ B(2)  a(2)  c(2)                     ]
!  [       B(3)  a(3)  c(3)               ]
!  [            ...   ...   ...           ]
!  [                    ...    ...  ...   ]
!  [                          B(N)  a(N)  ]
!-------------------------------------------------------------------------!
    U_one=qCMPLX(U(1) , 0.0q0)
    U_two=qCMPLX(U(2) , 0.0q0) 
    U_Nx=qCMPLX(U(N_X) , 0.0q0)
    U_Nx_mn1=qCMPLX(U(N_X-1) , 0.0q0)          
    U_Nx_mn2=qCMPLX(U(N_X-2) , 0.0q0)
            
    A(1) =Mat(1)-a_11*TAU*((-2q0*m_pov*( U_one**(m_pov-1q0))) /(3q0*H*H)) 
	C(1)=0-a_11*TAU*((2.0q0*m_pov*(U_two**(m_pov-1)))/(3.0q0*H*H)) 
    A(N_X)=Mat(N_X)-a_11*TAU*((p_pov*(U_Nx**(p_pov-1q0)))-(3q0*m_pov*(U_Nx**(m_pov-1q0)))/(2q0*H)) 
    B(N_X)=0q0-a_11*TAU*((2q0*m_pov*(U_Nx_mn1**(m_pov-1q0)))/(H)) 
              
    F(1)=((U_two**m_pov)-2.0q0*(U_one**m_pov)+((4.0q0*(U_one**m_pov)-(U_two**m_pov)))/3.0q0)/(H*H) 
    F(N_X)=(U_Nx**p_pov)-(3.0q0*(U_Nx**m_pov)-4.0q0*(U_Nx_mn1**m_pov)+(U_Nx_mn2**m_pov))/(2.0q0*H) 
         
	DO I= 2,N_X-1,1
        U_I_PLS=qCMPLX(U(i+1) , 0.0q0)
        U_I=qCMPLX(U(i) , 0.0q0)
        U_I_MNS=qCMPLX(U(i-1) , 0.0q0)
		!A(i)= Mat(i)-a_11*TAU*((-m_pov*2d0*(1/cqsqrt((1.0,0.0)*U(i))))/(H*H))
        A(i)= Mat(i)-a_11*TAU*((-m_pov*2q0*(U_I**(m_pov-1q0)))/(H*H))
              
        !B(i)= 0d0-a_11*TAU*((m_pov*(1/cqsqrt((1.0,0.0)*U(i-1))))/(H*H))
        B(i)= 0q0-a_11*TAU*((m_pov*(U_I_MNS**(m_pov-1q0)))/(H*H)) 
                
        !C(i)= 0d0-a_11*TAU*((m_pov*(1/cqsqrt((1.0,0.0)*U(i+1))))/(H*H))                
        C(i)= 0q0-a_11*TAU*((m_pov*(U_I_PLS**(m_pov-1q0)))/(H*H)) 
                
        !F(i)=((cqsqrt((1.0,0.0)*U(i+1)))-2*(cqsqrt((1.0,0.0)*U(i)))+(cqsqrt((1.0,0.0)*U(i-1))))/(H*H)
        F(i)=((U_I_PLS**m_pov)-2.0q0*(U_I**m_pov)+(U_I_MNS**m_pov))/(H*H)
    ENDDO
    !coef =0-a_11*TAU*((-1)*(m_pov*(1/cqsqrt((1.0,0.0)*U_Nx_mn2)))/(2*H))
    coef =0-a_11*TAU*((-1.0q0)*(m_pov*(U_Nx_mn2**(m_pov-1)))/(2.0q0*H))
    coeff= B(N_X-1)/coef 
    B(N_X)=A(N_X-1)-coeff*B(N_X) 
    A(N_X)=C(N_X-1)-coeff*A(N_X) 
    F(N_X)=F(N_X-1)-coeff*F(N_X) 
END
!------------END OF PREPARES_BEFORENEW_TRIDALGORITHM-----------
!------------------ТЕПЕРЬ TemplateForBlowUpDiagnostics------------------------------
subroutine TemplateForBlowUpDiagnostics(p_eff_ForEveryLayer,p_eff_ForParticularLayer,U,M_0,N_0,SGU,R_R)
	INTEGER:: P,M,M_0,S,SGU,N_0,N,L,I,J,Q
	REAL*16:: U_ARRAY(1:SGU,1:SGU,1:M_0 + 1,1:N_0 + 1)
	REAL*16:: U(1:M_0+1,1:N_0+1,1:SGU),R_rN,R_N,R
	REAL*16:: R_R(1:SGU-2,1:N_0+1,1:M_0+1),p_eff_ForEveryLayer(1:SGU-2,1:M_0),p_eff_ForParticularLayer(1:SGU-2,1:N_0-1,1:M_0+1)
!-------------------------ВВОДНАЯ ЧАСТЬ---------------------------------
    P = 2; ! Теоретический порядок точности схемы
    Q = 1;
    R = 2; ! Коэффициент сгущения сетки
	
    DO S = 1,SGU
        DO M = 1,M_0+1
            DO N = 1,N_0+1
                U_ARRAY(S,1,M,N) = U(M,N,S)
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
! M >= 2 (кроме первого, так как на нём решение задано точно)
!ALLOCATE(p_eff_ForEveryLayer(1:SGU-2,1:M_0))
    DO M = 2,(M_0 + 1)
        DO s = 3,SGU
            R_rN = qsqrt(sum((u_array(s,2,M,:)-u_array(s,1,M,:))*(u_array(s,2,M,:)-u_array(s,1,M,:))));
            R_N = qsqrt(sum((u_array(s-1,2,M,:)-u_array(s-1,1,M,:))*(u_array(s-1,2,M,:)-u_array(s-1,1,M,:))));
            p_eff_ForEveryLayer(s-2,M - 1)= ((qlog(qabs(R_N)))-(qlog(qabs(R_rN))))/(qlog(r));
        ENDDO
    ENDDO
! Вычисляем эффективный порядок точности (во всех пространственных точках кроме граничных) 
! Вычисляем решение на определённом временном слое с номером M
!ALLOCATE(p_eff_ForParticularLayer(1:SGU-2,1:N_0)) 
	DO M = 1,M_0+1
    ! Вычисляем эффективный порядок точности (во всех пространственных точках кроме граничных) 
    ! на определённом временном слое с номером M
	!p_eff_ForParticularLayer = zeros(S-2,N_0 - 1);
		DO N = 2,N_0
			DO s = 3,SGU
				R_rN = qabs(u_array(s,2,M,N)-u_array(s,1,M,N));
				R_N = qabs(u_array(s-1,2,M,N)-u_array(s-1,1,M,N));
                p_eff_ForParticularLayer(s-2,N - 1,M)= ((qlog(R_N))-(qlog(R_rN)))/(qlog(r));
                R_R(S-2,N,M)=R_N;
			ENDDO
		ENDDO
	ENDDO
END

