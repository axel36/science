program kk
    implicit none
    real(8) :: z(10)
    real(16) :: m_pov, p_pov,L,h,tt,tau,a,Aa,M_0,N_0,pi,y
    real(16) , allocatable :: b(:,:)	
    integer :: k,i,j
    complex(16) i_unit,x,U_Nx,pow
    complex(16), allocatable :: q(:)
    INTEGER SQ
    REAL(16) U1
    external pow
    pi= 3.141592653589793238462643383279502q0
    k=10q0
    m_pov=1.0q0/2.0q0
	p_pov=7.0q0/8.0q0
    U1=	0.0q0	 
    U_Nx=CMPLX(U1 , 0.0q0)
	x=pow(U_Nx,(p_pov-1q0))
	print *,"POW"
	print *,x
	pause
	
END

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