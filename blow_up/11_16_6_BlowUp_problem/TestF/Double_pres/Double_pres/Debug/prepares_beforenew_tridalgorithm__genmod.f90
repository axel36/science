        !COMPILER-GENERATED INTERFACE MODULE: Sun Dec 24 21:22:08 2017
        MODULE PREPARES_BEFORENEW_TRIDALGORITHM__genmod
          INTERFACE 
            SUBROUTINE PREPARES_BEFORENEW_TRIDALGORITHM(A,B,C,F,U,A_11, &
     &TAU,MAT,N_X,M_POV,P_POV,H)
              INTEGER(KIND=4) :: N_X
              COMPLEX(KIND=8) :: A(1:N_X)
              COMPLEX(KIND=8) :: B(1:N_X)
              COMPLEX(KIND=8) :: C(1:N_X)
              REAL(KIND=8) :: F(1:N_X)
              REAL(KIND=8) :: U(1:N_X)
              COMPLEX(KIND=8) :: A_11
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: MAT(1:N_X)
              REAL(KIND=8) :: M_POV
              REAL(KIND=8) :: P_POV
              REAL(KIND=8) :: H
            END SUBROUTINE PREPARES_BEFORENEW_TRIDALGORITHM
          END INTERFACE 
        END MODULE PREPARES_BEFORENEW_TRIDALGORITHM__genmod
