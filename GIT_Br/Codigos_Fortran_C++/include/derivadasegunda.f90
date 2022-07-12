!  derivadasegunda.f90 
!
!  FUNCTIONS:
!  derivadasegunda - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: derivadasegunda
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

!INCLUDE 'link_fnl_static.h'
!DEC$ OBJCOMMENT LIB:'libiomp5md.lib'

PROGRAM LISTA2
!USE LINRG_INT
IMPLICIT NONE
INTEGER :: I, N
REAL(8) :: C_1, C_2, DX, X, L, ERR
REAL(8) :: F0, F_0, ERRD, F_1, F_2,DF_1, DF_2, DF_3, D2F_1, D2F_2, D2F_3, TOL
C_1 = 2.D0/225.D0 !COEFICIENTES DA SOLUCAO ANALITICA.
C_2 = 4.D0/50625.D0 !COEFICIENTES DA SOLUCAO ANALITICA.
N = 51 ! (NUMERO DE PONTOS)
L = 100.0D0 ! TAMANHO
DX = 0.0002
TOL = 1.D-10
F0 = F_0 (X) ! FUNCAO A SER DERIVADA
ERR = ERRD (F_1,DF_1)
 
! RESOLVE A FUNCAO E SUA DERIVADA ANALITICAMENTE 
           DO I = -N,N-2

              X = I+DX
              F_1 = -C_1*X*(F_0 (X)) ! DERIVADA PRIMEIRA
              F_2 = -C_1*(F_0 (X)) + C_2*X**2*F_0 (X) ! DERIVADA SEGUNDA

                     
! RESOLVE A FUNCAO E SUA DERIVADA NUMERICAMENTE

        DF_1 = (F_0 (X+DX)-F_0 (X-DX))/(2*DX)   ! DERIVADA PRIMEIRA DE ORDEM 2:
         DF_2 = 2*(F_0 (X+DX)-F_0 (X-DX))/(3*DX) - (F_0 (X+2*DX)-F_0 (X-2*DX))/(12*DX)     ! DERIVADA PRIMEIRA DE ORDEM 4:
          DF_3 = 3*(F_0 (X+DX)-F_0 (X-DX))/(4*DX) - 3*(F_0 (X+2*DX)-F_0 (X-2*DX))/(20*DX) + (F_0 (X+3*DX)-F_0 (X-3*DX))/(60*DX)    ! DERIVADA PRIMEIRA DE ORDEM 6:
          
         D2F_1 =  (F_0 (X+DX) - 2*F_0 (X) + F_0 (X-DX))/DX**2 ! DERIVADA SEGUNDA DE ORDEM 2:
D2F_2 = (-5.0/2.0)*(F_0 (X))/DX**2 + (4.0/3.0)*(F_0 (X+DX)+F_0 (X-DX))/DX**2 - (1.0/2.0)*(F_0 (X+2*DX) + F_0 (X-2*DX))/DX**2  ! DERIVADA SEGUNDA DE ORDEM 4:
D2F_3 = (-49.0/2.0)*((1.0/18.0)*F_0 (X))/DX**2 + 3*(F_0 (X+DX) + F_0 (X-DX))/DX**2-(3.0/20)*(F_0 (X+2*DX)+F_0 (X-2*DX))/DX**2 
           
            D2F_3 = D2F_3 + (1.0/90.0)*(F_0 (X+3*DX) + F_0 (X-3*DX))/DX**2     ! CONTINUACAO DA DERIVADA NA LINHA 
             ! D2F_3 >> DERIVADA SEGUNDA DE ORDEM 6:  (OBS, NAO USAR!!)
          ERR = ERRD (F_2,D2F_2) 
          
          PRINT*, ERR 
        !  PRINT*, D2F_3
       
           OPEN (12, file = 'X.dat')     ! ESCREVE AS DERIVADAS ANALITICAS
            write (12,11) X
           OPEN (13, file = 'F_1.dat')
            write (13,11) F_1
           OPEN (14, file = 'F_2.dat')
            write (14,11) F_2
                                        ! ESCREVE AS DERIVADAS NUMERICAS DE ORDEM 2, 4 E 6
           OPEN (15, file = 'DF_1.dat')
            write (15,11) DF_1
           OPEN (16, file = 'DF_2.dat')
            write (16,11) DF_2
           OPEN (17, file = 'DF_3.dat')
            write (17,11) DF_3
                                        ! ESCREVE A SEGUNDA DERIVADA NUMERICA DE ORDEM 2, 4 E 6
             OPEN (18, file = 'D2F_1.dat')
            write (18,11) D2F_1
           OPEN (19, file = 'D2F_2.dat')
            write (19,11) D2F_2
           OPEN (20, file = 'D2F_3.dat')
            write (20,11) D2F_3
            OPEN (21, file = 'ERRD.dat')
            write (21,11) ERR
           
           ! ERR = MAXVAL(ABS(F_2 - D2F_2)) IMPLEMENTAR APENAS SE FOR VETORES!
           ! PRINT*, ERR
              11 FORMAT(1F10.6)
              
           END DO
           
    END PROGRAM LISTA2

    
    
    FUNCTION ERRD (F,DF)
    IMPLICIT NONE 
    REAL (8) ::ERRD, F, DF
        ERRD = (SQRT((F-DF)**2))/SQRT(F**2)
    RETURN
    END

    FUNCTION F_0 (X)
    IMPLICIT NONE 
    REAL (8):: X, F_0
    INTEGER :: S
        S = 15
        F_0 = EXP (-(X**2/S**2))
    RETURN
    END