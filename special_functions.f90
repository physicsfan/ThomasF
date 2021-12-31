MODULE special_functions
  ! Implemented functions:
  ! a) factorial - "factorial"
  ! b) gamma - "gama"
  ! c) log(gamma) - "log_gama"
  ! d) Student's t - "student_t"
  USE program_constants
!  use error_handler
!  implicit none
  
  PRIVATE


  PUBLIC :: besjn, factorial, gama, log_gama, par, student_t


CONTAINS
  
  RECURSIVE INTEGER FUNCTION factorial(x) RESULT(answer)
    ! Calculates the factorial of x, x!, using recursion
    ! Currently no error checking implemented
    IMPLICIT NONE
    INTEGER, INTENT(in) :: x
    
    IF(x == 0) THEN
       answer = 1
    ELSE
       answer = x * factorial(x-1)
    END IF
  END FUNCTION factorial
!
!===================================================================================
!  
  REAL(dp) FUNCTION my_gamma(xx) RESULT(answer)
    ! Calculates the Gamma function by taking the 
    ! exponential of the log(Gamma) function.  This
    ! minimizes the chance of over/under-flows
    IMPLICIT NONE
    !arguments
    REAL(sp), INTENT(in) :: xx
    answer = EXP(my_log_gamma(xx))
  END FUNCTION my_gamma
!
!===================================================================================
!  
  REAL(dp) FUNCTION my_log_gamma(xx) RESULT(answer)
    ! Calculates the log(gamma) function using the
    ! approximation given in Press et. al. (1992)
    ! for xx > 0
    IMPLICIT NONE
    ! arguments
    REAL(sp), INTENT(in) :: xx
    ! local variables
    INTEGER :: i
    REAL(dp) :: ser, stp, tmp, x
    REAL(dp), DIMENSION(6) :: coef

    ! set internal data
    ser  = 1.000000000190015_dp
    stp  = SQRT(2.0*pi)
    coef = (/76.18009172947146_dp,-86.50532032941677_dp, &
         24.01409824083091_dp,-1.231739572450155_dp, &
         1.208650973866179e-3_dp,-5.395239384953e-6_dp/)
    
    ! check if xx <= 0, return 0
    IF(xx .GT. 0.0) THEN
       tmp = xx + 5.5_dp
       tmp = (xx + 0.5_dp)*LOG(tmp) - tmp
       x = xx
       DO i=1,SIZE(coef)
          x = x + 1.0_dp
          ser = ser + coef(i)/x
       END DO
       answer = tmp + LOG(stp*ser/xx)
    ELSE
       answer = 0.0_dp    
    END IF
    
  END FUNCTION my_log_gamma
!
!=====================================================================================
!
!!$real(dp) function mbesselI(x, n) result(answer)
!!$  !  This function computes the modified spherical Bessel functions I
!!$  !  Use expansion for I(x) when x<0.5, otherwise use backward recurrsion.
!!$  implicit none
!!$  ! declare arguments
!!$  real(dp), intent(in) :: x
!!$  integer(ip), intent(in) :: n
!!$  ! declare locals and parameters
!!$  real(dp) :: sinhx
!!$  sinhx = sinh(x)
!!$  if
!!$end function mbesselI
!!$!
!!$!=====================================================================================
!!$!
!!$real(dp) recursive function mbesselK(x, n) result(answer)
!!$  ! This function computes the modified spherical Bessel function K
!!$  ! Uses forward recursion. 
!!$  implicit none
!!$  ! declare arguments
!!$  real(dp), intent(in) :: x
!!$  integer(ip), intent(in) :: n
!!$  ! declare locals and parameters
!!$  real(dp) :: ex
!!$  ! begin
!!$  ex = exp(x)
!!$  if (n == 0) then
!!$     answer = (1./ex) / x
!!$  else if (n == 1) then
!!$     answer = mbesselK(x, n-1) * (1. + (1./ x))
!!$  else
!!$     answer = real(2*n-3,dp)*mbesselK(x, n-1)/x + mbesselK(x, n-2)
!!$  end if
!!$end function mbesselK
!
!=====================================================================================
!
  REAL(dp) FUNCTION besjn(JY,N,X)                                            
    !                                                                       
    !      THIS FUNCTION COMPUTES THE SPHERICAL BESSEL FUNCTIONS OF         
    !   THE FIRST KIND AND SPHERICAL BESSEL FUNCTIONS OF THE SECOND         
    !   KIND (ALSO KNOWN AS SPHERICAL NEUMANN FUNCTIONS) FOR REAL           
    !   POSITIVE ARGUMENTS.                                                 
    !                                                                       
    !      INPUT:                                                           
    !         JY ...... KIND: 1(BESSEL) OR 2(NEUMANN).                      
    !         N ....... ORDER (INTEGER).                                    
    !         X ....... ARGUMENT (REAL AND POSITIVE).                       
    !                                                                       
    !   REF.: M. ABRAMOWITZ AND I.A. STEGUN, 'HANDBOOK OF MATHEMATI-        
    !         CAL FUNCTIONS'. DOVER, NEW YORK (1974). PP 435-478.           
    !
    ! (NB: This function was originally written in F77.)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                       
    TPI = PI + PI
    !  IF(X.LT.0) THEN                                                   
    !     WRITE(6,1000)                                                   
    !1000 FORMAT(1X,'*** NEGATIVE ARGUMENT IN FUNCTION BESJN.')           
    !     STOP                                                            
    !  ENDIF
    !  ****  ORDER AND PHASE CORRECTION FOR NEUMANN FUNCTIONS.              
    !        ABRAMOWITZ AND STEGUN, EQ. 10.1.15.                            
    IF(JY.EQ.2) THEN                                                  
       NL=-N-1                                                         
       IPH=2*MOD(IABS(N),2)-1                                          
    ELSE                                                              
       NL=N                                                            
       IPH=1                                                           
    ENDIF
    !  ****  SELECTION OF CALCULATION MODE.                                 
    IF(NL.LT.0) GOTO 10                                              
    IF(X.GT.1.0D0*NL) GOTO 7                                         
    XI=X*X                                                            
    IF(XI.GT.NL+NL+3.0D0) GOTO 4                                     
    !  ****  POWER SERIES FOR SMALL ARGUMENTS AND POSITIVE ORDERS.          
    !        ABRAMOWITZ AND STEGUN, EQ. 10.1.2.                             
    F1=1.0D0                                                          
    IPI=1                                                              
    IF(NL.NE.0) THEN                                                  
       DO I=1,NL                                                     
          IPI=IPI+2                                                         
          F1=F1*X/IPI
       END DO
    ENDIF
    XI=0.5D0*XI                                                       
    BESJN=1.0D0                                                       
    PS=1.0D0                                                          
    DO I=1,500                                                      
       IPI=IPI+2                                                           
       PS=-PS*XI/(I*IPI)                                                  
       BESJN=BESJN+PS                                                    
       IF(DABS(PS).LT.1.0D-18*DABS(BESJN)) EXIT                       
    END DO
    BESJN=IPH*F1*BESJN                                                
    RETURN                                                            
    !  ****  MILLER'S METHOD FOR POSITIVE ORDERS AND INTERMEDIATE           
    !        ARGUMENTS. ABRAMOWITZ AND STEGUN, EQ. 10.1.19.                 
4   CONTINUE
    XI=1.0D0/X                                                        
    F2=0.0D0                                                          
    F3=1.0D-35                                                        
    IPI=2*(NL+31)+3                                                    
    DO I=1,31                                                       
       F1=F2                                                             
       F2=F3                                                             
       IPI=IPI-2                                                           
       F3=IP*XI*F2-F1                                                    
       IF(DABS(F3).GT.1.0D30) THEN                                       
          F2=F2/F3                                                        
          F3=1.0D0                                                        
       ENDIF
    END DO
    BESJN=1.0D0                                                       
    F2=F2/F3                                                          
    F3=1.0D0                                                          
    DO I=1,NL                                                       
       F1=F2                                                             
       F2=F3                                                             
       IPI=IPI-2                                                           
       F3=IPI*XI*F2-F1                                                    
       IF(DABS(F3).GT.1.0D30) THEN                                       
          BESJN=BESJN/F3                                                  
          F2=F2/F3                                                        
          F3=1.0D0                                                        
       ENDIF
    END DO
    BESJN=IPH*XI*DSIN(X)*BESJN/F3                                     
    RETURN
  
    !  ****  RECURRENCE RELATION FOR ARGUMENTS GREATER THAN ORDER.          
    !        ABRAMOWITZ AND STEGUN, EQ. 10.1.19.                            
7   CONTINUE
    XI=1.0D0/X                                                        
    F3=XI*DSIN(X)                                                     
    IF(NL.EQ.0) GOTO 9                                               
    F2=F3                                                             
    F3=XI*(F2-DCOS(X))                                                
    IF(NL.EQ.1) GOTO 9                                               
    IPI=1                                                              
    DO I=2,NL                                                       
       F1=F2                                                             
       F2=F3                                                             
       IPI=IPI+2                                                           
       F3=IPI*XI*F2-F1
    END DO
9   CONTINUE
    BESJN=IPH*F3                                                      
    RETURN                                                            
    
    !  ****  RECURRENCE RELATION FOR NEGATIVE ORDERS.                       
    !        ABRAMOWITZ AND STEGUN, EQ. 10.1.19.                            
10  CONTINUE
    NL=IABS(NL)                                                       
    IF(X.LT.7.36D-1*(NL+1)*1.0D-35**(1.0D0/(NL+1))) THEN              
       BESJN=-1.0D35                                                   
       RETURN                                                          
    ENDIF
    XI=1.0D0/X                                                        
    F3=XI*DSIN(X)                                                     
    F2=XI*(F3-DCOS(X))                                                
    IPI=3                                                              
    DO I=1,NL                                                      
       F1=F2                                                             
       F2=F3                                                             
       IPI=IPI-2                                                           
       F3=IPI*XI*F2-F1                                                    
       IF(DABS(F3).GT.1.0D35) THEN                                       
          BESJN=-1.0D35                                                   
          RETURN                                                          
       ENDIF
    END DO
    BESJN=IPH*F3                                                      
    RETURN                                                            
  END FUNCTION besjn
!
!=====================================================================================
!
  REAL(dp) FUNCTION mbesjn(JY,N,X) RETURN(answer)                                           
    !                                                                       
    !   THIS FUNCTION COMPUTES THE MODIFIED SPHERICAL BESSEL
    !   FUNCTIONS OF THE FIRST KIND (i) AND SECOND KIND (k)
    !   FOR REAL POSITIVE ARGUMENTS AND INTEGER ORDER.                                                 
    !                                                                       
    !      INPUT:                                                           
    !         JY ...... KIND: 1(BESSEL) OR 2(NEUMANN).                      
    !         N ....... ORDER (INTEGER).                                    
    !         X ....... ARGUMENT (REAL AND POSITIVE).                       
    !                                                                       
    !   REF.: M. ABRAMOWITZ AND I.A. STEGUN, 'HANDBOOK OF MATHEMATI-        
    !         CAL FUNCTIONS'. DOVER, NEW YORK (1974). PP 435-478.           
    !

  END FUNCTION mbesjn
!
!=====================================================================================
!
  REAL(dp) FUNCTION par(i) RESULT(answer)
    !  Gives parity as defined: (-1)**I.
    IMPLICIT NONE
    INTEGER(ip) :: i
    answer = REAL(1-ABS(2*i-4*(i/2)),dp)
    RETURN
  END FUNCTION par
!  
!=====================================================================================
!  
subroutine mbessel(x,xi,xk)
  !  This subroutine computes the modified spherical Bessel functions
  !     i(x) and k(x) up to order 14.
  
  DIMENSION XI(15),XK(15),XJ(30)

!  Use expansion for i(x) when x<0.5, otherwise use backward recurrsion.

  EX=EXP(X)
  XI(1)=(EX-1./EX)/2./X
  IF(X.LT.0.5)THEN
     Y=0.5*X*X
     X1=1.
     DO L=2,15
        X3=FLOAT(2*L-1)
        X2=X*X1/X3
        XI(L)=X2*(1.+Y/(X3+2.)*(1.+Y/(X3+4.)/2.))
        X1=X2
     ENDDO
  ELSE
     TN=1.
     IF(X.LT.5.)TN=1.E-30
     TN1=0.
     DO I=1,30
        XJ(I)=FLOAT(63-2*I)*TN/X+TN1
        TN1=TN
        TN=XJ(I)
     ENDDO
     X1=XI(1)/XJ(30)
     DO L=2,15
        XI(L)=XJ(30-L+1)*X1
     ENDDO
  ENDIF
  !
  !  Forward recurrsion for k(x)
  !
  XK(1)=1./EX/X
  XK(2)=XK(1)*(1.+1./X)
  DO L=3,15
     XK(L)=FLOAT(2*L-3)*XK(L-1)/X+XK(L-2)
  ENDDO
  RETURN
end subroutine mbessel
!
!=================================================================================
!
real(dp) function parity(i) result(answer)
  !  Gives parity PARITY(I)=(-1)**I.
  implicit none
  integer(ip) :: i
  answer = real(1-IABS(2*I-4*(I/2)),dp)
  return
end function parity
!
!=================================================================================
!
  REAL(dp) FUNCTION student_t(y, nu) RESULT(answer)
    ! Calculates the density function for Student's t 
    ! random variable with nu > 0 degrees of freedom
    IMPLICIT NONE
    ! arguments
    REAL(sp), INTENT(in) :: nu, y
    
    answer = (gama(0.5*nu + 0.5) / (gama(0.5*nu)*SQRT(pi*nu))) &
         * (1. + y**2 / nu)**(-0.5*nu - 0.5)
  END FUNCTION student_t

END MODULE special_functions


PROGRAM sf_test
  USE program_constants
  USE special_functions
  
  INTEGER(ip) :: n
  REAL(dp) :: x = 1.0
  
  
  PRINT '(/,a12,a17,a17)', 'n', 'j_n(x)', 'y_n(x)'
  DO n = 0, 5
     PRINT *, n, besjn(1,n,x), besjn(2,n,x)
  END DO
  
END PROGRAM sf_test
