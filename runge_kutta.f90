MODULE runge_kutta
  !==========================================================
  ! A module for integrating differential equations using the
  ! fourth-order Runge-Kutta method as given in
  !
  ! Input:
  ! x -
  ! y -
  ! yp-
  ! h -
  ! rstatus -  return status flag (for error_handler)
  !==========================================================
  USE program_constants ONLY: ip, dp
  USE math_constants
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: runge


CONTAINS
  
  SUBROUTINE runge(x,y,yp,h,rstatus)
    !**********************************************
    ! Fourth-Order Runge-Kutta Method
    ! Abramowitz and Stegun 
    ! Handbook of Math Functions (1964 edition)
    ! NBS AMS 55
    ! Chapter 25, Equation 25.5.20
    !
    !**********************************************
    IMPLICIT NONE

    ! Arguments
    REAL(dp), intent(inout) :: x(:), y(:), yp(:)
    REAL(dp), INTENT(in)    :: h
    INTEGER(ip), intent(out) :: rstatus
    
    ! Local variables
    INTEGER(ip) :: i, ndim
    REAL(dp) :: k1, k2, k3, k4, u, v, w

    ! Initialize
    ndim = SIZE(x)
    IF ((SIZE(y) /= ndim) .OR. (SIZE(yp) /= ndim)) GOTO 100

    ! Begin integration
    DO i = 1, ndim-2
       u = x(i)
       v = y(i)
       w = yp(i)
       w = yp(i)
       k1 = h * abfo(u,v,w)
       u = x(i) + half * h
       v = y(i) + h * (half * yp(i) + eighth * k1)
       w = yp(i) + half * k1
       k2 = h * abfo(u,v,w)
       u = x(i) + half * h
       v = y(i) + h * (half * yp(i) + eighth * k2)
       w = yp(i) + half * k2
       k3 = h * abfo(u,v,w)
       u = x(i) + h
       v = y(i) + h * ( yp(i) + half * k3 )
       w = yp(i) +k3
       k4 = h * abfo(u,v,w)
       y(i+1) = y(i) + h * ( yp(i) + ( k1 + k2 + k3 )/six )
       yp(i+1) = yp(i) + (k1 + two * k2 + two * k3 + k4)/six
    END DO
    
    ! Special treatment for x = 0.0
    ! Adams-Bashforth Fourth-Order 
    ! Chapter 25, Equation 25.5.4
    i = ndim-1
    yp(i+1) = yp(i) + h * (55.0 * abfo(x(i),y(i),yp(i)) &
         - 59.0 * abfo(x(i-1),y(i-1),yp(i-1)) &
         + 37.0 * abfo(x(i-2),y(i-2),yp(i-2)) &
         - 9.0 * abfo(x(i-3),y(i-3),yp(i-3))) / 24.0

    y(i+1) = y(i) + h * (55.0 * yp(i) - 59.0 * yp(i-1) &
         + 37.0 * yp(i-2) - 9.0 * yp(i-3)) / 24.0

    ! Normal exit
    rstatus = RETURN_SUCCESS
    RETURN
    
    ! Error trap
100 rstatus = RETURN_FAIL
    RETURN
  END SUBROUTINE runge



  PURE FUNCTION abfo(x,y,yp) RESULT(answer)
    ! Function needed for the special treatment for x = 0.0
    ! in the Runge-Kutta algorithm.
    IMPLICIT NONE
    REAL(dp), INTENT(in) :: x, y, yp
    REAL(dp) :: answer
    IF (x == zero) THEN
       answer = zero
    ELSE
       answer = four * x * SQRT(ABS(y*y*y)) + yp / x
    ENDIF
    RETURN
  END FUNCTION abfo

END MODULE runge_kutta
