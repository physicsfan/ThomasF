module odeIntegrateRK
  ! A module to integrate differential equations using the 4-order Runge-Kutta
  ! method with adaptive stepsize.
  use program_constants
  use error_handler
  implicit none
  
  private                                        ! By default
  
  public :: odeIntegrate
  
  ! private parameters
  integer(ip),  parameter :: maxstep    = 10000   ! Maximum number of steps
  integer(ip),  parameter :: nmax       = 10      ! Maximum number of functions
  integer(ip),  parameter :: temp_steps = 200
  real(dp),     parameter :: pgrow     = -0.20
  real(dp),     parameter :: pshrink   = -0.20
  real(dp),     parameter :: fcorr     = 1./15.
  real(dp),     parameter :: zero      = 0.
  real(dp),     parameter :: one       = 1.
  real(dp),     parameter :: two       = 2.
  real(dp),     parameter :: safety    = 0.9
  real(dp),     parameter :: errcon    = 6.d-4
  real(dp),     parameter :: tiny      = 1.d-30
  
  character(len=*), parameter :: modname = "odeIntegrateRK"
  
  ! Private types
  type :: path
     ! For storage of intermediate results
     sequence
     private
     logical :: is_null = .true.
     integer(ip) :: kmax, kount
     real(dp)    :: dxsave
     real(dp), allocatable :: xp(:)
     real(dp), allocatable :: yp(:,:)
  end type path
  
  
contains
  
  
  subroutine odeIntegrate(ystart, nvar, x1, x2, eps, h1, hmin, nok, nbad, kmax, dxsave)
    ! Runge-Kutta driver with adaptive stepsize control.  Integrate the NVAR starting values
    ! YSTART from X1 to X2 with accuracy EPS, storing intermediate results in the object PATH.
    ! H1 should be set as a guessed first stepsize, HMIN as the minimum allowed stepsize (can
    ! be zero).  On the output NOK and NBAD are the number of good and bad (but retried and
    ! fixed) steps taken, and YSTART is replaced by values as the end of the integration interval.
    implicit none
    ! Arguments
    integer(ip), intent(in)     :: nvar
    real(dp),    intent(in)     :: x1, x2, eps, h1, hmin
    real(dp),    intent(inout)  :: ystart(nvar)
    integer(ip), intent(out)    :: nok, nbad
    integer(ip), optional       :: kmax
    real(dp),    optional       :: dxsave
    ! Local variables
    integer(ip) :: i, j, k, nstep
    real(dp)    :: h, hdid, hnext, xsav
    type(path)  :: storage
    real(dp), dimension(:), allocatable :: dydx, y, yscal
    ! Allocate arrays
    allocate(dydx(nmax), y(nmax), yscal(nmax))
    
    ! Allocate temporary storage (if asked for)
    if (present(kmax) .and. kmax .ne. 0) then 
       call createPath(storage)
       storage%kmax = kmax
       if (present(dxsave)) storage%dxsave = dxsave
    end if
    

    x = x1
    h = sign(h1, x2 - x1)
    nok = 0; nbad = 0; kount = 0
    y = ystart
    ! Assures storage of first step (if wanted)
    if (.not.storage%is_null) xsav = x - storage%dxsave * two
    main: do nstep = 1, maxstep
       call derivatives(x, y, dydx)
       do i = 1, nvar
          yscal(i) = abs(y(i)) + abs(h * dydx(i)) + tiny
       end do
       if (.not.storage%is_null) then
          if (abs(x - xsav) > abs(storage%dxsav)) then
             ! Store intermediate results
             if (storage%kount < storage%kmax-1) then
                storage%kount = storage%kount + 1
                storage%xp(storage%count) = x
                do i = 1, nvar
                   storage%yp(i,storage%kount) = y(i)
                end do
                xsav = x
             end if
          end if
       end if
       ! If step can oversoot end, cut down stepsize
       if ((x+h-x2)*(x+h-x1) > zero) h = x2 - x
       call rungeKuttaQC(x, y, dydx, nvar, h, eps, yscal, hdid, hnext)
       if (hdid == h) then
          nok = nok + 1
       else
          nbad = nbad + 1
       end if
       if ((x-x2)*(x2-x1) >= zero) then
          ! Are we done?
          ystart = y
          if (.not.storage%is_null) then
             ! Save final step
             storage%kount = storage%kount + 1
             storage%xp(storage%kount) = x
             storage%yp(:,storage%kount) = y(:)
          end if
          exit main  ! Finished
       end if
       if (abs(hnext) < hmin) goto 100
       h = hnext
    end do main
    if (i > maxstep) goto 200
    ! Normal exit
    deallocate(dydx, y, yscal)
    call nullifyPath(temp_storage)
    return      
    ! Error trap
100 stop 'Error: array dimensions not correct'
    return
200 stop 'Error: too many steps'
    return  
  end subroutine odeIntegrate
  
     
  ! Constructor
  subroutine createPath(self)
    ! Create temporary storage
    type(path), intent(inout) :: self
    if (.not.self%is_null) call nullifyPath(self)
    allocate(temp_storage%xp(temp_steps), temp_storage%yp(nmax,temp_steps))
  end subroutine createPath
  
  
  ! Destructor
  subroutine nullifyPath(self)
    ! Nullifies temporary storage
    type(path), intent(inout) :: self
    self%kmax   = 0
    self%kount  = 0
    self%dxsave = 0.
    if allocated(self%xp) deallocate(self%xp)
    if allocated(self%yp) deallocate(self%yp)
  end subroutine nullifyPath
  
  
  subroutine rungeKuttaQC(x, y, dydx, n, htry, eps, yscal, hdid, hnext)
    ! Fifth-order Runge-Kutta step with monitoring of local truncation error to ensure accuracy
    ! and adjust stepsize. Input are the dependent variable vector Y of length N and its
    ! derivative DYDX at the starting value of the independent variable X. Also input are the
    ! stepsize the be attempted HTRY, the required accuracy EPS, and the vector YSCAL against
    ! which
    implicit none
    ! Arguments
    integer(ib4), intent(in) :: n
    real(dp), intent(in) :: h, eps, htry
    real(dp), dimension(n), intent(in) :: dydx, yscal
    real(dp), dimension(n), intent(inout) :: x, y
    real(dp), intent(out) :: hdid, hnext
    ! Local variables
    real(dp), dimension(nmax) :: ytemp, ysave, dysave
    real(dp) :: errmax, hh, xsave
    ! Save initial values
    xsave  = x
    ysave  = y
    dysave = dydx
    ! Set stepsize to the initial trial value
    h = htry                                         
    do 
       ! Take two half steps
       hh = 0.5 * h                                  
       call runggekutta4(ysave, dysave, n, xsave, hh, ytemp)
       x = sxave + hh
       call derivatives(x, ytemp, dydx) 
       call runggekutta4(ytemp, dydx, n, x, hh, y)
       x = sxave + h
       if (x == xsave) pause 'Stepsize not significant'
       ! Take the large step
       call runggekutta4(ysave, dysave, n, xsave, hh, ytemp)
       ! Evaluate accuracy
       errmax = 0.
       ytemp = y - ytemp
       do i = 1, n
          errmax = max(errmax, abs(ytemp(i)/yscal(i)))
       end do
       ! Scale relative to required tolerance
       errmax = errmax / eps
       if (errmax > one) then
          ! Truncation error too large, reduce stepsize
          hnext = safety * h * errmax**pshrink
       else
          ! Step succeeded, compute size of next step
          hdid = h
          if (errmax > errcon) then
             hnext = safety * h * errmax**pgrow
          else
             hnext = 4. * h
          end if
          exit 
       end if
    end do
    ! Mop up fifth-order truncation error
    y = y + ytemp * fcor
    return
  end subroutine rungeKuttaQC
  
  
  subroutine rungeKutta4(y, dydx, n, x, h, yout)
    ! Given values for N variables Y and their derivatives DYDX known at X, use the fourth-order
    ! Runge-Kutta method to advance the solution over an interval H and return the incremented
    ! variables as YOUT, which need not be a distinct array from Y.
    implicit none
    ! Arguments
    integer(ib4), intent(in) :: n
    real(dp), intent(in) :: h, x
    real(dp), dimension(n), intent(in) :: y, dydx
    real(dp), dimension(n), intent(out) :: yout
    ! Local variables
    real(dp), dimension(nmax) :: yt, dyt, dym
    real(dp) :: hh, h6, xh
    integer(ib4) :: i
    ! Initialize
    hh = 0.5*h
    h6 = h / 6.
    xh = x + hh
    ! First step
    yt(:) = y(:) + hh * dydx(:)
    ! Second step
    call derivatives(xh, yt, dyt)
    yt(:) = y(:) + hh * dyt(:)
    ! Third step
    call derivatives(xh, yt, dym)
    do i = 1, n
       yt(i) = y(i) + h * dym(i)
       dym(i) = dyt(i) + dym(i)
    end do
    ! Fourth step
    call derivatives(x+h, yt, dyt)
    ! Accumulate increments with proper weights
    yout(:) = y(:) + h6 * (dydx(:) + dyt(:) + 2. * dym(:))
    return
  end subroutine rungeKutta4
  
  
  subroutine derivatives(y, x, dydx)
    ! Calculates the derivative dy/dx using the arrays
    ! y and x
    implicit none
    ! Arguments
    real(dp), intent(in)    :: y(:)
    real(dp), intent(in)    :: x(:)
    real(dp), intent(inout) :: dydx(:)
    ! Local variables
    real(dp) :: den, h
    integer(ib4) :: i, j, k, ymax
    ! Check is sizes or arrays are correct
    if ((size(y) /= size(x)) .or. (size(y) /= size(dydx))) goto 100
    ymax = size(y)
    ! Initialize
    dydx = zero
    h = x(2) - x(1)
    ! Calculate initial point using forward difference
    den = one / h
    dydx(1) = (y(2) - y(1)) * den
    ! Calculate most points using central difference
    den = one / (two * h)
    do i = 2, ymax - 1
       dydx(i) = (y(i+1) - y(i-1)) * den
    end do
    ! Calculate final point using backwards difference
    den = one / h
    dydx(ymax) = (y(ymax) - y(ymax-1)) * den
    return
    ! Error trap
100 stop 'Error: array dimensions not correct'
  end subroutine derivatives
  
     
!!$  SUBROUTINE  NumericalDP(ia)
!!$    ! A potentially more accurate version of the derivative
!!$    ! routine using a 3-point and 5-point scheme.
!!$    IMPLICIT NONE
!!$    INTEGER, INTENT(in) :: ia
!!$    INTEGER :: i, n
!!$    REAL(8) :: den
!!$
!!$
!!$    DP(1:npX,ia) = 0.d0
!!$    DQ(1:npX,ia) = 0.d0
!!$
!!$    n = npt(ia)
!!$    i = 2
!!$    den = 1.d0/(2.d0*th)
!!$    dp(i,ia) = (P(i+1,ia) - P(i-1,ia))*den
!!$    dq(i,ia) = (Q(i+1,ia) - Q(i-1,ia))*den
!!$    i = 3
!!$    den = 1.d0/(th*12.d0)
!!$    dp(i,ia) = (8.d0*(P(i+1,ia)-P(i-1,ia))  &
!!$         - P(i+2,ia)+P(i-2,ia))*den
!!$    dq(i,ia) = (8.d0*(Q(i+1,ia)-Q(i-1,ia))  &
!!$         - Q(i+2,ia)+Q(i-2,ia))*den
!!$    den = 1.d0/(60.d0*th)
!!$    DO I = 4, NPx-4
!!$       dp(i,ia) = ( 45.d0*(P(i+1,ia)-P(i-1,ia))  &
!!$            -9.d0*(P(i+2,ia)-P(i-2,ia)) &
!!$            +   (P(i+3,ia)-P(i-3,ia)))*den
!!$       dq(i,ia) = ( 45.d0*(Q(i+1,ia)-Q(i-1,ia))  &
!!$            -9.d0*(Q(i+2,ia)-Q(i-2,ia)) &
!!$            +   (Q(i+3,ia)-Q(i-3,ia)))*den
!!$    END DO
!!$
!!$  END SUBROUTINE NumericalDP

end module odeIntegrateRK

