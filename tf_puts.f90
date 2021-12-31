! Put methods for the TF types module
!===========================================================================
subroutine put_tf_element_name(element_name, session, err, rstatus)
  ! Puts the element_name into the TF session
  implicit none
  ! declare arguments
  character(len=var_name_length), intent(in) :: element_name
  type(tf_session_type), intent(inout)       :: session
  type(error_type), intent(inout)            :: err
  integer(ip), intent(out)                   :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_element_name"
  ! begin
  rstatus = RETURN_FAIL
  ! nullify the entire session
  call nullify_tf_session(session, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 100
  ! assign name to session
  session%data%is_null = .false.
  session%data%element_name = adjustl(element_name)
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
  return
end subroutine put_tf_element_name

!=============================================================================

subroutine put_tf_Z(Z, session, err, rstatus)
  ! Puts the value of Z into a TF session
  implicit none
  ! declare arguments
  real(dp), intent(in)                 :: Z
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout)      :: err
  integer(ip), intent(out)             :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_Z"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  session%data%Z = Z
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been specified yet.")
end subroutine get_tf_Z

!=============================================================================

subroutine put_tf_N(N, session, err, rstatus)
  ! Puts the value of N into a TF session
  implicit none
  ! declare arguments
  real(dp), intent(in)                 :: N
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout)      :: err
  integer(ip), intent(out)             :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_N"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  session%data%N = N
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been loaded yet.")
end subroutine put_tf_N

!=============================================================================

subroutine put_tf_Ra(Ra, session, err, rstatus)
  ! Puts the value of Ra into a TF session
  implicit none
  ! declare arguments
  real(dp), intent(in)                 :: Ra
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout)      :: err
  integer(ip), intent(out)             :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_Ra"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  session%data%ra = ra
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been loaded yet.")
end subroutine put_tf_Ra

!=============================================================================

subroutine put_tf_Rb(Rb, session, err, rstatus)
  ! Puts the value of Rb into a TF session
  implicit none
  ! declare arguments
  real(dp), intent(in)                 :: Rb
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout)      :: err
  integer(ip), intent(out)             :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_Rb"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  session%data%Rb = Rb
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been loaded yet.")
end subroutine put_tf_Rb

!=============================================================================

subroutine put_tf_ndim(ndim, session, err, rstatus)
  ! Puts the value of ndim into a TF session
  implicit none
  ! declare arguments
  integer(ip), intent(in)              :: ndim
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout)      :: err
  integer(ip), intent(out)             :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_ndim"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  session%model%is_null = .false.
  session%model%ndim = ndim  
  if (session%model%ndim == 0) goto 200
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been loaded yet.")
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "The ndim value entered cannot be zero.")
  session%model%is_null = .true.
end subroutine put_tf_ndim

!=============================================================================

subroutine put_tf_maxits(maxits, session, err, rstatus)
  ! Puts the value of maxits into a TF session
  implicit none
  ! declare arguments
  integer(ip), intent(in) :: maxits
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "put_tf_maxits"
  ! begin
  rstatus = RETURN_FAIL
  session%model%is_null = .false.
  session%model%maxits  = maxits  
  if (session%model%maxits == 0) goto 100
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No model has been loaded yet.")
end subroutine get_tf_maxits

!=============================================================================

subroutine get_tf_eps(eps, session, err, rstatus)
  ! Gets the value of eps stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: eps
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_eps"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  eps = session%data%eps
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_eps

!=============================================================================

subroutine get_tf_x0(x0, session, err, rstatus)
  ! Gets the value of x0 stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: x0
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_x0"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  x0 = session%model%x0
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_x0

!=============================================================================

subroutine get_tf_xi(xi, session, err, rstatus)
  ! Gets the value of xi stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: xi
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_xi"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  xi = session%data%xi
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_eps

!=============================================================================

subroutine get_tf_xdphi(xdphi, session, err, rstatus)
  ! Gets the value of xdphi stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: xdphi
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_xdphi"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  xdphi = session%model%xdphi
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_h
