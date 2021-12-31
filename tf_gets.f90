! Get methods for the TF types module
!==============================================================================
subroutine get_tf_element_name(element_name, session, err, rstatus)
  ! Gets the element_name currently stored in the TF session
  implicit none
  ! declare arguments
  character(len=var_name_length) :: element_name
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = &
       "get_tf_element_name"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  element_name = session%data%element_name
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No element has been loaded yet.")
  return
end subroutine get_tf_element_name

!=============================================================================

subroutine get_tf_Z(Z, session, err, rstatus)
  ! Gets the value of Z stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: Z
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_Z"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  Z = session%data%Z
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_Z

!=============================================================================

subroutine get_tf_N(N, session, err, rstatus)
  ! Gets the value of N stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: N
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_N"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  N = session%data%N
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_N

!=============================================================================

subroutine get_tf_Ra(Ra, session, err, rstatus)
  ! Gets the value of Ra stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: Ra
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_Ra"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  Ra = session%data%Ra
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_Ra

!=============================================================================

subroutine get_tf_Rb(Rb, session, err, rstatus)
  ! Gets the value of Rb stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: Rb
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_Rb"
  ! begin
  rstatus = RETURN_FAIL
  if (session%data%is_null) goto 100
  Rb = session%data%Rb
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_Rb

!=============================================================================

subroutine get_tf_ndim(ndim, session, err, rstatus)
  ! Gets the value of ndim stored in a TF session
  implicit none
  ! declare arguments
  integer(ip), intent(out) :: ndim
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_ndim"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  ndim = session%model%ndim
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_ndim

!=============================================================================

subroutine get_tf_maxits(maxits, session, err, rstatus)
  ! Gets the value of maxits stored in a TF session
  implicit none
  ! declare arguments
  integer(ip), intent(out) :: maxits
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_maxits"
  ! begin
  rstatus = RETURN_FAIL
  if (session%model%is_null) goto 100
  maxits = session%model%maxits
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
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

!====================================================================================

subroutine get_tf_iter(iter, session, err, rstatus)
  ! Gets the number of iterations performed after a model has
  ! been fit.
  implicit none
  ! declare arguments
  integer(ip), intent(out) :: iter
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_iter"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  iter = session%results%iter
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No model has been fit yet.")
  return
end subroutine get_tf_iter

!=====================================================================================

subroutine get_tf_converged(converged, session, err, rstatus)
  ! Gets whether the potential generating procedure converged
  implicit none
  ! declare arguments
  logical,                  intent(out) :: converged
  type(elogit_session_type), intent(in) :: session
  type(error_type),       intent(inout) :: err
  integer(ip),              intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_converged"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  converged = session%results%converged
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No model has been fit yet.")
  return
end subroutine get_tf_converged

!=============================================================================

subroutine get_tf_rmax(rmax, session, err, rstatus)
  ! Gets the value of rmax stored in a TF session
  implicit none
  ! declare arguments
  real(dp), intent(out) :: rmax
  type(tf_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_rmax"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  rmax = session%results%rmax
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No dataset has been loaded yet.")
end subroutine get_tf_rmax

!====================================================================================

subroutine get_tf_R(R, session, err, rstatus)
  ! Gets the R array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: r(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_R"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(r, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  r = session%results%r
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_R

!====================================================================================

subroutine get_tf_phi(phi, session, err, rstatus)
  ! Gets the phi array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: phi(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_phi"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(phi, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  phi = session%results%phi
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_phi

!====================================================================================

subroutine get_tf_phip(phip, session, err, rstatus)
  ! Gets the phip array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: phip(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_phip"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(phip, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  phip = session%results%phip
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_phip

!===============================================================================

subroutine get_tf_N(N, session, err, rstatus)
  ! Gets the N array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: N(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_N"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(N, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  N = session%results%N
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_N

!===============================================================================

subroutine get_tf_U(U, session, err, rstatus)
  ! Gets the U array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: U(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_U"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(U, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  U = session%results%U
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_U

!=============================================================================

subroutine get_tf_Vel(Vel, session, err, rstatus)
  ! Gets the Vel array currently stored in the TF session
  implicit none
  ! declare arguments
  real(dp), pointer :: Vel(:)
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus 
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "get_tf_Vel"
  ! begin
  rstatus = RETURN_FAIL
  if (session%results%is_null) goto 100
  call dyn_alloc(Vel, session%model%ndim, err, rstatus)
  if (rstatus == RETURN_FAIL) goto 200
  Vel = session%results%Vel
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data matrix has been loaded yet.")
  return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine get_tf_phip

!===============================================================================
