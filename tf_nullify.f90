! Methods for nullifying parts of the ThomasF session
subroutine nullify_tf_session(session, err, rstatus, &
  save_data, save_model, save_results)
  ! Returns an tf_session_type to its initialized (null) state
  implicit none
  ! declare required arguments
  type(tf_session_type), intent(inout) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare optional arguments
  logical, optional :: save_data, save_model, save_results
  ! declare local variables and parameters
  logical :: savedata, savemodel, saveresults
  character(len=*), parameter :: subname = &
       "nullify_tf_session"
  ! begin
  rstatus = RETURN_FAIL
  session%is_null = .true.
  savedata = .false. ! by default
  if(present(save_data)) savedata = save_data
  savemodel = .false.
  if(present(save_model)) savemodel = save_model
  saveresults = .false.
  if(present(save_results)) saveresults = save_results
  if(.not.savedata) then
     call nullify_data(session%dataset, err, rstatus)
     if(rstatus == RETURN_FAIL) goto 100
  end if
  if(.not.savemodel) then
     call nullify_model(session%model, err, rstatus)
     if(rstatus == RETURN_FAIL) goto 100
  end if
  if(.not.savedata) then
     call nullify_results(session%results, err, rstatus)
     if(rstatus == RETURN_FAIL) goto 100
  end if
  ! normal exit
  answer = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end function nullify_tf_session


subroutine nullify_data(data, err, rstatus)
  ! Returns a data_type to its initialized (null) state
  implicit none
  ! declare arguments
  type(data_type), intent(inout) :: data
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "nullify_data"
  ! begin
  rstatus = RETURN_FAIL
  data%is_null = .true.
  data%z= 0.
  data%n = 0.
  data%Ra = 0.
  data%Rb = 0.
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine nullify_data


subroutine nullify_model(model, err, rstatus)
  ! Returns a model_type to its initialized (null) state
  implicit none
  ! declare arguments
  type(data_type), intent(inout) :: model
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "nullify_model"
  ! begin
  rstatus = RETURN_FAIL
  model%is_null = .true.
  model%ndim= 0. ; model%maxits = 0.
  model%eps = 0. ; model%h = 0.
  model%x0 = 0. ; model%xi = 0.
  model%xdphi = 0.
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine nullify_model


subroutine nullify_results(results, err, rstatus)
  ! Returns a results_type to its initialized (null) state
  implicit none
  ! declare arguments
  type(results_type), intent(inout) :: results
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = "nullify_results"
  ! begin
  rstatus = RETURN_FAIL
  results%is_null = .true.
  results%iter = 0
  results%converged = .false.
  results%rmax = 0.
  call dyn_dealloc(results%r, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  call dyn_dealloc(results%phi, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  call dyn_dealloc(results%phip, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  call dyn_dealloc(results%N, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  call dyn_dealloc(results%U, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  call dyn_dealloc(results%Vel, err, rstatus) 
  if(rstatus == RETURN_FAIL) goto 100
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
end subroutine nullify_results
