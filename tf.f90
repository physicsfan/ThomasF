program tf
  !===================================================================
  ! TF: A simple program to determine the Thomas-Fermi potential for
  !     an ion.
  !
  !  Input:  z (nuclear charge) and n (number of electrons < z)
  !          you will be asked to enter guesses for core radius
  !          follow on-screen directions!
  !
  !  Output: three data files
  !          1) phi.dat: r(i) and phi(r(i))
  !          2) nor.dat: r(i) and N(r(i))
  !          3) vel.dat: r(i) and U(r(i))
  !	     where V(r) = -Z/r + U(r)
  !
  ! For non-PC platforms, the value of "platform" may be changed to
  ! "UNIX/LINUX" or "MAC", so that carriage returns are handled
  ! correctly.
  !===================================================================
  use program_constants
  use error_handler
  use tf_types
  use tf_ctrlfile

  implicit none

  ! declare types
  type(error_type) :: err
  type(tf_ctrlfile_type) :: ctrlfile
  type(tf_session_type)  :: session 

  ! declare variables and parameters for the console application
  integer(ip) :: i, rstatus
  real(dp) :: cmd_line_args(4)
  character(len=*), parameter :: platform = "PC"
  character(len=2) :: element
  character(len=5) :: arg
  character(len=256) :: msg_string
  character(len=file_name_length) :: control_file_name

  ! Obtain command-line arguments (if any)
  select case (command_argument_count())
  case(1)
     ! Obtain name and read control file
     call get_command_argument(1,control_file_name)
     !  call read_tf_ctrlfile(control_file_name, ctrlfile, err, rstatus) 
     !  if ( rstatus == RETURN_FAIL) goto 100
  case(5)
     ! Only atomic arguments (first 5) entered on command-line
     call get_command_argument(1,arg)
     element = arg
     do i = 1, 4
        call get_command_argument(i+1,arg)
        read(arg,*) cmd_line_args(i)
     end do
     print '(a,4f8.2)', element, cmd_line_args
     print *, 1001, 1.d-10, 100
     print *, 'phi.dat  ', 'nor.dat  ', 'vel.dat  '
  case default
     ! Query the user for input data for the TF program 
     call prompt_user(ctrlfile, err, rstatus)
     if (rstatus == RETURN_FAIL) goto 100
     print '(a)', ctrlfile%element_name
     print '(4f8.2)', ctrlfile%Z, ctrlfile%N, ctrlfile%ra, &
          ctrlfile%rb
  end select

  ! Calculate Thomas-Fermi potential
  call get_thomas_fermi_potential(session, err, warn, rstatus)
  if (rstatus == RETURN_FAIL) goto 100
  
  ! Generate output files
  print '(a)', 'Generate output files'


  
100 continue
     ! report warnings, if present
!   if(err_msg_present(warn) ) then
!      call err_get_msgs(warn, msg_string, platform)
!      print "(A)", trim(msg_string)
!      print "(A)", ""   ! blank line
!   end if
  ! report "OK" or error message
  if (err_msg_present(err)) then
     call err_get_msgs(err, msg_string, platform)
     print '(a)', trim(msg_string)
     print '(a)', "Aborted"
  else
     print '(a)', "OK"
  end if
end program tf
