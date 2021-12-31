module tf_types
  ! This module defines the public tf_session_type along with
  ! its methods and properties.  To keep this file from getting too
  ! large, we have used include statements to place the procedures
  ! into auxiliary files.
  use program_constants
  use error_handler
  use dynamic_allocation
  implicit none

  private ! by default

  ! public types
  public :: tf_session_type

  ! public methods (more will be added as development continues)
  public :: nullify_tf_session

  
  ! private parameters
  character(len=*), parameter :: modname = "tf_types"


  type :: data_type
     sequence
     private
     logical :: is_null = .true.
     character(len=var_name_length) :: element_name = ""
     real(dp) :: z = 0., n = 0.
     real(dp) :: Ra = 0., Rb = 0.
  end type data_type


  type :: model_type
     sequence
     private
     logical :: is_null = .true.
     integer(ip) :: ndim = 0, maxits = 0
     real(dp) :: eps = 0., h = 0.
     real(dp) :: x0 = 0., xi = 0., xdphi = 0.
  end type model_type


  type :: results_type
     sequence
     private
     logical :: is_null = .true.
     integer(ip) :: iter = 0
     logical :: converged = .false.
     real(dp) :: rmax = 0.
     real(dp), pointer :: r(:)    => null()
     real(dp), pointer :: phi(:)  => null()
     real(dp), pointer :: phip(:) => null()
     real(dp), pointer :: N(:)    => null()
     real(dp), pointer :: U(:)    => null()
     real(dp), pointer :: Vel(:)  => null()
  end type results_type
  
  
  type :: tf_session_type
     sequence
     private
     logical :: is_null = .true.
     type(data_type)    :: data
     type(model_type)   :: model
     type(results_type) :: results
  end type tf_session_type

  ! interface blocks for overloading functions
  
contains

  ! Auxilliary files of puts and gets
  include 'tf_nullify.f90'
  include 'tf_puts.f90'
  include 'tf_gets.f90'
  include 'tf_io.f90'
  include 'tf_modelfit.f90'
  

end module tf_types
