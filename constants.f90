MODULE program_constants
  ! Programming constants used throughout the ThomasF program.
  ! Unlike most modules, everything here is public.
  IMPLICIT NONE
  PUBLIC
  ! Define compiler-specific KIND numbers for integers,
  ! single and double-precision reals to help ensure cosistency of
  ! performance across platforms:
  INTEGER, PARAMETER :: ip = selected_int_KIND(9)
  INTEGER, PARAMETER :: sp = selected_real_KIND(6,37)
  INTEGER, PARAMETER :: dp = selected_real_KIND(15,307)
  
  ! Define UNIT numbers for Fortran I/O:
  INTEGER, PARAMETER :: ctrl_file_handle  = 11
  INTEGER, PARAMETER :: data_file_handle  = 12
  INTEGER, PARAMETER :: names_file_handle = 13
  
  ! Define maximum lengths for various types of character strings:
  INTEGER, PARAMETER :: file_name_length = 256
  INTEGER, PARAMETER :: var_name_length  = 8
  INTEGER, PARAMETER :: in_data_length   = 8
  
  ! Define the maximum line widths for various types of files:
  INTEGER, PARAMETER :: ctrl_line_width  = 80
  INTEGER, PARAMETER :: data_line_width  = 1024
  INTEGER, PARAMETER :: names_line_width = 80

  ! Common integer values returned by all functions to indicate
  ! success of failure:
  INTEGER(ip), PARAMETER :: RETURN_SUCCESS = 0
  INTEGER(ip), PARAMETER :: RETURN_FAIL    = -1

  ! Numeric constants that are repetedly used
  REAL(dp),    PARAMETER :: six    = 6.0_dp
  REAL(dp),    PARAMETER :: two    = 2.0_dp
  REAL(dp),    PARAMETER :: half   = 0.5_dp
  REAL(dp),    PARAMETER :: eighth = 0.125_dp
  REAL(dp),    PARAMETER :: zero   = 0.0_dp

  ! Character strings describing this program:
  character(len=*), parameter :: &
       program_name = 'ThomasF', &
       program_description = &
       'A simple program for calculating the Thomas-Fermi potential', &
       program_version = 'Version 1.0', &
       program_date = 'June, 2021', &
       program_author = 'Written by A.S.M. Senchuk'

end module program_constants
