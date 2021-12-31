MODULE math_constants
  ! Mathematical constants used throughout the ThomasF program.
  ! Unlike most modules, everything here is public.
  IMPLICIT NONE
  PUBLIC
  ! Numeric constants
  REAL(dp), PARAMETER :: zero   = 0.0_dp
  REAL(dp), PARAMETER :: eighth = 0.125_dp
  REAL(dp), PARAMETER :: half   = 0.5_dp
  REAL(dp), PARAMETER :: one    = 1.0_dp
  REAL(dp), PARAMETER :: minus1 = -1.0_dp  
  REAL(dp), PARAMETER :: two    = 2.0_dp
  REAL(dp), parameter :: four   = 4.0_dp
  REAL(dp), PARAMETER :: six    = 6.0_dp

  REAL(dp), PARAMETER :: root2  = 1.414213562373095_dp
  REAL(dp), PARAMETER :: root3  = 1.732050807568877_dp
  REAL(dp), PARAMETER :: root5  = 2.236067977499790_dp
  
  REAL(dp), PARAMETER :: croot2 = 1.259921049894873_dp
  REAL(dp), PARAMETER :: croot3 = 1.442249570307408_dp

  ! Mathematical constants
  REAL(dp), PARAMETER :: pi  = 3.141592653589793_dp
  REAL(dp), PARAMETER :: phi = 1.618033988749895_dp  ! golden ratio
  REAL(dp), PARAMETER :: e   = 2.718281828459045_dp
  REAL(dp), parameter :: ln2 = 0.693147180559945_dp

  ! Physical constants
  REAL(dp), PARAMETER :: amu  = 1.66d-27_dp  ! atomic mass unit
  REAL(dp), PARAMETER :: alpha = 0.007297_dp ! fine-structure const
  
END MODULE math_constants

