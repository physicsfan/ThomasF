module dynamic_allocation
  ! Module for allocating and deallocating all pointers in the
  ! RHF program.  This includes integer, real and character arrays
  ! of sizes 1D, 2D and 3D.
  use program_constants
  use error_handler
  implicit none

  private   ! by default

  ! public methods
  public :: dyn_alloc, dyn_dealloc


  ! private parameters
  character(len=*), parameter :: modname = "dynamic_allocation"

  ! overload allocation
  interface dyn_alloc
     module procedure int1_alloc
     module procedure int2_alloc
     module procedure int3_alloc
     module procedure dbl1_alloc
     module procedure dbl2_alloc
     module procedure dbl3_alloc
     module procedure str1_alloc
     module procedure str2_alloc
     module procedure str3_alloc
  end interface dyn_alloc
  
  ! overload deallocation
  interface dyn_dealloc
     module procedure int1_dealloc
     module procedure int2_dealloc
     module procedure int3_dealloc
     module procedure dbl1_dealloc
     module procedure dbl2_dealloc
     module procedure dbl3_dealloc
     module procedure str1_dealloc
     module procedure str2_dealloc
     module procedure str3_dealloc
  end interface dyn_dealloc

contains

  ! allocators
  subroutine int1_alloc(intArray, dim1, err, return_status)
    ! allocates an integer array of rank 1
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:)
    integer, intent(in) :: dim1
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int1_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    allocate(intArray(dim1), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int1_alloc
!
!==============================================================================
!  
  subroutine int2_alloc(intArray, dim1, dim2, err, return_status)
    ! allocates an integer array of rank 2
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:,:)
    integer, intent(in) :: dim1, dim2
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int2_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    allocate(intArray(dim1, dim2), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int2_alloc
!
!=================================================================================  
!
  subroutine int3_alloc(intArray, dim1, dim2, dim3, err, return_status)
    ! allocates an integer array of rank 3
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:,:,:)
    integer, intent(in) :: dim1, dim2, dim3
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int3_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    allocate(intArray(dim1, dim2, dim3), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int3_alloc
!
!=================================================================================  
!  
  subroutine dbl1_alloc(dblArray, dim1, err, return_status)
    ! allocates an double precision array of rank 1
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:)
    integer, intent(in) :: dim1
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl1_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    allocate(dblArray(dim1), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl1_alloc
!
!=================================================================================  
!
  subroutine dbl2_alloc(dblArray, dim1, dim2, err, return_status)
    ! allocates an double precision array of rank 2
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:,:)
    integer, intent(in) :: dim1, dim2
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl2_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    allocate(dblArray(dim1, dim2), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl2_alloc
!
!=================================================================================  
!
  subroutine dbl3_alloc(dblArray, dim1, dim2, dim3, err, return_status)
    ! allocates an double precision array of rank 3
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:,:,:)
    integer, intent(in) :: dim1, dim2, dim3
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl3_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    allocate(dblArray(dim1, dim2, dim3), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl3_alloc
!
!=================================================================================  
!
  subroutine str1_alloc(strArray, dim1, err, return_status)
    ! allocates an character-string array of rank 1
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:)
    integer, intent(in) :: dim1
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str1_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    allocate(strArray(dim1), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str1_alloc
!
!=================================================================================  
!  
  subroutine str2_alloc(strArray, dim1, dim2, err, return_status)
    ! allocates an character-string array of rank 2
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:,:)
    integer, intent(in) :: dim1, dim2
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str2_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    allocate(strArray(dim1, dim2), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str2_alloc
!
!=================================================================================  
!
  subroutine str3_alloc(strArray, dim1, dim2, dim3, err, return_status)
    ! allocates an character-string array of rank 3
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:,:,:)
    integer, intent(in) :: dim1, dim2, dim3
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str3_alloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    allocate(strArray(dim1, dim2, dim3), stat=status)
    if (status /= 0) goto 200
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
200 call err_handle(err, 200, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str3_alloc

  
  ! deallocators
  subroutine int1_dealloc(intArray, err, return_status)
    ! deallocates an integer array of rank 1
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int1_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int1_dealloc
!
!=================================================================================  
! 
  subroutine int2_dealloc(intArray, err, return_status)
    ! deallocates an integer array of rank 2
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int2_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int2_dealloc
!
!=================================================================================  
!  
  subroutine int3_dealloc(intArray, err, return_status)
    ! deallocates an integer array of rank 3
    implicit none
    ! arguments
    integer(ip), pointer :: intArray(:,:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "int3_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(intArray)) deallocate(intArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine int3_dealloc
!
!=================================================================================  
!  
  subroutine dbl1_dealloc(dblArray, err, return_status)
    ! deallocates an double precision array of rank 1
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl1_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl1_dealloc
!
!=================================================================================  
!  
  subroutine dbl2_dealloc(dblArray, err, return_status)
    ! dallocates an double precision array of rank 2
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl2_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl2_dealloc
!
!=================================================================================  
!
  subroutine dbl3_dealloc(dblArray, err, return_status)
    ! deallocates an double precision array of rank 3
    implicit none
    ! arguments
    real(dp), pointer :: dblArray(:,:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "dbl3_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(dblArray)) deallocate(dblArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine dbl3_dealloc
!
!=================================================================================  
!  
  subroutine str1_dealloc(strArray, err, return_status)
    ! deallocates an character-string array of rank 1
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str1_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str1_dealloc
!
!=================================================================================  
!  
  subroutine str2_dealloc(strArray, err, return_status)
    ! deallocates an character-string array of rank 2
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str2_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str2_dealloc
!
!=================================================================================  
!
  subroutine str3_dealloc(strArray, err, return_status)
    ! deallocates an character-string array of rank 3
    implicit none
    ! arguments
    character(len=*), pointer :: strArray(:,:,:)
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: return_status
    ! locals
    integer :: status = 0
    character(len=*), parameter :: subname = "str3_dealloc"
    ! begin
    return_status = RETURN_FAIL
    if (associated(strArray)) deallocate(strArray, stat=status)
    if (status /= 0) goto 100
    ! normal exit
    return_status = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 201, &
         called_from = subname//" in MOD "//modname)
    return
  end subroutine str3_dealloc

end module dynamic_allocation
