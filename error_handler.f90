!#######################################################################################
module error_handler
  ! Generic error message handler for both console and non-console
  ! applications. The routines in this module do not halt program
  ! execution; they merely store error messages for subsequent
  ! retrieval.
  ! Written by ASMS, Date 05/27/19
  implicit none

  private ! by default

  ! declare public types
  public :: error_type

  ! declare public subroutines and functions
  public :: err_reset, err_handle, err_msg_present, err_get_msgs

  ! Parameters private to this module
  integer, parameter :: err_msg_width = 70 ! max width if any single error message line
  
  !####################################################################################

  type :: msg_line_type
     ! Private type for a single node in the linked list
     sequence
     character (len=err_msg_width) :: line = ""
     type(msg_line_type), pointer :: next => null()
  end type msg_line_type

  !####################################################################################

  type :: error_type
     ! Public type for holding a linked list of messages
     sequence
     private ! contents of this type are private to this module
     logical :: msg_present = .false.
     type(msg_line_type), pointer :: head => null(), tail => null()
  end type error_type

  !####################################################################################

contains

  !####################################################################################
  ! all subroutines and functions will be placed here
  !####################################################################################

  subroutine err_get_msgs(err, msg_string, platform)
    ! Public: Retrieves all stored messages as a single character
    ! string, with message lines separated by platform-appropriate
    ! ASCII carriage control characters.
    ! Values for platform may be "UNIX", "MAC" or "PC"
    implicit none

    ! required arguments
    type(error_type), intent(inout) :: err
    character(len=*), intent(out) :: msg_string

    ! optional arguments
    character(len=*), intent(in), optional :: platform

    ! local variables
    character(len=4) :: plat
    integer :: posn
    logical :: first_time
    type(msg_line_type), pointer :: cur_line

    ! determine platform
    if(present(platform)) then
       plat = platform
    else
       plat = "PC"
    end if

    ! clean out msg_string
    msg_string = ""

    ! step through the linked list, appending the lines
    first_time = .true.
    cur_line => err%head
    do
       if(.not.associated(cur_line)) exit
       posn = len_trim(msg_string)
       if((posn+3) >= len(msg_string)) exit  ! out of space
       posn = posn + 1
       if(.not.first_time) then
          select case(plat)
          case("UNIX")
             ! Separate lines with LF
             msg_string(posn:) = achar(10)
             posn = posn + 1
          case("MAC")
             ! Separate lines with CR
             msg_string(posn:) = achar(13)
             posn = posn + 1
          case default
             msg_string(posn:) = achar(13) // achar(10)
             posn = posn + 2
          end select
       end if
       msg_string(posn:) = trim(cur_line%line)
       first_time = .false.
       cur_line => cur_line%next
    end do
  end subroutine err_get_msgs

  !######################################################################################
  
  subroutine err_handle(err, err_code, called_from, file_name, &
       line_no, object_name, custom_1, custom_2, custom_3)
    ! Public: Stores a message in the error handler
    ! Meaning of err_code
    !     0 = no error
    ! 1-99: I/O errors
    !     1 = file could not be opened for read-access
    !     2 = file could not be opened for write-access
    !     3 = error in reading file
    !     4 = error in writing to file
    !     5 = error in reading file: EOR/EOF encountered
    !     6 = file open fo write-access could not be closed
    !     7 = file open for read-access could not be closed
    ! 100-199: numerical error
    !   100 = matrix apparently singular
    !   101 = matrix not positive definite
    !   102 = attempted division by zero
    !   103 = attempted logarithm of non-positive number
    !   104 = argument to exp function too large
    !   105 = attempted square root of negative number
    ! 200-299: memory errors
    !   200 = unable to dynamically allocate memory for object
    !   201 = unable to deallocate memory for object
    ! 300-399: array dimension errors
    !   300 = non-square matrix encountered where square matrix expected
    !   301 = dimensions of matrix arguments not conformable
    ! 1000: other error
    !  1000 = reserved for custom error messages
    implicit none

    ! declare required arguments
    type(error_type), intent(inout) :: err
    integer, intent(in) :: err_code

    ! declare optional arguments
    character(len=*), optional :: called_from, file_name, &
         object_name, custom_1, custom_2, custom_3
    integer , optional :: line_no

    ! local variables
    character(len=12) :: ichar

    ! begin
    select case(err_code)
    case(0)
       call insert_msg_line("No errors", err)

    ! I/O errors
    case(1)
       call insert_msg_line("File could not be opened for read-access", err)
    case(2)
       call insert_msg_line("File could not be opened for write-access", err)
    case(3)
       call insert_msg_line("Error in reading file", err)
    case(4)
       call insert_msg_line("Error in writing to file", err)
    case(5)
       call insert_msg_line("Error in reading file: EOR/EOF encountered", err)
    case(6)
       call insert_msg_line("File open fo write-access could not be closed", err)
    case(7)
       call insert_msg_line("File open for read-access could not be closed", err)

    ! 100-199: numerical error
    case(100)
       call insert_msg_line("Matrix apparently singular", err)
    case(101)
       call insert_msg_line("Matrix not positive definite", err)
    case(102)
       call insert_msg_line("Attempted division by zero", err)
    case(103)
       call insert_msg_line("Attempted logarithm of non-positive number", err)
    case(104)
       call insert_msg_line("Argument to exp function too large", err)
    case(105)
       call insert_msg_line("Attempted square root of negative number", err)

    ! 200-299: memory errors
    case(200)
       call insert_msg_line("Unable to dynamically allocate memory for object", err)
    case(201)
       call insert_msg_line("Unable to deallocate memory for object", err)
       
    ! 300-399: array dimension errors 
    case(300)
       call insert_msg_line("Non-square matrix encountered where square matrix expected", err)
    case(301)
       call insert_msg_line("Dimensions of matrix arguments not conformable", err)
       
    ! custom error message
    case(1000)
       ! don't do anything yet
    ! anything else
    case default
       call insert_msg_line("Unknown error code.", err)
    end select

    ! append other optional information if present
    if(present(custom_1)) &
         call insert_msg_line(custom_1, err) 
    if(present(custom_2)) &
         call insert_msg_line(custom_2, err)
    if(present(custom_3)) &
         call insert_msg_line(custom_3, err)
    if(present(file_name)) &
         call insert_msg_line("FILE: " // trim(file_name), err)
    if(present(line_no)) then
       write(ichar,"(i12)") line_no
       ichar = adjustl(ichar)
       call insert_msg_line("LINE: " // trim(ichar), err)
    end if
    if(present(object_name)) &
         call insert_msg_line(trim(object_name), err)
    if(present(called_from)) &
         call insert_msg_line("OCCURED IN: " // trim(called_from), err)
  end subroutine err_handle

  !####################################################################################

  logical function err_msg_present(err)
    ! Public: Queries the error_type to see if a message is present
    implicit none
    type(error_type), intent(inout) :: err
    err_msg_present = err%msg_present
  end function err_msg_present
  
  !####################################################################################
  
  subroutine err_reset(err)
    ! Public: deletes all messages from the list
    implicit none
    type(error_type), intent(inout) :: err
    type(msg_line_type), pointer :: current_line
    if(.not. err%msg_present) return
    do
       current_line => err%head
       err%head => err%head%next
       deallocate(current_line)
       if(.not. associated(err%head)) exit
    end do
    nullify(err%tail)
    err%msg_present = .false.
  end subroutine err_reset

  !####################################################################################

  subroutine insert_msg_line(text_line, err)
    ! inserts a new message line at the end of the list
    implicit none
    ! declare arguments
    character(len=*), intent(in) :: text_line
    type(error_type), intent(inout) :: err
    ! begin
    if(.not. err%msg_present) then
       ! begin a linked list
       allocate(err%head)
       err%tail => err%head
       err%head%line = text_line
       err%msg_present = .true.
    else
       ! add a node to the list; point tail to the new node
       allocate(err%tail%next)
       err%tail => err%tail%next
       err%tail%line = text_line
    end if
  end subroutine insert_msg_line

  !####################################################################################

  
end module error_handler
!######################################################################################
