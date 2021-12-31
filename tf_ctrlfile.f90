module tf_ctrlfile
  ! This module contains the definition of the tf_ctrlfile_type
  ! and a procedure for reading in the ThomasF control file.
  ! The tf_ctrlfile_type stores the information from the
  ! control file. It's contents will be public so that its
  ! components can easily be used as arguments to call various
  ! procedures. 
  use program_constants
  use error_handler
  use dynamic_allocation
  implicit none

  private ! by default

  ! public types
  public :: tf_ctrlfile_type
  ! public functions and subroutines
  public :: nullify_tf_ctrlfile, prompt_user

  ! private parameters
  character(len=*), parameter :: modname = "tf_ctrlfile"
  
  type :: tf_ctrlfile_type
     ! unlike other types, the constants of this one are public
     sequence
     ! data input section
     character(len=var_name_length) :: element_name = ""
     real(dp) :: z = 0., n = 0.
     real(dp) :: Ra = 0., Rb = 0.
     ! model specification section
     integer(ip) :: ndim = 0
     integer(ip) :: maxits = 0
     real(dp) :: eps = 0.D0
     ! output section
     character(len=file_name_length) :: r_phi_name = ""
     character(len=file_name_length) :: r_N_name   = ""
     character(len=file_name_length) :: r_V_name   = ""
  end type tf_ctrlfile_type


contains

  
  subroutine nullify_tf_ctrlfile(ctrlfile, err, rstatus)
    ! Returns an tf_ctrlfile_type to its initialized (null) state
    implicit none
    ! Arguments
    type(tf_ctrlfile_type), intent(inout) :: ctrlfile
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: rstatus
    ! declare local variables and parameters
    character(len=*), parameter :: subname = &
         "nullify_tf_ctrlfile"
    ! Nullify data section
     ctrlfile%element_name = ""
     ctrlfile%z = 0.
     ctrlfile%n = 0.
     ctrlfile%Ra = 0.
     ctrlfile%Rb = 0.
     ! Nullify model section
     ctrlfile%ndim = 0
     ctrlfile%maxits = 0
     ctrlfile%eps = 0.
     ! Nullify output section
     ctrlfile%r_phi_name = ""
     ctrlfile%r_N_name   = ""
     ctrlfile%r_V_name   = ""
    ! normal exit
    rstatus = RETURN_SUCCESS
    return
    ! error traps
800 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
    rstatus = RETURN_FAIL
  end subroutine nullify_tf_ctrlfile

!=====================================================================================

  subroutine prompt_user(ctrlfile, err, rstatus)
    ! Prompts the user to enter the data from the command-line in steps if the control
    ! file is not entered or command-line arguments not given.
    ! arguments
    type(tf_ctrlfile_type), intent(out) :: ctrlfile
    type(error_type), intent(inout) :: err
    integer(ip), intent(out) :: rstatus
    ! locals
    character(len=var_name_length) :: element_name = ""
    character(len=file_name_length) :: r_phi_name = ""
    character(len=file_name_length) :: r_N_name   = ""
    character(len=file_name_length) :: r_V_name   = ""
    real(dp)    :: z, n, ra, rb
    integer(ip) :: ndim, maxits
    real(dp)    :: eps
    character(len=*), parameter :: subname = "prompt_user"
    Z = 0; N = 0; ra = 0; rb = 0

    ! Get data
    print '(a,/)', 'Data Input:'
    print '(a)', 'Enter the element name (eg: H): '
    read(*,*) element_name
    if (element_name == "") goto 100
    ctrlfile%element_name = element_name
    print '(a)', 'Enter Z and N: '
    read(*,*) Z, N
    if (Z == 0 .or. N == 0) goto 100
    ctrlfile%Z = Z; ctrlfile%N = N
    print '(a)',  'Enter two initial guesses for R : '
    read(*,*) ra, rb
    if (ra == 0 .or. rb == 0) goto 100
    if (ra >= rb) goto 200
    ctrlfile%ra = ra; ctrlfile%rb = rb
    ! Get model
    print '(//,a,/)', 'Model Specification:'

    ! Get output names
    print '(//,a,/)', 'Output Specification:'
    print '(a)', 'Enter filename for R vs PHI:'
    read(*,*) r_phi_name
    if (r_phi_name == "") goto 100
    ctrlfile%r_phi_name = r_phi_name
    print '(a)', 'Enter filename for R vs N:'
    read(*,*) r_N_name
    if (r_N_name == "") goto 100
    ctrlfile%r_N_name = r_N_name
    print '(a)', 'Enter filename for R vs V:'
    read(*,*) r_V_name
    if (r_V_name == "") goto 100
    ctrlfile%r_V_name = r_V_name
    ! normal exit
    rstatus = RETURN_SUCCESS
    return
    ! error traps
100 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No data entered.")
    rstatus = RETURN_FAIL
    return
200 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "Incorrect data entered.")
    rstatus = RETURN_FAIL
    return
  end subroutine prompt_user
  
!=====================================================================================  
!!$  subroutine read_tf_ctrlfile(control_file_name, ctrlfile, err, rstatus)
!!$    ! Reads information from an ThomasF control file and stores it in
!!$    ! an tf_ctrlfile_type. If the read operation fails for any
!!$    ! reason, the tf_ctrlfile_type is nullified and the returned
!!$    ! value is RETURN_FAIL.
!!$    ! arguments
!!$    character(len=*), intent(in) :: control_file_name
!!$    type(tf_ctrlfile_type), intent(out) :: ctrlfile
!!$    type(error_type), intent(inout) :: err
!!$    integer(ip), intent(out) :: rstatus
!!$    ! locals
!!$    integer(ip) :: current_line, i, ijunk, posn, words_in_string
!!$    character(len=ctrl_line_width) :: line
!!$    character(len=*), parameter :: subname = "read_tf_ctrlfile"
!!$
!!$    ! open the control file
!!$    rstatus = RETURN_FAIL
!!$    if (control_file_name == "") goto 700
!!$    open(unit=ctrl_file_handle, file=control_file_name, status="old", err=800)
!!$    current_line = 0
!!$
!!$    ! Dataset input section
!!$    if (skip_comment_lines(ctrl_file_handle, current_line) &
!!$         == RETURN_FAIL) goto 900
!!$
!!$    ! read ncase, nvar, case_id_present
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt='(a)', err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string == 0) goto 910
!!$    if (words_in_string > 1)  goto 920
!!$!    read(line, *, err=900, end=900) ctrlfile%case_id_present
!!$!    read(line, *, err=900, end=900) &
!!$!         ctrlfile%ncase, ctrlfile%nvar, ctrlfile%case_id_present
!!$
!!$    ! read data_file_name and left-justify
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(a)", err=900, end=900) &
!!$         ctrlfile%data_file_name
!!$    ctrlfile%data_file_name = adjustl(ctrlfile%data_file_name)
!!$
!!$    ! read names_file_name and set names_file_present
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(a)", err=900, end=900) &
!!$         ctrlfile%names_file_name
!!$    if (ctrlfile%names_file_name == "") then
!!$       ctrlfile%names_file_present = .false.
!!$    else
!!$       ctrlfile%names_file_present = .true.
!!$       ctrlfile%names_file_name = adjustl(ctrlfile%names_file_name)
!!$    end if
!!$
!!$    ! Model specification section
!!$    if( skip_comment_lines(ctrl_file_handle, current_line) &
!!$         == RETURN_FAIL ) goto 900
!!$
!!$    ! read by_name, grouped
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string < 2) goto 910
!!$    if (words_in_string > 2)  goto 920
!!$    read(line, *, err=900, end=900) ctrlfile%by_name, &
!!$         ctrlfile%grouped
!!$
!!$    ! read yvar, nvar
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string < 2) goto 910
!!$    if (words_in_string > 2)  goto 920
!!$    if( ctrlfile%by_name ) then
!!$       if( ctrlfile%grouped ) then
!!$          if( dyn_alloc(ctrlfile%resp_name, 2, err) == &
!!$               RETURN_FAIL ) goto 800
!!$       else
!!$          if( dyn_alloc(ctrlfile%resp_name, 1, err) == &
!!$               RETURN_FAIL ) goto 800
!!$       end if
!!$       ! set pred_name
!!$       if( line == "" ) goto 900
!!$       line = adjustl(line)
!!$       posn = index(line, " ")
!!$       ctrlfile%resp_name(1) = line(:posn-1)
!!$       line = line(posn:)
!!$       if( ctrlfile%grouped ) then
!!$          if( line == "" ) goto 900
!!$          line = adjustl(line)
!!$          posn = index(line, " ")
!!$          ctrlfile%resp_name(2) = line(:posn-1)
!!$       end if
!!$    else
!!$       ! set pred_col
!!$       if( ctrlfile%grouped ) then
!!$          if( dyn_alloc(ctrlfile%resp_col, 2, err) == &
!!$               RETURN_FAIL ) goto 800
!!$          read(line, *, err=900, end=900) ctrlfile%resp_col(1), &
!!$               ctrlfile%resp_col(2)
!!$       else
!!$          if( dyn_alloc(ctrlfile%resp_col, 1, err) == &
!!$               RETURN_FAIL ) goto 800
!!$          read(line, *, err=900, end=900) ctrlfile%resp_col(1)
!!$       end if
!!$    end if
!!$    
!!$    ! read intercept_present
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string == 0) goto 910
!!$    if (words_in_string > 1)  goto 920
!!$    read(line, *, err=900, end=900) ctrlfile%intercept_present
!!$
!!$    ! read npred
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string == 0) goto 910
!!$    if (words_in_string > 1)  goto 920
!!$    read(line, *, err=900, end=900) ctrlfile%npred
!!$
!!$    ! read predictor variables, if any
!!$    if( ctrlfile%npred > 0 ) then
!!$       if( ctrlfile%by_name ) then
!!$          if( dyn_alloc(ctrlfile%pred_names, ctrlfile%npred, err) &
!!$               == RETURN_FAIL ) goto 800
!!$          do i = 1, ctrlfile%npred
!!$             current_line = current_line + 1
!!$             read(unit=ctrl_file_handle, fmt="(A)", err=900, &
!!$                  end=900) line
!!$             if( line == "" ) goto 900
!!$             line = adjustl(line)
!!$             if( line(1:1) == "*" ) goto 900
!!$             posn = index(line, " ")
!!$             ctrlfile%pred_names(i) = line(:posn-1)
!!$          end do
!!$       else
!!$          if( dyn_alloc(ctrlfile%pred_col, ctrlfile%npred, err) &
!!$               == RETURN_FAIL ) goto 800
!!$          do i = 1, ctrlfile%npred
!!$             current_line = current_line + 1
!!$             read(unit=ctrl_file_handle, fmt="(A)", err=900, &
!!$                  end=900) line
!!$             read(line, *, err=900, end=900) ctrlfile%pred_col(i)
!!$          end do
!!$       end if
!!$    end if
!!$    
!!$    ! Iteration control section
!!$    if( skip_comment_lines(ctrl_file_handle, current_line) &
!!$         == RETURN_FAIL ) goto 900
!!$    
!!$    ! read maxits, eps
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) line
!!$    if (count_words_in_string(line, words_in_string) &
!!$         == RETURN_FAIL) goto 800
!!$    if (words_in_string < 2) goto 910
!!$    if (words_in_string > 2)  goto 920
!!$    read(line, *, err=900, end=900) ctrlfile%maxits, ctrlfile%eps
!!$
!!$    ! Output section
!!$    if( skip_comment_lines(ctrl_file_handle, current_line) &
!!$         == RETURN_FAIL ) goto 900
!!$    
!!$    ! read output_file_name and left-justify
!!$    current_line = current_line + 1
!!$    read(unit=ctrl_file_handle, fmt="(A)", err=900, end=900) &
!!$         ctrlfile%output_file_name
!!$    ctrlfile%output_file_name = adjustl(ctrlfile%output_file_name)
!!$    
!!$    ! normal exit
!!$    close(unit=ctrl_file_handle)
!!$    rstatus = RETURN_SUCCESS
!!$    return
!!$    ! error trap
!!$700 call err_handle(err, 1000, &
!!$         called_from = subname//" in MOD "//modname, &
!!$         custom_1 = "No control, file name specified.")
!!$    goto 999
!!$800 call err_handle(err, 1, &
!!$         called_from = subname//" in MOD "//modname, &
!!$         file_name = control_file_name)
!!$    goto 999
!!$900 call err_handle(err, 3, &
!!$         called_from = subname//" in MOD "//modname, &
!!$         file_name = control_file_name, line_no = current_line)
!!$    goto 999
!!$910 call err_handle(err, 3, &
!!$         called_from = subname//" in MOD "//modname, &
!!$         file_name = control_file_name, line_no = current_line, &
!!$         custom_1 = "Too few arguments present.")
!!$    goto 999
!!$920 call err_handle(err, 3, &
!!$         called_from = subname//" in MOD "//modname, &
!!$         file_name = control_file_name, line_no = current_line, &
!!$         custom_1 = "Too many arguments present.")
!!$    goto 999
!!$    ! final cleanup in the event of an error
!!$999 continue
!!$    close(unit=ctrl_file_handle)
!!$    ijunk = nullify_elogit_ctrlfile(ctrlfile, err)
!!$  end function read_elogit_ctrlfile

!=============================================================================
  
  integer(ip) function skip_comment_lines(file_handle, current_line) &
       result(answer)
    ! Reads and skips over comment lines, if present.
    ! Assumes that the file has already been opened for read-access.
    ! If an error is encountered, this function does not report it;
    ! the error should be reported in the routine from which this
    ! function is called.
    implicit none
    ! arguments
    integer(ip), intent(in)    :: file_handle
    integer(ip), intent(inout) :: current_line
    ! locals
    character(len=ctrl_line_width) :: line

    ! begin
    do
       current_line = current_line + 1
       read(unit=file_handle, fmt='(a)', err=100, end=100) line
       line = adjustl(line)
       if (line(1:1) .eq. "*") then
          cycle
       else
          backspace file_handle
          current_line = current_line - 1
          exit
       end if
    end do
    ! normal exit
    answer = RETURN_SUCCESS
    return
    ! error trap
100 answer = RETURN_FAIL
  end function skip_comment_lines

!==================================================================================

  integer(ip) function count_words_in_string(char_string, word_count) result(answer)
    ! Reads and counts the number of words in line of text string, a
    ! word is text separated by spaces, tabs, commas but not new lines.
    ! If an error is encountered, this function does not report it;
    ! the error should be reported in the routine from which this
    ! function is called.
    implicit none
    ! arguments
    character(len=*), intent(in) :: char_string
    integer(ip), intent(out) :: word_count
    ! local variables and parameters
    integer :: i, string_length
    character(len=1) :: space = " "
    character(len=1) :: comma = ","
    character(len=1) :: tab = achar(9)
    logical :: in_word = .false.
    !begin
    word_count = 0
    string_length = len(char_string)
    if(string_length == 0) goto 100      ! String contains no characters
    do i = 1, string_length
       if ((char_string(i:i).ne.comma).and.(char_string(i:i).ne.space) &
            .and.(char_string(i:i).ne.tab)) then
          ! we are in a word
          in_word = .true.     
       else if (in_word) then
          ! we have just left a word, increment counter
          word_count = word_count + 1
          in_word = .false.
       end if
    end do
    ! normal exit
    answer = RETURN_SUCCESS
    return
    ! error trap
100 answer = RETURN_FAIL
  end function count_words_in_string

!============================================================================

  integer(ip) function count_text_lines(file_handle, number_of_lines, skip_blank_lines) &
       result(answer)
    ! Reads and counts the number of lines of text in the input file,
    ! with the option to skip over blank lines, if desired.
    ! Assumes that the file has already been opened for read-access.
    ! If an error is encountered, this function does not report it;
    ! the error should be reported in the routine from which this
    ! function is called.
    implicit none
    ! arguments
    integer(ip), intent(in) :: file_handle
    integer(ip), intent(out) :: number_of_lines
    logical, intent(in), optional :: skip_blank_lines
    ! local variables and parameters
    character(len=ctrl_line_width) :: line
    logical :: file_open, skip = .false.
    ! begin
    number_of_lines = 0
    if(present(skip_blank_lines)) skip = skip_blank_lines
    ! Check if file is open, return error if not
    inquire(unit=file_handle, opened=file_open)
    if (.not.file_open) goto 200
    ! count lines
    do
       read(unit=file_handle, fmt='(a)', err=200, end=100) line
       line = adjustl(line)
       if(skip .and. (line(1:1).eq." ")) then
          cycle
       else
          number_of_lines = number_of_lines + 1
       end if
    end do
    ! normal exit
    ! bring read position to initial position
100 rewind(unit=file_handle, err=200)  
    answer = RETURN_SUCCESS
    return
    ! error trap
200 answer = RETURN_FAIL
  end function count_text_lines


end module tf_ctrlfile
