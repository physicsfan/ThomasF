! File I/O methods for the ELOGIT types module
subroutine skip_comment_lines(file_handle, current_line, rstatus) 
  ! Reads and skips over comment lines, if present.
  ! Assumes that the file has already been opened for read-access.
  ! If an error is encountered, this function does not report it;
  ! the error should be reported in the routine from which this
  ! function is called.
  implicit none
  ! arguments
  integer(ip), intent(in) :: file_handle
  integer(ip), intent(inout) :: current_line
  integer(ip), intent(out) :: rstatus
  ! local variables and parameters
  character(len=ctrl_line_width) :: line
  ! begin
  do
     current_line = current_line + 1
     read(unit=file_handle, fmt='(a)', err=900, end=900) line
     line = adjustl(line)
     if(line(1:1).eq."*") then
        cycle
     else
        backspace file_handle
        current_line = current_line - 1
        exit
     end if
  end do
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error trap
900 rstatus = RETURN_FAIL
end subroutine skip_comment_lines

!===================================================================================

subroutine count_words_in_text(char_string, word_count, rstatus)
  ! Reads and counts the number of words in line of text string, a
  ! word is text separated by spaces, tabs, commas but not new lines.
  ! If an error is encountered, this function does not report it;
  ! the error should be reported in the routine from which this
  ! function is called.
  implicit none
  ! arguments
  character(len=*), intent(in) :: char_string
  integer(ip), intent(out) :: word_count
  integer(ip), intent(out) :: rstatus
  ! local variables and parameters
  integer :: i, string_length
  character(len=1) :: space = " "
  character(len=1) :: comma = ","
  character(len=1) :: tab = achar(9)
  logical :: in_word = .false.
  !begin
  word_count = 0
  string_length = len(char_string)
  if(string_length == 0) goto 900      ! String contains no characters
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
  rstatus = RETURN_SUCCESS
  return
  ! error trap
900 rstatus = RETURN_FAIL
end subroutine count_words_in_text

!============================================================================

subroutine count_text_lines(file_handle, number_of_lines, rstatus, skip_blank_lines)
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
  integer(ip), intent(out) :: rstatus
  logical, intent(in), optional :: skip_blank_lines
  ! local variables and parameters
  character(len=ctrl_line_width) :: line
  logical :: file_open, skip = .false.
  ! begin
  number_of_lines = 0
  if(present(skip_blank_lines)) skip = skip_blank_lines
  ! Check if file is open, return error if not
  inquire(unit=file_handle, opened=file_open)
  if (.not.file_open) goto 900
  ! count lines
  do
     read(unit=file_handle, fmt='(a)', err=900, end=800) line
     line = adjustl(line)
     if(skip .and. (line(1:1).eq." ")) then
        cycle
     else
        number_of_lines = number_of_lines + 1
     end if
  end do
  ! normal exit
  ! bring read position to initial position
800 rewind(unit=file_handle, err=900)  
  rstatus = RETURN_SUCCESS
  return
  ! error trap
900 rstatus = RETURN_FAIL
end subroutine count_text_lines

!===========================================================================

subroutine write_thomasf_results_to_outfile(session, err, output_file_name,
  rstatus)
  ! writes a formatted summary of results to the output file
  implicit none
  ! declare arguments
  character(len=file_name_length), intent(in) :: output_file_name
  type(elogit_session_type), intent(in) :: session
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  integer(ip) :: i, ncase, nvar, iter, df
  logical :: converged
  character(len=var_name_length), pointer :: var_names(:)=>null()
  real(dp), pointer :: beta(:)=>null(), &
       cov_beta(:,:)=>null()
  real(dp) :: est, SE, ratio, loglik, X2, G2
  character(len=12) :: sInt, sRealA, sRealB, sRealC
  character(len=var_name_length), parameter :: var_blank=""
  character(len=*), parameter :: subname = &
       "write_elogit_results_to_outfile"
  ! check arguments
  rstatus = RETURN_FAIL
  if(output_file_name == "") goto 700
  ! open output file
  open(unit=out_file_handle, file=output_file_name, &
       status="REPLACE", action="WRITE", err=800)
  ! stamp output file with program information
  if( write_program_info_to_outfile(out_file_handle, err) &
       == RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! stamp output file with time and date
  if( write_date_and_time_to_outfile(out_file_handle, err) &
       == RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! Write information about data set
  if( get_elogit_ncase(ncase, session, err) == RETURN_FAIL) &
       goto 990
  if( get_elogit_nvar(nvar, session, err) == RETURN_FAIL) &
       goto 990
  if( get_elogit_var_names(var_names, session, err) == &
       RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "Data set information"
  write(sInt,"(I12)") ncase
  sInt = adjustl(sInt)
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   Number of cases:       " // trim(sInt)
  write(sInt,"(I12)") nvar
  sInt = adjustl(sInt)
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   Number of variables:   " // trim(sInt)
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! List the variables
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   Variables"
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   -------------"
  do i = 1, nvar
     write(unit=out_file_handle, fmt="(3X,I3,2X,A)", err=820) &
          i, trim( var_names(i) )
  end do
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! Report model specification
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "Model specification"
  if( get_elogit_response(var_names, session, err) &
       == RETURN_FAIL ) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   Response (y):     " // trim(var_names(1))
  if( size(var_names) > 1 ) then
     write(unit=out_file_handle, fmt="(A)", err=820) &
          "   Denominator (n):  " // trim(var_names(2))
  end if
  if( get_elogit_beta_names(var_names, session, err) &
       == RETURN_FAIL) goto 990
  if( associated(var_names) ) then
     write(unit=out_file_handle, fmt="(A)", err=820) &
          "   Predictors:       " // trim(var_names(1))
     do i = 2, size(var_names)
        write(unit=out_file_handle, fmt="(A)", err=820) &
             "                     " // trim(var_names(i))
     end do
  end if
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! report iteration details
  if( get_elogit_iter(iter, session, err) == RETURN_FAIL) goto 990
  if( get_elogit_converged(converged, session, err) &
       == RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "Iteratively reweighted least-squares algorithm" 
  write(sInt,"(I12)") iter
  sInt = adjustl(sInt)
  if(converged) then
     write(unit=out_file_handle, fmt="(A)", err=820) &
          "   Converged at iteration " // trim(sInt)
  else
     write(unit=out_file_handle, fmt="(A)", err=820) &
          "   Failed to converge by iteration " // trim(sInt)
  end if
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! report coefficients and standard errors
  if( get_elogit_beta(beta, session, err) &
       == RETURN_FAIL) goto 990
  if( get_elogit_cov_beta(cov_beta, session, err) &
       == RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   " // var_blank // "  " // &
       "  estimate      std.err.      ratio"
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "   " // var_blank // "  " // &
       "------------  ------------  ------------"
  do i = 1, size(beta)
     est = beta(i)
     SE = sqrt( cov_beta(i,i) )
     ratio = est/SE
     write(sRealA,"(G12.5)") est
     write(sRealB,"(G12.5)") SE
     write(sRealC,"(G12.5)") ratio
     write(unit=out_file_handle, fmt="(A)", err=820) &
          "   " // var_names(i) // "  " // &
          sRealA // "  " // sRealB // "  " // sRealC
  end do
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  ! report goodness-of-fit measures
  if( get_elogit_loglik(loglik, session, err) &
       == RETURN_FAIL) goto 990
  if( get_elogit_X2(X2, session, err) == RETURN_FAIL) goto 990
  if( get_elogit_G2(G2, session, err) == RETURN_FAIL) goto 990
  if( get_elogit_df(df, session, err) == RETURN_FAIL) goto 990
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "Summary of model fit"
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  write(unit=out_file_handle, fmt="(A20,G15.8)", err=820) &
       "   Loglikelihood:  ", loglik
  write(unit=out_file_handle, fmt="(A20,G15.8)", err=820) &
       "   Deviance G^2:   ", G2
  write(unit=out_file_handle, fmt="(A20,G15.8)", err=820) &
       "   Pearson's X^2:  ", X2
  write(unit=out_file_handle, fmt="(A)", err=820) "" ! blank line
  write(sInt,"(I12)") df
  sInt = adjustl(sInt)
  write(unit=out_file_handle, fmt="(A)", err=820) &
       "    Degrees of freedom: " // trim(sInt)
  ! normal exit
  close(unit=out_file_handle)
  rstatus = RETURN_SUCCESS
  return
  ! error traps
700 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname, &
         custom_1 = "No output file name specified.")
  return
800 call err_handle(err, 2, &
         called_from = subname//" in MOD "//modname, &
         file_name = output_file_name)
  return
820 call err_handle(err, 4, &
         called_from = subname//" in MOD "//modname, &
         file_name = output_file_name)
  close(unit=out_file_handle)
  return
990 call err_handle(err, 1000, &
         called_from = subname//" in MOD "//modname)
  close(unit=out_file_handle)
  return
end subroutine write_thomasf_results_to_outfile

!==================================================================================

subroutine write_program_info_to_outfile(file_handle, err, rstatus)
  ! Stamps output file with program information
  ! This routine assumes that unit file_handle is already
  ! connected.
  implicit none
  integer(ip), intent(in) :: file_handle
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! local variables
  character(len=*), parameter :: subname = &
       "write_program_info_to_outfile"
  integer(kind=ip) :: i
  character(len=out_line_width) :: line_buffer
  character(len=1), parameter :: sFill = "#"
  integer(ip) :: stamp_width
  ! find stamp_width
  rstatus = RETURN_FAIL
  stamp_width = 0
  stamp_width = max(stamp_width, len(program_name))
  stamp_width = max(stamp_width, len(program_description))
!   stamp_width = max(stamp_width, len(program_version_and_date))
  stamp_width = max(stamp_width, len(program_date))
  stamp_width = max(stamp_width, len(program_version))
!  stamp_width = max(stamp_width, len(program_institution_1))
!  stamp_width = max(stamp_width, len(program_institution_2))
  stamp_width = stamp_width + 8
  ! do it
  line_buffer = ""
  do i = 1, stamp_width
     write(line_buffer(i:i), "(A)", err=810) sFill
  end do
  write(unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  write(line_buffer(1:4), "(A)", err=810) sFill
  write(line_buffer(stamp_width:stamp_width), &
       "(A)", err=810) sFill
  write( line_buffer(5:stamp_width-1), "(A)", err=810) &
       program_name
  write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  write( line_buffer(5:stamp_width-1), "(A)", err=810) &
       program_description
  write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
!  write( line_buffer(5:stamp_width-1), "(A)", err=810) &
!       program_version_and_date
!  write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  write( line_buffer(5:stamp_width-1), "(A)", err=810) &
       program_author
  write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
! write( line_buffer(5:stamp_width-1), "(A)", err=810) &
!      program_institution_1
! write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
! write( line_buffer(5:stamp_width-1), "(A)", err=810) &
!      program_institution_2
!  write( unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  do i = 1, stamp_width
     write(line_buffer(i:i), "(A)", err=810) sFill
  end do
  do i = 1, stamp_width
     write(line_buffer(i:i), "(A)", err=810) sFill
  end do
  write(unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
810 call err_handle(err, 4, &
         called_from = subname//" in MOD "//modname, &
         file_name = "internal: line_buffer")
  return
820 call err_handle(err, 4, &
         called_from = subname//" in MOD "//modname)
  return
end subroutine write_program_info_to_outfile

!===============================================================================

subroutine write_date_and_time_to_outfile(file_handle, err, rstatus)
  ! Stamps output file with time and date
  ! This routine assumes that unit file_handle is already
  ! connected.
  implicit none
  ! declare arguments
  integer(ip), intent(in) :: file_handle
  type(error_type), intent(inout) :: err
  integer(ip), intent(out) :: rstatus
  ! declare local variables and parameters
  character(len=*), parameter :: subname = &
       "write_date_and_time_to_outfile"
  character(len=out_line_width) :: line_buffer
  character(len=1), parameter :: sFill = "#"
  character(len=8) :: sDate
  character(len=4) :: sYear
  character(len=9) :: sMonth
  character(len=2) :: sDay
  character(len=10) :: sTime
  ! begin
  rstatus = RETURN_FAIL
  call date_and_time(sDate,sTime)
  sYear = sDate(1:4)
  select case(sDate(5:6))
  case("01")
     sMonth = "January"
  case("02")
     sMonth = "February"
  case("03")
     sMonth = "March"
  case("04")
     sMonth = "April"
  case("05")
     sMonth = "May"
  case("06")
     sMonth = "June"
  case("07")
     sMonth = "July"
  case("08")
     sMonth = "August"
  case("09")
     sMonth = "September"
  case("10")
     sMonth = "October"
  case("11")
     sMonth = "November"
  case("12")
     sMonth = "December"
  end select
  sDay = sDate(7:8)
  line_buffer = sDay // " " // trim(sMonth) // ", " // sYear
  write(unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  line_buffer = sTime(1:2) // ":" // sTime(3:4) // ":" &
       // sTime(5:6)
  write(unit=file_handle, fmt="(A)", err=820) trim(line_buffer)
  ! normal exit
  rstatus = RETURN_SUCCESS
  return
  ! error traps
820 call err_handle(err, 4, &
         called_from = subname//" in MOD "//modname)
  return
end subroutine write_date_and_time_to_outfile

!================================================================================

subroutine create_gnuplot_file()

  

end subroutine create_gnuplot_file
