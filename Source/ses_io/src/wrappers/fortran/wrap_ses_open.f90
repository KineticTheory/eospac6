
#undef DEBUG_WRAP


SES_FILE_HANDLE function ses_open_f90(filename, open_flags)

   implicit none

   SES_STRING  :: filename
   SES_OPEN_TYPE, intent(in) :: open_flags
   SES_FILE_HANDLE :: return_value, ses_open
   integer :: j

#ifdef DEBUG_WRAP
   print *, "ses_open_f90: filename is ", TRIM(filename)
#endif
   j = LEN_TRIM(filename)
   filename(j+1:j+1) = CHAR(0)
#ifdef DEBUG_WRAP
   print *, "ses_open_f90: open_flags is ", open_flags
#endif

   return_value = ses_open(TRIM(filename), open_flags)
#ifdef DEBUG_WRAP
   print *, "ses_open_f90:  return_value is ", return_value
   print *, "ses_open_f90:  ses_true is ", SES_TRUE
#endif

   ses_open_f90 = return_value

 
end function ses_open_f90


