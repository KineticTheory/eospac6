
#undef DEBUG_WRAP

function ses_format_f90(the_handle)

   implicit none

   SES_LARGE_STRING, target :: ses_format_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   integer :: ses_format
   integer :: return_value
   integer :: my_size, i

#ifdef DEBUG_WRAP
   print *, "ses_format_f90:  the_handle is ", the_handle
#endif
   return_value =  ses_format(the_handle, ses_format_f90, my_size)
#ifdef DEBUG_WRAP
   print *, "ses_format_f90:  size is ", my_size
#endif
   do i = my_size+1, SES_LEN_LARGE
     ses_format_f90(i:i) = ' '
   enddo
      
#ifdef DEBUG_WRAP

   print *, "ses_format_f90:  ses_format_f90 (the string) is ", TRIM(ses_format_f90)
   print *, "ses_format_f90:  return_value is ", return_value
#endif
 
end function ses_format_f90



