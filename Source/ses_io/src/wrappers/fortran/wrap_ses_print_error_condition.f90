

#undef DEBUG_WRAP

function ses_print_error_condition_f90(the_handle)

   implicit none

   SES_LARGE_STRING, target :: ses_print_error_condition_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
 
   integer  :: ses_print_error_condition
   integer  :: return_value
   integer :: i

#ifdef DEBUG_WRAP
   print *, "ses_print_error_condition_f90:  the_handle is ", the_handle
#endif

   return_value = ses_print_error_condition(the_handle, ses_print_error_condition_f90)
   /*  clean out the stuff between the returned size and 
          the ses_large_string size */

   do i = return_value+1, SES_LEN_LARGE    
     ses_print_error_condition_f90(i:i) = ' '
   enddo
#ifdef DEBUG_WRAP
   print *, "ses_print_error_condition_f90:  ses_print_error_condition_f90 (the string) is ", TRIM(ses_print_error_condition_f90)
   print *, "ses_print_error_condition_f90:  return_value is ", return_value
#endif

 
end function ses_print_error_condition_f90


