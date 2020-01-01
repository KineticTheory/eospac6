

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_close_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_ERROR_FLAG :: return_value, ses_close_c


#ifdef DEBUG_WRAP
   print *, "ses_close_f90:  the_handle is ", the_handle
#endif

   return_value = ses_close_c(the_handle)

#ifdef DEBUG_WRAP
   print *, "ses_close_f90:  return_value is ", return_value
#endif

   ses_close_f90 = return_value

 
end function ses_close_f90


