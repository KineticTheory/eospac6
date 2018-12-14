

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_set_validate_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_ERROR_FLAG :: return_value, ses_set_validate
#ifdef DEBUG_WRAP
   print *, "ses_set_validate_f90:  the_handle is ", the_handle
#endif

   return_value = ses_set_validate(the_handle)

#ifdef DEBUG_WRAP
   print *, "ses_set_validate_f90:  return_value is ", return_value
#endif

   ses_set_validate_f90 = return_value

 
end function ses_set_validate_f90


