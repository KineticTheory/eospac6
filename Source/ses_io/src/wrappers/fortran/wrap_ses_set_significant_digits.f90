

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_set_significant_digits_f90(the_handle, number_digits)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_NUMBER, intent(IN) :: number_digits

   SES_ERROR_FLAG :: return_value, ses_set_significant_digits
#ifdef DEBUG_WRAP
   print *, "ses_set_significant_digits:  the_handle is ", the_handle
   print *, "ses_set_significant_digits:  number_digits is ", number_digits
#endif


   return_value = ses_set_significant_digits(the_handle, number_digits)

#ifdef DEBUG_WRAP
   print *, "ses_set_significant_digits:  return_value is ", return_value
#endif

   ses_set_significant_digits_f90 = return_value

end function ses_set_significant_digits_f90


