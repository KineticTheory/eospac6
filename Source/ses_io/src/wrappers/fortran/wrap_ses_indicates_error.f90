



SES_BOOLEAN function ses_indicates_error_f90(the_error_flag)

   implicit none

   SES_ERROR_FLAG, INTENT(IN) :: the_error_flag
   SES_BOOLEAN :: return_value, ses_indicates_error


   return_value = ses_indicates_error(the_error_flag)

   ses_indicates_error_f90 = return_value

 
end function ses_indicates_error_f90


