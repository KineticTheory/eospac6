



SES_BOOLEAN function ses_is_valid_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_NUMBER :: return_value, ses_is_valid

   return_value = ses_is_valid(the_handle)

   ses_is_valid_f90 = return_value

 
end function ses_is_valid_f90


