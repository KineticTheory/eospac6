



SES_BOOLEAN function ses_has_next_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_BOOLEAN :: return_value, ses_has_next


   return_value = ses_has_next(the_handle)

   ses_has_next_f90 = return_value

 
end function ses_has_next_f90


