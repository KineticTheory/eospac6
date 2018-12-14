



SES_ERROR_FLAG function ses_delete_next_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_ERROR_FLAG :: return_value, ses_delete_next

   return_value = ses_delete_next(the_handle)

   ses_delete_next_f90 = return_value

 
end function ses_delete_next_f90


