



SES_ERROR_FLAG function ses_delete_table_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_ERROR_FLAG :: return_value, ses_delete_table

   return_value = ses_delete_table(the_handle)
   ses_delete_table_f90 = return_value

 
end function ses_delete_table_f90


