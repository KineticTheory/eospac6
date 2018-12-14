

#undef DEBUG_WRAP


SES_ERROR_FLAG function ses_set_table_index_f90(the_handle, date1, date2, version)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   LONG, INTENT(IN) :: date1
   LONG, INTENT(IN) :: date2
   LONG, INTENT(IN) :: version

   SES_BOOLEAN :: return_value, ses_set_table_index

#ifdef DEBUG_WRAP
   print *, "ses_set_table_index_f90:  the_handle is ", the_handle
#endif

   return_value = ses_set_table_index(the_handle, date1, date2, version)

   ses_set_table_index_f90 = return_value

 
end function ses_set_table_index_f90


