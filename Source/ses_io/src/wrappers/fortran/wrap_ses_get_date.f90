#undef DEBUG_WRAP

LONG function ses_get_date_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   LONG :: return_value, ses_get_date_f90
   LONG :: ses_get_date

   return_value = ses_get_date(the_handle)

   ses_get_date_f90 = return_value

 
end function ses_get_date_f90
