#undef DEBUG_WRAP

function ses_get_date_f90(the_handle, return_value)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   LONG, INTENT(OUT) :: return_value
   LONG :: ses_get_date, ses_get_date_f90

   return_value = ses_get_date(the_handle)

 
end function ses_get_date_f90
