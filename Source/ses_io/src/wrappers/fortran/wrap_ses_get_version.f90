#undef DEBUG_WRAP

function ses_get_version_f90(the_handle, return_value)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   LONG, INTENT(OUT) :: return_value
   LONG :: ses_get_version, ses_get_version_f90

#ifdef DEBUG_WRAP
   print *, "ses_get_version_f90: calling ses_get_version with the_handle = ", the_handle
#endif

   return_value = ses_get_version(the_handle)

#ifdef DEBUG_WRAP
   print *, "ses_get_version_f90:  return_value is ", return_value
#endif

 
end function ses_get_version_f90
