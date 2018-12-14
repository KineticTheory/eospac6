

#undef DEBUG_WRAP

function ses_version_f90(the_handle)

   implicit none

   SES_LARGE_STRING, target :: ses_version_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   integer :: ses_version
   integer :: return_value


#ifdef DEBUG_WRAP
   print *, "ses_version_f90:  the_handle is ", the_handle
#endif
   return_value =  ses_version(the_handle, ses_version_f90)
   /*  clean out the stuff between the returned size and 
          the ses_large_string size */

   ses_version_f90(return_value+1:SES_LEN_LARGE) = ''


#ifdef DEBUG_WRAP
   print *, "ses_version_f90:  ses_version_f90 (the string) is ", TRIM(ses_version_f90)
   print *, "ses_version_f90:  return_value is ", return_value
#endif

   

 
end function ses_version_f90


