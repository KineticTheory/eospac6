

#undef DEBUG_WRAP

function ses_set_version_f90(the_handle, the_version)

   implicit none

   SES_ERROR_FLAG :: ses_set_version_f90
   LONG :: the_version

   SES_FILE_HANDLE, INTENT(IN) :: the_handle

   long :: ses_version
 
 
#ifdef DEBUG_WRAP
   print *, "ses_set_version_f90:  the_handle is ", the_handle
   print *, "ses_set_version_f90:  the_version is ", the_version
#endif
   ses_set_version_f90 =  ses_set_version(the_handle, the_version)
   
#ifdef DEBUG_WRAP
    print *, "ses_set_version_f90:  return_value is ", ses_set_version_f90
#endif


   

 
end function ses_set_version_f90


