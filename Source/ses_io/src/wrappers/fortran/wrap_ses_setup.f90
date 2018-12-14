

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_setup_f90(the_handle, the_mid, the_tid)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID, intent(in) :: the_mid
   SES_TABLE_ID, intent(in) :: the_tid

   SES_ERROR_FLAG :: return_value, ses_setup

#ifdef DEBUG_WRAP
   print *, "ses_setup_f90:  the_handle is ", the_handle
   print *, "ses_setup_f90:  the_mid is ", the_mid
   print *, "ses_setup_f90:  the_tid is ", the_tid
#endif

   return_value = ses_setup(the_handle, the_mid, the_tid)

#ifdef DEBUG_WRAP
   print *, "ses_setup_f90:  return_value is ", return_value
#endif

   ses_setup_f90 = return_value

 
end function ses_setup_f90


