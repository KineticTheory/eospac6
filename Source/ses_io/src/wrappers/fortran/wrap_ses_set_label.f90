

#define DEBUG_WRAP

SES_ERROR_FLAG function ses_set_label_f90(the_handle, the_label)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_LABEL :: the_label

   SES_ERROR_FLAG :: return_value, ses_set_label

#ifdef DEBUG_WRAP
   print *, "ses_set_label_f90:  the_handle is ", the_handle
   print *, "ses_set_label_f90:  the_label is ", the_label
#endif

   return_value = ses_set_label(the_handle, the_label)

#ifdef DEBUG_WRAP
   print *, "ses_set_label_f90:  return_value is ", return_value 
#endif

   ses_set_label_f90 = return_value

 
end function ses_set_label_f90


