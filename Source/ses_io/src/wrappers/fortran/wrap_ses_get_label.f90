

#undef DEBUG_WRAP

function ses_get_label_f90(the_handle)

   implicit none

   SES_LABEL_STRING, target :: ses_get_label_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_LABEL :: pt_label
   integer :: ses_get_label, return_value

   pt_label=>ses_get_label_f90

#ifdef DEBUG_WRAP
   print *, "ses_get_label_f90:  the_handle is ", the_handle
#endif

   return_value = ses_get_label(the_handle, pt_label)

   /*  clean out the stuff between the returned size and 
       the ses_label_string size */

#ifdef DEBUG_WRAP
   print *, "ses_get_label_f90:  return_value is  ", return_value
#endif

   pt_label(return_value+1:SES_LEN_LABEL) = ' '

 
end function ses_get_label_f90


