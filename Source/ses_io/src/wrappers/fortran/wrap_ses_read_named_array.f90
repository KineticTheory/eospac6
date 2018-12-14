
#undef DEBUG_WRAP


SES_ERROR_FLAG function ses_read_named_array_f90(the_handle, the_label, buf1)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_LABEL :: the_label
   SES_WORD_REFERENCE :: buf1

   SES_ERROR_FLAG :: return_value, ses_read_named_array

#ifdef DEBUG_WRAP
   print *, "ses_read_named_array_f90:  the_handle is ", the_handle
   print *, "ses_read_named_array_f90:  the_label is ", TRIM(the_label)

#endif

   return_value = ses_read_named_array(the_handle,   TRIM(the_label), buf1)

#ifdef DEBUG_WRAP
   print *, "ses_read_named_array_f90:  return_value is ", return_value
   print *, "ses_read_named_array_f90:  buf1 is ", buf1
#endif

   ses_read_named_array_f90 = return_value

 
end function ses_read_named_array_f90


