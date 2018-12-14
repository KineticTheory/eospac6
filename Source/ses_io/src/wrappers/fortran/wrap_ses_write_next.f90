
#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_write_next_f90(the_handle, the_buffer, dim1, the_label)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE :: the_buffer
   SES_NUMBER, INTENT(IN) :: dim1
   SES_LABEL  :: the_label

   SES_NUMBER :: return_value, ses_write_next

#ifdef DEBUG_WRAP
   print *, "ses_write_next_f90:  the_handle is ", the_handle
   print *, "ses_write_next_f90:  the_buffer(1) is ", the_buffer(1)
   print *, "ses_write_next_f90:  dim1 is ", dim1
   print *, "ses_write_next_f90:  the_label is ", the_label
#endif

   return_value = ses_write_next(the_handle, the_buffer, dim1, the_label)

#ifdef DEBUG_WRAP
   print *,"ses_write_next_f90:  return_value is ", return_value
#endif

   ses_write_next_f90 = return_value

 
end function ses_write_next_f90


