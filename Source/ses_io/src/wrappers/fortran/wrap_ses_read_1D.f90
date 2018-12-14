

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_read_1D_f90(the_handle, the_buffer, dim1)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE :: the_buffer
   SES_NUMBER :: dim1

   SES_ERROR_FLAG :: return_value, ses_read_1D

#ifdef DEBUG_WRAP
   print *, "ses_read_1D_f90:  the_handle is ", the_handle
   print *, "ses_read_1D_f90:  dim1 is ", dim1
#endif

   return_value = ses_read_1D(the_handle, the_buffer, dim1)

#ifdef DEBUG_WRAP
   print *, "ses_read_1D_f90:  return_value is ", return_value
#endif

   ses_read_1D_f90 = return_value

 
end function ses_read_1D_f90


