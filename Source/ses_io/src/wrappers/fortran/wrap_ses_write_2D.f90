
#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_write_2D_f90(the_handle, the_buffer, dim1, dim2)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE :: the_buffer
   SES_NUMBER, intent(in) :: dim1
   SES_NUMBER, intent(in) :: dim2

   SES_NUMBER :: return_value, ses_write_2D

#ifdef DEBUG_WRAP
   print *, "ses_write_2D_f90:  the_handle is ", the_handle
   print *, "ses_write_2D_f90:  the_buffer(1) is ", the_buffer(1)
   print *, "ses_write_2D_f90:  dim1 is ", dim1
   print *, "ses_write_2D_f90:  dim2 is ", dim2
#endif

   return_value = ses_write_2D(the_handle, the_buffer, dim1, dim2)

#ifdef DEBUG_WRAP
   print *, "ses_write_2D_f90:  return_value is ", return_value
#endif

   ses_write_2D_f90 = return_value

 
end function ses_write_2D_f90


