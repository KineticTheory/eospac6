

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_read_3D_f90(the_handle, the_buffer, dim1, dim2, dim3)


   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_WORD_REFERENCE :: the_buffer
   SES_NUMBER, intent(IN) :: dim1
   SES_NUMBER, intent(IN) :: dim2
   SES_NUMBER, intent(IN) :: dim3
   SES_NUMBER :: return_value, ses_read_3D
#ifdef DEBUG_WRAP
   print *, "ses_read_3D_f90:  the_handle is ", the_handle
   print *, "ses_read_3D_f90:  the_buffer(1) is ", the_buffer(1);
   print *, "ses_read_3D_f90:  dim1 is ", dim1
   print *, "ses_read_3D_f90:  dim2 is ", dim2
   print *, "ses_read_3D_f90:  dim3 is ", dim3
#endif


   return_value = ses_read_3D(the_handle, the_buffer, dim1, dim2, dim3)
#ifdef DEBUG_WRAP
   print *, "ses_read_3D_f90:  return_value is ", return_value
#endif

   ses_read_3D_f90 = return_value

 
end function ses_read_3D_f90


