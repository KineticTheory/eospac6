

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_write_comments_f90(the_handle, the_comments, dim1)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_STRING  :: the_comments
   SES_NUMBER, intent(IN) :: dim1   

   SES_ERROR_FLAG :: return_value, ses_write_comments_c

  return_value = SES_WRITE_ERROR

#ifdef DEBUG_WRAP
   print *, "ses_write_comments_f90:  the_handle is ", the_handle
   print *, "ses_write_comments_f90:  the_comments are", the_comments
   print *, "ses_write_comments_f90:  dim1 is ", dim1
#endif

   return_value = ses_write_comments_c(the_handle, the_comments, dim1)
#ifdef DEBUG_WRAP
   print *, "ses_write_comments_f90:  return_value is ", return_value
#endif

   ses_write_comments_f90 = return_value

 
end function ses_write_comments_f90


