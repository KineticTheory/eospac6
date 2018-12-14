

#undef DEBUG_WRAP


SES_ERROR_FLAG function ses_change_next_f90(the_handle, pt_buffer, dim1)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_NUMBER, intent(IN) :: dim1
   SES_WORD_REFERENCE :: pt_buffer
   SES_ERROR_FLAG :: r_value, ses_change_next


#ifdef DEBUG_WRAP
   print *, "ses_change_next_f90: the_handle is ", the_handle
   print *, "ses_change_next_f90:  pt_buffer(1) is ", pt_buffer(1)
   print *, "ses_change_next_f90:  dim1 is ", dim1
#endif
   r_value = ses_change_next(the_handle, pt_buffer, dim1)

#ifdef DEBUG_WRAP
   print *, "ses_change_next_f90:  return_value is ", r_value
#endif

   ses_change_next_f90 = r_value

 
end function ses_change_next_f90


