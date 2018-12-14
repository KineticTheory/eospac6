

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_read_word_f90(the_handle, pt_buffer)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE  :: pt_buffer

   SES_ERROR_FLAG :: return_value, ses_read_word

#ifdef DEBUG_WRAP
   print *, "ses_read_word_f90:  the_handle is ", the_handle
#endif

   return_value = ses_read_word(the_handle, pt_buffer)

#ifdef DEBUG_WRAP
   print *, "ses_read_word_f90:  return_value is ", return_value
   print *, "ses_read_word_f90:  the_buffer is ", pt_buffer
#endif

   ses_read_word_f90 = return_value

end function ses_read_word_f90


