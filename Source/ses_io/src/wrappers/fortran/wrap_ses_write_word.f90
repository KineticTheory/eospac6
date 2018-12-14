
#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_write_word_f90(the_handle, the_buffer)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE :: the_buffer

   SES_ERROR_FLAG :: return_value, ses_write_word
#ifdef DEBUG_WRAP
   print *, "ses_write_word_f90:  the_handle is ", the_handle
   print *, "ses_write_word_f90:  the_buffer is ", the_buffer
#endif

   return_value = ses_write_word(the_handle, the_buffer)
#ifdef DEBUG_WRAP
   print *, "ses_write_word_f90:  return_value is ", return_value
#endif

   ses_write_word_f90 = return_value

 
end function ses_write_word_f90


