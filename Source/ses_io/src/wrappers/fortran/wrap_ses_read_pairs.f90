

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_read_pairs_f90(the_handle, buf1, buf2, dim1)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_WORD_REFERENCE :: buf1
   SES_WORD_REFERENCE :: buf2
   SES_NUMBER, intent(IN) :: dim1   

   SES_ERROR_FLAG :: return_value, ses_read_pairs

#ifdef DEBUG_WRAP
   print *, "ses_read_pairs_f90:  the_handle is ", the_handle
   print *, "ses_read_pairs_f90:  dim1 is ", dim1
#endif

   return_value = ses_read_pairs(the_handle, buf1, buf2, dim1)

#ifdef DEBUG_WRAP

   print *, "ses_read_pairs_f90:  buf1(1) is ", buf1(1)
   print *, "ses_read_pairs_f90:  buf2(1) is ", buf2(1)
#endif

   ses_read_pairs_f90 = return_value

 
end function ses_read_pairs_f90


