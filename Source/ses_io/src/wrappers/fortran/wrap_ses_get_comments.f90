#undef DEBUG_WRAP

function ses_get_comments_f90(the_handle, the_string)

   implicit none

   SES_ERROR_FLAG :: ses_get_comments_f90

   SES_FILE_HANDLE, INTENT(IN)  :: the_handle
   SES_STRING :: the_string

   SES_ERROR_FLAG :: ses_get_comments

   SES_NUMBER :: the_size

   integer :: i

#ifdef DEBUG_WRAP
   print *, "WRAP_SES_GET_COMMENTS_F90:  the_handle is ", the_handle
#endif

   ses_get_comments_f90 = ses_get_comments(the_handle, the_string, the_size)
   do i = the_size+1, SES_LEN_LARGE
     the_string(i:i) = ''
   enddo 
 

#ifdef DEBUG_WRAP
   print *, "WRAP_SES_GET_COMMENTS_F90:  ses_comments_f90 is ", ses_get_comments_f90
   print *, "WRAP_SES_GET_COMMENTS_F90:  the_string is ", TRIM(the_string)
   print *, "WRAP_SES_GET_COMMENTS_F90:  the_size is ", the_size
#endif

end function ses_get_comments_f90

