

#undef DEBUG_WRAP

function ses_read_next_f90(the_handle, buffer, dim1)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_NUMBER :: ses_read_next_f90
   SES_NUMBER :: dim1

   SES_WORD_REFERENCE :: pt_buffer

   SES_WORD, dimension(dim1), target :: buffer
   SES_NUMBER :: return_value
   SES_NUMBER :: ses_read_next

   pt_buffer=>buffer


#ifdef DEBUG_WRAP
   print *, "ses_read_next_f90:  the_handle is ", the_handle
   print *, "ses_read_next_f90:  array_size is ", dim1
#endif

   return_value = ses_read_next(the_handle, buffer, dim1)

#ifdef DEBUG_WRAP
   print *, "pt_buffer:  1, 2, and 3 are "
   print *, pt_buffer(1)
   print *, pt_buffer(2)
   print *, pt_buffer(3)
#endif


   if (associated(pt_buffer)) then

#ifdef DEBUG_WRAP
     print *, "ses_read_next_f90:  buffer(3) is ", buffer(3)
#endif
   else
 
#ifdef DEBUG_WRAP
     print *, "ses_read_next_f90:  pointer not associated"
#endif

   endif

   ses_read_next_f90 = return_value
 
end function ses_read_next_f90


