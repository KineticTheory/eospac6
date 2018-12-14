

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_read_number_f90(the_handle, the_buffer)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   SES_NUMBER_REFERENCE  :: the_buffer

   SES_NUMBER :: return_value, ses_read_number

#ifdef DEBUG_WRAP
   print *, "ses_read_number_f90:  the_handle is ", the_handle
#endif

   return_value = ses_read_number(the_handle, the_buffer)

#ifdef DEBUG_WRAP

   print *, "ses_read_number_f90:  return_value is ", return_value
   print *, "ses_read_number_f90:  new buffer value is ", the_buffer
#endif

   ses_read_number_f90 = return_value

 
end function ses_read_number_f90


