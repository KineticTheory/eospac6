
#undef DEBUG_WRAP

SES_BOOLEAN function ses_print_error_message_f90(the_error_flag, the_buffer, dim1)

   implicit none

   SES_ERROR_FLAG, INTENT(IN) :: the_error_flag
   SES_STRING :: the_buffer
   LONG, INTENT(OUT) :: dim1

   SES_BOOLEAN :: return_value
   SES_BOOLEAN  :: ses_print_error_message

   SES_STRING  :: the_string
   SES_LARGE_STRING, target :: actual_string

   integer :: my_dim1, i

   the_string => actual_string

   actual_string(1:SES_LEN_LARGE) = ' '

#ifdef DEBUG_WRAP
   print *, "ses_print_error_message_f90:  the_error_flag is ", the_error_flag
#endif
   my_dim1 = 0;
   return_value = ses_print_error_message(the_error_flag, the_string, my_dim1)
   dim1 = my_dim1;
#ifdef DEBUG_WRAP
   print *, "ses_print_error_message_f90:  the_string is ", actual_string, " and my_dim1 is ", my_dim1
#endif

   do i = 1, dim1
       the_buffer(i:i) = actual_string(i:i)
   enddo

   ses_print_error_message_f90 = return_value
 
end function ses_print_error_message_f90

