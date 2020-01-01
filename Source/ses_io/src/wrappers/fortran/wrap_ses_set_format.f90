

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_set_format_f90(num_params, the_handle, the_file_type, the_ascii_word_type)

   implicit none

   SES_NUMBER, INTENT(IN)          :: num_params
   SES_FILE_HANDLE, INTENT(IN)     :: the_handle
   SES_FILE_TYPE, INTENT(IN)       :: the_file_type
   SES_ASCII_WORD_TYPE, INTENT(IN) :: the_ascii_word_type

   SES_ERROR_FLAG :: return_value, ses_set_format

#ifdef DEBUG_WRAP
    print *, "ses_set_format_f90:  num_params is ", num_params
    print *, "ses_set_format_f90:  the_handle is ", the_handle
    print *, "ses_set_format_f90:  the_format is ", the_file_type
    print *, "ses_set_format_f90:  the_ascii_word_type is ", the_ascii_word_type
#endif

   return_value = ses_set_format(num_params, the_handle, the_file_type, the_ascii_word_type)

#ifdef DEBUG_WRAP
   print *, "ses_set_format_f90:  return_value is ", return_value
#endif

   ses_set_format_f90 = return_value

 
end function ses_set_format_f90


