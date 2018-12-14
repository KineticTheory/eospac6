

#undef DEBUG_WRAP


SES_ERROR_FLAG function ses_combine_f90(handle1, handle2, new_filename)

   implicit none

   SES_FILE_HANDLE, intent(in)  :: handle1
   SES_FILE_HANDLE, intent(in) :: handle2
   SES_STRING :: new_filename
   SES_ERROR_FLAG :: return_value, ses_combine

#ifdef DEBUG_WRAP
   print *, "ses_combine_f90:  handle1 is ", handle1
   print *, "ses_combine_f90:  handle2 is ", handle2
   print *, "ses_combine_f90:  new_filename is ", TRIM(new_filename)
#endif

   return_value = ses_combine(handle1, handle2, TRIM(new_filename))

   ses_combine_f90 = return_value

 
end function ses_combine_f90


