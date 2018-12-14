

#undef DEBUG_WRAP

SES_NUMBER function ses_array_size_next_f90(the_handle)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_NUMBER :: return_value, ses_array_size_next


#ifdef DEBUG_32BIT
   integer(kind = 4) :: return_4
   integer(kind = 8) :: return_8
#endif
#ifdef DEBUG_WRAP
   print *, "ses_array_size_next_f90:  the_handle is ", the_handle
#endif


#ifdef DEBUG_32BIT

   !print *, "debug_32bit defined"
   return_4 = ses_array_size_next(the_handle);
#ifdef DEBUG_WRAP
   print *, "return_4 is ", return_4
#endif
   return_value = return_4

#else

   !print *, "debug_32bit not defined"
   return_value = ses_array_size_next(the_handle)

#endif

#ifdef DEBUG_WRAP
   print *, "ses_array_size_next_f90:  return_value is ", return_value
#endif

   ses_array_size_next_f90 = return_value

 
end function ses_array_size_next_f90


