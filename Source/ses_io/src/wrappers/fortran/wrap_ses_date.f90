

#undef DEBUG_WRAP

function ses_date_f90(the_handle)

   implicit none

   SES_LARGE_STRING, target :: ses_date_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   integer :: ses_date
   integer :: return_value
   integer :: size, i

 
#ifdef DEBUG_WRAP
   print *, "ses_date_f90:  the_handle is ", the_handle
#endif
   return_value =  ses_date(the_handle, ses_date_f90, size)
   do i = size, SES_LEN_LARGE
     ses_date_f90(i:i) = ' '
   enddo
   
#ifdef DEBUG_WRAP

   print *, "ses_date_f90:  ses_date_f90 (the string) is ", TRIM(ses_date_f90)
   print *, "ses_date_f90:  return_value is ", return_value
#endif


   

 
end function ses_date_f90


