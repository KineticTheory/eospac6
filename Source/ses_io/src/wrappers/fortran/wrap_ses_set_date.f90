

#undef DEBUG_WRAP

function ses_set_date_f90(the_handle, the_date)

   implicit none

   SES_ERROR_FLAG :: ses_set_date_f90

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   LONG :: the_date

   long :: ses_date
 
 
#ifdef DEBUG_WRAP
   print *, "ses_set_date_f90:  the_handle is ", the_handle
   print *, "ses_set_date_f90:  the_date is ", the_date
#endif
   ses_set_date_f90 =  ses_set_date(the_handle, the_date)
   
#ifdef DEBUG_WRAP

    print *, "ses_set_date_f90:  return_value is ", ses_set_date_f90
#endif


   

 
end function ses_set_date_f90


