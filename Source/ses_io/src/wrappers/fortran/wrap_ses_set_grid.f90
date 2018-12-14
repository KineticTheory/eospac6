
#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_set_grid_f90(the_handle, nr, nt, ntab)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_NUMBER, intent(IN) :: nr
   SES_NUMBER, intent(IN) :: nt
   SES_NUMBER, intent(IN) :: ntab

   SES_NUMBER :: return_value, ses_set_grid

#ifdef DEBUG_WRAP
   print *, "ses_set_grid_f90:  the_handle is ", the_handle
   print *, "ses_set_grid_f90:  nr is ", nr
   print *, "ses_set_grid_f90:  nt is ", nt   
   print *, "ses_set_grid_f90:  ntab is ", ntab
#endif

   return_value = ses_set_grid(the_handle, nr, nt, ntab)

#ifdef DEBUG_WRAP
   print *, "ses_set_grid_f90:  return_value is ", return_value
#endif

   ses_set_grid_f90 = return_value

end function ses_set_grid_f90


