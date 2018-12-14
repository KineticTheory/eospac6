
#undef DEBUG_WRAP

SES_BOOLEAN function ses_get_grid_f90(the_handle, the_mid, the_tid, arg_nr, arg_nt, arg_ntab) 

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID, INTENT(IN) :: the_mid
   SES_TABLE_ID, INTENT(IN) :: the_tid
   LONG, INTENT(OUT) :: arg_nr
   LONG, INTENT(OUT) :: arg_nt
   LONG, INTENT(OUT) :: arg_ntab

   SES_BOOLEAN :: return_value, ses_get_grid

   LONG, TARGET :: nr, nt, ntab
   LONG_POINTER :: pt_nr, pt_nt, pt_ntab
   pt_nr => nr
   pt_nt => nt
   pt_ntab => ntab

   nr = 0
   nt = 0
   ntab = 0

#ifdef DEBUG_WRAP
   print *, "SES_GET_GRID_F90: the_mid is ", the_mid
   print *, "SES_GET_GRID_F90: the_tid is ", the_tid
   print *, "SES_GET_GRID_F90: nr is ", nr
   print *, "SES_GET_GRID_F90: nt is ", nt
#endif

   return_value = ses_get_grid(the_handle, the_mid, the_tid, pt_nr, pt_nt, pt_ntab)

   arg_nr = nr
   arg_nt = nt
   arg_ntab = ntab

   ses_get_grid_f90 = return_value

end function ses_get_grid_f90

