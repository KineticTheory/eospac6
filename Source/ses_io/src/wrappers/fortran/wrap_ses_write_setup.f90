
#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_write_setup_f90(the_handle, the_mid, the_tid, nr, nt, ntab)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID, intent(in) :: the_mid
   SES_TABLE_ID, intent(in) :: the_tid
   SES_NUMBER, intent(in) :: nr
   SES_NUMBER, intent(in) :: nt
   SES_NUMBER, intent(in) :: ntab

   SES_ERROR_FLAG :: return_value, ses_write_setup


   return_value = ses_write_setup(the_handle, the_mid, the_tid, nr, nt, ntab)

   ses_write_setup_f90 = return_value

 
end function ses_write_setup_f90


