

#undef DEBUG_WRAP


SES_NUMBER function ses_access_table_index_f90(the_handle, return_tblid, nwds, iadr, date1, date2, version)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_TABLE_ID_REFERENCE, dimension(:) :: return_tblid
   LONG_POINTER, dimension(:) :: nwds
   LONG_POINTER, dimension(:) :: iadr
   LONG :: date1
   LONG :: date2
   LONG :: version
   LONG :: i


   SES_NUMBER :: return_value, ses_access_table_index



#ifdef DEBUG_WRAP
   print *, "ses_access_table_index_f90:  the_handle is ", the_handle
#endif

   !return_value = 0
   return_value = ses_access_table_index(the_handle, return_tblid, nwds, iadr, date1, date2, version)

#ifdef DEBUG_WRAP
   print *, "ses_access_table_index_f90:  return_value is ", return_value
   do i = 1, return_value
     print *, "ses_access_table_index_f90:  return_tblid[i] is ", return_tblid(i)
     print *, "ses_access_table_index_f90:  nwds is ", nwds(i)
     print *, "ses_access_table_index_f90:  iadr is ", iadr(i)
   end do
   print *, "ses_access_table_index_f90:  date1 is ", date1
   print *, "ses_access_table_index_f90:  date2 is ", date2
   print *, "ses_access_table_index_f90:  version is ", version
#endif

   ses_access_table_index_f90 = return_value

 
end function ses_access_table_index_f90


