

#undef DEBUG_WRAP


SES_NUMBER function ses_access_directory_f90(the_handle, return_matid, nwds, iadr, date, version)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID_REFERENCE :: return_matid
   LONG_POINTER, dimension(:) :: nwds
   LONG_POINTER, dimension(:) :: iadr
   LONG :: date
   LONG :: version
   LONG :: i


   SES_NUMBER :: return_value, ses_access_directory



#ifdef DEBUG_WRAP
   print *, "ses_access_directory_f90:  the_handle is ", the_handle
#endif

   !return_value = 0
   return_value = ses_access_directory(the_handle, return_matid, nwds, iadr, date, version)

#ifdef DEBUG_WRAP
   print *, "ses_access_directory_f90:  return_value is ", return_value
   do i = 1, return_value
     print *, "ses_access_directory_f90:  return_matid[i] is ", return_matid(i)
     print *, "ses_access_directory_f90:  nwds is ", nwds(i)
     print *, "ses_access_directory_f90:  iadr is ", iadr(i)
   end do
   print *, "ses_access_directory_f90:  date is ", date
   print *, "ses_access_directory_f90:  version is ", version
#endif

   ses_access_directory_f90 = return_value

 
end function ses_access_directory_f90


