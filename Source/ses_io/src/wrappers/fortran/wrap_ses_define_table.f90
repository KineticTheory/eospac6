#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_define_table_f90(tid, description, nr, nt, num_independent, num_arrays, size_arrays, labels)

   implicit none

   SES_TABLE_ID, intent(IN) :: tid
   SES_LABEL :: description
   LONG :: nr, nt, num_independent
   LONG :: num_arrays, i
   LONG_POINTER  :: size_arrays(:)
   character(len=60), pointer, dimension(:)  :: labels
   SES_ERROR_FLAG :: return_value, ses_define_table
 
   integer :: nsize = 60

   return_value = SES_NO_ERROR

   ! Add a space at the end , to be used for EOL in c...
   if (LEN_TRIM(description) == 0) then
      return_value = SES_NO_DATA_ERROR
      stop SES_NO_DATA_ERROR
   else
      description = TRIM(description)//" "
   endif

#ifdef DEBUG_WRAP
   print *, "ses_define_table_f90:  tid = ", tid
   print *, "ses_define_table_f90:  description = ", description
   print *, "ses_define_table_f90:  nr = ", nr
   print *, "ses_define_table_f90:  nt = ", nt
   print *, "ses_define_table_f90:  num_arrays = ", num_arrays
   print *, "ses_define_table_f90:  num_independent = ", num_independent
   do i = 1, num_arrays
      print *, "ses_define_table_f90:  size_arrays(", i, ") = ", size_arrays(i)
      print *, "ses_define_table_f90:  labels(", i, ") = ", labels(i)
   enddo
#endif

   !  convert the labels into one long label array

   do i = 1, num_arrays
     labels(i) = TRIM(labels(i)) // char(0)
   enddo

#ifdef DEBUG_WRAP
  print*, "ses_define_table_f90:  calling ses_define_table"
#endif
   return_value = ses_define_table(tid, description, nr, nt, num_independent, num_arrays, size_arrays, nsize, labels)
#ifdef DEBUG_WRAP
  print *, "ses_define_table_f90: return_value is ", return_value
#endif

   ses_define_table_f90 = return_value

 
end function ses_define_table_f90


