
#undef DEBUG_WRAP

function ses_get_table_sizes_f90(the_handle, mid, the_buffer, the_size)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID, INTENT(IN) :: mid
   SES_TABLE_SIZES_REFERENCE, dimension(:)   :: the_buffer
   LONG, INTENT(OUT)           :: the_size

   SES_BOOLEAN :: ses_get_table_sizes
   SES_BOOLEAN :: return_value, ses_get_table_sizes_f90
   SES_BOOLEAN :: didit_getit

   LONG_POINTER :: pt_size
   LONG, target :: my_size

   integer :: i

   SES_TABLE_SIZES_REFERENCE, DIMENSION(:) :: my_buffer
   LONG, DIMENSION(1000), target :: buffer1

   my_buffer=>buffer1

   pt_size=>my_size



#ifdef DEBUG_WRAP
   print *, "ses_get_table_sizes.f90:  calling ses_get_table_sizes"
   print *, "ses_get_table_sizes.f90:  mid is ", mid
#endif

   return_value = ses_get_table_sizes(the_handle, mid, my_buffer, pt_size)
#ifdef DEBUG_WRAP
   print *, "ses_get_table_sizes.f90:  got past function call"
#endif

   the_size = my_size
#ifdef DEBUG_WRAP
   do i = 1, the_size
	print *, "ses_get_table_sizes.f90: value in index ", i, " is ", buffer1(i)
   enddo
#endif


   do i = 1, the_size
	the_buffer(i) = buffer1(i)
   enddo

   ses_get_table_sizes_f90 = return_value

 
end function ses_get_table_sizes_f90


