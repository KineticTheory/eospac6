
#undef DEBUG_32BIT

#undef DEBUG_WRAP



function ses_get_table_ids_f90(the_handle, mid, the_size)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_MATERIAL_ID :: mid
   LONG_POINTER, dimension(:) :: the_size
 
   SES_TABLE_ID_REFERENCE, dimension(:) :: ses_get_table_ids_f90
   SES_TABLE_ID, dimension(1000), target :: actual_table

   SES_TABLE_ID :: ses_get_table_ids
#ifdef DEBUG_32BIT
   integer(kind = 4), dimension(1000), target :: actual_table_4
   integer(kind = 8), dimension(1000), target :: actual_table_8
   integer(kind = 4), dimension(:), pointer :: return_4
   integer(kind = 8), dimension(:), pointer :: return_8
#endif

   SES_TABLE_ID, target :: rv_target(2)
   integer :: dummy

   ses_get_table_ids_f90=>actual_table
#ifdef DEBUG_32BIT
   return_4=>actual_table_4
   return_8=>actual_table_8
#endif
 
#ifdef DEBUG_WRAP
   print *, "ses_get_table_ids_f90:  the_handle is ", the_handle
   print *, "ses_get_table_ids_f90:  mid is ", mid
#endif

#ifdef DEBUG_32BIT
   dummy = ses_get_table_ids(the_handle, mid, the_size, return_4)
   ses_get_table_ids_f90 = return_4
#else
   dummy = ses_get_table_ids(the_handle, mid, the_size, ses_get_table_ids_f90)
#endif

#ifdef DEBUG_WRAP
   print *, "ses_get_table_ids_f90:  ses_get_table_ids_f90 is ", ses_get_table_ids_f90(1)
   print *, "ses_get_table_ids_f90:  the_size is ", the_size(1)
#endif

 
end function ses_get_table_ids_f90


