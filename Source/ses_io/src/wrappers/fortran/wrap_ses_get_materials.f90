

#undef DEBUG_WRAP

function ses_get_materials_f90(the_handle, pt_size)

   implicit none

   SES_FILE_HANDLE, intent(IN) :: the_handle
   LONG, dimension(1), target :: the_size
   
   SES_MATERIAL_ID_REFERENCE :: ses_get_materials_f90
   SES_MATERIAL_ID, dimension(1000), target :: actual_materials
#ifdef DEBUG_32BIT
   integer(kind = 4), dimension(1000), target :: actual_table_4
   integer(kind = 8), dimension(1000), target :: actual_table_8
   integer(kind = 4), dimension(:), pointer :: return_4
   integer(kind = 8), dimension(:), pointer :: return_8
#endif

   integer :: ses_get_materials, dummy
   LONG_ARRAY_POINTER :: pt_size   !  has no memory here

   ses_get_materials_f90=>actual_materials
#ifdef DEBUG_32BIT
   return_4=>actual_table_4
   return_8=>actual_table_8
#endif

   
#ifdef DEBUG_WRAP
   print *, "wrap_ses_get_materials_f90:  the_handle is ", the_handle
#endif

#ifdef DEBUG_32BIT
   dummy = ses_get_materials(the_handle, pt_size, return_4)
   ses_get_materials_f90 = return_4
#else
   dummy = ses_get_materials(the_handle, pt_size, ses_get_materials_f90)
#endif
#ifdef DEBUG_WRAP   
   print *, "wrap_ses_get_materials_f90:  dummy is ", dummy
   print *, "wrap_ses_get_materials_f90:  ses_get_materials_f90(1) is ", ses_get_materials_f90(1)
   print *, "wrap_ses_get_materials_f90:  the_size is ", pt_size(1)
#endif

   
  
end function ses_get_materials_f90


