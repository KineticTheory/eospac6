

#undef DEBUG_WRAP

function ses_get_material_id_f90(the_material_name)

   implicit none

   SES_MATERIAL_ID :: ses_get_material_id_f90
   SES_STRING :: the_material_name

   SES_MATERIAL_ID :: ses_get_material_id_c

#ifdef DEBUG_WRAP
   print *, "ses_get_material_id_f90:  the_name is ", TRIM(the_material_name)
#endif
   ses_get_material_id_f90 = ses_get_material_id_c(TRIM(the_material_name), ses_get_material_id_f90)
#ifdef DEBUG_WRAP
    print *, "ses_get_material_id_f90:  return_value is ", ses_get_material_id_f90
#endif 
end function ses_get_material_id_f90


