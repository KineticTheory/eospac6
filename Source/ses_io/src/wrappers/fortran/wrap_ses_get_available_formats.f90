#undef DEBUG_WRAP

function ses_get_available_formats_f90(pt_string)

   implicit none
   
   integer :: ses_get_available_formats_f90, ses_get_available_formats
   SES_STRING :: pt_string

   SES_STRING  :: pt_string2
   SES_LARGE_STRING, target :: the_string

   pt_string2=> the_string

   print *, "wrap_ses_get_available_formats_f90:  call ses_get_available_formats"

   ses_get_available_formats_f90 =  ses_get_available_formats(pt_string2)
 
   print *, "wrap_ses_get_available_formats_f90:  returning with string of size ", ses_get_available_formats_f90
   print *, "wrap_ses_get_available_formats_f90:  the_string is ", the_string(1:ses_get_available_formats_f90)

   pt_string = the_string(1:ses_get_available_formats_f90)
  
end function ses_get_available_formats_f90
