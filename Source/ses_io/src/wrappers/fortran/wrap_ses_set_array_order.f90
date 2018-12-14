

#undef DEBUG_WRAP

SES_ERROR_FLAG function ses_set_array_order_f90(the_handle, the_order)

   implicit none

   SES_FILE_HANDLE, INTENT(IN) :: the_handle
   SES_ARRAY_ORDER, intent(IN) :: the_order

   SES_ERROR_FLAG :: return_value, ses_set_array_order

#ifdef DEBUG_WRAP
   print *, "ses_set_array_order_f90:  the_handle is ", the_handle
   print *, "ses_set_array_order_f90:  the_order is ", the_order
#endif

   return_value = ses_set_array_order(the_handle, the_order)

#ifdef DEBUG_WRAP
   print *, "ses_set_array_order_f90:  return_value is ", return_value
#endif

   ses_set_array_order_f90 = return_value

 
end function ses_set_array_order_f90


