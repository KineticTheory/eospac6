
#undef DEBUG_WRAP

SES_BOOLEAN function ses_exit_f90()

   implicit none

#define DEBUG_32BIT
#ifdef DEBUG_32BIT
   integer(kind = 4) :: return_4
   integer(kind = 8) :: return_8
#endif

   SES_BOOLEAN :: ses_exit, return_value

   return_value = ses_exit()

   ses_exit_f90 = return_value

end function ses_exit_f90


