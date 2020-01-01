!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!
!  Copyright -- see file named COPYRIGHTNOTICE
!
!********************************************************************

!> @file
!! @ingroup Fortran90 tests quick
!! @brief Ensure the error handing works correctly for materials that EOSPAC6
!!        cannot produce entropy and free energy tables.
!!        See SourceForge~ Issue #artf50835 for more details:
!!        https://tf.lanl.gov/sf/go/artf50835
!!
!! \note
!! MATIDS TO TEST: 5030
program main

  use eos_Interface

  integer itablehandle(4)
  integer errorcode
  integer numtabs
  integer nn
  integer ideos_array(4)
  integer itype_array(4)

  numtabs = 4

  ideos_array(1:4) = 5030
  itype_array(1) = EOS_T_DUt
  itype_array(2) = EOS_Pt_DUt
  itype_array(3) = EOS_St_DUt
  itype_array(4) = EOS_At_DUt

  do nn=1,numtabs
     call eos_CreateTables(1,itype_array(nn),ideos_array(nn),ITABLEHANDLE(nn), errorcode)

     print *," errorcode =",errorcode
     if(nn.lt.3)then
        call eos_setOption(itablehandle(nn),EOS_INVERT_AT_SETUP,EOS_NullVal, errorCode)
        print *," invert errorCode=",errorCode
     endif
  enddo

  print *," itablehandle=",itablehandle
  call eos_LoadTables(numtabs,itablehandle(1:numtabs),errorcode)
  print *," load table handle errorcode=",errorcode

  call eos_DestroyAll(errorcode)

end program main
