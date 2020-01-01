!*********************************************************************
!  Module for Fortran 90 code to set constants used by EOSPAC
!  ----------------------------------------------------------
!  Filetype: (HEADER)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************
module eos_Interface
  !
  ! Define kind parameters
  !
  implicit none
  integer, parameter :: EOS_CHAR = KIND('c')
  integer, parameter :: EOS_BOOLEAN = KIND(.TRUE.)
  integer, parameter :: EOS_INTEGER = SELECTED_INT_KIND(r=8)
  integer, parameter :: EOS_REAL = SELECTED_REAL_KIND(p=12,r=250)
  integer(EOS_INTEGER), private, parameter :: idum = 1_EOS_INTEGER
  integer(EOS_INTEGER), private, parameter :: intSize = BIT_SIZE(idum)/8_EOS_INTEGER

  include 'eos_InterfaceFortranCommon.f90'  ! this is the code shared with the Fortran 2003 module: eos_Interface2003

end module eos_Interface
