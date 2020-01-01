!*********************************************************************
!  Module for Fortran 90 code to set constants used by EOSPAC
!  ----------------------------------------------------------
!  Filetype: (HEADER)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

  ! Define hidden option flags that are currently not defined in the public interface.
  !  NOTE: If you want to use these in a test code, copy these defines into your code.
  integer, parameter :: EOS_NUM_PRIVATE_OPTIONS = 4
  integer, parameter :: EOS_MIN_PRIVATE_OPTION_FLAG_VALUE = 11000
  integer, parameter :: DISABLE_FTBLS_INVT_MASK = 11000
  integer, parameter :: EOS_DEBUG_PRINT = 11001
  integer, parameter :: EOS_DISABLE_GHOST_NODES = 11002
  integer, parameter :: EOS_ALLOW_ALL_INFO_ITEMS = 11003
