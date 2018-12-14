!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

!> @file
!! @ingroup tests quick
!! @brief Ensure the new eos_SetDataFileName error handling works as expected,
!!        and that invalid table handles are not allowed to be used internally.
!!        See SourceForge© Issue #artf37549 for more details:
!!        https://tf.lanl.gov/sf/go/artf37549
!!
!! \note
!! REQUIRED FILE:  data/sesame.993716.bin
!! MATIDS TO TEST: 993716
!!
program test_artf37549

  use eos_Interface

  integer itablehandle(35)
  integer errorcode

  integer packedtablessize

  integer numtabs
  integer nn, i
  integer length_eosfilename
  character*4096 eosfilename

  integer ideos_array(35)
  integer itype_array(35)

  logical :: lexist

  length_eosfilename = 4096
  numtabs = 35

  ideos_array(1:8) = 993716
  ideos_array(9:17) = 993716
  ideos_array(18:26) = 993716  
  ideos_array(27:35) = 993716  

  itype_array(1) = EOS_T_DUt
  itype_array(9) = EOS_T_DUt
  itype_array(18)= EOS_T_DUt
  itype_array(27)= EOS_T_DUt


  itype_array(2) = EOS_Pt_DUt
  itype_array(10)= EOS_Pt_DUt
  itype_array(19)= EOS_Pt_DUt
  itype_array(28)= EOS_Pt_DUt


  itype_array(3) = EOS_Ut_DT
  itype_array(11)= EOS_Ut_DT
  itype_array(20)= EOS_Ut_DT
  itype_array(29)= EOS_Ut_DT


  itype_array(4) = EOS_Pt_DT
  itype_array(12)= EOS_Pt_DT
  itype_array(21)= EOS_Pt_DT
  itype_array(30)= EOS_Pt_DT


  itype_array(5) = EOS_D_PtT
  itype_array(13)= EOS_D_PtT
  itype_array(22)= EOS_D_PtT
  itype_array(31)= EOS_D_PtT


  itype_array(6) = EOS_Tm_D
  itype_array(14)= EOS_Tm_D
  itype_array(23)= EOS_Tm_D
  itype_array(32)= EOS_Tm_D


  itype_array(7) = EOS_Gs_D
  itype_array(15)= EOS_Gs_D
  itype_array(24)= EOS_Gs_D
  itype_array(33)= EOS_Gs_D


  itype_array(8) = EOS_Comment
  itype_array(16)= EOS_Comment
  itype_array(25)= EOS_Comment
  itype_array(34)= EOS_Comment


  itype_array(9) = EOS_Info
  itype_array(17)= EOS_Info
  itype_array(26)= EOS_Info
  itype_array(35)= EOS_Info


  do nn=1,length_eosfilename
     eosfilename(nn:nn)=" "
  enddo

  eosfilename =  "./tests/data/sesame.993716.bin"
  do i=1,10
     inquire(FILE=trim(eosfilename), EXIST=lexist)
     if (lexist) exit
     eosfilename = './.' // trim(eosfilename)
  enddo
  inquire(FILE=trim(eosfilename), EXIST=lexist)
  if (.NOT.lexist) stop

  print *,"eosfilename = '",trim(eosfilename)

  do nn=1,numtabs
     call eos_CreateTables(1,itype_array(nn),ideos_array(nn),ITABLEHANDLE(nn), errorcode)
     call eos_SetDataFileName(itablehandle(nn),ideos_array(nn),itype_array(nn),eosfilename , errorCode)
     print *," TH =",ITABLEHANDLE(nn)," errorcode =",errorcode
  enddo

  call eos_LoadTables(numtabs,itablehandle,errorcode)
  call eos_GetPackedTablesSize(numtabs,itablehandle,PACKEDTABLESSIZE,errorcode)

end program test_artf37549
