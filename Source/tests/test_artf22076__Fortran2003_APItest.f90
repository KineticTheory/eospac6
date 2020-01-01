!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

!> @file
!! @ingroup Fortran2003 tests quick
!! @brief Ensure the new eos_SetDataFileName and eos_GetMaxDataFileNameLength
!!        work as expected. See SourceForge© Issue #artf22076 for more details:
!!        https://tf.lanl.gov/sf/go/artf22076
!!
!! \note
!! MATIDS TO TEST: 3721 3722
!! REQUIRED FILE: data/ses003721
!! REQUIRED FILE: data/ses003722

program twocall

  use eos_Interface2003
  integer(EOS_INTEGER) ::  err1

  integer(EOS_INTEGER) ::  firsthandle
  integer(EOS_INTEGER) ::  secondhandle

  integer(EOS_INTEGER) :: itabletype
  integer(EOS_INTEGER) ::  ntables
  character*4096 filename
  character(EOS_MaxErrMsgLen) :: errorMessage

  integer i

  logical lexist

     ntables = 1
 
!FIRST TABLE

     matid = 3721
     itabletype =  EOS_T_DUt
     filename = "./tests/data/ses003721"
     do i=1,10
        inquire(FILE=trim(filename), EXIST=lexist)
        if (lexist) exit
        filename = "./." // trim(filename)
     enddo
     inquire(FILE=trim(filename), EXIST=lexist)
     if (.NOT.lexist) STOP

!create first table handle

     call eos_CreateTables(ntables,itabletype,matid,firsthandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a)') " AFTER FIRST create tables err1=",err1,": " // trim(errorMessage)
     print *," firsthandle =",firsthandle

!set the file name for first table handle
     call eos_SetDataFilename(firsthandle, matid, itabletype, filename,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a,i6,a)') " AFTER FIRST SetDataFilename err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

!Load first table handle
     call eos_LoadTables(ntables, firsthandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a)') " AFTER FIRST load tables err1=",err1,": " // trim(errorMessage)


!SECOND TABLE

     matid = 3722
     itabletype = EOS_Pt_DUt
     filename = "./tests/data/ses003722"
     do i=1,10
        inquire(FILE=trim(filename), EXIST=lexist)
        if (lexist) exit
        filename = "./." // trim(filename)
     enddo
     inquire(FILE=trim(filename), EXIST=lexist)
     if (.NOT.lexist) STOP

!create second table handle

     call eos_CreateTables(ntables,itabletype,matid,secondhandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a)') " AFTER SECOND create tables err1=",err1,": " // trim(errorMessage)
     print *," secondhandle =",secondhandle

!set the file name for second table handle
     call eos_SetDataFilename(secondhandle, matid, itabletype, filename,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a,i6,a)') " AFTER SECOND SetDataFilename err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

!Load second table handle
     call eos_LoadTables(ntables, secondhandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a)') " AFTER SECOND load tables err1=",err1,": " // trim(errorMessage)

     call eos_DestroyAll (err1)

end program twocall

