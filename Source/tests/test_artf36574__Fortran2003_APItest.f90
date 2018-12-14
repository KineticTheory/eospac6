!*********************************************************************
!* Test Program
!* ---------------------------------------------------------
!* Filetype: (SOURCE)
!* 
!* Copyright -- see file named COPYRIGHTNOTICE
!* 
!*********************************************************************

!>  \file
!!  \ingroup tests
!!  \brief Verify the filename length issue is resolved when calling the
!!         eos_SetDataFileName routine from Fortran with a filename character
!!         string variable's fixed-length declared to be less than the
!!         PATH_MAX value on currently-supported platforms.
!!
!!  See SourceForge issue
!!  <a href="https://tf.lanl.gov/sf/go/artf36574">artf36574</a>
!!  for details.
!!
!! \note
!! MATIDS TO TEST: 3721 3722

program main

  use eos_Interface2003

  integer(EOS_INTEGER) ::  err1

  integer(EOS_INTEGER) ::  firsthandle
  integer(EOS_INTEGER) ::  secondhandle

  integer(EOS_INTEGER) :: itabletype
  integer(EOS_INTEGER) ::  ntables
  character*100 filename
  character(EOS_MaxErrMsgLen) :: errorMessage

  integer i, count

  logical lexist
  logical equal

  ntables = 1
  count = 0

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
  if (.NOT.lexist) then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif

  !create first table handle

  call eos_CreateTables(ntables,itabletype,matid,firsthandle,err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a)') " AFTER FIRST create tables err1=",err1,": " // trim(errorMessage)
  print *," firsthandle =",firsthandle

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !set the file name for first table handle
  call eos_SetDataFilename(firsthandle, matid, itabletype, filename, err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a,i6,a)') " AFTER FIRST SetDataFilename err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
  write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

  call eos_ErrorCodesEqual(EOS_OPEN_SESAME_FILE_FAILED, err1, equal)
  if ( equal ) then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !Load first table handle
  call eos_LoadTables(ntables, firsthandle,err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a)') " AFTER FIRST load tables err1=",err1,": " // trim(errorMessage)

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter


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

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !set the file name for second table handle
  call eos_SetDataFilename(secondhandle, matid, itabletype, filename, err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a,i6,a)') " AFTER SECOND SetDataFilename err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
  write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

  call eos_ErrorCodesEqual(EOS_OPEN_SESAME_FILE_FAILED, err1, equal)
  if ( equal ) then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !Load second table handle
  call eos_LoadTables(ntables, secondhandle,err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a)') " AFTER SECOND load tables err1=",err1,": " // trim(errorMessage)

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  write(*,*)
  write(*,'(i2,1x,a)') count, 'tests passed'

end program main
