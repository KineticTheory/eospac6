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
!!  \brief Verify the new public function, eos_GetTableMetaData, returns meta data,
!!         per a user's specific request, functions as advertised within a Fortran code.
!!
!!  See SourceForge issue
!!  <a href="https://tf.lanl.gov/sf/go/artf36719">artf36719</a>
!!  for details.
!!
!! \note
!! MATIDS TO TEST: 3720 3721 3722

program main

  use eos_Interface2003

  integer(EOS_INTEGER) ::  err1

  integer(EOS_INTEGER) ::  firsthandle
  integer(EOS_INTEGER) ::  secondhandle
  integer(EOS_INTEGER) ::  thirdhandle

  integer(EOS_INTEGER) :: itabletype
  integer(EOS_INTEGER) ::  ntables
  integer(EOS_INTEGER) :: errorCode
  character*100 filename
  character(EOS_MaxErrMsgLen) :: errorMessage
  character(EOS_META_DATA_STRLEN) :: infoString

  integer i, j, k, count

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
  write(*,'(a,1x,i6)') " firsthandle =",firsthandle

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !set the file name for first table handle
  call eos_SetDataFileName(firsthandle, matid, itabletype, filename, err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a,i6,a)') " AFTER FIRST SetDataFileName err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
  write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

  call eos_ErrorCodesEqual(EOS_OPEN_SESAME_FILE_FAILED, err1, equal)
  if ( equal ) then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !Load first table handle
  call eos_LoadTables(ntables, firsthandle, err1)
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

  !fetch meta data for firsthandle
  infoString = ' '
  call eos_GetTableMetaData (firsthandle, EOS_File_Name, infoString, errorCode)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
     write(*,'(a,i6,a)') " AFTER FIRST eos_GetTableMetaData err1=",err1,": " // trim(errorMessage)
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  else
     errormessage = ""
     k = index(infoString,char(0)) ! find first NULL character
     infoString(k:len(infoString)) = ' ' ! remove NULL character and all other trailing characters
     k = index(infoString,'/',.true.) ! find last '/' character
     j = len_trim(infoString)
     write(*,'(a,a,a)') " AFTER FIRST eos_GetTableMetaData filename: ", '.../tests/data' // trim(infoString(k:j))
  endif
  count = count + 1 ! increment passed test counter

  if ( trim(filename) == trim(infoString) ) then
     write(*,'(a,i2)') " AFTER FIRST eos_GetTableMetaData filename == infoString", 1
     count = count + 1 ! increment passed test counter
  else
     write(*,'(a,i2)') " AFTER FIRST eos_GetTableMetaData filename != infoString", 0
  endif

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
  write(*,'(a,1x,i6)') " secondhandle =",secondhandle

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !set the file name for second table handle
  call eos_SetDataFileName(secondhandle, matid, itabletype, filename, err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a,i6,a)') " AFTER SECOND SetDataFileName err1=",err1," itabletype=",itabletype,": " // trim(errorMessage)
  write(*,'(a,a)') "               fcmp_ignore filename = ", trim(filename)

  call eos_ErrorCodesEqual(EOS_OPEN_SESAME_FILE_FAILED, err1, equal)
  if ( equal ) then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !Load second table handle
  call eos_LoadTables(ntables, secondhandle, err1)
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

  !fetch meta data for secondhandle
  infoString = ' '
  call eos_GetTableMetaData (secondhandle, EOS_File_Name, infoString, errorCode)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
     write(*,'(a,i6,a)') " AFTER SECOND eos_GetTableMetaData err1=",err1,": " // trim(errorMessage)
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  else
     errormessage = ""
     k = index(infoString,char(0)) ! find first NULL character
     infoString(k:len(infoString)) = ' ' ! remove NULL character and all other trailing characters
     k = index(infoString,'/',.true.) ! find last '/' character
     j = len_trim(infoString)
     write(*,'(a,a,a)') " AFTER SECOND eos_GetTableMetaData filename: ", '.../tests/data' // trim(infoString(k:j))
  endif
  count = count + 1 ! increment passed test counter

  if ( trim(filename) == trim(infoString) ) then
     write(*,'(a,i2)') " AFTER SECOND eos_GetTableMetaData filename == infoString", 1
     count = count + 1 ! increment passed test counter
  else
     write(*,'(a,i2)') " AFTER SECOND eos_GetTableMetaData filename != infoString", 0
  endif

  !THIRD TABLE

  matid = 3720
  itabletype = EOS_Pt_DUt

  !create third table handle

  call eos_CreateTables(ntables,itabletype,matid,thirdhandle,err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a)') " AFTER THIRD create tables err1=",err1,": " // trim(errorMessage)
  write(*,'(a,1x,i6)') " thirdhandle =",thirdhandle

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !Load third table handle
  call eos_LoadTables(ntables, thirdhandle,err1)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
  else
     errormessage = ""
  endif
  write(*,'(a,i6,a)') " AFTER THIRD load tables err1=",err1,": " // trim(errorMessage)

  if(err1.ne.0)then
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  endif
  count = count + 1 ! increment passed test counter

  !fetch meta data for thirdhandle
  infoString = ' '
  call eos_GetTableMetaData (thirdhandle, EOS_File_Name, infoString, errorCode)
  if(err1.ne.0)then
     call eos_GetErrorMessage(err1, errorMessage)
     write(*,'(a,i6,a)') " AFTER THIRD eos_GetTableMetaData err1=",err1,": " // trim(errorMessage)
     write(*,*)
     write(*,'(i2,1x,a)') count, 'tests passed'
     STOP
  else
     errormessage = ""
     k = index(infoString,char(0)) ! find first NULL character
     infoString(k:len(infoString)) = ' ' ! remove NULL character and all other trailing characters
     k = index(infoString,'/',.true.) ! find last '/' character
     j = len_trim(infoString)
     write(*,'(a,a,a)') " AFTER THIRD eos_GetTableMetaData filename: ", '.../projects/eos/data' // trim(infoString(k:j))
  endif
  count = count + 1 ! increment passed test counter

  write(*,'(a)') ''
  write(*,'(i2,1x,a)') count, 'tests passed'

end program main
