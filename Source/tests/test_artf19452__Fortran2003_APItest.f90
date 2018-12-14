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
!! @brief Ensure the new eos_SetDataFileName and eos_GetMaxDataFileNameLength
!!        work as expected. See SourceForge© Issue #artf19452 for more details:
!!        https://tf.lanl.gov/sf/go/artf19452
!!
!! \note
!! MATIDS TO TEST: 93720
program TestF90

  use eos_Interface2003

  implicit none

  integer(EOS_INTEGER),parameter :: nTables = 2
  integer(EOS_INTEGER),parameter :: nXYPairs = 4

  integer(EOS_INTEGER) ::  i, j
  integer(EOS_INTEGER) :: matID(nTables)
  integer(EOS_INTEGER) :: tableHandle(nTables), tableType(nTables)
  integer(EOS_INTEGER) :: errorCode, max_length
  character(kind=EOS_CHAR,len=4096) :: fileName, tmp
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs), F(nXYPairs), dFx(nXYPairs), dFy(nXYPairs)
  character(kind=EOS_CHAR,len=EOS_MaxErrMsgLen) :: errorMessage

  character(kind=EOS_CHAR,len=31) :: indexFileName = 'sesameFilesDir.txt'
  integer(EOS_INTEGER),parameter :: MAX_ARRAY_DIM = 1000
  character(kind=EOS_CHAR,len=4096) :: file_str(MAX_ARRAY_DIM)

  integer :: stat, line_cnt = 0
  logical(kind=EOS_BOOLEAN) :: lexist

  tableType  = EOS_Pt_DT
  matID      = 93720

  errorCode = EOS_OK
  tableHandle = 0

  ! Read sesameFilesDir.txt into memory
  open(99, FILE=indexFileName, STATUS='OLD')
  do
     read(UNIT=99, FMT='(a)', IOSTAT=stat) tmp
     if (stat /= 0) exit
     line_cnt = line_cnt + 1 
     if (line_cnt .GT. MAX_ARRAY_DIM) stop
     file_str(line_cnt) = trim(tmp)
  enddo
  close(99)

  ! Modify sesameFilesDir.txt to include only one valid file name and the END token
  call eos_GetMaxDataFileNameLength (max_length);
  fileName = './tests/data/sesame3'
  do i=1,10
     inquire(FILE=trim(fileName), EXIST=lexist)
     if (lexist) exit
     fileName = './.' // trim(fileName)
  enddo
  open(99, FILE=trim(indexFileName), STATUS='REPLACE', IOSTAT=stat)
  if (stat == 0) then
     write(99,'(a)') trim(fileName)
     write(99,'(a)') '    END    ' ! append END token with leading and trailing white space
     close(99)
  else
     write(*,*) 'data file not found (' // trim(fileName) // ')'
     stop
  endif

  !
  ! initialize table handles
  !
  call eos_CreateTables ( nTables, tableType, matID, tableHandle, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_CreateTables ERROR ', errorCode, ': ', &
                  errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! load data results in an expected error
  !
  call eos_LoadTables ( nTables, tableHandle, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! define custom matID-to-file association
  !
  fileName = './tests/data/93270littlebin'
  do i=1,10
     inquire(FILE=trim(fileName), EXIST=lexist)
     if (lexist) exit
     fileName = './.' // trim(fileName)
  enddo
  call eos_SetDataFileName (tableHandle(1), matID(1), tableType(1), fileName, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! define custom matID-to-file association
  !
  fileName = './tests/data/testbin'
  do i=1,10
     inquire(FILE=trim(fileName), EXIST=lexist)
     if (lexist) exit
     fileName = './.' // trim(fileName)
  enddo
  call eos_SetDataFileName (tableHandle(2), matID(2), tableType(2), fileName, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! set dump options
  !
  do i=1, nTables
     if (i == 1) then
        call eos_SetOption ( tableHandle(i), EOS_DUMP_DATA, EOS_NullVal, errorCode )
     else
        call eos_SetOption ( tableHandle(i), EOS_APPEND_DATA, EOS_NullVal, errorCode )
     endif
     if (errorCode /= EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,'(a,i5,a,i2,a,a)') 'eos_SetOption ERROR ', errorCode, ' (TH=', &
             tableHandle(i), '): ', &
             errorMessage(1:(len_trim(errorMessage)-1))
     endif
  enddo

  !
  ! load data into table data objects
  !
  call eos_LoadTables ( nTables, tableHandle, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! attempt to interpolate data associated with each table handle
  !
  do i=1, nTables

     if (i > 1) then
        do j=1, 163
           write(*,'(a)',ADVANCE='NO') '-'
        enddo
     endif
     write(*,'(/,"TH ",i1,": EOS_Pt_DT for material ",i6)') tableHandle(i), matID(i)

     X(1) = 1000._EOS_REAL
     Y(1) = 500._EOS_REAL
     do j=2, nXYPairs
        X(j) = X(j-1) + 1500.0_EOS_REAL
        Y(j) = Y(j-1) + 150.0_EOS_REAL
     end do

     call eos_Interpolate ( tableHandle(i), nXYPairs, X, Y, F, dFx, dFy, errorCode)
     if (errorCode /= EOS_OK .AND. errorCode /= EOS_INTERP_EXTRAPOLATED) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,994) 'eos_Interpolate ERROR ', errorCode, ' (TH=', &
                     tableHandle(i), '): ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     else
        do j=2, nXYPairs
           write(*,'(TR8,"i=",i1,TR4,"X = ",1p,e23.15," Y = ",e23.15," F = ",e23.15," dFx = ",e23.15," dFy = ",e23.15)') &
                j, X(j), Y(j), F(j), dFx(j), dFy(j)
        enddo
     endif
  enddo

  !
  ! test some error handling
  !
  write(*,'(/,"The following error message is expected:")')
  call eos_SetDataFileName (tableHandle(2), matID(2), tableType(2), fileName, errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage (errorCode, errorMessage)
     write(*,998) 'eos_SetDataFileName ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !
  ! Destroy all data objects
  !
  call eos_DestroyAll (errorCode)
  if (errorCode /= EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_DestroyAll ERROR ', errorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
  endif

  ! Write file_str to sesameFilesDir.txt
  open(99, FILE=indexFileName, STATUS='REPLACE')
  do i=1,line_cnt
     write(99,'(a)') trim(file_str(i))
  enddo

994 format (a,i6,a,i2,2a)
998 format (a,i6,2a)

end program TestF90
