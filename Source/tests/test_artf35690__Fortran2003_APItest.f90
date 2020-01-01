!*********************************************************************
!* Test Program
!* ---------------------------------------------------------
!* Filetype: (SOURCE)
!* 
!* Copyright -- see file named COPYRIGHTNOTICE
!* 
!*********************************************************************

!>  \file
!!  \ingroup Fortran2003 tests
!!  \brief Verify the new public function, eos_GetTableMetaData, returns meta data,
!!         per a user's specific request, functions as advertised within a Fortran code.
!!
!!  See SourceForge issue
!!  <a href="https://tf.lanl.gov/sf/go/artf36719">artf36719</a>
!!  for details.
!!
!! \note
!! MATIDS TO TEST: 2161 3720

program main

  use eos_Interface2003

  integer(EOS_INTEGER) ::  err1

  integer(EOS_INTEGER) :: tablehandle
  integer(EOS_INTEGER) :: itabletype = EOS_Comment
  integer(EOS_INTEGER) :: ntables = 1
  integer(EOS_INTEGER), dimension(2) :: matids = (/ 2161, 3720 /);
  character(EOS_MaxErrMsgLen) :: errorMessage
  character(EOS_META_DATA_STRLEN) :: infoString

  integer(EOS_INTEGER) ::  i, j, count = 0
  integer(EOS_INTEGER) :: errorCode

  integer(EOS_INTEGER), dimension(7) :: infoTypes = (/ &
       EOS_Material_Name, &
       EOS_Material_Source, &
       EOS_Material_Date, &
       EOS_Material_Ref, &
       EOS_Material_Composition, &
       EOS_Material_Codes, &
       EOS_Material_Phases &
       /)
  character(len=24), dimension(7) :: infoTypes_str = (/ &
       'EOS_Material_Name       ', &
       'EOS_Material_Source     ', &
       'EOS_Material_Date       ', &
       'EOS_Material_Ref        ', &
       'EOS_Material_Composition', &
       'EOS_Material_Codes      ', &
       'EOS_Material_Phases     ' &
       /)


  do j=1,ubound(matids, 1)

     !create table handle

     call eos_CreateTables(ntables,itabletype,matids(j),tablehandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a)') ''
     write(*,'(a,i6,a)') " AFTER create tables err1=",err1,": " // trim(errorMessage)
     write(*,'(a,1x,i6)') " tablehandle =", tablehandle

     if(err1.ne.0)then
        write(*,'(a)') ''
        write(*,'(i2,1x,a)') count, 'tests passed'
        STOP
     endif
     count = count + 1 ! increment passed test counter

     !Load
     call eos_LoadTables(ntables, tablehandle, err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a)') " AFTER load tables err1=",err1,": " // trim(errorMessage)

     if(err1.ne.0)then
        write(*,'(a)') ''
        write(*,'(i2,1x,a)') count, 'tests passed'
        STOP
     endif
     count = count + 1 ! increment passed test counter

     write(*,'(a)') ''
     write(*,'(1x,a,i4)') "Test eos_GetTableMetaData using material id ", matids(j);

     do i=1,ubound(infoTypes, 1)

        !fetch meta data for tablehandle
        infoString = ' '
        call eos_GetTableMetaData (tablehandle, infoTypes(i), infoString, errorCode)
        if(err1.ne.0)then
           call eos_GetErrorMessage(err1, errorMessage)
           write(*,'(a,i6,a)') " AFTER eos_GetTableMetaData err1=",err1,": " // trim(errorMessage)
           write(*,'(a)') ''
           write(*,'(i2,1x,a)') count, 'tests passed'
           STOP
        else
           errormessage = ""
           k = index(infoString,char(0)) ! find first NULL character
           infoString(k:len(infoString)) = ' ' ! remove NULL character and all other trailing characters
           write(*,'(5x,a,a,a)') infoTypes_str(i), ': ', trim(infoString)
        endif
        count = count + 1 ! increment passed test counter

     enddo

     call eos_DestroyAll(err1);

  enddo

  write(*,'(a)') ''
  write(*,'(i2,1x,a)') count, 'tests passed'

end program main
