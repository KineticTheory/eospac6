!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

!> @file
!! @ingroup Fortran90 tests
!! @brief Ensure the correct errors are returned for specific use cases.
!!        Both use cases in this test are expected to return a non-zero error
!!        for the table handle associated with the EOS_St_DT type.
!!        See SourceForge© Issue #artf47577 for more details:
!!        https://tf.lanl.gov/sf/go/artf47577
!!
!! \note
!! MATIDS TO TEST: 5030
program test_artf47577

  use eos_Interface
  integer(EOS_INTEGER), parameter :: ntablesE = 2
  integer(EOS_INTEGER) :: err, i, outer
  integer(EOS_INTEGER) :: tabletype(ntablesE), th(ntablesE), matid(ntablesE)
  character(EOS_MaxErrMsgLen) :: errorMessage
  logical(EOS_BOOLEAN) :: flag = .FALSE.

  matid = 5030

  flag = .TRUE.

  do outer = 1, 2

     if (flag) then
        ntables = ntablesE
        tabletype    =  0
        tabletype(1) =  EOS_St_DT
        tabletype(2) =  EOS_Ut_DT
     else
        ntables = ntablesE
        tabletype    =  0
        tabletype(1) =  EOS_Ut_DT
        tabletype(2) =  EOS_St_DT
     endif

     write(*,*)
     write(*,'(a,i2,a,l2)') '----- TEST', outer, '. flag=', flag

     !create first table handle

     call eos_CreateTables(ntables,tabletype,matid,th,err)
     if(err /= 0)then
        call eos_GetErrorMessage(err, errorMessage)
        write(*,'(a,i6,a)') "eos_CreateTables ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
     else
        write(*,'(a)') "eos_CreateTables OK"
     endif

     !Load first table handle
     call eos_LoadTables(ntables, th,err)
     if(err /= 0)then
        call eos_GetErrorMessage(err, errorMessage)
        write(*,'(a,i6,a)') "eos_LoadTables ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
        do i=1,ntables
           err = EOS_OK
           call eos_GetErrorCode ( th(i), err )
           call eos_GetErrorMessage ( err, errorMessage )
           if (err /= EOS_OK) then
              write(*,'(a,i1,a,i6,2a)') '  TH ', th(i), ', eos_LoadTables ERROR ', err, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
           endif
        enddo
     else
        write(*,'(a)') "eos_LoadTables OK"
     endif

     call eos_DestroyAll(err)
     if(err /= 0)then
        call eos_GetErrorMessage(err, errorMessage)
        write(*,'(a,i6,a)') "eos_DestroyAll ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
     else
        write(*,'(a)') "eos_DestroyAll OK"
     endif

     flag = .FALSE.

  enddo

  write(*,*)

end program test_artf47577
