!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

!> @file
!! @ingroup tests
!! @brief Ensure the new EOS_CREATE_TZERO option works as expected.
!!        See SourceForge© Issue #artf22354 for more details:
!!        https://tf.lanl.gov/sf/go/artf22354
!!
!! \note
!! MATIDS TO TEST: 5030
program testartf22354

  use eos_Interface
  integer(EOS_INTEGER), parameter :: ntables = 6
  integer(EOS_INTEGER) :: err, i
  integer(EOS_INTEGER) :: tabletype(ntables), th(ntables), matid(ntables)
  logical :: create_tzero_option(ntables)
  character(EOS_MaxErrMsgLen) :: errorMessage

  matid = 5030
  tabletype(1) =  EOS_At_DT
  tabletype(2) =  EOS_Ut_DT
  tabletype(3) =  EOS_At_DT
  tabletype(4) =  EOS_St_DT ! EOS_CREATE_TZERO option enabled
  tabletype(5) =  EOS_At_DT
  tabletype(6) =  EOS_At_DT ! EOS_CREATE_TZERO option enabled

  create_tzero_option    = EOS_FALSE
  create_tzero_option(4) = EOS_TRUE
  create_tzero_option(6) = EOS_TRUE

  !create first table handle

  call eos_CreateTables(ntables,tabletype,matid,th,err)
  if(err /= 0)then
     call eos_GetErrorMessage(err, errorMessage)
     write(*,'(a,i6,a)') "eos_CreateTables ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
  endif

  !set either the EOS_DUMP_DATA or the EOS_APPEND_DATA option for the table handles
  do i=1,ntables
     if (i == 1) then
        call eos_SetOption(th(1), EOS_DUMP_DATA, EOS_NullPtr, err)
     else
        call eos_SetOption(th(i), EOS_APPEND_DATA, EOS_NullPtr, err)
     endif
     if(err /= 0)then
        call eos_GetErrorMessage(err, errorMessage)
        write(*,'(a,i6,a)') "eos_SetOption ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
     endif
  enddo

  !set the EOS_CREATE_TZERO option for desired table handles
  do i=1,ntables
     if (create_tzero_option(i)) then
        call eos_SetOption(th(i), EOS_CREATE_TZERO, EOS_NullPtr, err)
        if(err /= 0)then
           call eos_GetErrorMessage(err, errorMessage)
           write(*,'(a,i6,a)') "eos_SetOption ERROR ", err, ": " // errorMessage(1:(len_trim(errorMessage)-1))
        endif
     endif
  enddo

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
           write(*,'(a,i1,a,i4,2a)') '  TH ', th(i), ', eos_LoadTables ERROR ', err, ': ', &
                                     errorMessage(1:(len_trim(errorMessage)-1))
        endif
     enddo
  endif

end program testartf22354

