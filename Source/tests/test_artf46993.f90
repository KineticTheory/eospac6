!*********************************************************************
!     Test program
!     ----------------------------------------------------------
!     Filetype: (SOURCE)
!     
!     Copyright -- see file named COPYRIGHTNOTICE
!     
!********************************************************************

!> \file
!! \ingroup Fortran90 tests
!! \brief Perform the following tests:
!!    -# Verify that invalid material ID(s) (i.e., 0) are not associated with valid table handle(s)
!!       and attempted to be loaded.
!!       See SourceForge<A9> Issue #artf46993 for more details:
!!       https://tf.lanl.gov/sf/go/artf46993
!!
!! \note
!! MATIDS TO TEST: 3720
program test

  use eos_Interface

  implicit none

  integer(EOS_INTEGER) :: err, thErr

  integer(EOS_INTEGER), parameter :: ntablesE=15
  integer(EOS_INTEGER), parameter :: ntables=5

  integer(EOS_INTEGER) :: i, j
  integer(EOS_INTEGER) :: th(ntablesE), type(ntablesE), matid(ntablesE)

  real(EOS_REAL), parameter :: u_convert = 1.0d-2 ! Mbar-cc/g to MJ/kg
  real(EOS_REAL), parameter :: p_convert = 1.0d-2 ! Mbar to GPa

  character(EOS_MaxErrMsgLen) :: errorMessage

  type(1 ) = EOS_Pt_DUt
  type(2 ) = EOS_T_DUt
  type(3 ) = EOS_Ut_DT
  type(4 ) = EOS_Ut_DPt
  type(5 ) = EOS_T_DPt
  type(6 ) = EOS_Pt_DAt
  type(7 ) = EOS_Pt_DSt
  type(8 ) = EOS_Ut_DAt
  type(9 ) = EOS_Ut_DSt
  type(10) = EOS_At_DPt
  type(11) = EOS_At_DUt
  type(12) = EOS_At_DSt
  type(13) = EOS_St_DPt
  type(14) = EOS_St_DUt
  type(15) = EOS_St_DAt

  j = -1

10 continue

  j = j + 1

  matid(:) = 0               ! initialize the entire array
  if (j .EQ. 0) then
     matid(:) = 3720         ! fill the entire array
  else
     matid(1:ntables) = 3720 ! partially-fill the array
  endif
  write(*,'(a,15i5,a)') 'matid = [', matid, ']'

  !     Load SESAME data
  call eos_CreateTables(ntablesE,type,matid,th,err)

  if (err .NE. EOS_OK) then
     call eos_GetErrorMessage ( err, errorMessage )
     write(*,998) 'eos_CreateTables ERROR ', err, ': ', errorMessage(1:(len_trim(errorMessage)-1))
     if (err .NE. EOS_OK) then
        do i=1, nTablesE
           thErr = EOS_OK
           call eos_GetErrorCode ( th(i), thErr )
           if (thErr .NE. EOS_OK) then
              call eos_GetErrorMessage ( thErr, errorMessage )
              write(*,998) 'eos_CreateTables ERROR ', thErr, ': ', errorMessage(1:(len_trim(errorMessage)-1))
           endif
        enddo
     endif
  endif

  call eos_SetOption(th(1),EOS_INVERT_AT_SETUP,0,err)
  call eos_SetOption(th(2),EOS_INVERT_AT_SETUP,0,err)
  call eos_SetOption(th(4),EOS_INVERT_AT_SETUP,0,err)
  if (ntables.GE.5) &
       call eos_SetOption(th(5),EOS_INVERT_AT_SETUP,0,err)
  do i=6,ntablesE
     call eos_SetOption(th(i),EOS_INVERT_AT_SETUP,0,err)
  enddo

  do i=1,ntablesE
     call eos_SetOption(th(i),EOS_APPEND_DATA,0,err)
  enddo
  if (j .EQ. 0) call eos_SetOption(th(1),EOS_DUMP_DATA,0,err)

  !     Apply necessary conversion factors
  call eos_SetOption(th(1),EOS_Y_CONVERT,u_convert,err)
  call eos_SetOption(th(1),EOS_F_CONVERT,p_convert,err)
  call eos_SetOption(th(2),EOS_Y_CONVERT,u_convert,err)
  call eos_SetOption(th(3),EOS_F_CONVERT,u_convert,err)
  call eos_SetOption(th(4),EOS_Y_CONVERT,p_convert,err)
  call eos_SetOption(th(4),EOS_F_CONVERT,u_convert,err)
  if (ntables.GE.5) call eos_SetOption(th(5),EOS_Y_CONVERT,p_convert,err)

  call eos_LoadTables(ntablesE,th,err)

  if (err .NE. EOS_OK) then
     call eos_GetErrorMessage ( err, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', err, ': ', errorMessage(1:(len_trim(errorMessage)-1))
     if (err .NE. EOS_OK) then
        do i=1, nTablesE
           thErr = EOS_OK
           call eos_GetErrorCode ( th(i), thErr )
           if (thErr .NE. EOS_OK) then
              call eos_GetErrorMessage ( thErr, errorMessage )
              write(*,998) 'eos_LoadTables ERROR ', thErr, ': ', errorMessage(1:(len_trim(errorMessage)-1))
           endif
        enddo
     endif
  endif

  call eos_DestroyAll(err)

  write(*,'(a,i2,a)') 'test ', j, ' PASSED :: all tables were successfully loaded and destroyed'

  if (j .EQ. 0) goto 10

998 format (a,i6,2a)

end program test
