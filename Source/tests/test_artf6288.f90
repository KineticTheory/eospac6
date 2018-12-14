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
!! @brief Verify that EOS_V_PtT is available via the eos_Interface F90 module.
!!
!! \note
!! MATIDS TO TEST: 2140
program test_artf6288

  use eos_Interface

  implicit none

  integer(EOS_INTEGER),parameter :: nTables = 1
  integer(EOS_INTEGER),parameter :: nXYPairs = 16

  integer(EOS_INTEGER) ::  i, j
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs), F(nXYPairs), dFx(nXYPairs), dFy(nXYPairs)
  integer(EOS_INTEGER) :: tableType(nTables), numIndVars(nTables)
  integer(EOS_INTEGER) :: matID(nTables)
  integer(EOS_INTEGER) :: tableHandle(nTables)
  integer(EOS_INTEGER) :: errorCode
  integer(EOS_INTEGER) :: tableHandleErrorCode
  character(11) :: tableTypeLabel(nTables) = (/ &
       'EOS_V_PtT  ' &
       /)
  character(EOS_MaxErrMsgLen) :: errorMessage

  !     EOS_Pt_DT, material 2140 works for Sesame table 301 (record type 1)
  !     EOS_Dv_T, material 2140 works for Sesame table 401 (record type 2)
  !     EOS_Ogb, material 12140 works for Sesame table 501 (record type 3)
  !     EOS_Comment, material 2140 works for Sesame tables 101-199 (record type 4)
  !     EOS_Info, material 2140 works for Sesame table 201 (record type 5)
  tableType(1) = EOS_V_PtT

  numIndVars(1) = 2

  matID(1) = 2140

  errorCode = EOS_OK
  do i=1, nTables
     tableHandle(i) = 0
  enddo

  !
  !     initialize table data objects
  !
  call eos_CreateTables ( nTables, tableType, matID, tableHandle, errorCode)
  if (errorCode.NE.EOS_OK) then
     do i=1, nTables
        tableHandleErrorCode = EOS_OK
        call eos_GetErrorCode ( tableHandle(i), tableHandleErrorCode )
        call eos_GetErrorMessage ( tableHandleErrorCode, errorMessage )
        write(*,998) 'eos_CreateTables ERROR ', tableHandleErrorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     enddo
  endif

  !
  !     set some options
  !
  do i=1, nTables
     !        enable data dump
     call eos_SetOption ( tableHandle(i), EOS_DUMP_DATA, EOS_NullVal, errorCode )
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,998) 'eos_SetOption ERROR ', errorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     endif
     !        enable custom smoothing
     call eos_SetOption ( tableHandle(i), EOS_PT_SMOOTHING, EOS_NullVal, errorCode )
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,998) 'eos_SetOption ERROR ', errorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     endif
  enddo

  !
  !     load data into table data objects
  !
  call eos_LoadTables ( nTables, tableHandle, errorCode)
  if (errorCode.NE.EOS_OK) then
     call eos_GetErrorMessage ( errorCode, errorMessage )
     write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ', &
                   errorMessage(1:(len_trim(errorMessage)-1))
     do i=1, nTables
        tableHandleErrorCode = EOS_OK
        call eos_GetErrorCode ( tableHandle(i), tableHandleErrorCode )
        call eos_GetErrorMessage ( tableHandleErrorCode, errorMessage )
        write(*,994) 'eos_LoadTables ERROR ', tableHandleErrorCode, ' (TH=', &
                     tableHandle(i), '): ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     enddo
  endif

  !
  !     interpolate -- errors codes are intentionally produced
  !
  X(1) = 1.000000000000001e-16_EOS_REAL
  X(2) = 5.263157894736736e+04_EOS_REAL
  X(3) = 1.052631578947347e+05_EOS_REAL
  X(4) = 1.578947368421021e+05_EOS_REAL
  X(5) = 1.000000000000001e-16_EOS_REAL
  X(6) = 5.263157894736736e+04_EOS_REAL
  X(7) = 1.052631578947347e+05_EOS_REAL
  X(8) = 1.578947368421021e+05_EOS_REAL
  X(9) = 1.000000000000001e-16_EOS_REAL
  X(10) = 5.263157894736736e+04_EOS_REAL
  X(11) = 1.052631578947347e+05_EOS_REAL
  X(12) = 1.578947368421021e+05_EOS_REAL
  X(13) = 1.000000000000001e-16_EOS_REAL
  X(14) = 5.263157894736736e+04_EOS_REAL
  X(15) = 1.052631578947347e+05_EOS_REAL
  X(16) = 1.578947368421021e+05_EOS_REAL

  Y(1) = 1.160450500000000e+02_EOS_REAL
  Y(2) = 1.160450500000000e+02_EOS_REAL
  Y(3) = 1.160450500000000e+02_EOS_REAL
  Y(4) = 1.160450500000000e+02_EOS_REAL
  Y(5) = 6.107635309900391e+08_EOS_REAL
  Y(6) = 6.107635309900391e+08_EOS_REAL
  Y(7) = 6.107635309900391e+08_EOS_REAL
  Y(8) = 6.107635309900391e+08_EOS_REAL
  Y(9) = 1.221526945935028e+09_EOS_REAL
  Y(10) = 1.221526945935028e+09_EOS_REAL
  Y(11) = 1.221526945935028e+09_EOS_REAL
  Y(12) = 1.221526945935028e+09_EOS_REAL
  Y(13) = 1.832290360880018e+09_EOS_REAL
  Y(14) = 1.832290360880018e+09_EOS_REAL
  Y(15) = 1.832290360880018e+09_EOS_REAL
  Y(16) = 1.832290360880018e+09_EOS_REAL

  do i=1, nTables

     !        enable custom interpolation
     call eos_SetOption ( tableHandle(i), EOS_USE_CUSTOM_INTERP, EOS_NullVal, errorCode )
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,998) 'eos_SetOption ERROR ', errorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     endif

     write(*,*) ' '
     write(*,997) '--- Interpolate using tableType ', tableTypeLabel(i),' ---'
     call eos_Interpolate ( tableHandle(i), nXYPairs, X, Y, F, dFx, dFy, errorCode)
     write(*,997) tableTypeLabel(i), ' Interpolation Results:'
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,994) 'eos_Interpolate ERROR ', errorCode, ' (TH=', &
                     tableHandle(i), '): ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     else
        do j=1, nXYPairs
           if (numIndVars(i).EQ.1) then
              write(*,996) j-1,X(j),F(j),dFx(j),errorCode
           endif
           if (numIndVars(i).EQ.2) then
              write(*,999) j-1,X(j),Y(j),F(j),errorCode
           endif
        enddo
     endif
  enddo

  !
  !     Destroy all data objects
  !
  call eos_DestroyAll (errorCode)
  if (errorCode.NE.EOS_OK) then
     do i=1, nTables
        tableHandleErrorCode = EOS_OK
        call eos_GetErrorCode ( tableHandle(i), tableHandleErrorCode )
        call eos_GetErrorMessage ( tableHandleErrorCode, errorMessage )
        write(*,998) 'eos_DestroyAll ERROR ', tableHandleErrorCode, ': ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
     enddo
  endif

994 format (a,i4,a,i1,2a)
996 format ('    i=',i2,'    X =',1pe23.15, &
            ', F =',1pe23.15,', dFx =',1pe23.15,', errorCode: ',i4)
997 format (a,:,a,:,2(a,:,i2))
998 format (a,i4,2a)
999 format ('    i=',i2,'    X =',1pe23.15,', Y =',1pe23.15, &
       ', F =',1pe23.15,', errorCode: ',i4)

end program test_artf6288
