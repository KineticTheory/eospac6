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
!! @brief Perform the following tests:
!!    -# Verify that the unexpected errors are not returned for a
!!       valid EOS_Gs_D table using material 34272.
!!
!!  See SourceForge issue
!!  <a href="https://tf.lanl.gov/sf/go/artf9708">artf9708</a>
!!  for details.
!!
!! \note
!! MATIDS TO TEST: 4272 34272
program TestF90

  use eos_Interface

  implicit none

  integer(EOS_INTEGER),parameter :: nTables = 13
  integer(EOS_INTEGER),parameter :: nXYPairs = 4
  integer(EOS_INTEGER),parameter :: nInfoItems = 12

  integer(EOS_INTEGER) ::  i, j
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs), F(nXYPairs), dFx(nXYPairs), dFy(nXYPairs)
  integer(EOS_INTEGER) :: tableType(nTables), numIndVars(nTables)
  integer(EOS_INTEGER) :: matID(nTables)
  integer(EOS_INTEGER) :: tableHandle(nTables)
  integer(EOS_INTEGER) :: errorCode
  integer(EOS_INTEGER) :: tableHandleErrorCode
  real(EOS_REAL) :: infoVals(nInfoItems)
  integer(EOS_INTEGER) :: infoItems(nInfoItems) = (/ &
       EOS_Cmnt_Len, &
       EOS_Exchange_Coeff, &
       EOS_F_Convert_Factor, &
       EOS_Log_Val, &
       EOS_Material_ID, &
       EOS_Mean_Atomic_Mass, &
       EOS_Mean_Atomic_Num, &
       EOS_Modulus, &
       EOS_Normal_Density, &
       EOS_Table_Type, &
       EOS_X_Convert_Factor, &
       EOS_Y_Convert_Factor &
       /)
  character(82) :: infoItemDescriptions(nInfoItems) = (/ &
       'The length in characters of the comments available for the specified data table   ', &
       'The exchange coefficient                                                          ', &
       'The conversion factor corresponding to the dependent variable, F(x,y)             ', &
       'Non-zero if the data table is in a log10 format                                   ', &
       'The SESAME material identification number                                         ', &
       'The mean atomic mass                                                              ', &
       'The mean atomic number                                                            ', &
       'The solid bulk modulus                                                            ', &
       'The normal density                                                                ', &
       'The type of data table. Corresponds to the parameters in APPENDIX B and APPENDIX C', &
       'The conversion factor corresponding to the primary independent variable, x        ', &
       'The conversion factor corresponding to the secondary independent variable, y      '  &
       /)
  character(11) :: tableTypeLabel(nTables) = (/ &
       'EOS_Ut_DT  ', &
       'EOS_Pt_DUt ', &
       'EOS_Pt_DT  ', &
       'EOS_T_DUt  ', &
       'EOS_Gs_D   ', &
       'EOS_Comment', &
       'EOS_Info   ', &
       'EOS_Ut_DT  ', &
       'EOS_Pt_DUt ', &
       'EOS_Pt_DT  ', &
       'EOS_T_DUt  ', &
       'EOS_Comment', &
       'EOS_Info   '  &
       /)
  character(EOS_MaxErrMsgLen) :: errorMessage

  logical equal

  tableType(1)  = EOS_Ut_DT
  tableType(2)  = EOS_Pt_DUt
  tableType(3)  = EOS_Pt_DT
  tableType(4)  = EOS_T_DUt
  tableType(5)  = EOS_Gs_D
  tableType(6)  = EOS_Comment
  tableType(7)  = EOS_Info
  tableType(8)  = EOS_Ut_DT
  tableType(9)  = EOS_Pt_DUt
  tableType(10) = EOS_Pt_DT
  tableType(11) = EOS_T_DUt
  tableType(12) = EOS_Comment
  tableType(13) = EOS_Info

  numIndVars(1)  = 2
  numIndVars(2)  = 2
  numIndVars(3)  = 2
  numIndVars(4)  = 2
  numIndVars(5)  = 1
  numIndVars(6)  = 0
  numIndVars(7)  = 0
  numIndVars(8)  = 2
  numIndVars(9)  = 2
  numIndVars(10) = 2
  numIndVars(11) = 2
  numIndVars(12) = 0
  numIndVars(13) = 0

  matID = 4272
  matID(5) = 34272

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
     !        enable smoothing
     call eos_SetOption ( tableHandle(i), EOS_SMOOTH, EOS_NullVal, errorCode )
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,'(a,i6,a,i2,a,a)') 'eos_SetOption ERROR ', errorCode, ' (TH=', &
             tableHandle(i), '): ', &
             errorMessage(1:(len_trim(errorMessage)-1))
     endif
     if (i.EQ.1) then
        call eos_SetOption ( tableHandle(i), EOS_DUMP_DATA, EOS_NullVal, errorCode )
     else
        call eos_SetOption ( tableHandle(i), EOS_APPEND_DATA, EOS_NullVal, errorCode )
     endif
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,'(a,i6,a,i2,a,a)') 'eos_SetOption ERROR ', errorCode, ' (TH=', &
             tableHandle(i), '): ', &
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
  X(1) = 3000._EOS_REAL
  X(2) = 6000._EOS_REAL
  X(3) = 8200._EOS_REAL
  X(4) = 8300._EOS_REAL

  Y(1) = 20000.0_EOS_REAL
  Y(2) = 620000.0_EOS_REAL
  Y(3) = 4000000.0_EOS_REAL
  Y(4) = 200000000.0_EOS_REAL

  do i=1, nTables
     write(*,*) ' '
     write(*,997) '--- Interpolate using tableType ', tableTypeLabel(i),' ---'
     call eos_Interpolate ( tableHandle(i), nXYPairs, X, Y, F, dFx, dFy, errorCode)
     write(*,997) tableTypeLabel(i), ' Interpolation Results:'
     if (errorCode.NE.EOS_OK) then
        call eos_GetErrorMessage ( errorCode, errorMessage )
        write(*,994) 'eos_Interpolate ERROR ', errorCode, ' (TH=', &
                     tableHandle(i), '): ', &
                     errorMessage(1:(len_trim(errorMessage)-1))
        call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED, errorCode, equal)
        if (.NOT. equal) cycle
     endif
     do j=1, nXYPairs
        if (numIndVars(i).EQ.1) then
           write(*,996) j-1,X(j),F(j),dFx(j),errorCode
        endif
        if (numIndVars(i).EQ.2) then
           write(*,999) j-1,X(j),Y(j),F(j),dFx(j),dFy(j),errorCode
        endif
     enddo
  enddo

  !
  !     Retrieve all miscellaneous table info
  !
  do i=1, nTables
     write(*,*) ' '
     write(*,997) '--- Table information for tableType ', tableTypeLabel(i), &
                  ', tableHandle=', tableHandle(i), ' ---'
     do j=1, nInfoItems
        call eos_GetTableInfo ( tableHandle(i), 1_EOS_INTEGER, infoItems(j), &
                                infoVals(j), errorCode )
        call eos_ErrorCodesEqual(EOS_INVALID_INFO_FLAG, errorCode, equal)
        if (errorCode.EQ.EOS_OK) then
           write(*,995) j,'. ',infoItemDescriptions(j), ': ', infoVals(j)
        else if (.NOT. equal) then
           ! Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
           ! applicable to a specific tableHandle.
           call eos_GetErrorMessage ( errorCode, errorMessage )
           write(*,998) 'eos_GetTableInfo ERROR ', errorCode, ': ', &
                        errorMessage(1:(len_trim(errorMessage)-1))
        endif
     enddo
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

994 format (a,i6,a,i2,2a)
995 format (i2,a,a,a,f13.6)
996 format ('    i=',i2,'    X =',1pe13.6, &
            ', F =',1pe13.6,', dFx =',1pe13.6,', errorCode: ',i4)
997 format (a,:,a,:,2(a,:,i2))
998 format (a,i6,2a)
999 format ('    i=',i2,'    X =',1pe13.6,', Y =',1pe13.6, &
       ', F =',1pe13.6,', dFx =',1pe13.6,', dFy =', &
       1pe13.6,', errorCode: ',i6)

end program TestF90
