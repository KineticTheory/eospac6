!*********************************************************************
!  Module for Fortran 2003 code to set constants used by EOSPAC
!  ------------------------------------------------------------
!  Filetype: (HEADER)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************
module eos_Interface2003
  use iso_fortran_env
  use iso_c_binding
  !
  ! Import F90 parameters
  !
  use eos_Interface

  implicit none
  public eos_GetErrorMessage, eos_GetPackedTables, eos_GetTableCmnts, eos_SetPackedTables, eos_SetDataFileName, eos_GetVersion

  ! C binding Interfaces

  interface eos_CheckExtrap
     pure subroutine eos_CheckExtrap(tableHandle, nXYPairs, xVals, yVals, xyBounds, errorCode) &
          bind(C, name="eos_CheckExtrap")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: xVals(nXYPairs)
       real(EOS_REAL), intent(in) :: yVals(nXYPairs)
       integer(EOS_INTEGER), intent(out) :: xyBounds(nXYPairs)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_CheckExtrap
     pure subroutine eos_CheckExtrap_scalar(tableHandle, nXYPairs, xVals, yVals, xyBounds, errorCode) &
          bind(C, name="eos_CheckExtrap")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: xVals
       real(EOS_REAL), intent(in) :: yVals
       integer(EOS_INTEGER), intent(out) :: xyBounds
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_CheckExtrap_scalar
  end interface

  interface eos_CreateTables
     pure subroutine eos_CreateTables(nTables, tableType, matID, tableHandles, errorCode) &
          bind(C, name="eos_CreateTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableType(nTables)
       integer(EOS_INTEGER), intent(in) :: matID(nTables)
       integer(EOS_INTEGER), intent(out) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_CreateTables
     pure subroutine eos_CreateTables_scalar(nTables, tableType, matID, tableHandles, errorCode) &
          bind(C, name="eos_CreateTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableType
       integer(EOS_INTEGER), intent(in) :: matID
       integer(EOS_INTEGER), intent(out) :: tableHandles
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_CreateTables_scalar
  end interface

  interface eos_DestroyAll
     pure subroutine eos_DestroyAll(errorcode) &
          bind(C, name="eos_DestroyAll")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_DestroyAll
  end interface

  interface eos_DestroyTables
     pure subroutine eos_DestroyTables(nTables, tableHandles, errorCode) &
          bind(C, name="eos_DestroyTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_DestroyTables
     pure subroutine eos_DestroyTables_scalar(nTables, tableHandles, errorCode) &
          bind(C, name="eos_DestroyTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_DestroyTables_scalar
  end interface

  interface eos_GetErrorCode
     pure subroutine eos_GetErrorCode(tableHandle, errorCode) &
          bind(C, name="eos_GetErrorCode")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetErrorCode
  end interface

  interface eos_GetPackedTablesSize
     pure subroutine eos_GetPackedTablesSize(nTables, tableHandles, packedTablesSize, errorCode) &
          bind(C, name="eos_GetPackedTablesSize")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(out) :: packedTablesSize
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetPackedTablesSize
     pure subroutine eos_GetPackedTablesSize_scalar(nTables, tableHandles, packedTablesSize, errorCode) &
          bind(C, name="eos_GetPackedTablesSize")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles
       integer(EOS_INTEGER), intent(out) :: packedTablesSize
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetPackedTablesSize_scalar
  end interface

  interface eos_GetTableInfo
     pure subroutine eos_GetTableInfo(tableHandle, numInfoItems, infoItems, infoVals, errorCode) &
          bind(C, name="eos_GetTableInfo")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: numInfoItems
       integer(EOS_INTEGER), intent(in) :: infoItems(numInfoItems)
       real(EOS_REAL), intent(out) :: infoVals(numInfoItems)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetTableInfo
     pure subroutine eos_GetTableInfo_scalar(tableHandle, numInfoItems, infoItems, infoVals, errorCode) &
          bind(C, name="eos_GetTableInfo")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: numInfoItems
       integer(EOS_INTEGER), intent(in) :: infoItems
       real(EOS_REAL), intent(out) :: infoVals
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetTableInfo_scalar
  end interface

  interface eos_Interpolate
     pure subroutine eos_Interpolate(tableHandle, nXYPairs, xVals, yVals, fVals, dFx, dFy, errorCode) &
          bind(C, name="eos_Interpolate")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: xVals(*)
       real(EOS_REAL), intent(in) :: yVals(*)
       real(EOS_REAL), intent(out) :: fVals(*)
       real(EOS_REAL), intent(out) :: dFx(*)
       real(EOS_REAL), intent(out) :: dFy(*)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_Interpolate
     pure subroutine eos_Interpolate_scalar(tableHandle, nXYPairs, xVals, yVals, fVals, dFx, dFy, errorCode) &
          bind(C, name="eos_Interpolate")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: xVals
       real(EOS_REAL), intent(in) :: yVals
       real(EOS_REAL), intent(out) :: fVals
       real(EOS_REAL), intent(out) :: dFx
       real(EOS_REAL), intent(out) :: dFy
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_Interpolate_scalar
  end interface

  interface eos_LoadTables
     pure subroutine eos_LoadTables(nTables, tableHandles, errorCode) &
          bind(C, name="eos_LoadTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(out) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_LoadTables
     pure subroutine eos_LoadTables_scalar(nTables, tableHandles, errorCode) &
          bind(C, name="eos_LoadTables")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(out) :: tableHandles
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_LoadTables_scalar
  end interface

  interface eos_Mix
     pure subroutine eos_Mix(nTables, tableHandles, nXYPairs, concInMix, xVals, yVals, fVals, dFx, dFy, errorCode) &
          bind(C, name="eos_Mix")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: concInMix(nXYPairs, nTables)
       real(EOS_REAL), intent(in) :: xVals(nXYPairs)
       real(EOS_REAL), intent(in) :: yVals(nXYPairs)
       real(EOS_REAL), intent(out) :: fVals(nXYPairs)
       real(EOS_REAL), intent(out) :: dFx(nXYPairs)
       real(EOS_REAL), intent(out) :: dFy(nXYPairs)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_Mix
     pure subroutine eos_Mix_scalar(nTables, tableHandles, nXYPairs, concInMix, xVals, yVals, fVals, dFx, dFy, errorCode) &
          bind(C, name="eos_Mix")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(in) :: nXYPairs
       real(EOS_REAL), intent(in) :: concInMix(nXYPairs, nTables)
       real(EOS_REAL), intent(in) :: xVals
       real(EOS_REAL), intent(in) :: yVals
       real(EOS_REAL), intent(out) :: fVals
       real(EOS_REAL), intent(out) :: dFx
       real(EOS_REAL), intent(out) :: dFy
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_Mix_scalar
  end interface

  interface eos_SetOption
     pure subroutine eos_SetOption(tableHandle, tableOption, tableOptionVal, errorCode) &
          bind(C, name="eos_SetOption")
       import :: EOS_INTEGER, EOS_REAL
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: tableOption
       real(EOS_REAL), intent(in) :: tableOptionVal
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_SetOption
  end interface

  interface eos_ResetOption
     pure subroutine eos_ResetOption(tableHandle, tableOption, errorCode) &
          bind(C, name="eos_ResetOption")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: tableOption
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_ResetOption
  end interface

  interface eos_GetMaxDataFileNameLength
     pure subroutine eos_GetMaxDataFileNameLength(max_length) &
          bind(C, name="eos_GetMaxDataFileNameLength")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(out) :: max_length
     end subroutine eos_GetMaxDataFileNameLength
  end interface

  interface eos_ErrorCodesEqual
     pure subroutine eos_ErrorCodesEqual(err1, err2, result) &
          bind(C, name="eos_ErrorCodesEqual")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(in) :: err1
       integer(EOS_INTEGER), intent(in) :: err2
       logical, intent(out) :: result
     end subroutine eos_ErrorCodesEqual
  end interface

  interface eos_time
     pure subroutine eos_time(reset, wctime, cputime, cpucycles, err) &
          bind(C, name="eos_Time")
       import :: EOS_INTEGER, EOS_REAL
       logical, intent(in) :: reset
       real(EOS_REAL), intent(out) :: wctime
       real(EOS_REAL), intent(out) :: cputime
       real(EOS_REAL), intent(out) :: cpucycles
       integer(EOS_INTEGER), intent(out) :: err
     end subroutine eos_time
  end interface

  interface eos_GetVersionLength 
     pure subroutine  eos_GetVersionLength(length) &
          bind(C, name="eos_GetVersionLength")
       import :: EOS_INTEGER
       integer(EOS_INTEGER), intent(out) :: length
     end subroutine  eos_GetVersionLength
  end interface

  interface eos_GetErrorMessage_c
     pure subroutine eos_GetErrorMessage_c(errorcode, errorMsg) &
          bind(C, name="eos_GetErrorMessage")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: errorCode
       character(kind=c_char, len=1), dimension(*), intent(out) :: errorMsg
     end subroutine eos_GetErrorMessage_c
  end interface

  interface eos_GetPackedTables_c
     pure subroutine eos_GetPackedTables_c(nTables, tableHandles, packedTables, errorCode) &
          bind(C, name="eos_GetPackedTables")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       character(kind=c_char, len=1), dimension(*),  intent(out) :: packedTables
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetPackedTables_c
     pure subroutine eos_GetPackedTables_c_scalar(nTables, tableHandles, packedTables, errorCode) &
          bind(C, name="eos_GetPackedTables")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: tableHandles
       character(kind=c_char, len=1), dimension(*),  intent(out) :: packedTables
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetPackedTables_c_scalar
  end interface

  interface eos_GetMetaData_c
     pure subroutine eos_GetMetaData_c(infoItem, infoItemCategory, infoStr, errorCode) &
          bind(C, name="eos_GetMetaData")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: infoItem
       integer(EOS_INTEGER), intent(in) :: infoItemCategory
       character(kind=c_char, len=1), dimension(*), intent(out) :: infoStr
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetMetaData_c
  end interface

  interface eos_GetTableMetaData_c
     pure subroutine eos_GetTableMetaData_c(tableHandle, infoItem, infoStr, errorCode) &
          bind(C, name="eos_GetTableMetaData")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: infoItem
       character(kind=c_char, len=1), dimension(*), intent(inout) :: infoStr
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetTableMetaData_c
  end interface

  interface eos_GetTableCmnts_c
     pure subroutine eos_GetTableCmnts_c(tableHandle, cmntStr, errorCode) &
          bind(C, name="eos_GetTableCmnts")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: tableHandle
       character(kind=c_char, len=1), dimension(*), intent(out) :: cmntStr
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_GetTableCmnts_c
  end interface

  interface eos_SetPackedTables_c
     pure subroutine eos_SetPackedTables_c(nTables, packedTablesSize, packedTables, tableHandles, errorCode) &
          bind(C, name="eos_SetPackedTables")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: packedTablesSize
       character(kind=c_char, len=1), dimension(*), intent(in) :: packedTables
       integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_SetPackedTables_c
     pure subroutine eos_SetPackedTables_c_scalar(nTables, packedTablesSize, packedTables, tableHandles, errorCode) &
          bind(C, name="eos_SetPackedTables")
       import :: EOS_INTEGER, c_char
       integer(EOS_INTEGER), intent(in) :: nTables
       integer(EOS_INTEGER), intent(in) :: packedTablesSize
       character(kind=c_char, len=1), dimension(*), intent(in) :: packedTables
       integer(EOS_INTEGER), intent(in) :: tableHandles
       integer(EOS_INTEGER), intent(out) :: errorCode
     end subroutine eos_SetPackedTables_c_scalar
  end interface

  interface  eos_SetDataFileName_c
     pure subroutine eos_SetDataFileName_c(tableHandle, matID, tableType, fileName, errorCode, fileName_len) &
          bind(C, name="eos_SetDataFileName")
       import :: EOS_INTEGER, C_LONG, c_char
       integer(EOS_INTEGER), intent(in) :: tableHandle
       integer(EOS_INTEGER), intent(in) :: matID
       integer(EOS_INTEGER), intent(in) :: tableType
       character(kind=c_char, len=1), dimension(*), intent(in) :: fileName
       integer(EOS_INTEGER), intent(out) :: errorCode
       integer(C_LONG), intent(in) :: fileName_len
     end subroutine eos_SetDataFileName_c
  end interface

  interface eos_GetVersion_c
     pure subroutine eos_GetVersion_c(version) &
          bind(C, name="eos_GetVersion")
       import :: EOS_INTEGER, c_char
       character(kind=c_char, len=1), dimension(*), intent(out) :: version
     end subroutine eos_GetVersion_c
  end interface

!
! The following are wrapper routines specifically-designed to properly handle string passing.
!

contains

  pure subroutine eos_GetErrorMessage(errorcode, errorMsg)
    integer(EOS_INTEGER), intent(in) :: errorCode
    character(*), intent(out) :: errorMsg
    character(kind=c_char, len=1) c_errorMsg(len(errorMsg))
    integer :: n
    call eos_GetErrorMessage_c(errorcode, c_errorMsg)
    do n = 1, len(errorMsg)
       errorMsg(n:n) = c_errorMsg(n)
    enddo
  end subroutine eos_GetErrorMessage

  pure subroutine eos_GetPackedTables(nTables, tableHandles, packedTables, errorCode)
    integer(EOS_INTEGER), intent(in) :: nTables
    integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
    character(*), intent(out) :: packedTables
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) c_packedTables(len(packedTables))
    integer :: n
    call eos_GetPackedTables_c(nTables, tableHandles, c_packedTables, errorCode)
    do n=1, len(packedTables)
       packedTables(n:n) =  c_packedTables(n)
    enddo
  end subroutine eos_GetPackedTables

  pure subroutine eos_GetMetaData(infoItem, infoItemCategory, infoStr, errorCode)
    integer(EOS_INTEGER), intent(in) :: infoItem
    integer(EOS_INTEGER), intent(in) :: infoItemCategory
    character(*), intent(out) :: infoStr
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) c_infoStr(len(infoStr))
    integer :: n
    call eos_GetMetaData_c(infoItem, infoItemCategory, c_infoStr, errorCode)
    do n=1, len(infoStr)
       infoStr(n:n) =  c_infoStr(n)
    enddo
  end subroutine eos_GetMetaData

  pure subroutine eos_GetTableMetaData(tableHandle, infoItem, infoStr, errorCode)
    integer(EOS_INTEGER), intent(in) :: tableHandle
    integer(EOS_INTEGER), intent(in) :: infoItem
    character(*), intent(inout) :: infoStr
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) c_infoStr(len(infoStr))
    integer :: n
    call eos_GetTableMetaData_c(tableHandle, infoItem, c_infoStr, errorCode)
    do n=1, len(infoStr)
       infoStr(n:n) =  c_infoStr(n)
    enddo
  end subroutine eos_GetTableMetaData

  pure subroutine eos_GetTableCmnts(tableHandle, cmntStr, errorCode)
    integer(EOS_INTEGER), intent(in) :: tableHandle
    character(*), intent(out) :: cmntStr
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) c_cmntStr(len(cmntStr))
    integer :: n
    call eos_GetTableCmnts_c(tableHandle, c_cmntStr, errorCode)
    do n=1, len(cmntStr)
       cmntStr(n:n) =  c_cmntStr(n)
    enddo
  end subroutine eos_GetTableCmnts

  pure subroutine eos_SetPackedTables(nTables, packedTablesSize, packedTables, tableHandles, errorCode)
    integer(EOS_INTEGER), intent(in) :: nTables
    integer(EOS_INTEGER), intent(in) :: packedTablesSize
    character(*), intent(in) :: packedTables
    integer(EOS_INTEGER), intent(in) :: tableHandles(nTables)
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) :: c_packedTables(len(packedTables))
    integer :: n
    do n=1, len(packedTables)
       c_packedTables(n) =  packedTables(n:n)
    enddo
    call eos_SetPackedTables_c(nTables, packedTablesSize, c_packedTables, tableHandles, errorCode)
  end subroutine eos_SetPackedTables

  pure subroutine eos_SetDataFileName(tableHandle, matID, tableType, fileName, errorCode)
    integer(EOS_INTEGER), intent(in) :: tableHandle
    integer(EOS_INTEGER), intent(in) :: matID
    integer(EOS_INTEGER), intent(in) :: tableType
    character(*), intent(in) :: fileName
    integer(EOS_INTEGER), intent(out) :: errorCode
    character(kind=c_char, len=1) :: c_fileName(len_trim(fileName)+1)
    integer(C_LONG) :: fileName_len
    integer :: n
    fileName_len = len_trim(fileName)
    do n=1,len_trim(fileName)
       c_fileName(n) = fileName(n:n)
    enddo
    c_fileName(n) = char(0)
    call eos_SetDataFileName_c(tableHandle, matID, tableType, c_fileName, errorCode, fileName_len)
  end subroutine eos_SetDataFileName

  pure subroutine eos_GetVersion(version)
    character(*), intent(out) :: version
    character(kind=c_char, len=1) c_version(len(version))
    integer :: n
    call eos_GetVersion_c(c_version)
    do n=1, len(version)
       version(n:n) =  c_version(n)
    enddo
  end subroutine eos_GetVersion

end module eos_Interface2003
