!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!
!  PURPOSE: Run test006 using EOSPAC 5 functionality.
!  
!********************************************************************

program test006c

  use eos_Interface
  use dataInterface
  implicit none

  integer(EOS_INTEGER),parameter :: nTableTypes = 1
  integer(EOS_INTEGER),parameter :: nMatIDs = 3
  integer(EOS_INTEGER),parameter :: nTables = nTableTypes * nMatIDs
  integer(EOS_INTEGER),parameter :: nColdTables = nTableTypes * nMatIDs
  integer(EOS_INTEGER),parameter :: nXYPairs = 25

  integer(EOS_INTEGER) ::  i, j, k, m, n, p
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs), Ycold(nXYPairs), F(nXYPairs), dFx(nXYPairs), &
                    dFy(nXYPairs)
  real(EOS_REAL) :: Fideal(nXYPairs,3) ! Fideal and dF/dx and dF/dx for each nXYPair
  real(EOS_REAL) :: fcold5(nXYPairs,3,1), fcold6(nXYPairs,3,1) ! Fcold and dF/dx for each nXYPair
  integer(EOS_INTEGER) :: tableTypes(nTables), matID(nTables), coldCurveTypes(nTables), &
                          coldCurveTH(nColdTables)
  logical :: mask(nTables)
  integer(EOS_INTEGER) :: tableTypesList(nTableTypes), matIDList(nMatIDs), &
                          coldCurveTypesList(nTableTypes)
  integer(EOS_INTEGER) :: errorCode, iType, materialID, extrapCode(nXYPairs)
  
  character*(13) :: codeLabel

  !     ES1TABS subroutine parameters
  integer, parameter :: mtyps=nTableTypes   ! number of tables to load
  integer, parameter :: mregs=nMatIDs       ! number of problem regions
  integer            :: iopt(mtyps*mregs), inams(mtyps), ieosf, &
                        imids(mtyps*mregs), mtabs, ierrs(mtyps*mregs)
  logical            :: llog1(mtyps*mregs)
  real*8             :: ucons(mtyps*3), ftabs
  pointer (ktabs,ftabs(1))

  !     ES1VALS subroutine parameters
  integer, parameter :: nzons=nXYPairs ! number of interpolated F values to return (zones)
  integer            :: iregn
  real*8             :: fvals(nzons,3)   ! F, dF/dx, and dF/dy in each zone
      
  character*60 errorMessage
  character*8  cname

  ! EOS_Pc_D = ES4_PRCLD
  ! EOS_Uc_D = ES4_ENCLD
  coldCurveTypesList(1) = EOS_Pc_D

  matIDList(1:nMatIDs) = ( /9991, 9993, 9995/ )

  errorCode = EOS_OK

  coldCurveTypes = coldCurveTypesList(1)
  matID          = reshape(spread(matIDList,1,nTableTypes),(/nTables/))

  !
  !     Load desired data tables using EOSPAC 5
  !
  ! Define EOSPAC 5 data types
  inams(1:mtyps) = ( /ES4_PRCLD/ )
  llog1 = .FALSE.
  iopt  = 0
  ucons = 1.d0
  imids = matIDList

  call es1tabs(llog1, iopt, .TRUE., 0, mtyps, mregs, inams, ucons, imids, &
               0, mtabs, ktabs, ierrs)

  if (sum(ierrs).NE.ES5_OK) then
     do j = 1, mregs
        do i = 1, mtyps
           k = i+(j-1)*mtyps
           call es1errmsg(ierrs(k),errorMessage)
           call es1name(inams(i),cname)
           write(*,'(a,i3,2a)') 'es1tabs ERROR', ierrs(k), '(' // trim(cname) // ')', &
                                trim(': '//errorMessage)
        enddo
     enddo
  endif

  !
  !     initialize table data objects
  !
  call eos_CreateTables(nColdTables, coldCurveTypes, matID, coldCurveTH, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(coldCurveTH, nColdTables, 'eos_CreateTables')
  endif

  !
  !     Enable data dump to file
  !
  call eos_SetOption(coldCurveTH(1), EOS_DUMP_DATA, EOS_NullVal, errorCode)

  !
  !     load data into table data objects
  !
  call eos_LoadTables(nColdTables, coldCurveTH, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(coldCurveTH, nColdTables, 'eos_LoadTables')
  endif

  !
  !     interpolate pure materials
  !
  do i=1, nColdTables

     Ycold = 0_EOS_REAL
     fcold5  = 0_EOS_REAL
     fcold6  = 0_EOS_REAL

     ! Set X and Y values for all nXYPairs and get data type and material ID for
     ! the current tableHandle
     call setXandYVALS(coldCurveTH, nColdTables, i, nXYPairs, X, Y, iType, materialID)

     write(*,*) ' '
     write(*,*) '"--- Interpolate using tableType ', &
                trim(codeLabel(iType)), '(', iType, ') and materialID ', &
                int(materialID), ' ---"'

     ! get Pressure cold curve data
     call getType5fromTH(coldCurveTH(i),ieosf)
     iregn = i

     call eos_Interpolate(coldCurveTH(i), nXYPairs, X, Ycold, fcold6(:,1,1), fcold6(:,2,1), fcold6(:,3,1), errorCode)

     extrapCode = EOS_OK
     if (errorCode.EQ.EOS_INTERP_EXTRAPOLATED) then
        call eos_CheckExtrap(coldCurveTH(i), nXYPairs, X, Y, extrapCode, errorCode)
     endif

     call es1vals(ieosf,ES4_DERIVS,ES4_BIRATF,ftabs,mtabs,nzons,iregn,X,Ycold,fcold5(:,:,1),3_EOS_INTEGER,errorCode)

     fcold5(:,2,1) = fcold5(:,2,1) / max(X, 1.0d-99)

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'eos_Interpolate(Pc)')
     endif

     ! write results to STDOUT
     write(*,997) 'EOSPAC 6                                     ','EOSPAC 5                                     '
     write(*,997) '---------------------------------------------','---------------------------------------------'
     write(*,998) 'i','X','Y','Pc','dPc/dx','Pc','dPc/dx'
     do j=1, nXYPairs
        if (extrapCode(j).NE.EOS_OK) then
           write(*,999) j,X(j),Y(j),fcold6(j,1,1),fcold6(j,2,1),fcold5(j,1,1),fcold5(j,2,1),codeLabel(extrapCode(j))
        else
           write(*,999) j,X(j),Y(j),fcold6(j,1,1),fcold6(j,2,1),fcold5(j,1,1),fcold5(j,2,1)
        endif
     enddo
  enddo

997 format(50x,a46,a46)
998 format(a4,6a23)
999 format(i4,6es23.15,:,1x,a)

end program test006c

! ===========================================================================
! Print error messages for all the tables.
! ===========================================================================
subroutine print_table_errors(tableHandles, nTables, label)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: nTables
  integer(EOS_INTEGER) :: i
  integer(EOS_INTEGER) :: tableHandles(nTables)
  character(*) :: label
  integer(EOS_INTEGER) :: errorCode, ierr
  character(EOS_MaxErrMsgLen) :: errorMessage

  ierr = EOS_OK

  do i=1, nTables
     errorCode = EOS_OK
     call eos_GetErrorCode(tableHandles(i), errorCode)
     call eos_GetErrorMessage(errorCode, errorMessage)
     write(*,'(1x,i4,3a,i4,2a)') i, '. ', trim(label), ' ERROR ', errorCode, ': ', &
          errorMessage(1:(len_trim(errorMessage)-1))
     if (errorCode.EQ.EOS_INTERP_EXTRAPOLATED) then
        ! do nothing here
     else
        ierr = -1_EOS_INTEGER
     endif
  enddo

  if (ierr.NE.EOS_OK) stop

end subroutine print_table_errors

! ===========================================================================
! Print a single error.
! ===========================================================================
subroutine print_error(errorCode, label)
  use eos_Interface
  implicit none
  character(*) :: label
  integer(EOS_INTEGER) :: errorCode
  character(EOS_MaxErrMsgLen) :: errorMessage

  call eos_GetErrorMessage(errorCode, errorMessage)
  write(*,'(2a,i4,2a)') trim(label), ' ERROR ', errorCode, ': ', &
       errorMessage(1:(len_trim(errorMessage)-1))

  if (errorCode.EQ.EOS_INTERP_EXTRAPOLATED) then
     ! do nothing here
  else
     stop
  endif
end subroutine print_error

module reallocate_mod
contains
! ===========================================================================
! Reallocate a 1-D EOS_INTEGER array to n elements.
! ===========================================================================
function reallocate(p, n)               ! reallocate EOS_INTEGER
  use eos_Interface
  implicit none
  integer(EOS_INTEGER), pointer, dimension(:) :: p, reallocate
  integer(EOS_INTEGER), intent(in) :: n
  integer(EOS_INTEGER) :: nold, ierr
  allocate(reallocate(1:n), STAT=ierr)
  if(ierr /= 0) stop "reallocate: allocation error"
  if(.NOT.associated(p)) then
     ! Allocate array
     allocate(p(n), STAT=ierr)
     if(ierr /= 0) stop "reallocate: allocation error"
  endif
  nold = min(size(p), n)
  reallocate(1:nold) = p(1:nold)
  deallocate(p) 
end function reallocate
end module reallocate_mod

! ===========================================================================
! Assign X and Y values for interpolation based upon data type.
! ===========================================================================
subroutine setXandYVALS( &
     tableHandles, nTableHandles, THindex,nXYPairs,X,Y,iType,matID)
  use eos_Interface
  use reallocate_mod
  implicit none
  integer(EOS_INTEGER) :: tableHandles(nTableHandles), nTableHandles, &
                          THindex, nXYPairs, iType, matID
  real(EOS_REAL) :: minD, maxD, minT, maxT, minP, maxP, minU, maxU
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs)
  real(EOS_REAL) :: minX, maxX, minY, maxY, infoVal, rhoref, dx, dy
  integer(EOS_INTEGER) :: i, errorCode, iTH, infoTableIndex
  logical :: infoLoaded
  
  integer(EOS_INTEGER), pointer, save :: infoTH(:)
  integer(EOS_INTEGER), pointer, save :: infoMatIDs(:)
  integer(EOS_INTEGER), save :: ninfoTables = 0

  ! Cross-reference to EOSPAC 5 data type labels
  ! EOS_Kec_DT   = ES4_ECONDE
  ! EOS_Uc_D     = ES4_ENCLD
  ! EOS_Ue_DT    = ES4_ENELC
  ! EOS_Uic_DT   = ES4_ENION
  ! EOS_Ut_DT    = ES4_ENTOT
  ! EOS_Keo_DT   = ES4_OPACC2
  ! EOS_Kc_DT    = ES4_OPACC3
  ! EOS_Kp_DT    = ES4_OPACP
  ! EOS_Kr_DT    = ES4_OPACR
  ! EOS_Pc_D     = ES4_PRCLD
  ! EOS_Pe_DT    = ES4_PRELC
  ! EOS_Pic_DT   = ES4_PRION
  ! EOS_Pt_DT    = ES4_PRTOT
  ! EOS_Ktc_DT   = ES4_TCONDE
  ! EOS_B_DT     = ES4_THERME
  ! EOS_Zfo_DT   = ES4_ZFREE2
  ! EOS_Zfc_DT   = ES4_ZFREE3
  ! EOS_Ue_DPe   = ES4_EPELC
  ! EOS_Uic_DPic = ES4_EPION
  ! EOS_Ut_DPt   = ES4_EPTOT
  ! EOS_T_DPe    = ES4_TPELC
  ! EOS_T_DPic   = ES4_TPION
  ! EOS_T_DPt    = ES4_TPTOT
  ! EOS_Pe_DUe   = ES4_PNELC
  ! EOS_Pic_DUic = ES4_PNION
  ! EOS_Pt_DUt   = ES4_PNTOT
  ! EOS_T_DUe    = ES4_TNELC
  ! EOS_T_DUic   = ES4_TNION
  ! EOS_T_DUt    = ES4_TNTOT

  ! Get data type for current tableHandle
  call eos_GetTableInfo(tableHandles(THindex), 1_EOS_INTEGER, EOS_Table_Type, &
                        infoVal, errorCode)
  iType = infoVal
  if (errorCode.NE.EOS_OK) then
     call print_error(errorCode, 'setXandYVALS->eos_GetTableInfo')
  endif

  ! Get matID for current tableHandle
  call eos_GetTableInfo(tableHandles(THindex), 1_EOS_INTEGER, EOS_Material_ID, &
                        infoVal, errorCode)
  matID = infoVal
  if (errorCode.NE.EOS_OK) then
     call print_error(errorCode, 'setXandYVALS->eos_GetTableInfo')
  endif

  ! Gather independent variable ranges
!  minD = 1.767701112000000e-07_EOS_REAL
  minD = 1.700000000000000E+02_EOS_REAL
!  maxD = 1.767701112000000e+03_EOS_REAL
  maxD = 1.000000000000000E+06_EOS_REAL
  minT = 1.450606250000000e+02_EOS_REAL
  maxT = 1.160485000000000e+09_EOS_REAL

  !
  ! Assign minX, maxX, minY and maxY values
  !
  minX = log10(max(0.000001_EOS_REAL,minD))
  maxX = log10(max(0.000001_EOS_REAL,maxD))
  minY = log10(max(0.000001_EOS_REAL,minT))
  maxY = log10(max(0.000001_EOS_REAL,maxT))

  do i = 1, nXYPairs
     X(i) = (maxX-minX) * dble(i-1) / dble(nXYPairs-1) + minX
     X(i) = 10._EOS_REAL**(X(i))
     Y(i) = (maxY-minY) * dble(i-1) / dble(nXYPairs-1) + minY
     Y(i) = 10._EOS_REAL**(Y(i))
  enddo
end subroutine setXandYVALS

! ===========================================================================
! Return character string label corresponding to iType.
! ===========================================================================
character*(13) function codeLabel(iType)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: iType
  select case (iType)
  case(EOS_Ae_DPe)
     codeLabel = 'EOS_Ae_DPe'
  case(EOS_Ae_DSe)
     codeLabel = 'EOS_Ae_DSe'
  case(EOS_Ae_DUe)
     codeLabel = 'EOS_Ae_DUe'
  case(EOS_Aic_DPic)
     codeLabel = 'EOS_Aic_DPic'
  case(EOS_Aic_DSic)
     codeLabel = 'EOS_Aic_DSic'
  case(EOS_Aic_DUic)
     codeLabel = 'EOS_Aic_DUic'
  case(EOS_Aiz_DPiz)
     codeLabel = 'EOS_Aiz_DPiz'
  case(EOS_Aiz_DSiz)
     codeLabel = 'EOS_Aiz_DSiz'
  case(EOS_Aiz_DUiz)
     codeLabel = 'EOS_Aiz_DUiz'
  case(EOS_At_DPt)
     codeLabel = 'EOS_At_DPt'
  case(EOS_At_DSt)
     codeLabel = 'EOS_At_DSt'
  case(EOS_At_DUt)
     codeLabel = 'EOS_At_DUt'
  case(EOS_Pe_DAe)
     codeLabel = 'EOS_Pe_DAe'
  case(EOS_Pe_DSe)
     codeLabel = 'EOS_Pe_DSe'
  case(EOS_Pe_DUe)
     codeLabel = 'EOS_Pe_DUe'
  case(EOS_Pic_DAic)
     codeLabel = 'EOS_Pic_DAic'
  case(EOS_Pic_DSic)
     codeLabel = 'EOS_Pic_DSic'
  case(EOS_Pic_DUic)
     codeLabel = 'EOS_Pic_DUic'
  case(EOS_Piz_DAiz)
     codeLabel = 'EOS_Piz_DAiz'
  case(EOS_Piz_DSiz)
     codeLabel = 'EOS_Piz_DSiz'
  case(EOS_Piz_DUiz)
     codeLabel = 'EOS_Piz_DUiz'
  case(EOS_Pt_DAt)
     codeLabel = 'EOS_Pt_DAt'
  case(EOS_Pt_DSt)
     codeLabel = 'EOS_Pt_DSt'
  case(EOS_Pt_DUt)
     codeLabel = 'EOS_Pt_DUt'
  case(EOS_Se_DAe)
     codeLabel = 'EOS_Se_DAe'
  case(EOS_Se_DPe)
     codeLabel = 'EOS_Se_DPe'
  case(EOS_Se_DUe)
     codeLabel = 'EOS_Se_DUe'
  case(EOS_Sic_DAic)
     codeLabel = 'EOS_Sic_DAic'
  case(EOS_Sic_DPic)
     codeLabel = 'EOS_Sic_DPic'
  case(EOS_Sic_DUic)
     codeLabel = 'EOS_Sic_DUic'
  case(EOS_Siz_DAiz)
     codeLabel = 'EOS_Siz_DAiz'
  case(EOS_Siz_DPiz)
     codeLabel = 'EOS_Siz_DPiz'
  case(EOS_Siz_DUiz)
     codeLabel = 'EOS_Siz_DUiz'
  case(EOS_St_DAt)
     codeLabel = 'EOS_St_DAt'
  case(EOS_St_DPt)
     codeLabel = 'EOS_St_DPt'
  case(EOS_St_DUt)
     codeLabel = 'EOS_St_DUt'
  case(EOS_T_DAe)
     codeLabel = 'EOS_T_DAe'
  case(EOS_T_DAic)
     codeLabel = 'EOS_T_DAic'
  case(EOS_T_DAiz)
     codeLabel = 'EOS_T_DAiz'
  case(EOS_T_DAt)
     codeLabel = 'EOS_T_DAt'
  case(EOS_T_DPe)
     codeLabel = 'EOS_T_DPe'
  case(EOS_T_DPic)
     codeLabel = 'EOS_T_DPic'
  case(EOS_T_DPiz)
     codeLabel = 'EOS_T_DPiz'
  case(EOS_T_DPt)
     codeLabel = 'EOS_T_DPt'
  case(EOS_T_DSe)
     codeLabel = 'EOS_T_DSe'
  case(EOS_T_DSic)
     codeLabel = 'EOS_T_DSic'
  case(EOS_T_DSiz)
     codeLabel = 'EOS_T_DSiz'
  case(EOS_T_DSt)
     codeLabel = 'EOS_T_DSt'
  case(EOS_T_DUe)
     codeLabel = 'EOS_T_DUe'
  case(EOS_T_DUic)
     codeLabel = 'EOS_T_DUic'
  case(EOS_T_DUiz)
     codeLabel = 'EOS_T_DUiz'
  case(EOS_T_DUt)
     codeLabel = 'EOS_T_DUt'
  case(EOS_Ue_DAe)
     codeLabel = 'EOS_Ue_DAe'
  case(EOS_Ue_DPe)
     codeLabel = 'EOS_Ue_DPe'
  case(EOS_Ue_DSe)
     codeLabel = 'EOS_Ue_DSe'
  case(EOS_Uic_DAic)
     codeLabel = 'EOS_Uic_DAic'
  case(EOS_Uic_DPic)
     codeLabel = 'EOS_Uic_DPic'
  case(EOS_Uic_DSic)
     codeLabel = 'EOS_Uic_DSic'
  case(EOS_Uiz_DAiz)
     codeLabel = 'EOS_Uiz_DAiz'
  case(EOS_Uiz_DPiz)
     codeLabel = 'EOS_Uiz_DPiz'
  case(EOS_Uiz_DSiz)
     codeLabel = 'EOS_Uiz_DSiz'
  case(EOS_Ut_DAt)
     codeLabel = 'EOS_Ut_DAt'
  case(EOS_Ut_DPt)
     codeLabel = 'EOS_Ut_DPt'
  case(EOS_Ut_DSt)
     codeLabel = 'EOS_Ut_DSt'
  case(EOS_Comment)
     codeLabel = 'EOS_Comment'
  case(EOS_Info)
     codeLabel = 'EOS_Info'
  case(EOS_NullTable)
     codeLabel = 'EOS_NullTable'
  case(EOS_Ogb)
     codeLabel = 'EOS_Ogb'
  case(EOS_B_DT)
     codeLabel = 'EOS_B_DT'
  case(EOS_Dls_T)
     codeLabel = 'EOS_Dls_T'
  case(EOS_Dv_Dls)
     codeLabel = 'EOS_Dv_Dls'
  case(EOS_Als_Dls)
     codeLabel = 'EOS_Als_Dls'
  case(EOS_Av_Dls)
     codeLabel = 'EOS_Av_Dls'
  case(EOS_Pv_Dls)
     codeLabel = 'EOS_Pv_Dls'
  case(EOS_T_Dls)
     codeLabel = 'EOS_T_Dls'
  case(EOS_Uls_Dls)
     codeLabel = 'EOS_Uls_Dls'
  case(EOS_Uv_Dls)
     codeLabel = 'EOS_Uv_Dls'
  case(EOS_Dls_Dv)
     codeLabel = 'EOS_Dls_Dv'
  case(EOS_Dv_T)
     codeLabel = 'EOS_Dv_T'
  case(EOS_Als_Dv)
     codeLabel = 'EOS_Als_Dv'
  case(EOS_Av_Dv)
     codeLabel = 'EOS_Av_Dv'
  case(EOS_Pv_Dv)
     codeLabel = 'EOS_Pv_Dv'
  case(EOS_T_Dv)
     codeLabel = 'EOS_T_Dv'
  case(EOS_Uls_Dv)
     codeLabel = 'EOS_Uls_Dv'
  case(EOS_Uv_Dv)
     codeLabel = 'EOS_Uv_Dv'
  case(EOS_Ac_D)
     codeLabel = 'EOS_Ac_D'
  case(EOS_Ae_DT)
     codeLabel = 'EOS_Ae_DT'
  case(EOS_D_Af)
     codeLabel = 'EOS_D_Af'
  case(EOS_Af_D)
     codeLabel = 'EOS_Af_D'
  case(EOS_Pf_Af)
     codeLabel = 'EOS_Pf_Af'
  case(EOS_Tf_Af)
     codeLabel = 'EOS_Tf_Af'
  case(EOS_Uf_Af)
     codeLabel = 'EOS_Uf_Af'
  case(EOS_Aic_DT)
     codeLabel = 'EOS_Aic_DT'
  case(EOS_Aiz_DT)
     codeLabel = 'EOS_Aiz_DT'
  case(EOS_Dls_Als)
     codeLabel = 'EOS_Dls_Als'
  case(EOS_Dv_Als)
     codeLabel = 'EOS_Dv_Als'
  case(EOS_Als_T)
     codeLabel = 'EOS_Als_T'
  case(EOS_Av_Als)
     codeLabel = 'EOS_Av_Als'
  case(EOS_Pv_Als)
     codeLabel = 'EOS_Pv_Als'
  case(EOS_T_Als)
     codeLabel = 'EOS_T_Als'
  case(EOS_Uls_Als)
     codeLabel = 'EOS_Uls_Als'
  case(EOS_Uv_Als)
     codeLabel = 'EOS_Uv_Als'
  case(EOS_D_Am)
     codeLabel = 'EOS_D_Am'
  case(EOS_Am_D)
     codeLabel = 'EOS_Am_D'
  case(EOS_Pm_Am)
     codeLabel = 'EOS_Pm_Am'
  case(EOS_Tm_Am)
     codeLabel = 'EOS_Tm_Am'
  case(EOS_Um_Am)
     codeLabel = 'EOS_Um_Am'
  case(EOS_D_Gs)
     codeLabel = 'EOS_D_Gs'
  case(EOS_Gs_D)
     codeLabel = 'EOS_Gs_D'
  case(EOS_At_DT)
     codeLabel = 'EOS_At_DT'
  case(EOS_Dls_Av)
     codeLabel = 'EOS_Dls_Av'
  case(EOS_Dv_Av)
     codeLabel = 'EOS_Dv_Av'
  case(EOS_Als_Av)
     codeLabel = 'EOS_Als_Av'
  case(EOS_Av_T)
     codeLabel = 'EOS_Av_T'
  case(EOS_Pv_Av)
     codeLabel = 'EOS_Pv_Av'
  case(EOS_T_Av)
     codeLabel = 'EOS_T_Av'
  case(EOS_Uls_Av)
     codeLabel = 'EOS_Uls_Av'
  case(EOS_Uv_Av)
     codeLabel = 'EOS_Uv_Av'
  case(EOS_Kc_DT)
     codeLabel = 'EOS_Kc_DT'
  case(EOS_Kec_DT)
     codeLabel = 'EOS_Kec_DT'
  case(EOS_Keo_DT)
     codeLabel = 'EOS_Keo_DT'
  case(EOS_Kp_DT)
     codeLabel = 'EOS_Kp_DT'
  case(EOS_Kr_DT)
     codeLabel = 'EOS_Kr_DT'
  case(EOS_Ktc_DT)
     codeLabel = 'EOS_Ktc_DT'
  case(EOS_Pc_D)
     codeLabel = 'EOS_Pc_D'
  case(EOS_Pe_DT)
     codeLabel = 'EOS_Pe_DT'
  case(EOS_D_Pf)
     codeLabel = 'EOS_D_Pf'
  case(EOS_Af_Pf)
     codeLabel = 'EOS_Af_Pf'
  case(EOS_Pf_D)
     codeLabel = 'EOS_Pf_D'
  case(EOS_Tf_Pf)
     codeLabel = 'EOS_Tf_Pf'
  case(EOS_Uf_Pf)
     codeLabel = 'EOS_Uf_Pf'
  case(EOS_Pic_DT)
     codeLabel = 'EOS_Pic_DT'
  case(EOS_Piz_DT)
     codeLabel = 'EOS_Piz_DT'
  case(EOS_D_Pm)
     codeLabel = 'EOS_D_Pm'
  case(EOS_Am_Pm)
     codeLabel = 'EOS_Am_Pm'
  case(EOS_Pm_D)
     codeLabel = 'EOS_Pm_D'
  case(EOS_Tm_Pm)
     codeLabel = 'EOS_Tm_Pm'
  case(EOS_Um_Pm)
     codeLabel = 'EOS_Um_Pm'
  case(EOS_D_PtT)
     codeLabel = 'EOS_D_PtT'
  case(EOS_Pt_DT)
     codeLabel = 'EOS_Pt_DT'
  case(EOS_Ut_PtT)
     codeLabel = 'EOS_Ut_PtT'
  case(EOS_Dls_Pv)
     codeLabel = 'EOS_Dls_Pv'
  case(EOS_Dv_Pv)
     codeLabel = 'EOS_Dv_Pv'
  case(EOS_Als_Pv)
     codeLabel = 'EOS_Als_Pv'
  case(EOS_Av_Pv)
     codeLabel = 'EOS_Av_Pv'
  case(EOS_Pv_T)
     codeLabel = 'EOS_Pv_T'
  case(EOS_T_Pv)
     codeLabel = 'EOS_T_Pv'
  case(EOS_Uls_Pv)
     codeLabel = 'EOS_Uls_Pv'
  case(EOS_Uv_Pv)
     codeLabel = 'EOS_Uv_Pv'
  case(EOS_Se_DT)
     codeLabel = 'EOS_Se_DT'
  case(EOS_Sic_DT)
     codeLabel = 'EOS_Sic_DT'
  case(EOS_Siz_DT)
     codeLabel = 'EOS_Siz_DT'
  case(EOS_St_DT)
     codeLabel = 'EOS_St_DT'
  case(EOS_D_Tf)
     codeLabel = 'EOS_D_Tf'
  case(EOS_Af_Tf)
     codeLabel = 'EOS_Af_Tf'
  case(EOS_Pf_Tf)
     codeLabel = 'EOS_Pf_Tf'
  case(EOS_Tf_D)
     codeLabel = 'EOS_Tf_D'
  case(EOS_Uf_Tf)
     codeLabel = 'EOS_Uf_Tf'
  case(EOS_D_Tm)
     codeLabel = 'EOS_D_Tm'
  case(EOS_Am_Tm)
     codeLabel = 'EOS_Am_Tm'
  case(EOS_Pm_Tm)
     codeLabel = 'EOS_Pm_Tm'
  case(EOS_Tm_D)
     codeLabel = 'EOS_Tm_D'
  case(EOS_Um_Tm)
     codeLabel = 'EOS_Um_Tm'
  case(EOS_Uc_D)
     codeLabel = 'EOS_Uc_D'
  case(EOS_Ue_DT)
     codeLabel = 'EOS_Ue_DT'
  case(EOS_D_Uf)
     codeLabel = 'EOS_D_Uf'
  case(EOS_Af_Uf)
     codeLabel = 'EOS_Af_Uf'
  case(EOS_Pf_Uf)
     codeLabel = 'EOS_Pf_Uf'
  case(EOS_Tf_Uf)
     codeLabel = 'EOS_Tf_Uf'
  case(EOS_Uf_D)
     codeLabel = 'EOS_Uf_D'
  case(EOS_Uic_DT)
     codeLabel = 'EOS_Uic_DT'
  case(EOS_Uiz_DT)
     codeLabel = 'EOS_Uiz_DT'
  case(EOS_Dls_Uls)
     codeLabel = 'EOS_Dls_Uls'
  case(EOS_Dv_Uls)
     codeLabel = 'EOS_Dv_Uls'
  case(EOS_Als_Uls)
     codeLabel = 'EOS_Als_Uls'
  case(EOS_Av_Uls)
     codeLabel = 'EOS_Av_Uls'
  case(EOS_Pv_Uls)
     codeLabel = 'EOS_Pv_Uls'
  case(EOS_T_Uls)
     codeLabel = 'EOS_T_Uls'
  case(EOS_Uls_T)
     codeLabel = 'EOS_Uls_T'
  case(EOS_Uv_Uls)
     codeLabel = 'EOS_Uv_Uls'
  case(EOS_D_Um)
     codeLabel = 'EOS_D_Um'
  case(EOS_Am_Um)
     codeLabel = 'EOS_Am_Um'
  case(EOS_Pm_Um)
     codeLabel = 'EOS_Pm_Um'
  case(EOS_Tm_Um)
     codeLabel = 'EOS_Tm_Um'
  case(EOS_Um_D)
     codeLabel = 'EOS_Um_D'
  case(EOS_Ut_DT)
     codeLabel = 'EOS_Ut_DT'
  case(EOS_Dls_Uv)
     codeLabel = 'EOS_Dls_Uv'
  case(EOS_Dv_Uv)
     codeLabel = 'EOS_Dv_Uv'
  case(EOS_Als_Uv)
     codeLabel = 'EOS_Als_Uv'
  case(EOS_Av_Uv)
     codeLabel = 'EOS_Av_Uv'
  case(EOS_Pv_Uv)
     codeLabel = 'EOS_Pv_Uv'
  case(EOS_T_Uv)
     codeLabel = 'EOS_T_Uv'
  case(EOS_Uls_Uv)
     codeLabel = 'EOS_Uls_Uv'
  case(EOS_Uv_T)
     codeLabel = 'EOS_Uv_T'
  case(EOS_Zfc_DT)
     codeLabel = 'EOS_Zfc_DT'
  case(EOS_Zfo_DT)
     codeLabel = 'EOS_Zfo_DT'
  case(EOS_xHi_yHi)
     codeLabel = 'EOS_xHi_yHi'
  case(EOS_xHi_yOk)
     codeLabel = 'EOS_xHi_yOk'
  case(EOS_xHi_yLo)
     codeLabel = 'EOS_xHi_yLo'
  case(EOS_xOk_yLo)
     codeLabel = 'EOS_xOk_yLo'
  case(EOS_xLo_yLo)
     codeLabel = 'EOS_xLo_yLo'
  case(EOS_xLo_yOk)
     codeLabel = 'EOS_xLo_yOk'
  case(EOS_xLo_yHi)
     codeLabel = 'EOS_xLo_yHi'
  case(EOS_xOk_yHi)
     codeLabel = 'EOS_xOk_yHi'
  case default
     codeLabel = 'UNDETERMINED'
  end select
end function codeLabel

! ===========================================================================
! Return EOSPAC5 data type corresponding to EOSPAC6 value, iType.
! ===========================================================================
subroutine getType5fromTH(TH,ieosf)
  use eos_Interface
  use dataInterface
  implicit none
  integer(EOS_INTEGER) :: TH, ieosf, iType, errorCode
  real(EOS_REAL) :: infoVal

  call eos_GetTableInfo(TH, 1_EOS_INTEGER, EOS_Table_Type, infoVal, errorCode)
  iType = infoVal
  if (errorCode.NE.EOS_OK) then
     call print_error(errorCode, 'getType5fromTH->eos_GetTableInfo')
  endif
  

  select case (iType)
  case(EOS_Pt_DT)
     ieosf = ES4_PRTOT
  case(EOS_Ut_DT)
     ieosf = ES4_ENTOT
  case(EOS_T_DPt)
     ieosf = ES4_TPTOT
  case(EOS_Ut_DPt)
     ieosf = ES4_EPTOT
  case(EOS_T_DUt)
     ieosf = ES4_TNTOT
  case(EOS_Pt_DUt)
     ieosf = ES4_PNTOT
  case(EOS_Pic_DT)
     ieosf = ES4_PRION
  case(EOS_Uic_DT)
     ieosf = ES4_ENION
  case(EOS_T_DPic)
     ieosf = ES4_TPION
  case(EOS_Uic_DPic)
     ieosf = ES4_EPION
  case(EOS_T_DUic)
     ieosf = ES4_TNION
  case(EOS_Pic_DUic)
     ieosf = ES4_PNION
  case(EOS_Pe_DT)
     ieosf = ES4_PRELC
  case(EOS_Ue_DT)
     ieosf = ES4_ENELC
  case(EOS_T_DPe)
     ieosf = ES4_TPELC
  case(EOS_Ue_DPe)
     ieosf = ES4_EPELC
  case(EOS_T_DUe)
     ieosf = ES4_TNELC
  case(EOS_Pe_DUe)
     ieosf = ES4_PNELC
  case(EOS_Pc_D)
     ieosf = ES4_PRCLD
  case(EOS_Uc_D)
     ieosf = ES4_ENCLD
  case(EOS_Kr_DT)
     ieosf = ES4_OPACR
  case(EOS_Keo_DT)
     ieosf = ES4_OPACC2
  case(EOS_Zfo_DT)
     ieosf = ES4_ZFREE2
  case(EOS_Kp_DT)
     ieosf = ES4_OPACP
  case(EOS_Zfc_DT)
     ieosf = ES4_ZFREE3
  case(EOS_Kec_DT)
     ieosf = ES4_ECONDE
  case(EOS_Ktc_DT)
     ieosf = ES4_TCONDE
  case(EOS_B_DT)
     ieosf = ES4_THERME
  case(EOS_Kc_DT)
     ieosf = ES4_OPACC3
  case default
     ieosf = ES4_NULLPTR
  end select
end subroutine getType5fromTH
