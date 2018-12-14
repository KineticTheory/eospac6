!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!
!  PURPOSE: Interpolate using both EOSPAC 5 and EOSPAC 6 functionality.
!  
!********************************************************************

program test006b

  use eos_Interface
  use dataInterface
  implicit none

  integer(EOS_INTEGER),parameter :: nTableTypes = 4
  integer(EOS_INTEGER),parameter :: nMatIDs = 6
  integer(EOS_INTEGER),parameter :: nMatIDsToMix = nMatIDs / 2
  integer(EOS_INTEGER),parameter :: nTables = nTableTypes * nMatIDs
  integer(EOS_INTEGER),parameter :: nMixSets = nTables / nMatIDsToMix
  integer(EOS_INTEGER),parameter :: nColdTables = nTableTypes * nMatIDs
  integer(EOS_INTEGER),parameter :: nXYPairs = 10

  integer(EOS_INTEGER) ::  i, j, k, m, n, p
  real(EOS_REAL) :: X(nXYPairs), Y(nXYPairs), Ycold(nXYPairs), F(nXYPairs), dFx(nXYPairs), &
                    dFy(nXYPairs), Xtmp(nXYPairs), Ytmp(nXYPairs)
  real(EOS_REAL) :: Fideal(nXYPairs,3) ! Fideal and dF/dx and dF/dx for each nXYPair
  real(EOS_REAL) :: fcold(nXYPairs,3,2) ! Fcold and dF/dx for each nXYPair
  integer(EOS_INTEGER) :: tableTypes(nTables), matID(nTables), coldCurveTypes(nTables), &
                          tableHandles(nTables), coldCurveTH(nColdTables), &
                          tableHandlesToMix(nMatIDsToMix)
  logical :: mask(nTables), notSet=.TRUE.
  integer(EOS_INTEGER) :: tableTypesList(nTableTypes), matIDList(nMatIDs), &
                          coldCurveTypesList(nTableTypes)
  integer(EOS_INTEGER) :: errorCode, iType, materialID, extrapCode(nXYPairs)
  real(EOS_REAL) :: infoVal, atomicWeight(nMatIDs), gamma(nMatIDs), &
                    atomicWeights(nTables), gammas(nTables)
  real(EOS_REAL) :: cmixrvals(nMatIDsToMix,nMixSets), C(nXYPairs,nMatIDsToMix), gamma_bar, Abar, &
                    imixrvals(nMatIDsToMix,nMixSets)

  real(EOS_REAL) :: wctmp = 0_EOS_REAL, cputmp = 0_EOS_REAL, cpucyclestmp = 0_EOS_REAL
  real(EOS_REAL) :: wctime_setup5 = 0_EOS_REAL, cputime_setup5 = 0_EOS_REAL, cpucycles_setup5 = 0_EOS_REAL, &
                    wctime_setup6 = 0_EOS_REAL, cputime_setup6 = 0_EOS_REAL, cpucycles_setup6 = 0_EOS_REAL
  real(EOS_REAL) :: wctime_interp = 0_EOS_REAL, cputime_interp = 0_EOS_REAL, cpucycles_interp = 0_EOS_REAL
  real(EOS_REAL) :: wctime_mix = 0_EOS_REAL, cputime_mix = 0_EOS_REAL, cpucycles_mix = 0_EOS_REAL
  integer(EOS_INTEGER), parameter :: nInterpTypes=2
  integer(EOS_INTEGER) :: errtmp, interpTypes(nInterpTypes,2), interpType
  
  character*(13) :: typeLabel
  character      :: s(nXYPairs)

  ! *** THE FOLLOWING DATA ARE TEST-SPECIFIC INPUT ***
  ! Define ideal gas properties which were used to generate the Sesame tables
  data atomicWeight / &      ! Atomic weights (g/mol):
       1.00797000e+00_EOS_REAL, &     ! H, diatomic, ideal gas
       1.59994000e+01_EOS_REAL, &     ! O, diatomic, ideal gas
       1.31300000e+02_EOS_REAL, &     ! Xe, monatomic, ideal gas
       1.00797000e+00_EOS_REAL, &     ! H, diatomic, ideal gas
       1.59994000e+01_EOS_REAL, &     ! O, diatomic, ideal gas
       1.31300000e+02_EOS_REAL  &     ! Xe, monatomic, ideal gas
       /
  data gamma / &             ! Specific heat ratios:
       1.40000000e+00_EOS_REAL, &     ! H, diatomic, ideal gas
       1.40000000e+00_EOS_REAL, &     ! O, diatomic, ideal gas
       1.66666667e+00_EOS_REAL, &     ! Xe, monatomic, ideal gas
       1.40000000e+00_EOS_REAL, &     ! H, diatomic, ideal gas
       1.40000000e+00_EOS_REAL, &     ! O, diatomic, ideal gas
       1.66666667e+00_EOS_REAL  &     ! Xe, monatomic, ideal gas
       /
  real(EOS_REAL), parameter :: R = 8.31440000_EOS_REAL ! Universal Gas Constant (J/mol-K)
  ! *** END OF TEST-SPECIFIC INPUT ***

  !     ES1TABS subroutine parameters
  integer, parameter :: mtyps=nTableTypes+4 ! number of tables to load
  integer, parameter :: mregs=nMatIDs       ! number of problem regions
  integer            :: iopt(mtyps*mregs), inams(mtyps), ieosf, ieosfc, &
                        imids(mtyps*mregs), mtabs, ierrs(mtyps*mregs)
  logical            :: llog1(mtyps*mregs)
  real*8             :: ucons(mtyps*3), ftabs
  pointer (ktabs,ftabs(1))

  !     ES1VALS subroutine parameters
  integer, parameter :: nzons=nXYPairs ! number of interpolated F values to return (zones)
  integer            :: iregn
  real*8             :: fvals(nzons,3)   ! F, dF/dx, and dF/dy in each zone
      
  real*8             :: d1, d2, d3, diff1, diff2, diff3, mxdiff1, mxdiff2, mxdiff3
  character*60 :: errorMessage, interpType_s
  character*8  :: cname
  character*11 :: ieosf_s, ieosf_sc
  character    :: tmp_s

  ! EOS_Pic_DT   = ES4_PRION
  ! EOS_Uic_DT   = ES4_ENION
  ! EOS_Pic_DUic = ES4_PNION
  ! EOS_Uic_DPic = ES4_EPION
  tableTypesList(1) = EOS_Pic_DT
  tableTypesList(2) = EOS_Uic_DT
  tableTypesList(3) = EOS_T_DPic
  tableTypesList(4) = EOS_T_DUic

  ! EOS_Pc_D = ES4_PRCLD
  ! EOS_Uc_D = ES4_ENCLD
  coldCurveTypesList(1) = EOS_Pc_D
  coldCurveTypesList(2) = EOS_Uc_D
  coldCurveTypesList(3) = EOS_Pc_D
  coldCurveTypesList(4) = EOS_Uc_D

  ! Balance functions used by EOSPAC 5
  ! ES4_TNION : energy-based temperature (PNION balance function)
  ! ES4_TPION : pressure-based temperature (EPION balance function)

  matIDList(1:nMatIDs) = ( /9991, 9993, 9995, 9981, 9983, 9985/ )

  errorCode = EOS_OK
  do i=1, nTables
     tableHandles(i) = 0
  enddo

  atomicWeights = reshape(spread(atomicWeight,1,nTableTypes),(/nTables/))
  gammas        = reshape(spread(gamma,1,nTableTypes),(/nTables/))

  coldCurveTypes = reshape(spread(coldCurveTypesList,2,nMatIDs),(/nTables/))
  matID          = reshape(spread(matIDList,1,nTableTypes),(/nTables/))
  tableTypes     = reshape(spread(tableTypesList,2,nMatIDs),(/nTables/))

  !
  !     Load desired data tables using EOSPAC 5
  !
  ! Define EOSPAC 5 data types
  inams(1:mtyps) = ( /&
       ES4_PRCLD, ES4_ENCLD, ES4_PRION, ES4_ENION, &
       ES4_PNION, ES4_EPION, ES4_TNION, ES4_TPION/ )
  llog1 = .FALSE.
  iopt  = 0
  ucons = 1.d0
  imids = reshape(spread(matIDList,1,mtyps),(/mtyps*mregs/))

  call eos_time(.TRUE.,wctime_setup5,cputime_setup5,cpucycles_setup5,errorCode)

  call es1tabs(llog1, iopt, .TRUE., 0, mtyps, mregs, inams, ucons, imids, &
               0, mtabs, ktabs, ierrs)

  call eos_time(.FALSE.,wctime_setup5,cputime_setup5,cpucycles_setup5,errorCode)

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
  call eos_time(.TRUE.,wctime_setup6,cputime_setup6,cpucycles_setup6,errorCode)

  call eos_CreateTables(nColdTables, coldCurveTypes, matID, coldCurveTH, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(coldCurveTH, nColdTables, 'eos_CreateTables')
  endif
  call eos_CreateTables(nTables, tableTypes, matID, tableHandles, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(tableHandles, nTables, 'eos_CreateTables')
  endif

  !
  !     Enable data dump to file
  !
  call eos_SetOption(coldCurveTH(1), EOS_DUMP_DATA, EOS_NullVal, errorCode)
  call eos_SetOption(coldCurveTH(2), EOS_APPEND_DATA, EOS_NullVal, errorCode)
  do i=1, nTables
     call eos_SetOption(tableHandles(i), EOS_APPEND_DATA, EOS_NullVal, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'eos_SetOption')
     endif
  enddo

  !
  !     load data into table data objects
  !
  call eos_LoadTables(nColdTables, coldCurveTH, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(coldCurveTH, nColdTables, 'eos_LoadTables')
  endif
  call eos_LoadTables(nTables, tableHandles, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(tableHandles, nTables, 'eos_LoadTables')
  endif

  call eos_time(.FALSE.,wctime_setup6,cputime_setup6,cpucycles_setup6,errorCode)

  !
  !     interpolate pure materials
  !
  interpTypes(1,1) = ES4_BIRATF
  interpTypes(2,1) = ES4_BILINE
  interpTypes(1,2) = EOS_RATIONAL
  interpTypes(2,2) = EOS_LINEAR

  do interpType=1, nInterpTypes
  do i=1, nTables

     ! FIRST, USE EOSPAC 5 FUNCTIONS

     Ycold  = 0_EOS_REAL
     fcold  = 0_EOS_REAL
     fvals  = 0_EOS_REAL

     ! Set X and Y values for all nXYPairs and get data type and material ID for
     ! the current tableHandle
     !notSet=.TRUE.
     call setXandYVALS(tableHandles, nTables, i, nXYPairs, Xtmp, Ytmp, iType, materialID)
     if (notSet .OR. (materialID.EQ.9991_EOS_INTEGER)) then
        X = Xtmp
        Y = Ytmp
        notSet=.FALSE.
     endif

     call getType5fromTH(tableHandles(i),ieosf,ieosf_s)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     if (interpTypes(interpType,1).EQ.ES4_BIRATF) then
        interpType_s = 'Rational'
     elseif (interpTypes(interpType,1).EQ.ES4_BILINE) then
        interpType_s = 'Linear'
     endif

     write(*,*) '"--- ' // trim(interpType_s) // ' Interpolate using EOSPAC 5 tableType ', &
                trim(ieosf_s), '(', ieosf, ') and materialID ', &
                int(materialID), ' (a.k.a. ' // trim(typeLabel(iType)) // ', ',iType,') ---"'

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call es1vals(ieosf,ES4_DERIVS,interpTypes(interpType,1),ftabs,mtabs,nzons,iregn,X,Y,fvals,3_EOS_INTEGER,errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     fvals(:,2) = fvals(:,2) / max(X, 1.0d-99)
     fvals(:,3) = fvals(:,3) / max(Y, 1.0d-99)

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'es1vals')
     endif

     ! get Pressure cold curve data
     if (mod(i,2).NE.0) then
        j = i
     else
        j = i - 1
     endif

     call getType5fromTH(coldCurveTH(j),ieosfc,ieosf_sc)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call es1vals(ieosfc,ES4_DERIVS,interpTypes(interpType,1),ftabs,mtabs,nzons,iregn,X,Ycold,fcold(:,:,1),3_EOS_INTEGER,errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     fcold(:,2,1) = fcold(:,2,1) / max(X, 1.0d-99)

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'es1vals(Pc)')
     endif

     ! get Internal Energy cold curve data
     if (mod(i,2).NE.0) then
        j = i + 1
     else
        j = i
     endif

     call getType5fromTH(coldCurveTH(j),ieosfc,ieosf_sc)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call es1vals(ieosfc,ES4_DERIVS,interpTypes(interpType,1),ftabs,mtabs,nzons,iregn,X,Ycold,fcold(:,:,2),3_EOS_INTEGER,errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     fcold(:,2,2) = fcold(:,2,2) / max(X, 1.0d-99)

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'es1vals(Uc)')
     endif

     ! write results to STDOUT
     if ((ieosf .EQ. ES4_TPTOT) .OR. (ieosf .EQ. ES4_EPTOT) .OR. &
         (ieosf .EQ. ES4_TPION) .OR. (ieosf .EQ. ES4_EPION)) then
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy','(Y-Yc)/X'
        do j=1, nXYPairs
           write(*,999) ' ',j,X(j),Y(j),fvals(j,1),fvals(j,2),fvals(j,3),(Y(j)-fcold(j,1,1))/X(j)
        enddo
     elseif ((ieosf .EQ. ES4_TNTOT) .OR. (ieosf .EQ. ES4_PNTOT) .OR. &
             (ieosf .EQ. ES4_TNION) .OR. (ieosf .EQ. ES4_PNION)) then
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy','(Y-Yc)'
        do j=1, nXYPairs
           write(*,999) ' ',j,X(j),Y(j),fvals(j,1),fvals(j,2),fvals(j,3),(Y(j)-fcold(j,1,2))
        enddo
     else
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy'
        do j=1, nXYPairs
           write(*,999) ' ',j,X(j),Y(j),fvals(j,1),fvals(j,2),fvals(j,3)
        enddo
     endif


     ! SECOND, USE EOSPAC 6 FUNCTIONS

     Ycold  = 0_EOS_REAL
     fcold  = 0_EOS_REAL
     F      = 0_EOS_REAL
     dFx    = 0_EOS_REAL
     dFy    = 0_EOS_REAL

     ! Set X and Y values for all nXYPairs and get data type and material ID for
     ! the current tableHandle
     !call setXandYVALS(tableHandles, nTables, i, nXYPairs, X, Y, iType, materialID)

     call getType5fromTH(tableHandles(i),ieosf,ieosf_s)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     write(*,*) ' '
     write(*,*) '"--- ' // trim(interpType_s) // ' Interpolate using EOSPAC 6 tableType ', &
                trim(typeLabel(iType)), '(', iType, ') and materialID ', &
                int(materialID), ' (a.k.a. ' // trim(ieosf_s) // ') ---"'

     call eos_SetOption(tableHandles(i), interpTypes(interpType,2), EOS_NullVal, errorCode)

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call eos_Interpolate(tableHandles(i), nXYPairs, X, Y, F, dFx, dFy, errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'eos_Interpolate')
        if (errorCode.EQ.EOS_INTERP_EXTRAPOLATED) then
           extrapCode = EOS_OK
           call eos_CheckExtrap(tableHandles(i), nXYPairs, X, Y, extrapCode, errorCode)
           write(*,*) 'extrapCode: ',extrapCode
        endif
     endif

     ! get Pressure cold curve data
     if (mod(i,2).NE.0) then
        j = i
     else
        j = i - 1
     endif

     call getType5fromTH(coldCurveTH(j),ieosfc,ieosf_sc)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call eos_Interpolate(coldCurveTH(j), nXYPairs, X, Ycold, fcold(:,1,1), fcold(:,2,1), fcold(:,3,1), errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'eos_Interpolate(Pc)')
     endif

     ! get Internal Energy cold curve data
     if (mod(i,2).NE.0) then
        j = i + 1
     else
        j = i
     endif

     call getType5fromTH(coldCurveTH(j),ieosfc,ieosf_sc)
     do k=1,mregs*mtyps
        if (imids(k).EQ.materialID) then
           iregn = k / mtyps + 1
           exit
        endif
     enddo

     call eos_time(.TRUE.,wctmp,cputmp,cpucyclestmp,errtmp)

     call eos_Interpolate(coldCurveTH(j), nXYPairs, X, Ycold, fcold(:,1,2), fcold(:,2,2), fcold(:,3,2), errorCode)

     call eos_time(.FALSE.,wctmp,cputmp,cpucyclestmp,errtmp)
     wctime_interp = wctime_interp + wctmp
     cputime_interp = cputime_interp + cputmp
     cpucycles_interp = cpucycles_interp + cpucyclestmp

     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'eos_Interpolate(Uc)')
     endif

     ! write results to STDOUT
     s=' '
     where (extrapCode.NE.EOS_OK)
        s='E'
     end where
     if ((ieosf .EQ. ES4_TPTOT) .OR. (ieosf .EQ. ES4_EPTOT) .OR. &
         (ieosf .EQ. ES4_TPION) .OR. (ieosf .EQ. ES4_EPION)) then
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy','(Y-Yc)/X'
        do j=1, nXYPairs
           write(*,999) s(j),j,X(j),Y(j),F(j),dFx(j),dFy(j),(Y(j)-fcold(j,1,1))/X(j)
        enddo
     elseif ((ieosf .EQ. ES4_TNTOT) .OR. (ieosf .EQ. ES4_PNTOT) .OR. &
             (ieosf .EQ. ES4_TNION) .OR. (ieosf .EQ. ES4_PNION)) then
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy','(Y-Yc)'
        do j=1, nXYPairs
           write(*,999) s(j),j,X(j),Y(j),F(j),dFx(j),dFy(j),(Y(j)-fcold(j,1,2))
        enddo
     else
        write(*,998) 'i','X','Y','F','dF/dx','dF/dy'
        do j=1, nXYPairs
           write(*,999) s(j),j,X(j),Y(j),F(j),dFx(j),dFy(j)
        enddo

     endif

     write(*,*) ' '
     write(*,*) 'RELATIVE DIFFERENCES (EOSPAC 5 versus EOSPAC 6)'
     write(*,998) 'i',' ',' ','F','dF/dx','dF/dy'
     mxdiff1 = 0.0_EOS_REAL
     mxdiff2 = 0.0_EOS_REAL
     mxdiff3 = 0.0_EOS_REAL
     do j=1, nXYPairs
        d1 = 1.0_EOS_REAL
        d2 = 1.0_EOS_REAL
        d3 = 1.0_EOS_REAL
        if (fvals(j,1) .NE. 0.0_EOS_REAL) d1 = fvals(j,1)
        if (fvals(j,2) .NE. 0.0_EOS_REAL) d2 = fvals(j,2)
        if (fvals(j,3) .NE. 0.0_EOS_REAL) d3 = fvals(j,3)
        diff1 = (F(j)-fvals(j,1))/d1
        diff2 = (dFx(j)-fvals(j,2))/d2
        diff3 = (dFy(j)-fvals(j,3))/d3
        if (abs(mxdiff1) .LT. abs(diff1)) mxdiff1 = diff1
        if (abs(mxdiff2) .LT. abs(diff2)) mxdiff2 = diff2
        if (abs(mxdiff3) .LT. abs(diff3)) mxdiff3 = diff3

        write(*,997) j,' ',' ', diff1, diff2, diff3
     enddo

     write(*,998) ' ', ' ', ' ', ' ----------------------', ' ----------------------', ' ----------------------'
     write(*,996) ' ', ' ', 'BIGGEST DIFFERENCE:', mxdiff1, mxdiff2, mxdiff3     

     tmp_s = ''

     if ((ieosf .EQ. ES4_EPTOT) .OR. (ieosf .EQ. ES4_ENION) .OR. &
         (ieosf .EQ. ES4_PNTOT) .OR. (ieosf .EQ. ES4_PRION) .OR. &
         (ieosf .EQ. ES4_TNION) .OR. (ieosf .EQ. ES4_TPION)) then

        ! calculate ideal gas data
        call idealGas(1_EOS_INTEGER, iType, matIDList, nXYPairs, X, Y, R, fcold, &
                      atomicWeights(i), 1.0_EOS_REAL, gammas(i), Fideal, gamma_bar, Abar)

        write(*,*) ' '
        write(*,*) '"---  Calculate Ideal Gas data corresponding to tableType ', &
                   trim(typeLabel(iType)), '(', iType, ') and materialID ', &
                   int(materialID), ' (a.k.a. ' // trim(ieosf_s) // ') ---"'
        write(*,*) 'Atomic Weight: ',Abar
        write(*,*) '    gamma_bar: ',gamma_bar
        write(*,998) 'i','X','Y','Fideal','dFideal/dx','dFideal/dy'
        do j=1, nXYPairs
           write(*,999) ' ',j,X(j),Y(j),Fideal(j,1),Fideal(j,2),Fideal(j,3)
        enddo

        tmp_s = char(12)
        write(*,*) ' '
        write(*,*) 'RELATIVE DIFFERENCES (EOSPAC 5 versus Ideal Gas Function)'
        write(*,998) 'i',' ',' ','F','dF/dx','dF/dy'
        mxdiff1 = 0.0_EOS_REAL
        mxdiff2 = 0.0_EOS_REAL
        mxdiff3 = 0.0_EOS_REAL
        do j=1, nXYPairs
           d1 = 1.0_EOS_REAL
           d2 = 1.0_EOS_REAL
           d3 = 1.0_EOS_REAL
           if (Fideal(j,1) .NE. 0.0_EOS_REAL) d1 = Fideal(j,1)
           if (Fideal(j,2) .NE. 0.0_EOS_REAL) d2 = Fideal(j,2)
           if (Fideal(j,3) .NE. 0.0_EOS_REAL) d3 = Fideal(j,3)
           diff1 = (fvals(j,1)-Fideal(j,1))/d1
           diff2 = (fvals(j,2)-Fideal(j,2))/d2
           diff3 = (fvals(j,3)-Fideal(j,3))/d3
           if (abs(mxdiff1) .LT. abs(diff1)) mxdiff1 = diff1
           if (abs(mxdiff2) .LT. abs(diff2)) mxdiff2 = diff2
           if (abs(mxdiff3) .LT. abs(diff3)) mxdiff3 = diff3
           
           write(*,997) j,' ',' ', diff1, diff2, diff3
        enddo

        write(*,998) ' ', ' ', ' ', ' ----------------------', ' ----------------------', ' ----------------------'
        write(*,996) ' ', ' ', 'BIGGEST DIFFERENCE:', mxdiff1, mxdiff2, mxdiff3     

        write(*,*) ' '
        write(*,*) 'RELATIVE DIFFERENCES (EOSPAC 6 versus Ideal Gas Function)'
        write(*,998) 'i',' ',' ','F','dF/dx','dF/dy'
        mxdiff1 = 0.0_EOS_REAL
        mxdiff2 = 0.0_EOS_REAL
        mxdiff3 = 0.0_EOS_REAL
        do j=1, nXYPairs
           d1 = 1.0_EOS_REAL
           d2 = 1.0_EOS_REAL
           d3 = 1.0_EOS_REAL
           if (Fideal(j,1) .NE. 0.0_EOS_REAL) d1 = Fideal(j,1)
           if (Fideal(j,2) .NE. 0.0_EOS_REAL) d2 = Fideal(j,2)
           if (Fideal(j,3) .NE. 0.0_EOS_REAL) d3 = Fideal(j,3)
           diff1 = (F(j)-Fideal(j,1))/d1
           diff2 = (dFx(j)-Fideal(j,2))/d2
           diff3 = (dFy(j)-Fideal(j,3))/d3
           if (abs(mxdiff1) .LT. abs(diff1)) mxdiff1 = diff1
           if (abs(mxdiff2) .LT. abs(diff2)) mxdiff2 = diff2
           if (abs(mxdiff3) .LT. abs(diff3)) mxdiff3 = diff3

           write(*,997) j,' ',' ', diff1, diff2, diff3
        enddo

        write(*,998) ' ', ' ', ' ', ' ----------------------', ' ----------------------', ' ----------------------'
        write(*,996) ' ', ' ', 'BIGGEST DIFFERENCE:', mxdiff1, mxdiff2, mxdiff3     
     endif

     write(*,*) ' '
     write(*,*) repeat('<>',70)

     ! force new page if needed
     if ((mod(i,2).EQ.0)) tmp_s = char(12)
     write(*,*) tmp_s

  enddo
  enddo

  !
  !     Destroy all data objects
  !
  call eos_DestroyAll (errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_table_errors(tableHandles, nTables, 'eos_DestroyAll')
  endif

  write(*,*) 'EOSPAC 5 setup wall clock time (sec):', wctime_setup5
  write(*,*) 'EOSPAC 5 setup cpu  clock time (sec):', cputime_setup5
  write(*,*) 'EOSPAC 5 setup cpu  cycles          :', cpucycles_setup5
  write(*,*) 'EOSPAC 6 setup wall clock time (sec):', wctime_setup6
  write(*,*) 'EOSPAC 6 setup cpu  clock time (sec):', cputime_setup6
  write(*,*) 'EOSPAC 6 setup cpu  cycles          :', cpucycles_setup6
  write(*,*) ' '
  write(*,*) 'EOSPAC 5 interpolation wall clock time (sec):', wctime_interp
  write(*,*) 'EOSPAC 5 interpolation cpu  clock time (sec):', cputime_interp
  write(*,*) 'EOSPAC 5 interpolation cpu  cycles          :', cpucycles_interp

996 format(a4,2a23,1p,11e23.15,:,e23.15)
997 format(i4,2a23,1p,11e23.15,:,e23.15)
998 format(a4,13a23,:,a23)
999 format(a1,i3,1p,13e23.15,:,e23.15)

end program test006b

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

  ! Check to see if info data is loaded for matID
  infoLoaded = .FALSE.
  do i = 1, ninfoTables
     if (infoMatIDs(i).EQ.matID) then
        infoLoaded = .TRUE.
        infoTableIndex = i
        exit
     endif
  enddo

  if (.NOT.infoLoaded) then
     ! Allocate/Reallocate arrays
     ninfoTables = ninfoTables + 1_EOS_INTEGER
     infoTH => reallocate(infoTH, ninfoTables)
     infoMatIDs => reallocate(infoMatIDs, ninfoTables)
     infoMatIDs(ninfoTables) = matID

     ! Load 201 table associated with matID
     !     initialize table data objects
     call eos_CreateTables(1_EOS_INTEGER, EOS_Info, &
                           matID, infoTH(ninfoTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             infoTH(ninfoTables), 1_EOS_INTEGER, 'setXandYVALS->eos_CreateTables')
     endif
     !     enable data dump to file
     call eos_SetOption(infoTH(ninfoTables), EOS_APPEND_DATA, EOS_NullVal, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'setXandYVALS->eos_SetOption')
     endif
     !     load data into table data objects
     call eos_LoadTables(1_EOS_INTEGER, infoTH(ninfoTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             infoTH(ninfoTables), 1_EOS_INTEGER, 'setXandYVALS->eos_LoadTables')
     endif

     infoTableIndex = ninfoTables
  endif

  ! Get reference density for matID associated with tableHandle
  call eos_GetTableInfo(infoTH(infoTableIndex), 1_EOS_INTEGER, EOS_Normal_Density, &
                        rhoref, errorCode)
  if (errorCode.NE.EOS_OK) then
     call print_error(errorCode, 'setXandYVALS->eos_GetTableInfo')
  endif

  ! Gather independent variable ranges
  minD = 1.767701112000000e-07_EOS_REAL
  maxD = 1.767701112000000e+03_EOS_REAL
  minT = 1.450606250000000e+02_EOS_REAL
  maxT = 1.160485000000000e+09_EOS_REAL

  !
  ! Assign minX, maxX, minY and maxY values
  !
  call getCrossRefXYRanges(tableHandles, nTableHandles, &
       iType, matID, rhoref, minD, maxD, minT, maxT, minX, maxX, minY, maxY)
  minX = log10(max(0.000001_EOS_REAL,minX))
  maxX = log10(max(0.000001_EOS_REAL,maxX))
  minY = log10(max(0.000001_EOS_REAL,minY))
  maxY = log10(max(0.000001_EOS_REAL,maxY))

  do i = 1, nXYPairs
     X(i) = (maxX-minX) * dble(i-1) / dble(nXYPairs-1) + minX
     X(i) = 10._EOS_REAL**(X(i))
     Y(i) = (maxY-minY) * dble(i-1) / dble(nXYPairs-1) + minY
     Y(i) = 10._EOS_REAL**(Y(i))
  enddo
end subroutine setXandYVALS

! ===========================================================================
! Get appropriate data ranges from a non-inverted data type corresponding
! to iType. Load data if necessary.
! ===========================================================================
subroutine getCrossRefXYRanges(tableHandles, nTableHandles, &
  iType, matID, rhoref, minD, maxD, minT, maxT, minX, maxX, minY, maxY)
  use eos_Interface
  use reallocate_mod
  implicit none

  integer(EOS_INTEGER), intent(in) :: tableHandles(nTableHandles), &
                                      nTableHandles, iType, matID
  real(EOS_REAL), intent(in)  :: rhoref, minD, maxD, minT, maxT
  real(EOS_REAL), intent(out) :: minX, maxX, minY, maxY

  integer(EOS_INTEGER), pointer, save :: TH(:)
  integer(EOS_INTEGER), save :: nTables = 0
  integer(EOS_INTEGER), save :: ninfoTables = 0

  integer(EOS_INTEGER), pointer :: tmpTH(:)

  logical :: xRefTHLoaded, yRefTHLoaded
  integer(EOS_INTEGER) :: i, xRefType, yRefType, tmpType, tmpMatID, &
                          xRefTH, yRefTH, errorCode, localMatID
  real(EOS_REAL) :: infoVal, dx, dy

  character*(13) :: typeLabel

  call getCrossRefDataTypes(iType, xRefType, yRefType)

  ! join TH and tableHandles arrays into a single list
  tmpTH => reallocate(tmpTH, nTables+nTableHandles)
  tmpTH(1:nTables) = TH
  tmpTH(nTables+1:nTables+nTableHandles) = tableHandles

 ! use data with zero cold curve to simplify debug comparisons
  localMatID = matID
  if (localMatID > 9990) localMatID = localMatID - 10
  
  !
  ! determine if required data is already loaded w.r.t. xRefType and localMatID, and
  ! determine if required data is already loaded w.r.t. yRefType and localMatID
  !
  xRefTHLoaded = .FALSE.
  yRefTHLoaded = .FALSE.
  do i = 1, nTables+nTableHandles
     call eos_GetTableInfo(tmpTH(i), 1_EOS_INTEGER, EOS_Table_Type, &
                           infoVal, errorCode)
     tmpType = infoVal
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_GetTableInfo')
     endif
     call eos_GetTableInfo(tmpTH(i), 1_EOS_INTEGER, EOS_Material_ID, &
                           infoVal, errorCode)
     tmpMatID = infoVal
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_GetTableInfo')
     endif
     if (tmpMatID.EQ.localMatID .AND. tmpType.EQ.xRefType) then
        xRefTHLoaded = .TRUE.
        xRefTH = tmpTH(i)
        exit
     endif
     if (tmpMatID.EQ.localMatID .AND. tmpType.EQ.yRefType) then
        yRefTHLoaded = .TRUE.
        yRefTH = tmpTH(i)
        exit
     endif
  enddo
  
  if (.NOT.xRefTHLoaded .AND. xRefType.NE.-1_EOS_INTEGER) then
     !
     ! load the referenced data type for minX and maxX
     !
     ! Allocate/Reallocate arrays
     nTables = nTables + 1_EOS_INTEGER
     TH => reallocate(TH, nTables)

     ! Load table associated with localMatID and xRefType
     !     initialize table data objects
     call eos_CreateTables(1_EOS_INTEGER, xRefType, localMatID, TH(nTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             TH(nTables), 1_EOS_INTEGER, 'getCrossRefXYRanges->eos_CreateTables')
     endif
     !     enable data dump to file
     call eos_SetOption(TH(nTables), EOS_APPEND_DATA, EOS_NullVal, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_SetOption')
     endif
     !     load data into table data objects
     call eos_LoadTables(1_EOS_INTEGER, TH(nTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             TH(nTables), 1_EOS_INTEGER, 'getCrossRefXYRanges->eos_LoadTables')
     endif
     xRefTH = TH(nTables)
  endif

  if (.NOT.yRefTHLoaded .AND. yRefType.NE.-1_EOS_INTEGER) then
     !
     ! load the referenced data type for minY and maxY
     !
     ! Allocate/Reallocate arrays
     nTables = nTables + 1_EOS_INTEGER
     TH => reallocate(TH, nTables)

     ! Load table associated with localMatID and xRefType
     !     initialize table data objects
     call eos_CreateTables(1_EOS_INTEGER, yRefType, localMatID, TH(nTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             TH(nTables), 1_EOS_INTEGER, 'getCrossRefXYRanges->eos_CreateTables')
     endif
     !     enable data dump to file
     call eos_SetOption(TH(nTables), EOS_APPEND_DATA, EOS_NullVal, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_SetOption')
     endif
     !     load data into table data objects
     call eos_LoadTables(1_EOS_INTEGER, TH(nTables), errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_table_errors( &
             TH(nTables), 1_EOS_INTEGER, 'getCrossRefXYRanges->eos_LoadTables')
     endif
     yRefTH = TH(nTables)
  endif

  ! interpolation is not needed by all values of xRefTH; take care of that here
  select case (xRefType)
  case(-1_EOS_INTEGER)
     minX = minD
     maxX = maxD
  case(-2_EOS_INTEGER)
     write(*,*) 'getCrossRefXYRanges ERROR: invalid data type for interpolation, xRefType = ', &
                trim(typeLabel(xRefType)), '(', xRefType, ')'
     stop
  case default ! assume all other data types from getCrossRefDataTypes are valid
     ! INTERPOLATE minX and maxX DATA HERE
     call eos_Interpolate(xRefTH, 1_EOS_INTEGER, minD, minT, minX, dx, dy, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_Interpolate(minX)')
     endif
     call eos_Interpolate(xRefTH, 1_EOS_INTEGER, maxD, maxT, maxX, dx, dy, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_Interpolate(maxX)')
     endif
  end select

  ! interpolation is not needed by all values of yRefTH; take care of that here
  select case (yRefType)
  case(-1_EOS_INTEGER)
     minY = minT
     maxY = maxT
  case(-2_EOS_INTEGER)
     write(*,*) 'getCrossRefXYRanges ERROR: invalid data type for interpolation, yRefType = ', &
                trim(typeLabel(yRefType)), '(', yRefType, ')'
     stop
  case default ! assume all other data types from getCrossRefDataTypes are valid
     ! INTERPOLATE minX and maxX DATA HERE
     call eos_Interpolate(yRefTH, 1_EOS_INTEGER, minD, minT, minY, dx, dy, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_Interpolate(minY)')
     endif
     call eos_Interpolate(yRefTH, 1_EOS_INTEGER, maxD, maxT, maxY, dx, dy, errorCode)
     if (errorCode.NE.EOS_OK) then
        call print_error(errorCode, 'getCrossRefXYRanges->eos_Interpolate(maxY)')
     endif
  end select

!  write(*,*) ' '
!  write(*,*) 'xRefTH =', xRefTH, ', xRefType =', xRefType, ', minX =', minX, ', maxX =', maxX
!  write(*,*) 'yRefTH =', yRefTH, ', yRefType =', yRefType, ', minY =', minY, ', maxY =', maxY
  
end subroutine getCrossRefXYRanges

! ===========================================================================
! Calculate the ideal gas data based upon the following:
!   - EOSPAC data type
!   - Sesame material ID
! ===========================================================================
subroutine idealGas(N, dataType, matID, nXYPairs, x, y, R, fcold, A, C, gamma, F, gamma_bar, Abar)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: N  ! Total ideal gases to use here
  integer(EOS_INTEGER) :: dataType ! input EOSPAC data type
  integer(EOS_INTEGER) :: matID    ! input Sesame material ID
  integer(EOS_INTEGER) :: nXYPairs ! number of nXYPairs
  real(EOS_REAL) :: x(nXYPairs)    ! input mixture density (g/cc), pressure (GPa) or internal energy (MJ/kg)
  real(EOS_REAL) :: y(nXYPairs)    ! input mixture temperature (K), pressure (GPa) or internal energy (MJ/kg)
  real(EOS_REAL) :: R        ! Universal Gas Constant (J/mol-K)
  real(EOS_REAL) :: fcold(nXYPairs,3,2) ! Fcold and dF/dx for each nXYPair
  real(EOS_REAL) :: A(N)     ! input ideal gas atomic weight (g/mol)
  real(EOS_REAL) :: C(N)     ! Number fraction concentrations of ideal gas
  real(EOS_REAL) :: gamma(N) ! input ideal gas specific heat ratio
  real(EOS_REAL) :: F(nXYPairs,3)     ! output function value and derivatives for all zones
  real(EOS_REAL) :: gamma_bar         ! output averaged gamma for mixture
  real(EOS_REAL) :: Abar              ! output averaged atomic weight for mixture

  integer(EOS_INTEGER) :: i
  real(EOS_REAL) :: tiny = 1.0e-99_EOS_REAL

  if (dataType.EQ.EOS_Pic_DT) then
     ! Calculate the ideal pressure values for all zones
     do i = 1, nXYPairs
        call idealP(N, x(i), y(i), 'T', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  elseif (dataType.EQ.EOS_Pic_DUic) then
     ! Calculate the ideal pressure values for all zones
     ! (Don't forget to subtract Uc from y)
     do i = 1, nXYPairs
        call idealP(N, x(i), y(i)-fcold(i,1,2), 'U', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  elseif (dataType.EQ.EOS_Uic_DT) then
     ! Calculate the ideal internal energy values for all zones
     do i = 1, nXYPairs
        call idealU(N, x(i), y(i), 'T', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  elseif (dataType.EQ.EOS_Uic_DPic) then
     ! Calculate the ideal internal energy values for all zones
     ! (Don't forget to subtract Pc from y)
     do i = 1, nXYPairs
        call idealU(N, x(i), y(i)-fcold(i,1,1), 'P', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  elseif (dataType.EQ.EOS_T_DPic) then
     ! Calculate the ideal temperature values for all zones
     do i = 1, nXYPairs
        call idealT(N, x(i), y(i), 'P', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  elseif (dataType.EQ.EOS_T_DUic) then
     ! Calculate the ideal temperature values for all zones
     do i = 1, nXYPairs
        call idealT(N, x(i), y(i), 'U', R, fcold(i,:,1), fcold(i,:,2), A, C, gamma, F(i,:), gamma_bar, Abar)
     enddo

  else
     stop 'idealGas ERROR: invalid data type'
  endif
end subroutine idealGas

! ===========================================================================
! Calculate the temperature of a mixture of one or more ideal gases
! ===========================================================================
subroutine idealT(N, rho, y, yFlag, R, Pc, Uc, A, C, gamma, T, gamma_bar, Abar)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: N  ! Total ideal gases to use here
  real(EOS_REAL) :: rho      ! input mixture density (g/cc)
  real(EOS_REAL) :: y        ! input mixture temperature (K) or internal energy (MJ/kg)
  character yFlag            ! input 'P' indicates y=pressure (GPa)
                             !       'U' indicates y=internal energy (MJ/kg)
  real(EOS_REAL) :: Pc(3)    ! input cold curve pressure (GPa) and derivative
  real(EOS_REAL) :: Uc(3)    ! input cold curve internal energy (MJ/kg) and derivative
  real(EOS_REAL) :: A(N)     ! input ideal gas atomic weight (g/mol)
  real(EOS_REAL) :: gamma(N) ! input ideal gas specific heat ratio
  real(EOS_REAL) :: T(3)     ! output pressure (GPa) and derivatives
  real(EOS_REAL) :: R        ! Universal Gas Constant (J/mol-K)
  real(EOS_REAL) :: C(N)     ! Number fraction concentrations of ideal gas
  real(EOS_REAL) :: Abar, gamma_bar
  integer(EOS_INTEGER) :: i
  Abar = 0_EOS_REAL
  gamma_bar = 0_EOS_REAL
  do i=1, N
     Abar = Abar + C(i) * A(i)
     gamma_bar = gamma_bar + C(i) / dble(gamma(i) - 1)
  enddo
  if (yFlag.EQ."P") then
     T(1) = 1000._EOS_REAL * Abar / R * (y - Pc(1)) / rho                     ! K
     T(2) = Abar / R * (Pc(1) / rho**2 - Pc(2) / rho - 1000._EOS_REAL * y / rho**2) ! dT/d(rho)
     T(3) = 1000._EOS_REAL * Abar / rho / R                                   ! dT/dP
  else if (yFlag.EQ."U") then
     T(1) = 1000._EOS_REAL * Abar / R / gamma_bar * (y - Uc(1)) ! K
     T(2) = - 1000._EOS_REAL * Abar / R / gamma_bar * Uc(2)     ! dT/d(rho)
     T(3) = 1000._EOS_REAL * Abar / R / gamma_bar               ! dT/dU
  else
     write(*,*) 'idealP ERROR: invalid yFlag = ' // yFlag
     stop
  endif
end subroutine idealT

! ===========================================================================
! Calculate the pressure of a mixture of one or more ideal gases
! ===========================================================================
subroutine idealP(N, rho, y, yFlag, R, Pc, Uc, A, C, gamma, P, gamma_bar, Abar)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: N  ! Total ideal gases to use here
  real(EOS_REAL) :: rho      ! input mixture density (g/cc)
  real(EOS_REAL) :: y        ! input mixture temperature (K) or internal energy (MJ/kg)
  character yFlag            ! input 'T' indicates y=temperature (K)
                             !       'U' indicates y=internal energy (MJ/kg)
  real(EOS_REAL) :: Pc(3)    ! input cold curve pressure (GPa) and derivative
  real(EOS_REAL) :: Uc(3)    ! input cold curve internal energy (MJ/kg) and derivative
  real(EOS_REAL) :: A(N)     ! input ideal gas atomic weight (g/mol)
  real(EOS_REAL) :: gamma(N) ! input ideal gas specific heat ratio
  real(EOS_REAL) :: P(3)     ! output pressure (GPa) and derivatives
  real(EOS_REAL) :: R        ! Universal Gas Constant (J/mol-K)
  real(EOS_REAL) :: C(N)     ! Number fraction concentrations of ideal gas
  real(EOS_REAL) :: Abar, gamma_bar
  integer(EOS_INTEGER) :: i
  Abar = 0_EOS_REAL
  gamma_bar = 0_EOS_REAL
  do i=1, N
     Abar = Abar + C(i) * A(i)
     gamma_bar = gamma_bar + C(i) / dble(gamma(i) - 1)
  enddo
  if (yFlag.EQ."T") then
     P(1) = rho * R * y / Abar / 1000._EOS_REAL   ! GPa
     P(2) = R * y / Abar / 1000._EOS_REAL + Pc(2) ! dP/d(rho)
     P(3) = rho * R / Abar / 1000._EOS_REAL       ! dP/dT
     P(1) = P(1) + Pc(1)
  else if (yFlag.EQ."U") then
     P(1) = rho * y / gamma_bar                             ! GPa
     P(2) = y / gamma_bar + Pc(2) - rho / gamma_bar * Uc(2) ! dP/d(rho)
     P(3) = rho / gamma_bar                                 ! dP/dU
     P(1) = P(1) + Pc(1)
  else
     write(*,*) 'idealP ERROR: invalid yFlag = ' // yFlag
     stop
  endif
end subroutine idealP

! ===========================================================================
! Calculate the internal energy of a mixture of one or more ideal gases
! ===========================================================================
subroutine idealU(N, rho, y, yFlag, R, Pc, Uc, A, C, gamma, U, gamma_bar, Abar)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: N  ! Total ideal gases to use here
  real(EOS_REAL) :: rho      ! input mixture density (g/cc)
  real(EOS_REAL) :: y        ! input mixture temperature (K) or pressure (GPa)
  character yFlag            ! input 'T' indicates y=temperature (K)
                             !       'P' indicates y=pressure (GPa)
  real(EOS_REAL) :: A(N)     ! input ideal gas atomic weight (g/mol)
  real(EOS_REAL) :: Pc(3)    ! input cold curve pressure (GPa) and derivative
  real(EOS_REAL) :: Uc(3)    ! input cold curve internal energy (MJ/kg) and derivative
  real(EOS_REAL) :: gamma(N) ! input ideal gas specific heat ratio
  real(EOS_REAL) :: U(3)     ! output internal energy (MJ/kg) and derivatives
  real(EOS_REAL) :: R        ! Universal Gas Constant (J/mol-K)
  real(EOS_REAL) :: C(N)     ! Number fraction concentrations of ideal gas
  real(EOS_REAL) :: Abar, gamma_bar
  integer(EOS_INTEGER) :: i
  Abar = 0_EOS_REAL
  gamma_bar = 0_EOS_REAL
  do i=1, N
     Abar = Abar + C(i) * A(i)
     gamma_bar = gamma_bar + C(i) / dble(gamma(i) - 1)
  enddo
  if (yFlag.EQ."T") then
     U(1) = R * y / Abar * gamma_bar / 1000._EOS_REAL ! MJ/kg
     U(2) = Uc(2)                                     ! dU/d(rho)
     U(3) = R / Abar * gamma_bar / 1000._EOS_REAL     ! dU/dT
     U(1) = U(1) + Uc(1)
  else if (yFlag.EQ."P") then
     U(1) = gamma_bar * y / rho                        ! MJ/kg
     U(2) = Uc(2) - gamma_bar / rho * ( y/rho + Pc(2) ) ! dU/d(rho)
     U(3) = gamma_bar / rho                            ! dU/dT
     U(1) = U(1) + Uc(1)
  else
     write(*,*) 'idealU ERROR: invalid yFlag = ' // yFlag
     stop
  endif
end subroutine idealU

! ===========================================================================
! Return the the non-inverted data type(s) corresponding to iType.
! Special Cases:
!    xRefType = -1 if minX = minD should be used
!    xRefType = -2 if not a valid table for interpolation
!    yRefType = -1 if minY = minT should be used
!    yRefType = -2 if not a valid table for interpolation
! ===========================================================================
subroutine getCrossRefDataTypes(iType, xRefType, yRefType)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: iType, xRefType, yRefType
  character*(13) :: typeLabel
  !
  ! Determine xRefType
  !
  select case (iType)
  case(EOS_Zfo_DT, EOS_Zfc_DT, EOS_Uv_T, &
       EOS_Ut_DT, EOS_Ut_DSt, EOS_Ut_DPt, EOS_Ut_DAt, EOS_Um_D, EOS_Uls_T, &
       EOS_Uiz_DT, EOS_Uiz_DSiz, EOS_Uiz_DPiz, EOS_Uiz_DAiz, EOS_Uic_DT, &
       EOS_Uic_DSic, EOS_Uic_DPic, EOS_Uic_DAic, EOS_Uf_D, EOS_Ue_DT, EOS_Ue_DSe, &
       EOS_Ue_DPe, EOS_Ue_DAe, EOS_Uc_D, EOS_Tm_D, EOS_Tf_D, &
       EOS_T_DUt, EOS_T_DUiz, EOS_T_DUic, EOS_T_DUe, &
       EOS_T_DSt, EOS_T_DSiz, EOS_T_DSic, EOS_T_DSe, EOS_T_DPt, &
       EOS_T_DPiz, EOS_T_DPic, EOS_T_DPe, &
       EOS_T_DAt, EOS_T_DAiz, &
       EOS_T_DAic, EOS_T_DAe, EOS_St_DUt, EOS_St_DT, &
       EOS_St_DPt, EOS_St_DAt, EOS_Siz_DUiz, EOS_Siz_DT, EOS_Siz_DPiz, EOS_Siz_DAiz, &
       EOS_Sic_DUic, EOS_Sic_DT, EOS_Sic_DPic, EOS_Sic_DAic, EOS_Se_DUe, &
       EOS_Se_DT, EOS_Se_DPe, EOS_Se_DAe, EOS_Pv_T, EOS_Pt_DUt, EOS_Pt_DT, &
       EOS_Pt_DSt, EOS_Pt_DAt, EOS_Pm_D, EOS_Piz_DUiz, EOS_Piz_DT, &
       EOS_Piz_DSiz, EOS_Piz_DAiz, EOS_Pic_DUic, EOS_Pic_DT, EOS_Pic_DSic, &
       EOS_Pic_DAic, EOS_Pf_D, EOS_Pe_DUe, EOS_Pe_DT, EOS_Pe_DSe, EOS_Pe_DAe, &
       EOS_Pc_D, EOS_Ktc_DT, EOS_Kr_DT, EOS_Kp_DT, EOS_Keo_DT, &
       EOS_Kec_DT, EOS_Kc_DT, EOS_Av_T, EOS_At_DUt, EOS_At_DT, EOS_At_DSt, &
       EOS_At_DPt, EOS_Gs_D, EOS_Am_D, EOS_Als_T, EOS_Aiz_DUiz, &
       EOS_Aiz_DT, EOS_Aiz_DSiz, EOS_Aiz_DPiz, EOS_Aic_DUic, EOS_Aic_DT, EOS_Aic_DSic, &
       EOS_Aic_DPic, EOS_Af_D, EOS_Ae_DUe, EOS_Ae_DT, EOS_Ae_DSe, &
       EOS_Ae_DPe, EOS_Ac_D, EOS_Dv_T, EOS_Dls_T, EOS_B_DT)
     xRefType = -1_EOS_INTEGER
  case(EOS_Ogb, EOS_NullTable, EOS_Info, EOS_Comment)
     xRefType = -2_EOS_INTEGER
  case(EOS_Uv_Dls, &
       EOS_Uls_Dls, EOS_T_Dls, EOS_Pv_Dls, EOS_Av_Dls, EOS_Als_Dls, EOS_Dv_Dls)
     xRefType = EOS_Dls_T
  case(EOS_Uv_Dv, EOS_Uls_Dv, EOS_T_Dv, EOS_Pv_Dv, EOS_Av_Dv, EOS_Als_Dv, EOS_Dls_Dv)
     xRefType = EOS_Dv_T
  case(EOS_Uf_Af, EOS_Tf_Af, EOS_Pf_Af, EOS_D_Af)
     xRefType = EOS_Af_D
  case(EOS_Uv_Als, &
       EOS_Uls_Als, EOS_T_Als, EOS_Pv_Als, EOS_Av_Als, EOS_Dv_Als, EOS_Dls_Als)
     xRefType = EOS_Als_T
  case(EOS_Um_Am, EOS_Tm_Am, EOS_Pm_Am, EOS_D_Am)
     xRefType = EOS_Am_D
  case(EOS_D_Gs)
     xRefType = EOS_Gs_D
  case(EOS_Uv_Av, EOS_Uls_Av, EOS_T_Av, EOS_Pv_Av, EOS_Als_Av, EOS_Dv_Av, EOS_Dls_Av)
     xRefType = EOS_Av_T
  case(EOS_Uf_Pf, EOS_Tf_Pf, EOS_Af_Pf, EOS_D_Pf)
     xRefType = EOS_Pf_D
  case(EOS_Um_Pm, EOS_Tm_Pm, EOS_Am_Pm, EOS_D_Pm)
     xRefType = EOS_Pm_D
  case(EOS_Ut_PtT, EOS_D_PtT)
     xRefType = EOS_Pt_DT
  case(EOS_Uv_Pv, EOS_Uls_Pv, EOS_T_Pv, EOS_Av_Pv, EOS_Als_Pv, EOS_Dv_Pv, EOS_Dls_Pv)
     xRefType = EOS_Pv_T
  case(EOS_Uf_Tf, EOS_Pf_Tf, EOS_Af_Tf, EOS_D_Tf)
     xRefType = EOS_Tf_D
  case(EOS_Um_Tm, EOS_Pm_Tm, EOS_Am_Tm, EOS_D_Tm)
     xRefType = EOS_Tm_D
  case(EOS_Tf_Uf, EOS_Pf_Uf, EOS_Af_Uf, EOS_D_Uf)
     xRefType = EOS_Uf_D
  case(EOS_Uv_Uls, &
       EOS_T_Uls, EOS_Pv_Uls, EOS_Av_Uls, EOS_Als_Uls, EOS_Dv_Uls, EOS_Dls_Uls)
     xRefType = EOS_Uls_T
  case(EOS_Tm_Um, EOS_Pm_Um, EOS_Am_Um, EOS_D_Um)
     xRefType = EOS_Um_D
  case(EOS_Uls_Uv, EOS_T_Uv, EOS_Pv_Uv, EOS_Av_Uv, EOS_Als_Uv, EOS_Dv_Uv, EOS_Dls_Uv)
     xRefType = EOS_Uv_T
  case default
     write(*,*) 'getCrossRefDataType ERROR: invalid data type for interpolation = ', &
          trim(typeLabel(iType)), '(', iType, ')'
     stop
  end select

  !
  ! Determine yRefType
  !
  select case (iType)
  case(EOS_Zfo_DT, &
       EOS_Zfc_DT, EOS_Ut_PtT, EOS_Ut_DT, &
       EOS_Uiz_DT, &
       EOS_Uic_DT, &
       EOS_Ue_DT, EOS_St_DT, &
       EOS_Siz_DT, &
       EOS_Sic_DT, &
       EOS_Se_DT, EOS_Pt_DT, &
       EOS_Piz_DT, &
       EOS_Pic_DT, EOS_Pe_DT, &
       EOS_Ktc_DT, EOS_Kr_DT, EOS_Kp_DT, EOS_Keo_DT, EOS_Kec_DT, &
       EOS_Kc_DT, EOS_At_DT, &
       EOS_Aiz_DT, &
       EOS_Aic_DT, &
       EOS_Ae_DT, &
       &
       EOS_D_PtT, &
       &
       EOS_B_DT)
     yRefType = -1_EOS_INTEGER
  case(EOS_Uv_Uls, EOS_Uv_T, EOS_Uv_Pv, EOS_Uv_Av, EOS_Uv_Als, EOS_Uv_Dv, &
       EOS_Uv_Dls, EOS_Um_Tm, EOS_Um_Pm, EOS_Um_Am, EOS_Um_D, &
       EOS_Uls_Uv, EOS_Uls_T, EOS_Uls_Pv, EOS_Uls_Av, EOS_Uls_Als, EOS_Uls_Dv, &
       EOS_Uls_Dls, EOS_Uf_Tf, EOS_Uf_Pf, EOS_Uf_Af, EOS_Uf_D, &
       EOS_Uc_D, EOS_Tm_Um, EOS_Tm_Pm, EOS_Tm_Am, &
       EOS_Tm_D, EOS_Tf_Uf, EOS_Tf_Pf, EOS_Tf_Af, EOS_Tf_D, &
       EOS_T_Uv, EOS_T_Uls, EOS_T_Pv, EOS_T_Av, EOS_T_Als, &
       EOS_T_Dv, EOS_T_Dls, EOS_Pv_Uv, EOS_Pv_Uls, EOS_Pv_T, EOS_Pv_Av, &
       EOS_Pv_Als, EOS_Pv_Dv, EOS_Pv_Dls, EOS_Pm_Um, EOS_Pm_Tm, &
       EOS_Pm_Am, EOS_Pm_D, EOS_Pf_Uf, EOS_Pf_Tf, EOS_Pf_Af, EOS_Pf_D, &
       EOS_Pc_D, EOS_Ogb, EOS_NullTable, &
       EOS_Info, EOS_Av_Uv, EOS_Av_Uls, EOS_Av_T, EOS_Av_Pv, EOS_Av_Als, &
       EOS_Av_Dv, EOS_Av_Dls, EOS_Gs_D, EOS_Am_Um, EOS_Am_Tm, &
       EOS_Am_Pm, EOS_Am_D, EOS_Als_Uv, EOS_Als_Uls, EOS_Als_T, &
       EOS_Als_Pv, EOS_Als_Av, EOS_Als_Dv, EOS_Als_Dls, EOS_Af_Uf, EOS_Af_Tf, &
       EOS_Af_Pf, EOS_Af_D, EOS_Ac_D, &
       EOS_Dv_Uv, EOS_Dv_Uls, EOS_Dv_T, EOS_Dv_Pv, EOS_Dv_Av, EOS_Dv_Als, &
       EOS_Dv_Dls, EOS_Dls_Uv, EOS_Dls_Uls, EOS_Dls_T, EOS_Dls_Pv, &
       EOS_Dls_Av, EOS_Dls_Als, EOS_Dls_Dv, EOS_D_Um, EOS_D_Uf, &
       EOS_D_Tm, EOS_D_Tf, EOS_D_Pm, EOS_D_Pf, &
       EOS_D_Gs, EOS_D_Am, EOS_D_Af, EOS_Comment)
     yRefType = -2_EOS_INTEGER
  case(EOS_Ue_DAe, EOS_T_DAe, EOS_Se_DAe, EOS_Pe_DAe)
     yRefType = EOS_Ae_DT
  case(EOS_Uic_DAic, EOS_T_DAic, EOS_Sic_DAic, EOS_Pic_DAic)
     yRefType = EOS_Aic_DT
  case(EOS_Uiz_DAiz, EOS_T_DAiz, EOS_Siz_DAiz, EOS_Piz_DAiz)
     yRefType = EOS_Aiz_DT
  case(EOS_Ut_DAt, EOS_T_DAt, EOS_St_DAt, EOS_Pt_DAt)
     yRefType = EOS_At_DT
  case(EOS_Ue_DPe, EOS_T_DPe, EOS_Se_DPe, EOS_Ae_DPe)
     yRefType = EOS_Pe_DT
  case(EOS_Uic_DPic, EOS_T_DPic, EOS_Sic_DPic, EOS_Aic_DPic)
     yRefType = EOS_Pic_DT
  case(EOS_Uiz_DPiz, EOS_T_DPiz, EOS_Siz_DPiz, EOS_Aiz_DPiz)
     yRefType = EOS_Piz_DT
  case(EOS_Ut_DPt, EOS_T_DPt, EOS_St_DPt, EOS_At_DPt)
     yRefType = EOS_Pt_DT
  case(EOS_Ue_DSe, EOS_T_DSe, EOS_Pe_DSe, EOS_Ae_DSe)
     yRefType = EOS_Se_DT
  case(EOS_Uic_DSic, EOS_T_DSic, EOS_Pic_DSic, EOS_Aic_DSic)
     yRefType = EOS_Sic_DT
  case(EOS_Uiz_DSiz, EOS_T_DSiz, EOS_Piz_DSiz, EOS_Aiz_DSiz)
     yRefType = EOS_Siz_DT
  case(EOS_Ut_DSt, EOS_T_DSt, EOS_Pt_DSt, EOS_At_DSt)
     yRefType = EOS_St_DT
  case(EOS_T_DUe, EOS_Se_DUe, EOS_Pe_DUe, EOS_Ae_DUe)
     yRefType = EOS_Ue_DT
  case(EOS_T_DUic, EOS_Sic_DUic, EOS_Pic_DUic, EOS_Aic_DUic)
     yRefType = EOS_Uic_DT
  case(EOS_T_DUiz, EOS_Siz_DUiz, EOS_Piz_DUiz, EOS_Aiz_DUiz)
     yRefType = EOS_Uiz_DT
  case(EOS_T_DUt, EOS_St_DUt, EOS_Pt_DUt, EOS_At_DUt)
     yRefType = EOS_Ut_DT
  case default
     write(*,*) 'getCrossRefDataType ERROR: invalid data type for interpolation = ', &
          trim(typeLabel(iType)), '(', iType, ')'
     stop
  end select
end subroutine getCrossRefDataTypes

! ===========================================================================
! Return character string label corresponding to iType.
! ===========================================================================
character*(13) function typeLabel(iType)
  use eos_Interface
  implicit none
  integer(EOS_INTEGER) :: iType
  select case (iType)
  case(EOS_Ae_DPe)
     typeLabel = 'EOS_Ae_DPe'
  case(EOS_Ae_DSe)
     typeLabel = 'EOS_Ae_DSe'
  case(EOS_Ae_DUe)
     typeLabel = 'EOS_Ae_DUe'
  case(EOS_Aic_DPic)
     typeLabel = 'EOS_Aic_DPic'
  case(EOS_Aic_DSic)
     typeLabel = 'EOS_Aic_DSic'
  case(EOS_Aic_DUic)
     typeLabel = 'EOS_Aic_DUic'
  case(EOS_Aiz_DPiz)
     typeLabel = 'EOS_Aiz_DPiz'
  case(EOS_Aiz_DSiz)
     typeLabel = 'EOS_Aiz_DSiz'
  case(EOS_Aiz_DUiz)
     typeLabel = 'EOS_Aiz_DUiz'
  case(EOS_At_DPt)
     typeLabel = 'EOS_At_DPt'
  case(EOS_At_DSt)
     typeLabel = 'EOS_At_DSt'
  case(EOS_At_DUt)
     typeLabel = 'EOS_At_DUt'
  case(EOS_Pe_DAe)
     typeLabel = 'EOS_Pe_DAe'
  case(EOS_Pe_DSe)
     typeLabel = 'EOS_Pe_DSe'
  case(EOS_Pe_DUe)
     typeLabel = 'EOS_Pe_DUe'
  case(EOS_Pic_DAic)
     typeLabel = 'EOS_Pic_DAic'
  case(EOS_Pic_DSic)
     typeLabel = 'EOS_Pic_DSic'
  case(EOS_Pic_DUic)
     typeLabel = 'EOS_Pic_DUic'
  case(EOS_Piz_DAiz)
     typeLabel = 'EOS_Piz_DAiz'
  case(EOS_Piz_DSiz)
     typeLabel = 'EOS_Piz_DSiz'
  case(EOS_Piz_DUiz)
     typeLabel = 'EOS_Piz_DUiz'
  case(EOS_Pt_DAt)
     typeLabel = 'EOS_Pt_DAt'
  case(EOS_Pt_DSt)
     typeLabel = 'EOS_Pt_DSt'
  case(EOS_Pt_DUt)
     typeLabel = 'EOS_Pt_DUt'
  case(EOS_Se_DAe)
     typeLabel = 'EOS_Se_DAe'
  case(EOS_Se_DPe)
     typeLabel = 'EOS_Se_DPe'
  case(EOS_Se_DUe)
     typeLabel = 'EOS_Se_DUe'
  case(EOS_Sic_DAic)
     typeLabel = 'EOS_Sic_DAic'
  case(EOS_Sic_DPic)
     typeLabel = 'EOS_Sic_DPic'
  case(EOS_Sic_DUic)
     typeLabel = 'EOS_Sic_DUic'
  case(EOS_Siz_DAiz)
     typeLabel = 'EOS_Siz_DAiz'
  case(EOS_Siz_DPiz)
     typeLabel = 'EOS_Siz_DPiz'
  case(EOS_Siz_DUiz)
     typeLabel = 'EOS_Siz_DUiz'
  case(EOS_St_DAt)
     typeLabel = 'EOS_St_DAt'
  case(EOS_St_DPt)
     typeLabel = 'EOS_St_DPt'
  case(EOS_St_DUt)
     typeLabel = 'EOS_St_DUt'
  case(EOS_T_DAe)
     typeLabel = 'EOS_T_DAe'
  case(EOS_T_DAic)
     typeLabel = 'EOS_T_DAic'
  case(EOS_T_DAiz)
     typeLabel = 'EOS_T_DAiz'
  case(EOS_T_DAt)
     typeLabel = 'EOS_T_DAt'
  case(EOS_T_DPe)
     typeLabel = 'EOS_T_DPe'
  case(EOS_T_DPic)
     typeLabel = 'EOS_T_DPic'
  case(EOS_T_DPiz)
     typeLabel = 'EOS_T_DPiz'
  case(EOS_T_DPt)
     typeLabel = 'EOS_T_DPt'
  case(EOS_T_DSe)
     typeLabel = 'EOS_T_DSe'
  case(EOS_T_DSic)
     typeLabel = 'EOS_T_DSic'
  case(EOS_T_DSiz)
     typeLabel = 'EOS_T_DSiz'
  case(EOS_T_DSt)
     typeLabel = 'EOS_T_DSt'
  case(EOS_T_DUe)
     typeLabel = 'EOS_T_DUe'
  case(EOS_T_DUic)
     typeLabel = 'EOS_T_DUic'
  case(EOS_T_DUiz)
     typeLabel = 'EOS_T_DUiz'
  case(EOS_T_DUt)
     typeLabel = 'EOS_T_DUt'
  case(EOS_Ue_DAe)
     typeLabel = 'EOS_Ue_DAe'
  case(EOS_Ue_DPe)
     typeLabel = 'EOS_Ue_DPe'
  case(EOS_Ue_DSe)
     typeLabel = 'EOS_Ue_DSe'
  case(EOS_Uic_DAic)
     typeLabel = 'EOS_Uic_DAic'
  case(EOS_Uic_DPic)
     typeLabel = 'EOS_Uic_DPic'
  case(EOS_Uic_DSic)
     typeLabel = 'EOS_Uic_DSic'
  case(EOS_Uiz_DAiz)
     typeLabel = 'EOS_Uiz_DAiz'
  case(EOS_Uiz_DPiz)
     typeLabel = 'EOS_Uiz_DPiz'
  case(EOS_Uiz_DSiz)
     typeLabel = 'EOS_Uiz_DSiz'
  case(EOS_Ut_DAt)
     typeLabel = 'EOS_Ut_DAt'
  case(EOS_Ut_DPt)
     typeLabel = 'EOS_Ut_DPt'
  case(EOS_Ut_DSt)
     typeLabel = 'EOS_Ut_DSt'
  case(EOS_Comment)
     typeLabel = 'EOS_Comment'
  case(EOS_Info)
     typeLabel = 'EOS_Info'
  case(EOS_NullTable)
     typeLabel = 'EOS_NullTable'
  case(EOS_Ogb)
     typeLabel = 'EOS_Ogb'
  case(EOS_B_DT)
     typeLabel = 'EOS_B_DT'
  case(EOS_Dls_T)
     typeLabel = 'EOS_Dls_T'
  case(EOS_Dv_Dls)
     typeLabel = 'EOS_Dv_Dls'
  case(EOS_Als_Dls)
     typeLabel = 'EOS_Als_Dls'
  case(EOS_Av_Dls)
     typeLabel = 'EOS_Av_Dls'
  case(EOS_Pv_Dls)
     typeLabel = 'EOS_Pv_Dls'
  case(EOS_T_Dls)
     typeLabel = 'EOS_T_Dls'
  case(EOS_Uls_Dls)
     typeLabel = 'EOS_Uls_Dls'
  case(EOS_Uv_Dls)
     typeLabel = 'EOS_Uv_Dls'
  case(EOS_Dls_Dv)
     typeLabel = 'EOS_Dls_Dv'
  case(EOS_Dv_T)
     typeLabel = 'EOS_Dv_T'
  case(EOS_Als_Dv)
     typeLabel = 'EOS_Als_Dv'
  case(EOS_Av_Dv)
     typeLabel = 'EOS_Av_Dv'
  case(EOS_Pv_Dv)
     typeLabel = 'EOS_Pv_Dv'
  case(EOS_T_Dv)
     typeLabel = 'EOS_T_Dv'
  case(EOS_Uls_Dv)
     typeLabel = 'EOS_Uls_Dv'
  case(EOS_Uv_Dv)
     typeLabel = 'EOS_Uv_Dv'
  case(EOS_Ac_D)
     typeLabel = 'EOS_Ac_D'
  case(EOS_Ae_DT)
     typeLabel = 'EOS_Ae_DT'
  case(EOS_D_Af)
     typeLabel = 'EOS_D_Af'
  case(EOS_Af_D)
     typeLabel = 'EOS_Af_D'
  case(EOS_Pf_Af)
     typeLabel = 'EOS_Pf_Af'
  case(EOS_Tf_Af)
     typeLabel = 'EOS_Tf_Af'
  case(EOS_Uf_Af)
     typeLabel = 'EOS_Uf_Af'
  case(EOS_Aic_DT)
     typeLabel = 'EOS_Aic_DT'
  case(EOS_Aiz_DT)
     typeLabel = 'EOS_Aiz_DT'
  case(EOS_Dls_Als)
     typeLabel = 'EOS_Dls_Als'
  case(EOS_Dv_Als)
     typeLabel = 'EOS_Dv_Als'
  case(EOS_Als_T)
     typeLabel = 'EOS_Als_T'
  case(EOS_Av_Als)
     typeLabel = 'EOS_Av_Als'
  case(EOS_Pv_Als)
     typeLabel = 'EOS_Pv_Als'
  case(EOS_T_Als)
     typeLabel = 'EOS_T_Als'
  case(EOS_Uls_Als)
     typeLabel = 'EOS_Uls_Als'
  case(EOS_Uv_Als)
     typeLabel = 'EOS_Uv_Als'
  case(EOS_D_Am)
     typeLabel = 'EOS_D_Am'
  case(EOS_Am_D)
     typeLabel = 'EOS_Am_D'
  case(EOS_Pm_Am)
     typeLabel = 'EOS_Pm_Am'
  case(EOS_Tm_Am)
     typeLabel = 'EOS_Tm_Am'
  case(EOS_Um_Am)
     typeLabel = 'EOS_Um_Am'
  case(EOS_D_Gs)
     typeLabel = 'EOS_D_Gs'
  case(EOS_Gs_D)
     typeLabel = 'EOS_Gs_D'
  case(EOS_At_DT)
     typeLabel = 'EOS_At_DT'
  case(EOS_Dls_Av)
     typeLabel = 'EOS_Dls_Av'
  case(EOS_Dv_Av)
     typeLabel = 'EOS_Dv_Av'
  case(EOS_Als_Av)
     typeLabel = 'EOS_Als_Av'
  case(EOS_Av_T)
     typeLabel = 'EOS_Av_T'
  case(EOS_Pv_Av)
     typeLabel = 'EOS_Pv_Av'
  case(EOS_T_Av)
     typeLabel = 'EOS_T_Av'
  case(EOS_Uls_Av)
     typeLabel = 'EOS_Uls_Av'
  case(EOS_Uv_Av)
     typeLabel = 'EOS_Uv_Av'
  case(EOS_Kc_DT)
     typeLabel = 'EOS_Kc_DT'
  case(EOS_Kec_DT)
     typeLabel = 'EOS_Kec_DT'
  case(EOS_Keo_DT)
     typeLabel = 'EOS_Keo_DT'
  case(EOS_Kp_DT)
     typeLabel = 'EOS_Kp_DT'
  case(EOS_Kr_DT)
     typeLabel = 'EOS_Kr_DT'
  case(EOS_Ktc_DT)
     typeLabel = 'EOS_Ktc_DT'
  case(EOS_Pc_D)
     typeLabel = 'EOS_Pc_D'
  case(EOS_Pe_DT)
     typeLabel = 'EOS_Pe_DT'
  case(EOS_D_Pf)
     typeLabel = 'EOS_D_Pf'
  case(EOS_Af_Pf)
     typeLabel = 'EOS_Af_Pf'
  case(EOS_Pf_D)
     typeLabel = 'EOS_Pf_D'
  case(EOS_Tf_Pf)
     typeLabel = 'EOS_Tf_Pf'
  case(EOS_Uf_Pf)
     typeLabel = 'EOS_Uf_Pf'
  case(EOS_Pic_DT)
     typeLabel = 'EOS_Pic_DT'
  case(EOS_Piz_DT)
     typeLabel = 'EOS_Piz_DT'
  case(EOS_D_Pm)
     typeLabel = 'EOS_D_Pm'
  case(EOS_Am_Pm)
     typeLabel = 'EOS_Am_Pm'
  case(EOS_Pm_D)
     typeLabel = 'EOS_Pm_D'
  case(EOS_Tm_Pm)
     typeLabel = 'EOS_Tm_Pm'
  case(EOS_Um_Pm)
     typeLabel = 'EOS_Um_Pm'
  case(EOS_D_PtT)
     typeLabel = 'EOS_D_PtT'
  case(EOS_Pt_DT)
     typeLabel = 'EOS_Pt_DT'
  case(EOS_Ut_PtT)
     typeLabel = 'EOS_Ut_PtT'
  case(EOS_Dls_Pv)
     typeLabel = 'EOS_Dls_Pv'
  case(EOS_Dv_Pv)
     typeLabel = 'EOS_Dv_Pv'
  case(EOS_Als_Pv)
     typeLabel = 'EOS_Als_Pv'
  case(EOS_Av_Pv)
     typeLabel = 'EOS_Av_Pv'
  case(EOS_Pv_T)
     typeLabel = 'EOS_Pv_T'
  case(EOS_T_Pv)
     typeLabel = 'EOS_T_Pv'
  case(EOS_Uls_Pv)
     typeLabel = 'EOS_Uls_Pv'
  case(EOS_Uv_Pv)
     typeLabel = 'EOS_Uv_Pv'
  case(EOS_Se_DT)
     typeLabel = 'EOS_Se_DT'
  case(EOS_Sic_DT)
     typeLabel = 'EOS_Sic_DT'
  case(EOS_Siz_DT)
     typeLabel = 'EOS_Siz_DT'
  case(EOS_St_DT)
     typeLabel = 'EOS_St_DT'
  case(EOS_D_Tf)
     typeLabel = 'EOS_D_Tf'
  case(EOS_Af_Tf)
     typeLabel = 'EOS_Af_Tf'
  case(EOS_Pf_Tf)
     typeLabel = 'EOS_Pf_Tf'
  case(EOS_Tf_D)
     typeLabel = 'EOS_Tf_D'
  case(EOS_Uf_Tf)
     typeLabel = 'EOS_Uf_Tf'
  case(EOS_D_Tm)
     typeLabel = 'EOS_D_Tm'
  case(EOS_Am_Tm)
     typeLabel = 'EOS_Am_Tm'
  case(EOS_Pm_Tm)
     typeLabel = 'EOS_Pm_Tm'
  case(EOS_Tm_D)
     typeLabel = 'EOS_Tm_D'
  case(EOS_Um_Tm)
     typeLabel = 'EOS_Um_Tm'
  case(EOS_Uc_D)
     typeLabel = 'EOS_Uc_D'
  case(EOS_Ue_DT)
     typeLabel = 'EOS_Ue_DT'
  case(EOS_D_Uf)
     typeLabel = 'EOS_D_Uf'
  case(EOS_Af_Uf)
     typeLabel = 'EOS_Af_Uf'
  case(EOS_Pf_Uf)
     typeLabel = 'EOS_Pf_Uf'
  case(EOS_Tf_Uf)
     typeLabel = 'EOS_Tf_Uf'
  case(EOS_Uf_D)
     typeLabel = 'EOS_Uf_D'
  case(EOS_Uic_DT)
     typeLabel = 'EOS_Uic_DT'
  case(EOS_Uiz_DT)
     typeLabel = 'EOS_Uiz_DT'
  case(EOS_Dls_Uls)
     typeLabel = 'EOS_Dls_Uls'
  case(EOS_Dv_Uls)
     typeLabel = 'EOS_Dv_Uls'
  case(EOS_Als_Uls)
     typeLabel = 'EOS_Als_Uls'
  case(EOS_Av_Uls)
     typeLabel = 'EOS_Av_Uls'
  case(EOS_Pv_Uls)
     typeLabel = 'EOS_Pv_Uls'
  case(EOS_T_Uls)
     typeLabel = 'EOS_T_Uls'
  case(EOS_Uls_T)
     typeLabel = 'EOS_Uls_T'
  case(EOS_Uv_Uls)
     typeLabel = 'EOS_Uv_Uls'
  case(EOS_D_Um)
     typeLabel = 'EOS_D_Um'
  case(EOS_Am_Um)
     typeLabel = 'EOS_Am_Um'
  case(EOS_Pm_Um)
     typeLabel = 'EOS_Pm_Um'
  case(EOS_Tm_Um)
     typeLabel = 'EOS_Tm_Um'
  case(EOS_Um_D)
     typeLabel = 'EOS_Um_D'
  case(EOS_Ut_DT)
     typeLabel = 'EOS_Ut_DT'
  case(EOS_Dls_Uv)
     typeLabel = 'EOS_Dls_Uv'
  case(EOS_Dv_Uv)
     typeLabel = 'EOS_Dv_Uv'
  case(EOS_Als_Uv)
     typeLabel = 'EOS_Als_Uv'
  case(EOS_Av_Uv)
     typeLabel = 'EOS_Av_Uv'
  case(EOS_Pv_Uv)
     typeLabel = 'EOS_Pv_Uv'
  case(EOS_T_Uv)
     typeLabel = 'EOS_T_Uv'
  case(EOS_Uls_Uv)
     typeLabel = 'EOS_Uls_Uv'
  case(EOS_Uv_T)
     typeLabel = 'EOS_Uv_T'
  case(EOS_Zfc_DT)
     typeLabel = 'EOS_Zfc_DT'
  case(EOS_Zfo_DT)
     typeLabel = 'EOS_Zfo_DT'
  case default
     typeLabel = 'UNDETERMINED'
  end select
end function typeLabel

! ===========================================================================
! Return EOSPAC5 data type corresponding to EOSPAC6 value, iType.
! ===========================================================================
subroutine getType5fromTH(TH,ieosf,ieosf_s)
  use eos_Interface
  use dataInterface
  implicit none
  integer(EOS_INTEGER) :: TH, ieosf, iType, errorCode
  character(11) ieosf_s
  real(EOS_REAL) :: infoVal

  call eos_GetTableInfo(TH, 1_EOS_INTEGER, EOS_Table_Type, infoVal, errorCode)
  iType = infoVal
  if (errorCode.NE.EOS_OK) then
     call print_error(errorCode, 'getType5fromTH->eos_GetTableInfo')
  endif
  

  select case (iType)
  case(EOS_Pt_DT)
     ieosf = ES4_PRTOT
     ieosf_s = "ES4_PRTOT"
  case(EOS_Ut_DT)
     ieosf = ES4_ENTOT
     ieosf_s = "ES4_ENTOT"
  case(EOS_T_DPt)
     ieosf = ES4_TPTOT
     ieosf_s = "ES4_TPTOT"
  case(EOS_Ut_DPt)
     ieosf = ES4_EPTOT
     ieosf_s = "ES4_EPTOT"
  case(EOS_T_DUt)
     ieosf = ES4_TNTOT
     ieosf_s = "ES4_TNTOT"
  case(EOS_Pt_DUt)
     ieosf = ES4_PNTOT
     ieosf_s = "ES4_PNTOT"
  case(EOS_Pic_DT)
     ieosf = ES4_PRION
     ieosf_s = "ES4_PRION"
  case(EOS_Uic_DT)
     ieosf = ES4_ENION
     ieosf_s = "ES4_ENION"
  case(EOS_T_DPic)
     ieosf = ES4_TPION
     ieosf_s = "ES4_TPION"
  case(EOS_Uic_DPic)
     ieosf = ES4_EPION
     ieosf_s = "ES4_EPION"
  case(EOS_T_DUic)
     ieosf = ES4_TNION
     ieosf_s = "ES4_TNION"
  case(EOS_Pic_DUic)
     ieosf = ES4_PNION
     ieosf_s = "ES4_PNION"
  case(EOS_Pe_DT)
     ieosf = ES4_PRELC
     ieosf_s = "ES4_PRELC"
  case(EOS_Ue_DT)
     ieosf = ES4_ENELC
     ieosf_s = "ES4_ENELC"
  case(EOS_T_DPe)
     ieosf = ES4_TPELC
     ieosf_s = "ES4_TPELC"
  case(EOS_Ue_DPe)
     ieosf = ES4_EPELC
     ieosf_s = "ES4_EPELC"
  case(EOS_T_DUe)
     ieosf = ES4_TNELC
     ieosf_s = "ES4_TNELC"
  case(EOS_Pe_DUe)
     ieosf = ES4_PNELC
     ieosf_s = "ES4_PNELC"
  case(EOS_Pc_D)
     ieosf = ES4_PRCLD
     ieosf_s = "ES4_PRCLD"
  case(EOS_Uc_D)
     ieosf = ES4_ENCLD
     ieosf_s = "ES4_ENCLD"
  case(EOS_Kr_DT)
     ieosf = ES4_OPACR
     ieosf_s = "ES4_OPACR"
  case(EOS_Keo_DT)
     ieosf = ES4_OPACC2
     ieosf_s = "ES4_OPACC2"
  case(EOS_Zfo_DT)
     ieosf = ES4_ZFREE2
     ieosf_s = "ES4_ZFREE2"
  case(EOS_Kp_DT)
     ieosf = ES4_OPACP
     ieosf_s = "ES4_OPACP"
  case(EOS_Zfc_DT)
     ieosf = ES4_ZFREE3
     ieosf_s = "ES4_ZFREE3"
  case(EOS_Kec_DT)
     ieosf = ES4_ECONDE
     ieosf_s = "ES4_ECONDE"
  case(EOS_Ktc_DT)
     ieosf = ES4_TCONDE
     ieosf_s = "ES4_TCONDE"
  case(EOS_B_DT)
     ieosf = ES4_THERME
     ieosf_s = "ES4_THERME"
  case(EOS_Kc_DT)
     ieosf = ES4_OPACC3
     ieosf_s = "ES4_OPACC3"
  case default
     ieosf = ES4_NULLPTR
     ieosf_s = "ES4_NULLPTR"
  end select
end subroutine getType5fromTH
