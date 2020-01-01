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
!!    -# Related to issue artf44811, Test inverse pressure interpolation.
!!       See SourceForge<A9> Issue #artf44811 for more details:
!!       https://tf.lanl.gov/sf/go/artf44811
!!
!! \note
!! MATIDS TO TEST: 3720
program test

  use eos_Interface

  implicit none

  integer(EOS_INTEGER) :: err, fh, num

  integer(EOS_INTEGER), parameter :: ntablesE=15
  integer(EOS_INTEGER), parameter :: ntables=5
  integer(EOS_INTEGER), parameter :: nxypairs=2500
  integer(EOS_INTEGER), parameter :: pass_tot=4

  integer(EOS_INTEGER) :: i, j
  integer(EOS_INTEGER) :: th(ntablesE), type(ntablesE), matid(ntablesE)

  integer(EOS_INTEGER) :: xybounds(nxypairs), pass_cnt

  real(EOS_REAL) :: time(nxypairs)                                  ! input data
  real(EOS_REAL) :: x_in(nxypairs),t_in(nxypairs)
  real(EOS_REAL) :: p_in(nxypairs),u_in(nxypairs)

  real(EOS_REAL) :: x(nxypairs),t(nxypairs),p(nxypairs),u(nxypairs) ! converted input data

  real(EOS_REAL) :: p_value(pass_tot,nxypairs)                      ! interpolated data
  real(EOS_REAL) :: e_value(pass_tot,nxypairs)
  real(EOS_REAL) :: t_value(pass_tot,nxypairs)
  real(EOS_REAL) :: t2_value(pass_tot,nxypairs)
  real(EOS_REAL) :: e2_Flag(nxypairs)
  real(EOS_REAL) :: e2_value(pass_tot,nxypairs)

  real(EOS_REAL) :: dfy(nxypairs),dfx(nxypairs)                     ! interpolated discard

  real(EOS_REAL), parameter :: u_convert = 1.0d-2 ! Mbar-cc/g to MJ/kg
  real(EOS_REAL), parameter :: p_convert = 1.0d-2 ! Mbar to GPa

  logical :: equal, preinvert

  character(len=1024) :: s, labels_s, units_s
  character(len=22)   :: labels(40), labels_mod(40), labels_in(40), units(40)
  character(len=22)   :: type_str(ntablesE)

  !     Load input and baseline data
  err = 0
  fh = 99
  open(UNIT=fh, FILE='test_artf44811.inp', STATUS='OLD', IOSTAT=err)
  if (err.ne.0) then
     write(*,*) 'open failed! err=',err
     stop
  endif
  num = 1
  do
     read(fh,'(a)', IOSTAT=err) s
     if (err .NE. 0 .OR. num .GT. nxypairs) exit
     if (s(1:1) .NE. '#') then
        read(s,*) time(num),x_in(num),u_in(num),p_in(num),t_in(num)
        num = num + 1
     else
        if (s(1:8) .EQ. '# UNITS:') then
           units_s = trim(s)
        else
           labels_s = trim(s)
        endif
     endif
  enddo
  num = num - 1

  read(labels_s,997) (labels(i), i=1,5)
  read(units_s,997) (units(i), i=1,5)
  do i=1,5
     labels(i) = adjustl(trim(labels(i)))
     labels(i) = adjustr(labels(i))
     units(i) = adjustl(trim(units(i)))
     units(i) = adjustr(units(i))
  enddo

  if (.FALSE.) then
     !     Regurgitate input and baseline data
     write(*,996) labels
     do i=1,num
        write(*,999) time(i), x(i), u(i), p(i), t(i)
     enddo
     write(*,*) '---'
  endif

  preinvert = .FALSE.
  pass_cnt = 1

10 continue

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

  type_str(1 ) = 'EOS_Pt_DUt            '
  type_str(2 ) = 'EOS_T_DUt             '
  type_str(3 ) = 'EOS_Ut_DT             '
  type_str(4 ) = 'EOS_Ut_DPt            '
  type_str(5 ) = 'EOS_T_DPt             '
  type_str(6 ) = 'EOS_Pt_DAt            '
  type_str(7 ) = 'EOS_Pt_DSt            '
  type_str(8 ) = 'EOS_Ut_DAt            '
  type_str(9 ) = 'EOS_Ut_DSt            '
  type_str(10) = 'EOS_At_DPt            '
  type_str(11) = 'EOS_At_DUt            '
  type_str(12) = 'EOS_At_DSt            '
  type_str(13) = 'EOS_St_DPt            '
  type_str(14) = 'EOS_St_DUt            '
  type_str(15) = 'EOS_St_DAt            '

  matid(:) = 3720 ! fill the array
  !print*, 'matid = [', matid, ']'

  !     Load SESAME data
  call eos_CreateTables(ntablesE,type,matid,th,err)

  !print*, 'eos_CreateTables::err = ', err

  if (preinvert) then
     call eos_SetOption(th(1),EOS_INVERT_AT_SETUP,0,err)
     call eos_SetOption(th(2),EOS_INVERT_AT_SETUP,0,err)
     call eos_SetOption(th(4),EOS_INVERT_AT_SETUP,0,err)
     if (ntables.GE.5) &
          call eos_SetOption(th(5),EOS_INVERT_AT_SETUP,0,err)
     do i=6,ntablesE
        call eos_SetOption(th(i),EOS_INVERT_AT_SETUP,0,err)
     enddo
  endif

  do i=1,ntablesE
     call eos_SetOption(th(i),EOS_APPEND_DATA,0,err)
  enddo
  if (pass_cnt .EQ. 1) &
       call eos_SetOption(th(1),EOS_DUMP_DATA,0,err)

  call eos_LoadTables(ntablesE,th,err)

  !     Apply necessary conversion factors
  if (pass_cnt .LE. 2) then
     do i=1,num
        x(i) = x_in(i)
        t(i) = t_in(i)
        p(i) = p_in(i) / p_convert
        u(i) = u_in(i) / u_convert
     enddo
  else
     call eos_SetOption(th(1),EOS_Y_CONVERT,u_convert,err)
     call eos_SetOption(th(1),EOS_F_CONVERT,p_convert,err)
     call eos_SetOption(th(2),EOS_Y_CONVERT,u_convert,err)
     call eos_SetOption(th(3),EOS_F_CONVERT,u_convert,err)
     call eos_SetOption(th(4),EOS_Y_CONVERT,p_convert,err)
     call eos_SetOption(th(4),EOS_F_CONVERT,u_convert,err)
     if (ntables.GE.5) call eos_SetOption(th(5),EOS_Y_CONVERT,p_convert,err)
     do i=1,num
        x(i) = x_in(i)
        t(i) = t_in(i)
        p(i) = p_in(i)
        u(i) = u_in(i)
     enddo
  endif

  !     Interpolate EOS_Pt_DUt

  err=EOS_OK
  call eos_Interpolate(th(1),nxypairs,x,u,p_value(pass_cnt,:),dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(1),nxypairs,x,u,xybounds,err)
  endif

  !     Interpolate EOS_Ut_DPt

  err=EOS_OK
  call eos_Interpolate(th(4),nxypairs,x,p_value(pass_cnt,:),e_value(pass_cnt,:),dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(4),nxypairs,x,p_value(pass_cnt,:),xybounds,err)
  endif

  !     Interpolate EOS_T_DUt

  err=EOS_OK
  call eos_Interpolate(th(2),nxypairs,x,u,t_value(pass_cnt,:),dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(2),nxypairs,x,u,xybounds,err)
  endif

  if (ntables.GE.5) then
  !     Interpolate EOS_T_DPt

  err=EOS_OK
  call eos_Interpolate(th(5),nxypairs,x,p,t2_value(pass_cnt,:),dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(5),nxypairs,x,u,xybounds,err)
  endif
  endif

  !     Interpolate EOS_Ut_DT using rho and t_value

  err=EOS_OK
  call eos_Interpolate(th(3),nxypairs,x,t_value(pass_cnt,:),e2_value(pass_cnt,:),dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(3),nxypairs,x,t_value(pass_cnt,:),xybounds,err)
  endif

  !     Prepare for next interpolation loop

  pass_cnt = pass_cnt + 1
  preinvert = .FALSE.
  if (MOD(pass_cnt,2) .EQ. 0) preinvert = .TRUE.

  !     Continue next interpolation loop if necessary

  if (pass_cnt .LE. pass_tot) goto 10


  !     ADDENDUM: Interpolate EOS_Ut_DT using rho and Flag's t

  err=EOS_OK
  call eos_Interpolate(th(3),nxypairs,x,t, &
       e2_Flag,dfx,dfy,err)
  call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED,err,equal)
  if (equal) then
     err=EOS_OK
     call eos_CheckExtrap(th(3),nxypairs,x,t,xybounds,err)
  endif



  !     Print tabular interpolated data

  do i=1,5
     labels_in(i) = adjustl(trim(labels(i))//'_in')
     labels_in(i) = adjustr(labels_in(i))
  enddo

  labels_mod(3) = adjustl(trim(type_str(1)))
  labels_mod(3) = adjustr(labels_mod(3))
  labels_mod(4) = adjustl(trim(type_str(4)))
  labels_mod(4) = adjustr(labels_mod(4))
  labels_mod(5) = adjustl(trim(type_str(2)))
  labels_mod(5) = adjustr(labels_mod(5))

  labels_mod(6) = adjustl(trim(type_str(1))//'(preinvt)')
  labels_mod(6) = adjustr(labels_mod(6))
  labels_mod(7) = adjustl(trim(type_str(4))//'(preinvt)')
  labels_mod(7) = adjustr(labels_mod(7))
  labels_mod(8) = adjustl(trim(type_str(2))//'(preinvt)')
  labels_mod(8) = adjustr(labels_mod(8))

  labels_mod(9) = trim(adjustl(type_str(3)))//'(rho,T_preinvt)'
  labels_mod(9) = adjustr(labels_mod(9))

  do j=3,1,-2

     do i=1,num
        units(3) = 'MJ/kg'
        units(3) = adjustr(units(3))
        units(4) = 'GPa'
        units(4) = adjustr(units(4))
        x(i) = x_in(i)
        t(i) = t_in(i)
        p(i) = p_in(i) / p_convert
        u(i) = u_in(i) / u_convert
     enddo

     if (ntables.GE.5) then
        write(*,998) &
             '# UNITS:                  ', &
             (units(i), i=2,5), &
             units(4), units(4), &
             units(3), units(3), &
             units(5), units(5), &
             units(3), units(3), &
             units(5), units(5)
        write(*,998) &
             (labels_in(i), i=1,5), &
             labels_mod(3), labels_mod(6), &
             labels_mod(4), labels_mod(7), &
             labels_mod(5), labels_mod(8), &
             'e(rho,T)', 'e(rho,T_preinvt)', &
             'EOS_T_DPt(rho,P_in)', 'EOS_T_DPt(rho,P_in)','_preinvt              '
     else
        write(*,998) &
             '# UNITS:                  ', &
             (units(i), i=2,5), &
             units(4), units(4), &
             units(3), units(3), &
             units(5), units(5), &
             units(3), units(3)
        write(*,998) &
             (labels_in(i), i=1,5), &
             labels_mod(3), labels_mod(6), &
             labels_mod(4), labels_mod(7), &
             labels_mod(5), labels_mod(8), &
             'e(rho,T)', 'e(rho,T_preinvt)'
     endif
     do i=1,num
        if (ntables.GE.5) then
           write(*,999) &
                time(i), x(i), u(i), p(i), t(i), & ! input data
                p_value(j,i),  p_value(j+1,i), &   ! P(rho,u_in)    with & without EOS_INVERT_AT_SETUP
                e_value(j,i),  e_value(j+1,i), &   ! u(rho,p_value) with & without EOS_INVERT_AT_SETUP
                t_value(j,i),  t_value(j+1,i), &   ! t(rho,u_in)    with & without EOS_INVERT_AT_SETUP
                e2_value(j,i), e2_value(j+1,i), &  ! u(rho,t_value) with & without EOS_INVERT_AT_SETUP
                t2_value(j,i), t2_value(j+1,i)     ! t(rho,p_in)    with & without EOS_INVERT_AT_SETUP
        else
           write(*,999) &
                time(i), x(i), u(i), p(i), t(i), & ! input data
                p_value(j,i),  p_value(j+1,i), &   ! P(rho,u_in)    with & without EOS_INVERT_AT_SETUP
                e_value(j,i),  e_value(j+1,i), &   ! u(rho,p_value) with & without EOS_INVERT_AT_SETUP
                t_value(j,i),  t_value(j+1,i), &   ! t(rho,u_in)    with & without EOS_INVERT_AT_SETUP
                e2_value(j,i), e2_value(j+1,i)     ! u(rho,t_value) with & without EOS_INVERT_AT_SETUP
        endif
     enddo

     write(*,*) ' '
     write(*,*) ' '

  enddo

  call eos_DestroyAll(err)

996 format(5a22)
997 format(1x,a15,4a18)
998 format(20a22)
999 format(1p,20e22.14)

end program test
