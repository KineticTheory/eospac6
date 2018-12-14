!*********************************************************************
!  Test program
!  ----------------------------------------------------------
!  Filetype: (SOURCE)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************

!> @file
!! @ingroup tests quick
!! @brief Ensure the extrapolation errors are set appropriately for EOS and Opacity
!!        tables as expected. See SourceForge© Issue #artf28105 for more details:
!!        https://tf.lanl.gov/sf/go/artf28105
!!
!! \note
!! MATIDS TO TEST: 5030 18221
program toy
  use eos_Interface2003

  integer itabletype, ideos(2), errorcode,ITABLEHANDLE(40)
  integer ntables, counter, kitems
  integer kinfo(10)
  real*8 dinfovals(10), Tmin_Table, Dmin_Table, Tmax_Table, Dmax_Table
  real*8 x(10), y(10), dFx(10), dFy(10), fvals(10)
  integer num


  character(EOS_MaxErrMsgLen) :: errorMessage

  integer i

  ITABLEHANDLE = huge(i)

  ntables = 1
  counter = 0
  ideos(1) = 5030
  ideos(2) = 18221 !this is an opacity id

  !  EOS_Pt_DT
  itabletype = EOS_Pt_DT
  counter = counter +1
  call eos_CreateTables(ntables,itabletype,ideos(1), ITABLEHANDLE(counter), ERRORCODE)
  write (*,997) "eos_CreateTables error code",errorcode,"EOS_Pt_DT",ideos(1)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     print *, errormessage(1:(len_trim(errorMessage)-1))
  endif

  !  EOS_Kr_DT
  itabletype = EOS_Kr_DT
  counter = counter +1
  call eos_CreateTables(ntables,itabletype,ideos(2), ITABLEHANDLE(counter), ERRORCODE)
  write (*,997) "eos_CreateTables error code",errorcode,"EOS_Kr_DT",ideos(2)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     print *, errormessage(1:(len_trim(errorMessage)-1))
  endif

  !  EOS_Kp_DT
  itabletype = EOS_Kp_DT
  counter = counter +1
  call eos_CreateTables(ntables,itabletype,ideos(2), ITABLEHANDLE(counter), ERRORCODE)
  write (*,997) "eos_CreateTables error code",errorcode,"EOS_Kp_DT",ideos(2)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     print *, errormessage(1:(len_trim(errorMessage)-1))
  endif

  !  EOS_Zfo_DT
  itabletype = EOS_Zfo_DT
  counter = counter +1
  call eos_CreateTables(ntables,itabletype,ideos(2), ITABLEHANDLE(counter), ERRORCODE)
  write (*,997) "eos_CreateTables error code",errorcode,"EOS_Zfo_DT",ideos(2)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     print *, errormessage(1:(len_trim(errorMessage)-1))
  endif

  ntables = counter

  call eos_LoadTables(ntables,ITABLEHANDLE,errorcode)
  write (*,997) "eos_LoadTables error code",errorcode
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     print *, errormessage(1:(len_trim(errorMessage)-1))
  endif

  kitems = 4
  kinfo(1) = EOS_Tmin
  kinfo(2) = EOS_Rmin
  kinfo(3) = EOS_Tmax
  kinfo(4) = EOS_Rmax

  !   EOS_Pt_DT
  call eos_GetTableinfo(ITABLEHANDLE(1),kitems,kinfo,dinfovals, errorcode)
  write (*,998) "dinfovals",dinfovals(1:kitems)
  Dmax_Table = dinfovals(4)
  Tmax_Table = dinfovals(3)
  Dmin_Table = dinfovals(2)
  Tmin_Table = dinfovals(1)
  num = 1
  x(1) = Dmin_Table
  y(1) = Tmin_Table - .1
  call eos_interpolate(ITABLEHANDLE(1),num,x,y,FVALS, dFx,dFy,errorcode)
  write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
  endif
  x(1) = Dmin_Table
  y(1) = Tmax_Table + .1
  call eos_interpolate(ITABLEHANDLE(1),num,x,y,FVALS, dFx,dFy,errorcode)
  write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
  if (errorcode .NE. EOS_OK) then
     call eos_GetErrorMessage (errorcode, errorMessage)
     write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
  endif

  do i=2,4
     !   EOS_Kr_DT
     !   EOS_Kp_DT
     !   EOS_Zfo_DT
     call eos_GetTableinfo(ITABLEHANDLE(i),kitems,kinfo,dinfovals, errorcode)
     write (*,998) "dinfovals",10.0_EOS_REAL**dinfovals(1:kitems)
     Dmax_Table = 10.0_EOS_REAL**(dinfovals(4))
     Tmax_Table = 10.0_EOS_REAL**(dinfovals(3))
     Dmin_Table = 10.0_EOS_REAL**(dinfovals(2))
     Tmin_Table = 10.0_EOS_REAL**(dinfovals(1))
     x(1) = Dmin_Table
     y(1) = Tmin_Table
     call eos_interpolate(ITABLEHANDLE(i),num,x,y,FVALS, dFx,dFy,errorcode)
     write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
     if (errorcode .NE. EOS_OK) then
        call eos_GetErrorMessage (errorcode, errorMessage)
        write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
     endif
     y(1) = Tmax_Table
     call eos_interpolate(ITABLEHANDLE(i),num,x,y,FVALS, dFx,dFy,errorcode)
     write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
     if (errorcode .NE. EOS_OK) then
        call eos_GetErrorMessage (errorcode, errorMessage)
        write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
     endif
     y(1) = Tmin_Table - .1
     call eos_interpolate(ITABLEHANDLE(i),num,x,y,FVALS, dFx,dFy,errorcode)
     write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
     if (errorcode .NE. EOS_OK) then
        call eos_GetErrorMessage (errorcode, errorMessage)
        write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
     endif
     y(1) = Tmax_Table + .1
     call eos_interpolate(ITABLEHANDLE(i),num,x,y,FVALS, dFx,dFy,errorcode)
     write (*,999) "FVALS",FVALS(1), "errorcode",errorcode,"X",x(1),"Y",y(1)
     if (errorcode .NE. EOS_OK) then
        call eos_GetErrorMessage (errorcode, errorMessage)
        write (*,'(a)') errormessage(1:(len_trim(errorMessage)-1))
     endif

  enddo

997 format(a,'=',i4,2x,a,'=',i6)
998 format(/,a,'=',10(1pe20.12))
999 format(a,'=',1pe20.12,2x,a,'=',i6,2(2x,a,'=',1pe20.12))

end program toy
