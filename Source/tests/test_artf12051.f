c*********************************************************************
c     Test program
c     ----------------------------------------------------------
c     Filetype: (SOURCE)
c     
c     Copyright -- see file named COPYRIGHTNOTICE
c     
c********************************************************************

c> \file
c> \ingroup Fortran77 tests
c> \brief Perform the following tests:
c>   -# Verify resolution of issue artf12051.
c>      See SourceForge<A9> for more details:
c>      https://tf.lanl.gov/sf/go/artf12051
c>
c> \note
c> MATIDS TO TEST: 3717
      program standalone

      implicit none

      integer i, j, icount, itabletype
      integer eoshandle, err1, err2, fh, ioerr, ierr, num

      parameter(num=5972)
      parameter(fh=7)

      integer xybounds(num), xybound

      real*8 x(num), y(num), value(num), dfy(num), dfx(num)
      real*8 sval(num)
      real*8 diff, denom

      logical equal
      
      include 'eos_Interface.fi'

      itabletype = EOS_Pt_DUt

      ioerr = 0
      open(UNIT=fh, FILE='test_artf12051.inp', IOSTAT=ioerr)
      if (ioerr.ne.0) then
         write(*,*) 'open failed!'
         stop
      endif
      do i=1,num
         read(fh,*) x(i),y(i)
      enddo
      do j=3717,3717

         call eos_CreateTables(1,itabletype,j,eoshandle,err1)
         call eos_SetOption(eoshandle,EOS_Y_CONVERT,1.d-02,err1)
         call eos_SetOption(eoshandle,EOS_F_CONVERT,1.d-02,err1)
         call eos_LoadTables(1, eoshandle,err1)

         err1=EOS_OK
         call eos_Interpolate(eoshandle,num,x,y,value,dfx,dfy,err1)
         call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED, err1, equal)
         if (equal) then
            ierr=EOS_OK
            call eos_CheckExtrap(eoshandle,num,x,y,xybounds,ierr)
         endif

         icount=0
         write(*,'(a6,5a23,2a8)') 'i','x(i)','y(i)',
     &        'value(i)','sval(i)','diff','extrap1','extrap2'
         do i=1,num
            err2=EOS_OK
            xybound=EOS_OK
            call eos_Interpolate(eoshandle,1,x(i),y(i),
     &           sval(i),dfx,dfy,err2)
            call eos_ErrorCodesEqual(EOS_INTERP_EXTRAPOLATED, err2,
     &                               equal)
            if (equal) then
               ierr=EOS_OK
               call eos_CheckExtrap(eoshandle,1,x(i),y(i),xybound,ierr)
            endif
            denom = value(i)
            if (denom.eq.0.) denom = 1.0
            diff  = abs(value(i)-sval(i))/denom
            if(diff.gt.1.0e-8
     &           .or. xybound.ne.EOS_OK .or. xybounds(i).ne.EOS_OK) then
               write(*,'(i6,4f23.15,e23.15,2i8)') i,x(i),y(i),
     &              value(i),sval(i),diff,xybounds(i),xybound
               icount=icount+1
            endif
         enddo
      enddo
      print *," icount=",icount

      call eos_DestroyAll (err1);

      end
