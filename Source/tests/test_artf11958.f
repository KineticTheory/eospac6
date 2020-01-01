c*********************************************************************
c  Test program
c  ----------------------------------------------------------
c  Filetype: (SOURCE)
c  
c  Copyright -- see file named COPYRIGHTNOTICE
c
c********************************************************************

c> \file
c> \ingroup Fortran77 tests
c> \brief Perform the following tests:
c>   -# Verify resolution of issue artf11958.
c>      See SourceForge<A9> Issue #artf11958 for more details:
c>      https://tf.lanl.gov/sf/go/artf11958
c>
c> \note
c> MATIDS TO TEST: 7760
      program standalone
      integer eoshandle, err1,err2
      real*8 x(4),y(4),value(4),dfy(4),dfx(4),sval(4)
      include 'eos_Interface.fi'
      itabletype = EOS_Pt_DUt
      do j=7760,7760

         call eos_CreateTables(1,itabletype,j,eoshandle,err1)
         call eos_SetOption(eoshandle,EOS_F_Covert,1.d-02,err1)
         call eos_LoadTables(1, eoshandle,err1)

         print *, 'First interpolation results for material ', j
         do i=1,4
            x(i)=5.66200168764060d+0
            y(i)=.4208405516769770627d+0
         enddo
         err1=0
         call eos_interpolate(eoshandle,4,x,y,value,dfx,dfy,err1)
         if(err1.ne.0)then
            print *,'err1=',err1
         endif
         do i=1,4
            write(*,'(a,i2,a,f20.15,a,f20.15,a,f20.15)')
     &           " i=",i," x=",x(i)," y=",y(i)," result=",value(i)
         enddo
         print *,' '

         print *, 'Second interpolation results for material ', j
         x(3)=4.07777804740058158d+0
         y(3)=6.726888235292128074d-08
         err2=0
         call eos_interpolate(eoshandle,4,x,y,sval,dfx,dfy,err2)
         if(err2.ne.0)then
            print *,'err2=',err2
         endif
         do i=1,4
            write(*,'(a,i2,a,f20.15,a,f20.15,a,f20.15)')
     &           " i=",i," x=",x(i)," y=",y(i),"   sval=",sval(i)
         enddo
      enddo

      call eos_DestroyAll (err1);

      end
