c*********************************************************************
c  Test program
c  ----------------------------------------------------------
c  Filetype: (SOURCE)
c  
c  Copyright -- see file named COPYRIGHTNOTICE
c
c********************************************************************

c> \file
c>  \ingroup Fortran77 tests
c>  \brief Verify that EOS_V_PtT is available via the eos_Interface F77 header.
c>
c> \note
c> MATIDS TO TEST: 2140
      program test_artf6976

      implicit none

      include "eos_Interface.fi"

      integer lentrim

      integer nTables, nXYPairs
      parameter (nTables = 1)
      parameter (nXYPairs = 16)

      integer i, j
      double precision X(nXYPairs), Y(nXYPairs), F(nXYPairs),
     &     dFx(nXYPairs), dFy(nXYPairs)
      integer tableType(nTables), numIndVars(nTables)
      integer matID(nTables)
      integer tableHandle(nTables)
      integer errorCode
      integer tableHandleErrorCode
      character*11 tableTypeLabel(nTables)
      character*(EOS_MaxErrMsgLen) errorMessage

      tableTypeLabel(1) = "EOS_V_PtT"

      tableType(1) = EOS_V_PtT

      numIndVars(1) = 2

      matID(1) = 2140

      errorCode = EOS_OK
      do i=1, nTables
         tableHandle(i) = 0
      enddo

c
c     initialize table data objects
c
      call eos_CreateTables ( nTables, tableType, matID, tableHandle,
     &     errorCode)
      if (errorCode.NE.EOS_OK) then
         do i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode ( tableHandle(i),
     &           tableHandleErrorCode )
            call eos_GetErrorMessage ( tableHandleErrorCode,
     &           errorMessage )
            write(*,998) 'eos_CreateTables ERROR ',
     &           tableHandleErrorCode, ': ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         enddo
      endif

c
c     set some options
c
      do i=1, nTables
c     enable data dump
         call eos_SetOption ( tableHandle(i), EOS_DUMP_DATA,
     &        EOS_NullVal, errorCode )
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            write(*,998) 'eos_SetOption ERROR ', errorCode, ': ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         endif
c     enable custom smoothing
         call eos_SetOption ( tableHandle(i), EOS_PT_SMOOTHING,
     &        EOS_NullVal, errorCode )
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            write(*,998) 'eos_SetOption ERROR ', errorCode, ': ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         endif
      enddo

c
c     load data into table data objects
c
      call eos_LoadTables ( nTables, tableHandle, errorCode)
      if (errorCode.NE.EOS_OK) then
         call eos_GetErrorMessage ( errorCode, errorMessage )
         write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ',
     &        errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         do i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode ( tableHandle(i),
     &           tableHandleErrorCode )
            call eos_GetErrorMessage ( tableHandleErrorCode,
     &           errorMessage )
            write(*,994) 'eos_LoadTables ERROR ', tableHandleErrorCode,
     &           ' (TH=', tableHandle(i), '): ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         enddo
      endif

c
c     interpolate -- errors codes are intentionally produced
c
      X(1) = 1.000000000000001d-16
      X(2) = 5.263157894736736d+04
      X(3) = 1.052631578947347d+05
      X(4) = 1.578947368421021d+05
      X(5) = 1.000000000000001d-16
      X(6) = 5.263157894736736d+04
      X(7) = 1.052631578947347d+05
      X(8) = 1.578947368421021d+05
      X(9) = 1.000000000000001d-16
      X(10) = 5.263157894736736d+04
      X(11) = 1.052631578947347d+05
      X(12) = 1.578947368421021d+05
      X(13) = 1.000000000000001d-16
      X(14) = 5.263157894736736d+04
      X(15) = 1.052631578947347d+05
      X(16) = 1.578947368421021d+05

      Y(1) = 1.160450500000000d+02
      Y(2) = 1.160450500000000d+02
      Y(3) = 1.160450500000000d+02
      Y(4) = 1.160450500000000d+02
      Y(5) = 6.107635309900391d+08
      Y(6) = 6.107635309900391d+08
      Y(7) = 6.107635309900391d+08
      Y(8) = 6.107635309900391d+08
      Y(9) = 1.221526945935028d+09
      Y(10) = 1.221526945935028d+09
      Y(11) = 1.221526945935028d+09
      Y(12) = 1.221526945935028d+09
      Y(13) = 1.832290360880018d+09
      Y(14) = 1.832290360880018d+09
      Y(15) = 1.832290360880018d+09
      Y(16) = 1.832290360880018d+09

      do i=1, nTables

c     enable custom interpolation
         call eos_SetOption ( tableHandle(i), EOS_USE_CUSTOM_INTERP,
     &        EOS_NullVal, errorCode )
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            write(*,998) 'eos_SetOption ERROR ', errorCode, ': ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         endif

         write(*,*) ' '
         write(*,997) '--- Interpolate using tableType ',
     &        tableTypeLabel(i),' ---'
         call eos_Interpolate ( tableHandle(i), nXYPairs, X, Y, F,
     &        dFx, dFy, errorCode)
         write(*,997) tableTypeLabel(i), ' Interpolation Results:'
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            write(*,994) 'eos_Interpolate ERROR ', errorCode,
     &           ' (TH=', tableHandle(i), '): ',
     &           errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
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

c
c     Destroy all data objects
c
      call eos_DestroyAll (errorCode)
      if (errorCode.NE.EOS_OK) then
         do i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode ( tableHandle(i),
     &           tableHandleErrorCode )
            call eos_GetErrorMessage ( tableHandleErrorCode,
     &           errorMessage )
            write(*,998) 'eos_DestroyAll ERROR ', tableHandleErrorCode,
     &           ': ', errorMessage(1:(lentrim(errorMessage,
     &           EOS_MaxErrMsgLen)-1))
         enddo
      endif

 994  format (a,i4,a,i1,2a)
 996  format ('    i=',i2,'    X =',1pe23.15,
     &     ', F =',1pe23.15,', dFx =',1pe23.15,', errorCode: ',i4)
 997  format (a,:,a,:,2(a,:,i2))
 998  format (a,i4,2a)
 999  format ('    i=',i2,'    X =',1pe23.15,', Y =',1pe23.15,
     &     ', F =',1pe23.15,', errorCode: ',i4)

      end

      integer function lentrim(s, slen)
      integer i, slen
      character*(*) s
      do i=slen,1,-1
         if (s(i:i) .NE. " ") then
            lentrim=i
            return
         endif
      enddo
      lentrim=i
      return
      end
