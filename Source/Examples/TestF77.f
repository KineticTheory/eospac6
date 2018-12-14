c*********************************************************************
c  Example F77 Program
c  ---------------------------------------------------------
c  Filetype: (SOURCE)
c
c  Copyright -- see file named COPYRIGHTNOTICE
c
c*********************************************************************

c> \file
c> \ingroup examples
c> \brief This is a simple F77 example of how to use EOSPAC6 interface.

      program TestF77

      implicit none

      include 'eos_Interface.fi'

      integer*4 nTables, nXYPairs, nInfoItems
      parameter (nTables = 5)
      parameter (nXYPairs = 4)
      parameter (nInfoItems = 12)

      integer*4 i, j
      real*8 X(nXYPairs), Y(nXYPairs), F(nXYPairs), dFx(nXYPairs),
     &         dFy(nXYPairs)
      integer*4 tableType(nTables), numIndVars(nTables)
      integer*4 matID(nTables)
      integer*4 tableHandle(nTables)
      integer*4 errorCode
      integer*4 tableHandleErrorCode
      real*8 infoVals(nInfoItems)
      integer*4 infoItems(nInfoItems)
      character*82 infoItemDescriptions(nInfoItems)
      character*20 tableTypeLabel(nTables)
      character*(EOS_MaxErrMsgLen) errorMessage
      integer k

      data infoItems /
     &     EOS_Cmnt_Len,
     &     EOS_Exchange_Coeff,
     &     EOS_F_Convert_Factor,
     &     EOS_Log_Val,
     &     EOS_Material_ID,
     &     EOS_Mean_Atomic_Mass,
     &     EOS_Mean_Atomic_Num,
     &     EOS_Modulus,
     &     EOS_Normal_Density,
     &     EOS_Table_Type,
     &     EOS_X_Convert_Factor,
     &     EOS_Y_Convert_Factor
     &     /
      data infoItemDescriptions /
     &'The length in characters of the comments available for the specif
     &ied data table',
     &'The exchange coefficient',
     &'The conversion factor corresponding to the dependent variable, F(
     &x,y)',
     &'Non-zero if the data table is in a log10 format',
     &'The SESAME material identification number',
     &'The mean atomic mass',
     &'The mean atomic number',
     &'The solid bulk modulus',
     &'The normal density',
     &'The type of data table. Corresponds to the parameters in APPENDIX
     & B and APPENDIX C',
     &'The conversion factor corresponding to the primary independent va
     &riable, x',
     &'The conversion factor corresponding to the secondary independent 
     &variable, y'
     &/
      data tableTypeLabel /
     &     'EOS_Pt_DT',
     &     'EOS_Dv_T',
     &     'EOS_Ogb',
     &     'EOS_Comment',
     &     'EOS_Info'
     &     /

      logical equal

c     EOS_Pt_DT, material 2140 works for Sesame table 301 (record type 1)
c     EOS_Dv_T, material 2140 works for Sesame table 401 (record type 2)
c     EOS_Ogb, material 12140 works for Sesame table 501 (record type 3)
c     EOS_Comment, material 2140 works for Sesame tables 101-199 (record type 4)
c     EOS_Info, material 2140 works for Sesame table 201 (record type 5)
      tableType(1) = EOS_Pt_DT
      tableType(2) = EOS_Dv_T
      tableType(3) = EOS_Ogb
      tableType(4) = EOS_Comment
      tableType(5) = EOS_Info

      numIndVars(1) = 2
      numIndVars(2) = 1
      numIndVars(3) = 0
      numIndVars(4) = 0
      numIndVars(5) = 0

      matID(1) = 2140
      matID(2) = 2140
      matID(3) = 12140
      matID(4) = 2140
      matID(5) = 2140

      errorCode = EOS_OK
      do 10 i=1, nTables
         tableHandle(i) = 0
 10   continue

c
c     initialize table data objects
c
      call eos_CreateTables ( nTables, tableType, matID,
     &                        tableHandle, errorCode)
      if (errorCode.NE.EOS_OK) then
         do 15 i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode
     &           ( tableHandle(i), tableHandleErrorCode )
            call eos_GetErrorMessage
     &           ( tableHandleErrorCode, errorMessage )
            call strLength(errorMessage, EOS_MaxErrMsgLen, k)
            write(*,998) 'eos_CreateTables ERROR ',tableHandleErrorCode,
     &                   ': ',errorMessage(1:k)
 15      continue
      endif

c
c     set some options
c
      do 20 i=1, nTables
c        enable smoothing
         call eos_SetOption ( tableHandle(i), EOS_SMOOTH,
     &                        EOS_NullVal, errorCode )
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            call strLength(errorMessage, EOS_MaxErrMsgLen, k)
            write(*,998) 'eos_SetOption ERROR ', errorCode,
     &                   ': ', errorMessage(1:k)
         endif
 20   continue

c
c     load data into table data objects
c
      call eos_LoadTables ( nTables, tableHandle, errorCode)
      if (errorCode.NE.EOS_OK) then
         call eos_GetErrorMessage ( errorCode, errorMessage )
         call strLength(errorMessage, EOS_MaxErrMsgLen, k)
         write(*,998) 'eos_LoadTables ERROR ', errorCode, ': ',
     &                errorMessage(1:k)
         do 25 i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode
     &           ( tableHandle(i), tableHandleErrorCode )
            call eos_GetErrorMessage
     &           ( tableHandleErrorCode, errorMessage )
            call strLength(errorMessage, EOS_MaxErrMsgLen, k)
            write(*,994) 'eos_LoadTables ERROR ', tableHandleErrorCode,
     &                   ' (TH=', tableHandle(i), '): ',
     &                   errorMessage(1:k)
 25      continue
      endif

c
c     interpolate -- errors codes are intentionally produced
c
      X(1) = 3000.d0
      X(2) = 6000.d0
      X(3) = 8200.d0
      X(4) = 8300.d0

      Y(1) = 20000.0d0
      Y(2) = 620000.0d0
      Y(3) = 4000000.0d0
      Y(4) = 200000000.0d0

      do 30 i=1, nTables
         write(*,*) ' '
         write(*,997) '--- Interpolate using tableType ',
     &                tableTypeLabel(i),' ---'
         call eos_Interpolate ( tableHandle(i), nXYPairs, X, Y, F,
     &                          dFx, dFy, errorCode)
         write(*,997) tableTypeLabel(i), ' Interpolation Results:'
         if (errorCode.NE.EOS_OK) then
            call eos_GetErrorMessage ( errorCode, errorMessage )
            call strLength(errorMessage, EOS_MaxErrMsgLen, k)
            write(*,994) 'eos_Interpolate ERROR ', errorCode,
     &                   ' (TH=', tableHandle(i), '): ',
     &                   errorMessage(1:k)
         else
            do 40 j=1, nXYPairs
               if (numIndVars(i).EQ.1) then
                  write(*,996) j-1,X(j),F(j),dFx(j),errorCode
               endif
               if (numIndVars(i).EQ.2) then
                 write(*,999) j-1,X(j),Y(j),F(j),dFx(j),dFy(j),errorCode
               endif
 40         continue
         endif
 30   continue

c
c     Retrieve all miscellaneous table info
c
      do 45 i=1, nTables
         write(*,*) ' '
         write(*,997) '--- Table information for tableType ',
     &        tableTypeLabel(i), ', tableHandle=', tableHandle(i),
     &        ' ---'
         do 50 j=1, nInfoItems
            call eos_GetTableInfo (tableHandle(i), 1,
     &                             infoItems(j), infoVals(j), errorCode)
            call eos_ErrorCodesEqual(EOS_INVALID_INFO_FLAG, errorCode,
     &                               equal)
            if (errorCode.EQ.EOS_OK) then
               write(*,995) j,'. ',infoItemDescriptions(j), ': ',
     &              infoVals(j)
            else if (.NOT.equal) then
c              Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
c              applicable to a specific tableHandle.
               call eos_GetErrorMessage ( errorCode, errorMessage )
               call strLength(errorMessage, EOS_MaxErrMsgLen, k)
               write(*,998) 'eos_LoadTables ERROR ', errorCode,
     &                      ': ', errorMessage(1:k)
            endif
 50      continue
 45   continue

c
c     Destroy all data objects
c
      call eos_DestroyAll (errorCode)
      if (errorCode.NE.EOS_OK) then
         do 35 i=1, nTables
            tableHandleErrorCode = EOS_OK
            call eos_GetErrorCode (
     &           tableHandle(i), tableHandleErrorCode )
            call eos_GetErrorMessage (
     &           tableHandleErrorCode, errorMessage )
            call strLength(errorMessage, EOS_MaxErrMsgLen, k)
            write(*,998) 'eos_DestroyAll ERROR ', tableHandleErrorCode,
     &                   ': ', errorMessage(1:k)
 35      continue
      endif

 994  format (a,i5,a,i1,2a)
 995  format (i2,a,a,a,f13.6)
 996  format ('    i=',i2,'    X =',1pe13.6,
     &        ', F =',1pe13.6,', dFx =',1pe13.6,', errorCode: ',i5)
 997  format (a,:,a,:,2(a,:,i2))
 998  format (a,i5,2a)
 999  format ('    i=',i2,'    X =',1pe13.6,', Y =',1pe13.6,
     &        ', F =',1pe13.6,', dFx =',1pe13.6,', dFy =',
     &        1pe13.6,', errorCode: ',i5)

      end

      subroutine strLength(str, length, trimmedLength)
      integer i, length, trimmedLength
      character*(*) str
      trimmedLength = 0
      do 5 i=length, 1, -1
         if (trimmedLength.EQ.0 .AND. str(i:i).NE.' ' .AND.
     &       str(i:i).NE.char(0)) then
         	trimmedLength = i
         endif
 5    continue
      end
