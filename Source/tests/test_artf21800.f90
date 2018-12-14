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
!! @brief Ensure the new eos_SetDataFileName and eos_GetMaxDataFileNameLength
!!        work as expected. See SourceForge© Issue #artf21800 for more details:
!!        https://tf.lanl.gov/sf/go/artf21800
!!
!! \note
!! MATIDS TO TEST: 2030 2110 2140 2145 2160 2290 2291 2292 2293 2330 2360 2370 2441 2448 2460 2550 2551 2552
!! MATIDS TO TEST: 2680 2700 2720 2740 2810 2860 2880 2961 2962 2963 2980 2981 2982 2983 2984 3050 3070 3100
!! MATIDS TO TEST: 3101 3120 3140 3180 3200 3210 3280 3332 3333 3334 3336 3510 3660 3713 3715 3716 3717 3718
!! MATIDS TO TEST: 3719 3720 3730 3810 3811 3830 3950 4100 4210 4270 4271 4272 5000 5001 5010 5011 5030 5040
!! MATIDS TO TEST: 5171 5172 5180 5181 5190 5191 5210 5211 5212 5250 5251 5263 5265 5266 5280 5300 5410 5411
!! MATIDS TO TEST: 5500 5501 5502 5520 5530 5531 5540 5550 5560 5760 5761 5762 5770 5780 5781 6013 6020 7010
!! MATIDS TO TEST: 7030 7100 7102 7111 7112 7120 7121 7122 7130 7150 7152 7153 7154 7155 7160 7171 7180 7190
!! MATIDS TO TEST: 7230 7252 7270 7271 7281 7282 7283 7330 7331 7380 7381 7383 7385 7386 7387 7390 7391 7410
!! MATIDS TO TEST: 7411 7432 7440 7450 7460 7470 7510 7520 7521 7530 7541 7542 7550 7560 7561 7570 7580 7590
!! MATIDS TO TEST: 7591 7592 7593 7601 7602 7603 7660 7740 7741 7750 7760 7761 7770 7771 7830 7831 7832 7833
!! MATIDS TO TEST: 7834 7930 7931 7940 7941 7970 7971 7980 7981 8020
program testsetfile
  use eos_Interface
  real(EOS_REAL) ::  comment_string_length
  integer(EOS_INTEGER) ::  err1
  integer(EOS_INTEGER) ::  eoshandle
  integer(EOS_INTEGER) :: itabletype
  integer(EOS_INTEGER), dimension (1:190) :: ideos
  integer(EOS_INTEGER) ::  ntables
  integer(EOS_INTEGER) :: one
  integer(EOS_INTEGER) :: info_item
  character*4096 filename, filename2, fn
  character(EOS_MaxErrMsgLen) :: errorMessage

  integer indx1

  logical lexist

  itabletype = -20

  ! Here are the list of material ids that are on
  ! file /usr/projects/data/eos/sesame

  ideos(1) = 2030
  ideos(2) = 2110
  ideos(3) = 2140
  ideos(4) = 2145
  ideos(5) = 2160
  ideos(6) = 2290
  ideos(7) = 2291
  ideos(8) = 2292
  ideos(9) = 2293
  ideos(10) = 2330
  ideos(11) = 2360
  ideos(12) = 2370
  ideos(13) = 2441
  ideos(14) = 2448
  ideos(15) = 2460
  ideos(16) = 2550
  ideos(17) = 2551
  ideos(18) = 2552
  ideos(19) = 2680
  ideos(20) = 2700
  ideos(21) = 2720
  ideos(22) = 2740
  ideos(23) = 2810
  ideos(24) = 2860
  ideos(25) = 2880
  ideos(26) = 2961
  ideos(27) = 2962
  ideos(28) = 2963
  ideos(29) = 2980
  ideos(30) = 2981
  ideos(31) = 2982
  ideos(32) = 2983
  ideos(33) = 2984
  ideos(34) = 3050
  ideos(35) = 3070
  ideos(36) = 3100
  ideos(37) = 3101
  ideos(38) = 3120
  ideos(39) = 3140
  ideos(40) = 3180
  ideos(41) = 3200
  ideos(42) = 3210
  ideos(43) = 3280
  ideos(44) = 3332
  ideos(45) = 3333
  ideos(46) = 3334
  ideos(47) = 3336
  ideos(48) = 3510
  ideos(49) = 3660
  ideos(50) = 3713
  ideos(51) = 3715
  ideos(52) = 3716
  ideos(53) = 3717
  ideos(54) = 3718
  ideos(55) = 3719
  ideos(56) = 3720
  ideos(57) = 3730
  ideos(58) = 3810
  ideos(59) = 3811
  ideos(60) = 3830
  ideos(61) = 3950
  ideos(62) = 4100
  ideos(63) = 4210
  ideos(64) = 4270
  ideos(65) = 4271
  ideos(66) = 4272
  ideos(67) = 5000
  ideos(68) = 5001
  ideos(69) = 5010
  ideos(70) = 5011
  ideos(71) = 5030
  ideos(72) = 5040
  ideos(73) = 5171
  ideos(74) = 5172
  ideos(75) = 5180
  ideos(76) = 5181
  ideos(77) = 5190
  ideos(78) = 5191
  ideos(79) = 5210
  ideos(80) = 5211
  ideos(81) = 5212
  ideos(82) = 5250
  ideos(83) = 5251
  ideos(84) = 5263
  ideos(85) = 5265
  ideos(86) = 5266
  ideos(87) = 5280
  ideos(88) = 5300
  ideos(89) = 5410
  ideos(90) = 5411
  ideos(91) = 5500
  ideos(92) = 5501
  ideos(93) = 5502
  ideos(94) = 5520
  ideos(95) = 5530
  ideos(96) = 5531
  ideos(97) = 5540
  ideos(98) = 5550
  ideos(99) = 5560
  ideos(100) = 5760
  ideos(101) = 5761
  ideos(102) = 5762
  ideos(103) = 5770
  ideos(104) = 5780
  ideos(105) = 5781
  ideos(106) = 6013
  ideos(107) = 6020
  ideos(108) = 7010
  ideos(109) = 7030
  ideos(110) = 7100
  ideos(111) = 7102
  ideos(112) = 7111
  ideos(113) = 7112
  ideos(114) = 7120
  ideos(115) = 7121
  ideos(116) = 7122
  ideos(117) = 7130
  ideos(118) = 7150
  ideos(119) = 7152
  ideos(120) = 7153
  ideos(121) = 7154
  ideos(122) = 7155
  ideos(123) = 7160
  ideos(124) = 7171
  ideos(125) = 7180
  ideos(126) = 7190
  ideos(127) = 7230
  ideos(128) = 7252
  ideos(129) = 7270
  ideos(130) = 7271
  ideos(131) = 7281
  ideos(132) = 7282
  ideos(133) = 7283
  ideos(134) = 7330
  ideos(135) = 7331
  ideos(136) = 7380
  ideos(137) = 7381
  ideos(138) = 7383
  ideos(139) = 7385
  ideos(140) = 7386
  ideos(141) = 7387
  ideos(142) = 7390
  ideos(143) = 7391
  ideos(144) = 7410
  ideos(145) = 7411
  ideos(146) = 7432
  ideos(147) = 7440
  ideos(148) = 7450
  ideos(149) = 7460
  ideos(150) = 7470
  ideos(151) = 7510
  ideos(152) = 7520
  ideos(153) = 7521
  ideos(154) = 7530
  ideos(155) = 7541
  ideos(156) = 7542
  ideos(157) = 7550
  ideos(158) = 7560
  ideos(159) = 7561
  ideos(160) = 7570
  ideos(161) = 7580
  ideos(162) = 7590
  ideos(163) = 7591
  ideos(164) = 7592
  ideos(165) = 7593
  ideos(166) = 7601
  ideos(167) = 7602
  ideos(168) = 7603
  ideos(169) = 7660
  ideos(170) = 7740
  ideos(171) = 7741
  ideos(172) = 7750
  ideos(173) = 7760
  ideos(174) = 7761
  ideos(175) = 7770
  ideos(176) = 7771
  ideos(177) = 7830
  ideos(178) = 7831
  ideos(179) = 7832
  ideos(180) = 7833
  ideos(181) = 7834
  ideos(182) = 7930
  ideos(183) = 7931
  ideos(184) = 7940
  ideos(185) = 7941
  ideos(186) = 7970
  ideos(187) = 7971
  ideos(188) = 7980
  ideos(189) = 7981
  ideos(190) = 8020

  print*,"-----------------------------------------"

  filename = "/usr/projects/data/eos/sesame"
  inquire(FILE=trim(fileName), EXIST=lexist)
  if (.NOT.lexist) then
     filename = "/opt/local/codes/data/eos/sesame"
     inquire(FILE=trim(fileName), EXIST=lexist)
     if (.NOT.lexist) then
        print*,"ERROR: no sesame file found"
        stop
     endif
  endif
  filename2 = "/usr/projects/data/eos/export-controlled/ieee64/sesame"
  inquire(FILE=trim(fileName2), EXIST=lexist)
  if (.NOT.lexist) then
     filename2 = "/opt/local/codes/data/eos/export-controlled/ieee64/sesame"
     inquire(FILE=trim(fileName), EXIST=lexist)
     if (.NOT.lexist) then
        print*,"ERROR: no sesame file found"
        stop
     endif
  endif

  do indx1 =1,190

     matid = ideos(indx1)

     if (matid > 9999) stop

     print *,"     MATERIAL ID=", ideos(indx1)

     eoshandle = -20
     ntables = 1

     itabletype = EOS_Comment
     fn = filename

     call eos_CreateTables(ntables,itabletype,matid,eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER create tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     call eos_SetDataFileName(eoshandle, matid, itabletype, fn, err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER call TO SetDataFilename errorcode=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(fn)

     call eos_LoadTables(ntables, eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER load tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     one = 1
     info_item = EOS_Cmnt_Len
     write(*,'(a,i6)') " info_item=",info_item
     call eos_GetTableInfo(eoshandle,one,info_item, comment_string_length,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER getinfo tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     call eos_GetTableInfo(eoshandle,one,info_item, comment_string_length,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER getinfo tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     write(*,'(a,i6)') " comment_string_length=",int(comment_string_length)

     itabletype = EOS_T_DUt
     call eos_CreateTables(ntables,itabletype,matid,eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER create tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     call eos_SetDataFileName(eoshandle, matid, itabletype, fn, err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a,i6,a)') " AFTER SetDataFilename err1=",err1," itabletype=",itabletype," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(fn)

     call eos_LoadTables(ntables, eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER load tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     fn = filename2

     itabletype = EOS_Pt_DUt
     call eos_CreateTables(ntables,itabletype,matid,eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER create tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     call eos_SetDataFileName(eoshandle, matid, itabletype, fn, err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a,i6,a)') " AFTER SetDataFilename err1=",err1," itabletype=",itabletype," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(fn)

     call eos_LoadTables(ntables, eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER load tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     itabletype = EOS_Ut_DT
     call eos_CreateTables(ntables,itabletype,matid,eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER create tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     call eos_SetDataFileName(eoshandle, matid, itabletype, fn, err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i6,a,i6,a)') " AFTER SetDataFilename err1=",err1," itabletype=",itabletype," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))
     write(*,'(a,a)') "               fcmp_ignore filename = ", trim(fn)

     call eos_LoadTables(ntables, eoshandle,err1)
     if(err1.ne.0)then
        call eos_GetErrorMessage(err1, errorMessage)
     else
        errormessage = ""
     endif
     write(*,'(a,i8,a)') " AFTER load tables err1=",err1," : " &
          // errorMessage(1:(len_trim(errorMessage)-1))

     print*,"-----------------------------------------"

  enddo

end program testsetfile
