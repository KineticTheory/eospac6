!*********************************************************************
!* Test Program
!* ---------------------------------------------------------
!* Filetype: (SOURCE)
!* 
!* Copyright -- see file named COPYRIGHTNOTICE
!* 
!*********************************************************************

!*  \file
!*  \ingroup tests
!*  \brief Verify the new public function, eos_GetMetaData, to return internal meta data
!*         per a user's specific request functions as advertised within a Fortran code.
!*
!*  See SourceForge issue
!*  <a href="https://tf.lanl.gov/sf/go/artf36302">artf36302</a>
!*  for details.
!*
!* \note
!* MATIDS TO TEST: none
!*

program main

  use eos_Interface2003

  integer(EOS_INTEGER), parameter :: M = 8
  integer(EOS_INTEGER), parameter :: N = 198

  integer(EOS_INTEGER), dimension(N) :: tableTypes = (/ &
       EOS_NullTable, EOS_Comment,   EOS_Info,      EOS_Pt_DT,     EOS_D_PtT, &
       EOS_T_DPt,     EOS_Pt_DUt,    EOS_Pt_DAt,    EOS_Pt_DSt,    EOS_Ut_DT, &
       EOS_T_DUt,     EOS_Ut_DPt,    EOS_Ut_DAt,    EOS_Ut_DSt,    EOS_Ut_PtT, &
       EOS_At_DT,     EOS_T_DAt,     EOS_At_DPt,    EOS_At_DUt,    EOS_At_DSt, &
       EOS_St_DT,     EOS_T_DSt,     EOS_St_DPt,    EOS_St_DUt,    EOS_St_DAt, &
       EOS_Pic_DT,    EOS_T_DPic,    EOS_Pic_DUic,  EOS_Pic_DAic,  EOS_Pic_DSic, &
       EOS_Uic_DT,    EOS_T_DUic,    EOS_Uic_DPic,  EOS_Uic_DAic,  EOS_Uic_DSic, &
       EOS_Aic_DT,    EOS_T_DAic,    EOS_Aic_DPic,  EOS_Aic_DUic,  EOS_Aic_DSic, &
       EOS_Sic_DT,    EOS_T_DSic,    EOS_Sic_DPic,  EOS_Sic_DUic,  EOS_Sic_DAic, &
       EOS_Pe_DT,     EOS_T_DPe,     EOS_Pe_DUe,    EOS_Pe_DAe,    EOS_Pe_DSe, &
       EOS_Ue_DT,     EOS_T_DUe,     EOS_Ue_DPe,    EOS_Ue_DAe,    EOS_Ue_DSe, &
       EOS_Ae_DT,     EOS_T_DAe,     EOS_Ae_DPe,    EOS_Ae_DUe,    EOS_Ae_DSe, &
       EOS_Se_DT,     EOS_T_DSe,     EOS_Se_DPe,    EOS_Se_DUe,    EOS_Se_DAe, &
       EOS_Piz_DT,    EOS_T_DPiz,    EOS_Piz_DUiz,  EOS_Piz_DAiz,  EOS_Piz_DSiz, &
       EOS_Uiz_DT,    EOS_T_DUiz,    EOS_Uiz_DPiz,  EOS_Uiz_DAiz,  EOS_Uiz_DSiz, &
       EOS_Aiz_DT,    EOS_T_DAiz,    EOS_Aiz_DPiz,  EOS_Aiz_DUiz,  EOS_Aiz_DSiz, &
       EOS_Siz_DT,    EOS_T_DSiz,    EOS_Siz_DPiz,  EOS_Siz_DUiz,  EOS_Siz_DAiz, &
       EOS_Pc_D,      EOS_Uc_D,      EOS_Ac_D,      EOS_Pv_T,      EOS_T_Pv, &
       EOS_Pv_Dv,     EOS_Pv_Dls,    EOS_Pv_Uv,     EOS_Pv_Uls,    EOS_Pv_Av, &
       EOS_Pv_Als,    EOS_Dv_T,      EOS_T_Dv,      EOS_Dv_Pv,     EOS_Dv_Dls, &
       EOS_Dv_Uv,     EOS_Dv_Uls,    EOS_Dv_Av,     EOS_Dv_Als,    EOS_Dls_T, &
       EOS_T_Dls,     EOS_Dls_Pv,    EOS_Dls_Dv,    EOS_Dls_Uv,    EOS_Dls_Uls, &
       EOS_Dls_Av,    EOS_Dls_Als,   EOS_Uv_T,      EOS_T_Uv,      EOS_Uv_Pv, &
       EOS_Uv_Dv,     EOS_Uv_Dls,    EOS_Uv_Uls,    EOS_Uv_Av,     EOS_Uv_Als, &
       EOS_Uls_T,     EOS_T_Uls,     EOS_Uls_Pv,    EOS_Uls_Dv,    EOS_Uls_Dls, &
       EOS_Uls_Uv,    EOS_Uls_Av,    EOS_Uls_Als,   EOS_Av_T,      EOS_T_Av, &
       EOS_Av_Pv,     EOS_Av_Dv,     EOS_Av_Dls,    EOS_Av_Uv,     EOS_Av_Uls, &
       EOS_Av_Als,    EOS_Als_T,     EOS_T_Als,     EOS_Als_Pv,    EOS_Als_Dv, &
       EOS_Als_Dls,   EOS_Als_Uv,    EOS_Als_Uls,   EOS_Als_Av,    EOS_Tm_D, &
       EOS_D_Tm,      EOS_Tm_Pm,     EOS_Tm_Um,     EOS_Tm_Am,     EOS_Pm_D, &
       EOS_D_Pm,      EOS_Pm_Tm,     EOS_Pm_Um,     EOS_Pm_Am,     EOS_Um_D, &
       EOS_D_Um,      EOS_Um_Tm,     EOS_Um_Pm,     EOS_Um_Am,     EOS_Am_D, &
       EOS_D_Am,      EOS_Am_Tm,     EOS_Am_Pm,     EOS_Am_Um,     EOS_Tf_D, &
       EOS_D_Tf,      EOS_Tf_Pf,     EOS_Tf_Uf,     EOS_Tf_Af,     EOS_Pf_D, &
       EOS_D_Pf,      EOS_Pf_Tf,     EOS_Pf_Uf,     EOS_Pf_Af,     EOS_Uf_D, &
       EOS_D_Uf,      EOS_Uf_Tf,     EOS_Uf_Pf,     EOS_Uf_Af,     EOS_Af_D, &
       EOS_D_Af,      EOS_Af_Tf,     EOS_Af_Pf,     EOS_Af_Uf,     EOS_Gs_D, &
       EOS_D_Gs,      EOS_Ogb,       EOS_Kr_DT,     EOS_Keo_DT,    EOS_Zfo_DT, &
       EOS_Kp_DT,     EOS_Zfc_DT,    EOS_Kec_DT,    EOS_Ktc_DT,    EOS_B_DT, &
       EOS_Kc_DT,     EOS_V_PtT,     EOS_M_DT &
       /)

  integer(EOS_INTEGER), dimension(M) :: infoCategories = (/ &
       EOS_Table_Type, &
       EOS_Dependent_Var, &
       EOS_Independent_Var1, &
       EOS_Independent_Var2, &
       EOS_Sesame_Table_List, &
       EOS_Pressure_Balance_Table_Type, &
       EOS_Temperature_Balance_Table_Type, &
       EOS_Table_Name &
       /)

  integer(EOS_INTEGER), dimension(M) :: L

  integer(EOS_INTEGER) :: i, j, k, infoItem, infoItemCategory
  integer(EOS_INTEGER) :: errorCode
  character(EOS_META_DATA_STRLEN) :: s

  character(256), dimension(M) :: labels = (/ &
       "Table Type         ", &
       "Dependent Var      ", &
       "Independent Var1   ", &
       "Independent Var2   ", &
       "Sesame Table List  ", &
       "Pressure Balance   ", &
       "Temperature Balance", &
       "Table Name         " &
       /)

  character(256) :: underline = ""

  L = 0

  do j=1, M

     k = len_trim(labels(j))
     L(j) = max(13, k)
     write(*,'(a,1x)',ADVANCE='NO') labels(j)(1:L(j))
     if (j > 1) then
        underline = trim(underline) // " " // repeat("-", L(j))
     else
        underline = trim(underline) // repeat("-", L(j))
     endif
  enddo
  write(*,'(/,a,1x)') trim(underline)

  do i=1, N

     errorCode = EOS_OK

     infoItem = tableTypes(i)

     do j=1, M

        s = ""
        infoItemCategory = infoCategories(j)
        call eos_GetMetaData (infoItem, infoItemCategory, s, errorCode);

        if (errorCode /= EOS_OK) stop

        k = scan(s,char(0)) ! find first NULL character
        s(k:len(s)) = ' ' ! remove NULL character and all other trailing characters
        k = max(len_trim(s),L(j)) ! find needed character length
        if (j < M) then
           write(*,'(a,1x)',ADVANCE='NO') s(1:k)
        else
           write(*,'(a,1x)') s(1:k)
        endif

     enddo

  enddo

end program main
