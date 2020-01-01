!*********************************************************************
!  Common parameter statements to set constants used by EOSPAC
!  ------------------------------------------------------------
!  Filetype: (HEADER)
!  
!  Copyright -- see file named COPYRIGHTNOTICE
!  
!********************************************************************
!
! Define constants used by EOSPAC
!

! define BOOLEAN values:
logical, parameter :: EOS_FALSE=.false., EOS_TRUE=.true.

! define NULL REAL pointer variable for passing
! into public function parameters:
real(EOS_REAL), parameter :: EOS_NullVal = 0.0_EOS_REAL, EOS_NullPtr = 0.0_EOS_REAL

!    table types:

integer(EOS_INTEGER), parameter :: EOS_NullTable  = 0_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Comment    = 1_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Info       = 2_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pt_DT      = 3_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_PtT      = 4_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DPt      = 5_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pt_DUt     = 6_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pt_DAt     = 7_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pt_DSt     = 8_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ut_DT      = 12_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DUt      = 14_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ut_DPt     = 15_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ut_DAt     = 16_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ut_DSt     = 17_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ut_PtT     = 18_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_At_DT      = 21_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DAt      = 23_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_At_DPt     = 24_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_At_DUt     = 25_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_At_DSt     = 26_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_St_DT      = 30_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DSt      = 32_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_St_DPt     = 33_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_St_DUt     = 34_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_St_DAt     = 35_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pic_DT     = 39_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DPic     = 41_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pic_DUic   = 42_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pic_DAic   = 43_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pic_DSic   = 44_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uic_DT     = 48_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DUic     = 50_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uic_DPic   = 51_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uic_DAic   = 52_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uic_DSic   = 53_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aic_DT     = 57_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DAic     = 59_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aic_DPic   = 60_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aic_DUic   = 61_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aic_DSic   = 62_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Sic_DT     = 66_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DSic     = 68_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Sic_DPic   = 69_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Sic_DUic   = 70_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Sic_DAic   = 71_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pe_DT      = 75_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DPe      = 77_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pe_DUe     = 78_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pe_DAe     = 79_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pe_DSe     = 80_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ue_DT      = 84_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DUe      = 86_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ue_DPe     = 87_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ue_DAe     = 88_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ue_DSe     = 89_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ae_DT      = 93_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DAe      = 95_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ae_DPe     = 96_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ae_DUe     = 97_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ae_DSe     = 98_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Se_DT      = 102_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DSe      = 104_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Se_DPe     = 105_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Se_DUe     = 106_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Se_DAe     = 107_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Piz_DT     = 111_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DPiz     = 113_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Piz_DUiz   = 114_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Piz_DAiz   = 115_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Piz_DSiz   = 116_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uiz_DT     = 120_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DUiz     = 122_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uiz_DPiz   = 123_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uiz_DAiz   = 124_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uiz_DSiz   = 125_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aiz_DT     = 129_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DAiz     = 131_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aiz_DPiz   = 132_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aiz_DUiz   = 133_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Aiz_DSiz   = 134_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Siz_DT     = 138_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_DSiz     = 140_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Siz_DPiz   = 141_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Siz_DUiz   = 142_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Siz_DAiz   = 143_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pc_D       = 147_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uc_D       = 151_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ac_D       = 155_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_T       = 159_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Pv       = 160_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Dv      = 161_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Dls     = 162_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Uv      = 163_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Uls     = 164_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Av      = 165_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pv_Als     = 166_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_T       = 167_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Dv       = 168_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Pv      = 169_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Dls     = 170_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Uv      = 171_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Uls     = 172_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Av      = 173_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dv_Als     = 174_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_T      = 175_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Dls      = 176_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Pv     = 177_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Dv     = 178_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Uv     = 179_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Uls    = 180_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Av     = 181_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dls_Als    = 182_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_T       = 183_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Uv       = 184_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Pv      = 185_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Dv      = 186_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Dls     = 187_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Uls     = 188_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Av      = 189_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uv_Als     = 190_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_T      = 191_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Uls      = 192_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Pv     = 193_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Dv     = 194_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Dls    = 195_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Uv     = 196_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Av     = 197_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uls_Als    = 198_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_T       = 199_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Av       = 200_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Pv      = 201_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Dv      = 202_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Dls     = 203_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Uv      = 204_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Uls     = 205_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Av_Als     = 206_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_T      = 207_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Als      = 208_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Pv     = 209_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Dv     = 210_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Dls    = 211_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Uv     = 212_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Uls    = 213_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Als_Av     = 214_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tm_D       = 215_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Tm       = 216_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tm_Pm      = 217_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tm_Um      = 218_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tm_Am      = 219_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pm_D       = 220_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Pm       = 221_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pm_Tm      = 222_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pm_Um      = 223_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pm_Am      = 224_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Um_D       = 225_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Um       = 226_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Um_Tm      = 227_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Um_Pm      = 228_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Um_Am      = 229_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Am_D       = 230_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Am       = 231_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Am_Tm      = 232_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Am_Pm      = 233_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Am_Um      = 234_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tf_D       = 235_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Tf       = 236_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tf_Pf      = 237_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tf_Uf      = 238_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tf_Af      = 239_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pf_D       = 240_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Pf       = 241_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pf_Tf      = 242_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pf_Uf      = 243_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pf_Af      = 244_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uf_D       = 245_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Uf       = 246_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uf_Tf      = 247_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uf_Pf      = 248_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Uf_Af      = 249_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Af_D       = 250_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Af       = 251_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Af_Tf      = 252_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Af_Pf      = 253_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Af_Uf      = 254_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Gs_D       = 255_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_D_Gs       = 256_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ogb        = 257_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Kr_DT      = 258_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Keo_DT     = 261_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Zfo_DT     = 264_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Kp_DT      = 267_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Zfc_DT     = 270_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Kec_DT     = 273_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Ktc_DT     = 276_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_B_DT       = 279_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Kc_DT      = 282_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_V_PtT      = 285_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_M_DT       = 305_EOS_INTEGER

! Table setup and interpolation option constants

integer(EOS_INTEGER), parameter :: EOS_NUM_TABLE_OPTIONS  = 28_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MIN_OPTION_FLAG_VALUE  = 1000_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_DUMP_DATA  = 1000_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_APPEND_DATA = 1001_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INSERT_DATA  = 1002_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MONOTONIC_IN_X  = 1003_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MONOTONIC_IN_Y  = 1004_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SMOOTH  = 1005_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SPLIT_COWAN  = 1006_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SPLIT_FORCED  = 1007_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SPLIT_IDEAL_GAS  = 1008_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SPLIT_NUM_PROP  = 1009_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_CHECK_ARGS  = 1010_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_LINEAR  = 1011_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_RATIONAL  = 1012_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_X_CONVERT  = 1013_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Y_CONVERT  = 1014_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_F_CONVERT  = 1015_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_PT_SMOOTHING  = 1016_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_ADJUST_VAP_PRES  = 1017_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_USE_CUSTOM_INTERP  = 1018_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SAVE_SPECIES_DATA = 1019
integer(EOS_INTEGER), parameter :: EOS_CALC_FREE_ENERGY = 1020
integer(EOS_INTEGER), parameter :: EOS_CREATE_TZERO = 1021
integer(EOS_INTEGER), parameter :: EOS_USE_TAYLOR_FIT = 1022
integer(EOS_INTEGER), parameter :: EOS_USE_MAXWELL_TABLE = 1023
integer(EOS_INTEGER), parameter :: EOS_DISCONTINUOUS_DERIVATIVES = 1024
integer(EOS_INTEGER), parameter :: EOS_XY_PASSTHRU = 1025
integer(EOS_INTEGER), parameter :: EOS_XY_MODIFY = 1026
integer(EOS_INTEGER), parameter :: EOS_INVERT_AT_SETUP = 1027

! Data information constants

integer(EOS_INTEGER), parameter :: EOS_NUM_INFO_CONSTANTS  = 40_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Cmnt_Len            = 1_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Exchange_Coeff      = 2_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_F_Convert_Factor    = EOS_F_CONVERT
integer(EOS_INTEGER), parameter :: EOS_Log_Val             = 4_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_ID         = 5_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Mean_Atomic_Mass    = 6_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Mean_Atomic_Num     = 7_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Modulus             = 8_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Normal_Density      = 9_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Table_Type          = 10_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_X_Convert_Factor    = EOS_X_CONVERT
integer(EOS_INTEGER), parameter :: EOS_Y_Convert_Factor    = EOS_Y_CONVERT
integer(EOS_INTEGER), parameter :: EOS_MaxErrMsgLen        = 1024_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_R_Array             = 11_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T_Array             = 12_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_F_Array             = 13_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NR                  = 14_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NT                  = 15_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Rmin                = 16_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Rmax                = 17_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tmin                = 18_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Tmax                = 19_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Fmin                = 20_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Fmax                = 21_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NT401               = 22_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_P401                = 23_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_T401                = 24_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_RG401               = 25_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_RL401               = 26_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_EG401               = 27_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_EL401               = 28_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_AG401               = 29_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_AL401               = 30_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NUM_PHASES          = 31_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_X_Species_Data      = 32_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Y_Species_Data      = 33_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_F_Species_Data      = 34_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_dFx_Species_Data    = 35_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_dFy_Species_Data    = 36_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_nXYPairs            = 37_EOS_INTEGER

! Internal meta data information constants
! No additional constants are currently needed here, because only table type constants are currently valid
integer(EOS_INTEGER), parameter :: EOS_NUM_META_DATA_CONSTANTS = 0_EOS_INTEGER

integer(EOS_INTEGER), parameter :: EOS_META_DATA_STRLEN = 4096_EOS_INTEGER

! Internal meta data category constants
integer(EOS_INTEGER), parameter :: EOS_NUM_META_DATA_CATEGORIES       = 7_EOS_INTEGER
!integer(EOS_INTEGER), parameter :: EOS_Table_Type ! defined above
integer(EOS_INTEGER), parameter :: EOS_Table_Name                     = 3001_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Dependent_Var                  = 3002_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Independent_Var1               = 3003_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Independent_Var2               = 3004_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Sesame_Table_List              = 3005_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Pressure_Balance_Table_Type    = 3006_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Temperature_Balance_Table_Type = 3007_EOS_INTEGER

! Table handle specific meta data information constants
!integer(EOS_INTEGER), parameter :: EOS_Table_Name        /* defined above */
!integer(EOS_INTEGER), parameter :: EOS_Dependent_Var     /* defined above */
!integer(EOS_INTEGER), parameter :: EOS_Independent_Var1  /* defined above */
!integer(EOS_INTEGER), parameter :: EOS_Independent_Var2  /* defined above */
integer(EOS_INTEGER), parameter :: EOS_NUM_TABLE_META_DATA_CONSTANTS  = 9_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_File_Name                      = 4001_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Name                  = 4002_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Source                = 4003_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Date                  = 4004_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Ref                   = 4005_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Composition           = 4006_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Codes                 = 4007_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Phases                = 4008_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_Material_Classification        = 4009_EOS_INTEGER

! Error code constants

integer(EOS_INTEGER), parameter :: EOS_OK  = 0_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MIN_ERROR_CODE_VALUE = 2001_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_BAD_DERIVATIVE_FLAG  = 2001_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_BAD_INTERPOLATION_FLAG  = 2002_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_BAD_MATERIAL_ID  = 2003_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_CONVERGENCE_FAILED  = 2004_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NO_COMMENTS  = 2005_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_DATA_TYPE_NOT_FOUND  = 2006_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_FAILED  = 2007_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INTERP_EXTRAPOLATED  = 2008_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MATERIAL_NOT_FOUND  = 2009_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MEM_ALLOCATION_FAILED  = 2010_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NO_DATA_TABLE  = 2011_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NO_SESAME_FILES  = 2012_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NOT_INITIALIZED  = 2013_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_BAD_DATA_TYPE  = 2014_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_OPEN_SESAME_FILE_FAILED  = 2015_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_READ_DATA_FAILED  = 2016_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_READ_FILE_VERSION_FAILED  = 2017_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_READ_MASTER_DIR_FAILED  = 2018_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_READ_MATERIAL_DIR_FAILED  = 2019_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_READ_TOTAL_MATERIALS_FAILED  = 2020_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_TABLE_HANDLE   = 2021_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_SUBTABLE_INDEX   = 2022_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xHi_yHi    = 2023_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xHi_yOk    = 2024_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xHi_yLo    = 2025_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xOk_yLo    = 2026_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xLo_yLo    = 2027_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xLo_yOk    = 2028_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xLo_yHi    = 2029_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_xOk_yHi    = 2030_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_OPTION_FLAG = 2031_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_DATA_TYPE = 2032_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_SPLIT_FLAG = 2033_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_UNDEFINED = 2034_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_NOT_ALLOCATED = 2035_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INTEGRATION_FAILED = 2036_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_DATA_TYPE_NO_MATCH = 2037_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_INFO_FLAG = 2038_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_CONC_SUM  = 2039_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INTERP_EXTRAP_TBAL = 2040_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INTERP_EXTRAP_PBAL = 2041_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_CANT_MAKE_MONOTONIC  = 2042_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_CANT_INVERT_DATA  = 2043_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_OPEN_OUTPUT_FILE_FAILED = 2044_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_NXYPAIRS = 2045_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_GEN401_AND_NOT_FOUND = 2046_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_WARNING = 2047_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_SPLIT_FAILED = 2048_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INDEX_FILE_ERROR = 2049_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_INVALID_INFO_CATEGORY_FLAG = 2050_EOS_INTEGER
integer(EOS_INTEGER), parameter :: EOS_MAX_ERROR_CODE_VALUE = 2050_EOS_INTEGER
