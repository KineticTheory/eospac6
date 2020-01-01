/*********************************************************************
 * Class Name : eos_Interface
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef EOSPAC6_EOS_INTERFACE
#define EOSPAC6_EOS_INTERFACE

#include "eos_universal_types.h"
#include "ses_defines.h"

/* The following will supersede the appropriate macro definitions of eos_wrappers */
#if defined(eos_SetDataFileName)
#undef eos_SetDataFileName
#endif
#if defined(HAVE_CONFIG_H)
#define eos_SetDataFileName FC_FUNC(eos_setdatafilename_cwrapper,EOS_SETDATAFILENAME_CWRAPPER)
#else
#define eos_SetDataFileName eos_SetDataFileName_Cwrapper
#endif

/************************************************************************
 * 
 * Data members
 * 
 ************************************************************************/

/* define NULL static REAL pointer const variable for passing
 * into public function parameters: */
static const EOS_REAL EOS_NullVal = (EOS_REAL) 0;
static const EOS_REAL *const EOS_NullPtr = &EOS_NullVal;

/* table types: */

static const EOS_INTEGER EOS_NullTable = 0;     /* null table */
static const EOS_INTEGER EOS_Comment = 1;       /* Descriptive Comments */
static const EOS_INTEGER EOS_Info = 2;          /* Atomic Number, Atomic Mass, Normal Density, Solid Bulk Modulus, Exchange Coefficient */
static const EOS_INTEGER EOS_Pt_DT = 3;         /* Total Pressure (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_D_PtT = 4;         /* Density (Total Pressure- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DPt = 5;         /* Temperature (Density- and Total Pressure-dependent) */
static const EOS_INTEGER EOS_Pt_DUt = 6;        /* Total Pressure (Density- and Total Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pt_DAt = 7;        /* Total Pressure (Density- and Total Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pt_DSt = 8;        /* Total Pressure (Density- and Total Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Ut_DT = 12;        /* Total Specific-Internal-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DUt = 14;        /* Temperature (Density- and Total Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Ut_DPt = 15;       /* Total Specific-Internal-Energy (Density- and Total Pressure-dependent) */
static const EOS_INTEGER EOS_Ut_DAt = 16;       /* Total Specific-Internal-Energy (Density- and Total Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Ut_DSt = 17;       /* Total Specific-Internal-Energy (Density- and Total Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Ut_PtT = 18;       /* Total Specific-Internal-Energy (Total Pressure- and Temperature-dependent) */
static const EOS_INTEGER EOS_At_DT = 21;        /* Total Specific-Free-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DAt = 23;        /* Temperature (Density- and Total Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_At_DPt = 24;       /* Total Specific-Free-Energy (Density- and Total Pressure-dependent) */
static const EOS_INTEGER EOS_At_DUt = 25;       /* Total Specific-Free-Energy (Density- and Total Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_At_DSt = 26;       /* Total Specific-Free-Energy (Density- and Total Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_St_DT = 30;        /* Total Specific-Entropy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DSt = 32;        /* Temperature (Density- and Total Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_St_DPt = 33;       /* Total Specific-Entropy (Density- and Total Pressure-dependent) */
static const EOS_INTEGER EOS_St_DUt = 34;       /* Total Specific-Entropy (Density- and Total Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_St_DAt = 35;       /* Total Specific-Entropy (Density- and Total Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pic_DT = 39;       /* Ion Pressure plus Cold Curve Pressure (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DPic = 41;       /* Temperature (Density- and Ion Pressure plus Cold Curve Pressure-dependent) */
static const EOS_INTEGER EOS_Pic_DUic = 42;     /* Ion Pressure plus Cold Curve Pressure (Density- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pic_DAic = 43;     /* Ion Pressure plus Cold Curve Pressure (Density- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pic_DSic = 44;     /* Ion Pressure plus Cold Curve Pressure (Density- and Ion Pressure plus Cold Curve Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Uic_DT = 48;       /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DUic = 50;       /* Temperature (Density- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uic_DPic = 51;     /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (Density- and Ion Pressure plus Cold Curve Pressure-dependent) */
static const EOS_INTEGER EOS_Uic_DAic = 52;     /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (Density- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uic_DSic = 53;     /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (Density- and Ion Pressure plus Cold Curve Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Aic_DT = 57;       /* Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DAic = 59;       /* Temperature (Density- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Aic_DPic = 60;     /* Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (Density- and Ion Pressure plus Cold Curve Pressure-dependent) */
static const EOS_INTEGER EOS_Aic_DUic = 61;     /* Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (Density- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Aic_DSic = 62;     /* Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (Density- and Ion Pressure plus Cold Curve Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Sic_DT = 66;       /* Ion Pressure plus Cold Curve Specific-Entropy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DSic = 68;       /* Temperature (Density- and Ion Pressure plus Cold Curve Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Sic_DPic = 69;     /* Ion Pressure plus Cold Curve Specific-Entropy (Density- and Ion Pressure plus Cold Curve Pressure-dependent) */
static const EOS_INTEGER EOS_Sic_DUic = 70;     /* Ion Pressure plus Cold Curve Specific-Entropy (Density- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Sic_DAic = 71;     /* Ion Pressure plus Cold Curve Specific-Entropy (Density- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pe_DT = 75;        /* Electron Pressure (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DPe = 77;        /* Temperature (Density- and Electron Pressure-dependent) */
static const EOS_INTEGER EOS_Pe_DUe = 78;       /* Electron Pressure (Density- and Electron Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pe_DAe = 79;       /* Electron Pressure (Density- and Electron Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pe_DSe = 80;       /* Electron Pressure (Density- and Electron Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Ue_DT = 84;        /* Electron Specific-Internal-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DUe = 86;        /* Temperature (Density- and Electron Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Ue_DPe = 87;       /* Electron Specific-Internal-Energy (Density- and Electron Pressure-dependent) */
static const EOS_INTEGER EOS_Ue_DAe = 88;       /* Electron Specific-Internal-Energy (Density- and Electron Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Ue_DSe = 89;       /* Electron Specific-Internal-Energy (Density- and Electron Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Ae_DT = 93;        /* Electron Specific-Free-Energy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DAe = 95;        /* Temperature (Density- and Electron Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Ae_DPe = 96;       /* Electron Specific-Free-Energy (Density- and Electron Pressure-dependent) */
static const EOS_INTEGER EOS_Ae_DUe = 97;       /* Electron Specific-Free-Energy (Density- and Electron Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Ae_DSe = 98;       /* Electron Specific-Free-Energy (Density- and Electron Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Se_DT = 102;       /* Electron Specific-Entropy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DSe = 104;       /* Temperature (Density- and Electron Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Se_DPe = 105;      /* Electron Specific-Entropy (Density- and Electron Pressure-dependent) */
static const EOS_INTEGER EOS_Se_DUe = 106;      /* Electron Specific-Entropy (Density- and Electron Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Se_DAe = 107;      /* Electron Specific-Entropy (Density- and Electron Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Piz_DT = 111;      /* Ion Pressure Including Zero Point (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DPiz = 113;      /* Temperature (Density- and Ion Pressure Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Piz_DUiz = 114;    /* Ion Pressure Including Zero Point (Density- and Ion Specific-Internal-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Piz_DAiz = 115;    /* Ion Pressure Including Zero Point (Density- and Ion Specific-Free-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Piz_DSiz = 116;    /* Ion Pressure Including Zero Point (Density- and Ion Pressure Including Zero Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Uiz_DT = 120;      /* Ion Specific-Internal-Energy Including Zero Point (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DUiz = 122;      /* Temperature (Density- and Ion Specific-Internal-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Uiz_DPiz = 123;    /* Ion Specific-Internal-Energy Including Zero Point (Density- and Ion Pressure Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Uiz_DAiz = 124;    /* Ion Specific-Internal-Energy Including Zero Point (Density- and Ion Specific-Free-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Uiz_DSiz = 125;    /* Ion Specific-Internal-Energy Including Zero Point (Density- and Ion Pressure Including Zero Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Aiz_DT = 129;      /* Ion Specific-Free-Energy Including Zero Point (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DAiz = 131;      /* Temperature (Density- and Ion Specific-Free-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Aiz_DPiz = 132;    /* Ion Specific-Free-Energy Including Zero Point (Density- and Ion Pressure Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Aiz_DUiz = 133;    /* Ion Specific-Free-Energy Including Zero Point (Density- and Ion Specific-Internal-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Aiz_DSiz = 134;    /* Ion Specific-Free-Energy Including Zero Point (Density- and Ion Pressure Including Zero Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Siz_DT = 138;      /* Ion Pressure Including Zero Specific-Entropy (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_T_DSiz = 140;      /* Temperature (Density- and Ion Pressure Including Zero Specific-Entropy-dependent) */
static const EOS_INTEGER EOS_Siz_DPiz = 141;    /* Ion Pressure Including Zero Specific-Entropy (Density- and Ion Pressure Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Siz_DUiz = 142;    /* Ion Pressure Including Zero Specific-Entropy (Density- and Ion Specific-Internal-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Siz_DAiz = 143;    /* Ion Pressure Including Zero Specific-Entropy (Density- and Ion Specific-Free-Energy Including Zero Point-dependent) */
static const EOS_INTEGER EOS_Pc_D = 147;        /* Pressure Cold Curve (Density-dependent) */
static const EOS_INTEGER EOS_Uc_D = 151;        /* Specific-Internal-Energy Cold Curve (Density-dependent) */
static const EOS_INTEGER EOS_Ac_D = 155;        /* Specific-Free-Energy Cold Curve (Density-dependent) */
static const EOS_INTEGER EOS_Pv_T = 159;        /* Vapor Pressure (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Pv = 160;        /* Temperature (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Pv_Dv = 161;       /* Vapor Pressure (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Pv_Dls = 162;      /* Vapor Pressure (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Pv_Uv = 163;       /* Vapor Pressure (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pv_Uls = 164;      /* Vapor Pressure (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pv_Av = 165;       /* Vapor Pressure (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pv_Als = 166;      /* Vapor Pressure (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Dv_T = 167;        /* Vapor Density on coexistence line (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Dv = 168;        /* Temperature (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Dv_Pv = 169;       /* Vapor Density on coexistence line (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Dv_Dls = 170;      /* Vapor Density on coexistence line (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Dv_Uv = 171;       /* Vapor Density on coexistence line (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Dv_Uls = 172;      /* Vapor Density on coexistence line (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Dv_Av = 173;       /* Vapor Density on coexistence line (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Dv_Als = 174;      /* Vapor Density on coexistence line (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Dls_T = 175;       /* Liquid or Solid Density on coexistence line (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Dls = 176;       /* Temperature (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Dls_Pv = 177;      /* Liquid or Solid Density on coexistence line (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Dls_Dv = 178;      /* Liquid or Solid Density on coexistence line (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Dls_Uv = 179;      /* Liquid or Solid Density on coexistence line (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Dls_Uls = 180;     /* Liquid or Solid Density on coexistence line (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Dls_Av = 181;      /* Liquid or Solid Density on coexistence line (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Dls_Als = 182;     /* Liquid or Solid Density on coexistence line (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uv_T = 183;        /* Vapor Specific-Internal-Energy (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Uv = 184;        /* Temperature (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uv_Pv = 185;       /* Vapor Specific-Internal-Energy (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Uv_Dv = 186;       /* Vapor Specific-Internal-Energy (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Uv_Dls = 187;      /* Vapor Specific-Internal-Energy (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Uv_Uls = 188;      /* Vapor Specific-Internal-Energy (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uv_Av = 189;       /* Vapor Specific-Internal-Energy (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uv_Als = 190;      /* Vapor Specific-Internal-Energy (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uls_T = 191;       /* Liquid or Solid Specific-Internal-Energy (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Uls = 192;       /* Temperature (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uls_Pv = 193;      /* Liquid or Solid Specific-Internal-Energy (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Uls_Dv = 194;      /* Liquid or Solid Specific-Internal-Energy (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Uls_Dls = 195;     /* Liquid or Solid Specific-Internal-Energy (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Uls_Uv = 196;      /* Liquid or Solid Specific-Internal-Energy (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uls_Av = 197;      /* Liquid or Solid Specific-Internal-Energy (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uls_Als = 198;     /* Liquid or Solid Specific-Internal-Energy (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Av_T = 199;        /* Vapor Specific-Free-Energy (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Av = 200;        /* Temperature (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Av_Pv = 201;       /* Vapor Specific-Free-Energy (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Av_Dv = 202;       /* Vapor Specific-Free-Energy (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Av_Dls = 203;      /* Vapor Specific-Free-Energy (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Av_Uv = 204;       /* Vapor Specific-Free-Energy (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Av_Uls = 205;      /* Vapor Specific-Free-Energy (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Av_Als = 206;      /* Vapor Specific-Free-Energy (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Als_T = 207;       /* Liquid or Solid Specific-Free-Energy (Temperature-dependent) */
static const EOS_INTEGER EOS_T_Als = 208;       /* Temperature (Liquid or Solid Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Als_Pv = 209;      /* Liquid or Solid Specific-Free-Energy (Vapor Pressure-dependent) */
static const EOS_INTEGER EOS_Als_Dv = 210;      /* Liquid or Solid Specific-Free-Energy (Vapor Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Als_Dls = 211;     /* Liquid or Solid Specific-Free-Energy (Liquid or Solid Density on coexistence line-dependent) */
static const EOS_INTEGER EOS_Als_Uv = 212;      /* Liquid or Solid Specific-Free-Energy (Vapor Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Als_Uls = 213;     /* Liquid or Solid Specific-Free-Energy (Liquid or Solid Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Als_Av = 214;      /* Liquid or Solid Specific-Free-Energy (Vapor Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Tm_D = 215;        /* Melt Temperature (Density-dependent) */
static const EOS_INTEGER EOS_D_Tm = 216;        /* Density (Melt Temperature-dependent) */
static const EOS_INTEGER EOS_Tm_Pm = 217;       /* Melt Temperature (Melt Pressure-dependent) */
static const EOS_INTEGER EOS_Tm_Um = 218;       /* Melt Temperature (Melt Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Tm_Am = 219;       /* Melt Temperature (Melt Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pm_D = 220;        /* Melt Pressure (Density-dependent) */
static const EOS_INTEGER EOS_D_Pm = 221;        /* Density (Melt Pressure-dependent) */
static const EOS_INTEGER EOS_Pm_Tm = 222;       /* Melt Pressure (Melt Temperature-dependent) */
static const EOS_INTEGER EOS_Pm_Um = 223;       /* Melt Pressure (Melt Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pm_Am = 224;       /* Melt Pressure (Melt Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Um_D = 225;        /* Melt Specific-Internal-Energy (Density-dependent) */
static const EOS_INTEGER EOS_D_Um = 226;        /* Density (Melt Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Um_Tm = 227;       /* Melt Specific-Internal-Energy (Melt Temperature-dependent) */
static const EOS_INTEGER EOS_Um_Pm = 228;       /* Melt Specific-Internal-Energy (Melt Pressure-dependent) */
static const EOS_INTEGER EOS_Um_Am = 229;       /* Melt Specific-Internal-Energy (Melt Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Am_D = 230;        /* Melt Specific-Free-Energy (Density-dependent) */
static const EOS_INTEGER EOS_D_Am = 231;        /* Density (Melt Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Am_Tm = 232;       /* Melt Specific-Free-Energy (Melt Temperature-dependent) */
static const EOS_INTEGER EOS_Am_Pm = 233;       /* Melt Specific-Free-Energy (Melt Pressure-dependent) */
static const EOS_INTEGER EOS_Am_Um = 234;       /* Melt Specific-Free-Energy (Melt Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Tf_D = 235;        /* Freeze Temperature (Density-dependent) */
static const EOS_INTEGER EOS_D_Tf = 236;        /* Density (Freeze Temperature-dependent) */
static const EOS_INTEGER EOS_Tf_Pf = 237;       /* Freeze Temperature (Freeze Pressure-dependent) */
static const EOS_INTEGER EOS_Tf_Uf = 238;       /* Freeze Temperature (Freeze Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Tf_Af = 239;       /* Freeze Temperature (Freeze Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Pf_D = 240;        /* Freeze Pressure (Density-dependent) */
static const EOS_INTEGER EOS_D_Pf = 241;        /* Density (Freeze Pressure-dependent) */
static const EOS_INTEGER EOS_Pf_Tf = 242;       /* Freeze Pressure (Freeze Temperature-dependent) */
static const EOS_INTEGER EOS_Pf_Uf = 243;       /* Freeze Pressure (Freeze Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Pf_Af = 244;       /* Freeze Pressure (Freeze Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Uf_D = 245;        /* Freeze Specific-Internal-Energy (Density-dependent) */
static const EOS_INTEGER EOS_D_Uf = 246;        /* Density (Freeze Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Uf_Tf = 247;       /* Freeze Specific-Internal-Energy (Freeze Temperature-dependent) */
static const EOS_INTEGER EOS_Uf_Pf = 248;       /* Freeze Specific-Internal-Energy (Freeze Pressure-dependent) */
static const EOS_INTEGER EOS_Uf_Af = 249;       /* Freeze Specific-Internal-Energy (Freeze Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Af_D = 250;        /* Freeze Specific-Free-Energy (Density-dependent) */
static const EOS_INTEGER EOS_D_Af = 251;        /* Density (Freeze Specific-Free-Energy-dependent) */
static const EOS_INTEGER EOS_Af_Tf = 252;       /* Freeze Specific-Free-Energy (Freeze Temperature-dependent) */
static const EOS_INTEGER EOS_Af_Pf = 253;       /* Freeze Specific-Free-Energy (Freeze Pressure-dependent) */
static const EOS_INTEGER EOS_Af_Uf = 254;       /* Freeze Specific-Free-Energy (Freeze Specific-Internal-Energy-dependent) */
static const EOS_INTEGER EOS_Gs_D = 255;        /* Shear Modulus (Density-dependent) */
static const EOS_INTEGER EOS_D_Gs = 256;        /* Density (Shear Modulus-dependent) */
static const EOS_INTEGER EOS_Ogb = 257;         /* Calculated versus Interpolated Opacity Grid Boundary */
static const EOS_INTEGER EOS_Kr_DT = 258;       /* Rosseland Mean Opacity (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Keo_DT = 261;      /* Electron Conductive Opacity (Opacity Model) (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Zfo_DT = 264;      /* Mean Ion Charge (Opacity Model) (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Kp_DT = 267;       /* Planck Mean Opacity (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Zfc_DT = 270;      /* Mean Ion Charge (Conductivity Model) (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Kec_DT = 273;      /* Electrical Conductivity (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Ktc_DT = 276;      /* Thermal Conductivity (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_B_DT = 279;        /* Thermoelectric Coefficient (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_Kc_DT = 282;       /* Electron Conductive Opacity (Conductivity Model) (Density- and Temperature-dependent) */
static const EOS_INTEGER EOS_V_PtT = 285;       /* Specific-Volume (Pressure- and Temperature-dependent) */
static const EOS_INTEGER EOS_M_DT = 305;        /* Mass Fraction (Density- and Temperature-dependent) */

/* Table setup and interpolation option constants */
#define EOS_NUM_TABLE_OPTIONS    28     /* Total number of public option flags */
#define EOS_MIN_OPTION_FLAG_VALUE    1000       /* Minimum option flag value */
static const EOS_INTEGER EOS_DUMP_DATA = 1000;  /* Write the loaded data table to a file. */
static const EOS_INTEGER EOS_APPEND_DATA = 1001;        /* Write the loaded data table to a file. (append) */
static const EOS_INTEGER EOS_INSERT_DATA = 1002;        /* Insert grid points between each original grid point with respect to all independent variables (i.e., increase grid resolution). The value of eos_SetOption parameter, tableOptionVal, is to contain the user-defined number of data points to insert between existing data points */
static const EOS_INTEGER EOS_MONOTONIC_IN_X = 1003;     /* Enable forced monotonicity with respect to x of F(x,y) */
static const EOS_INTEGER EOS_MONOTONIC_IN_Y = 1004;     /* Enable forced monotonicity with respect to y of F(x,y) */
static const EOS_INTEGER EOS_SMOOTH = 1005;     /* Enable data table smoothing that imposes a linear floor on temperature dependence, forces linear temperature dependence for low temp, and forces linear density dependence for low and high dens. */
static const EOS_INTEGER EOS_SPLIT_COWAN = 1006;        /* Allows splitting for ion-electron data table not found in the database using the cold curve plus Cowan-nuclear model for ions */
static const EOS_INTEGER EOS_SPLIT_FORCED = 1007;       /* Forces specified splitting option for data table */
static const EOS_INTEGER EOS_SPLIT_IDEAL_GAS = 1008;    /* Allows splitting for ion-electron data table not found in the database using the cold curve plus ideal gas model for ions */
static const EOS_INTEGER EOS_SPLIT_NUM_PROP = 1009;     /* Allows splitting for ion-electron data table not found in the database using the cold curve plus number-proportional model for ions */
static const EOS_INTEGER EOS_CHECK_ARGS = 1010; /* Turn on extensive argument checking, default is off. */
static const EOS_INTEGER EOS_LINEAR = 1011;     /* Bilinear (4-point) interpolation */
static const EOS_INTEGER EOS_RATIONAL = 1012;   /* Birational (12-point) interpolation */
static const EOS_INTEGER EOS_X_CONVERT = 1013;  /* Set the conversion factor used on the xVals independent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
static const EOS_INTEGER EOS_Y_CONVERT = 1014;  /* Set the conversion factor used on the yVals independent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
static const EOS_INTEGER EOS_F_CONVERT = 1015;  /* Set the conversion factor used on the fVals dependent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
static const EOS_INTEGER EOS_PT_SMOOTHING = 1016;       /* User-specific smoothing */
static const EOS_INTEGER EOS_ADJUST_VAP_PRES = 1017;    /* correct the initial low density, used in conjunction with EOS_PT_SMOOTHING */
static const EOS_INTEGER EOS_USE_CUSTOM_INTERP = 1018;  /* Use a custom inverse-interpolation algorithm that requires the setup option, EOS_PT_SMOOTHING, to be enabled for the specified table handle. This option is only valid for table types EOS_Ut_PtT and EOS_V_PtT */
static const EOS_INTEGER EOS_SAVE_SPECIES_DATA = 1019;  /* Enable the storage of species-specific data during EOS mixing */
static const EOS_INTEGER EOS_CALC_FREE_ENERGY = 1020;  /* Enforce the calculation of the free-energy table instead of loading it from file */
static const EOS_INTEGER EOS_CREATE_TZERO = 1021;  /* Using linear extrapolation along each isochore , create a T=0 isotherm if it's unavailable when loading 300-series Sesame data */
static const EOS_INTEGER EOS_USE_TAYLOR_FIT = 1022;  /* Use Taylor polynomial fit data instead of standard tabulated data */
static const EOS_INTEGER EOS_USE_MAXWELL_TABLE = 1023;  /* Use the Maxwell data in table 311 instead of the corresponding table 301 */
static const EOS_INTEGER EOS_DISCONTINUOUS_DERIVATIVES = 1024;  /* Enable the original linear/bilinear logic, which calculates discontinuous derivatives at the tabulated grid. This option requires the interpolation option, EOS_LINEAR, to be enabled for the specified table handle. */
static const EOS_INTEGER EOS_XY_PASSTHRU = 1025;  /* Neither create an internal copy nor modify the xVals and yVals inputs for eos_Interpolate, eos_Mix and eos_CheckExtrap. Use host code's arrays directly -- unmodified. Overrides previously-set EOS_XY_MODIFY option. */
static const EOS_INTEGER EOS_XY_MODIFY = 1026;  /* Do not create an internal copy of the xVals and yVals inputs for eos_Interpolate, eos_Mix and eos_CheckExtrap. Modify the xVals and yVals inputs in situ -- use host code's arrays directly. Overrides previously-set EOS_XY_PASSTHRU option. */
static const EOS_INTEGER EOS_INVERT_AT_SETUP = 1027; /* Table(s) are inverted at setup, which means inverse interpolation is avoided later */

/* Data information constants */
#define EOS_NUM_INFO_CONSTANTS =  40
static const EOS_INTEGER EOS_Cmnt_Len = 1;      /* The length in characters of the comments available for the specified data table */
static const EOS_INTEGER EOS_Exchange_Coeff = 2;        /* The exchange coefficient */
static const EOS_INTEGER EOS_F_Convert_Factor = 1015;   /* The conversion factor corresponding to the dependent variable, F(x,y) -- THIS MUST BE SAME AS EOS_F_CONVERT */
static const EOS_INTEGER EOS_Log_Val = 4;       /* Non-zero if the data table is in a log10 format */
static const EOS_INTEGER EOS_Material_ID = 5;   /* The SESAME material identification number */
static const EOS_INTEGER EOS_Mean_Atomic_Mass = 6;      /* The mean atomic mass */
static const EOS_INTEGER EOS_Mean_Atomic_Num = 7;       /* The mean atomic number */
static const EOS_INTEGER EOS_Modulus = 8;       /* The solid bulk modulus */
static const EOS_INTEGER EOS_Normal_Density = 9;        /* The normal density */
static const EOS_INTEGER EOS_Table_Type = 10;   /* The type of data table. Corresponds to the parameters in APPENDIX B and APPENDIX C */
static const EOS_INTEGER EOS_X_Convert_Factor = 1013;   /* The conversion factor corresponding to the primary independent variable, x -- THIS MUST BE SAME AS EOS_X_CONVERT */
static const EOS_INTEGER EOS_Y_Convert_Factor = 1014;   /* The conversion factor corresponding to the secondary independent variable, y -- THIS MUST BE SAME AS EOS_Y_CONVERT */
#define EOS_MaxErrMsgLen                         1024   /* The maximum length of an error message to be returned to the host code */
static const EOS_INTEGER EOS_R_Array = 11;      /* The density array */
static const EOS_INTEGER EOS_T_Array = 12;      /* The temperature array */
static const EOS_INTEGER EOS_F_Array = 13;      /* The F array */
static const EOS_INTEGER EOS_NR = 14;   /* The number of densities */
static const EOS_INTEGER EOS_NT = 15;   /* The number of temperatures */
static const EOS_INTEGER EOS_Rmin = 16; /* The minimum density */
static const EOS_INTEGER EOS_Rmax = 17; /* The maximum density */
static const EOS_INTEGER EOS_Tmin = 18; /* The minimum temperature */
static const EOS_INTEGER EOS_Tmax = 19; /* The maximum temperature */
static const EOS_INTEGER EOS_Fmin = 20; /* The minimum F value */
static const EOS_INTEGER EOS_Fmax = 21; /* The maximum F value */
static const EOS_INTEGER EOS_NT401 = 22;        /* The number of temperatures in Sesame 401 table */
static const EOS_INTEGER EOS_P401 = 23; /* The pressure array in Sesame 401 table */
static const EOS_INTEGER EOS_T401 = 24; /* The temperature array in Sesame 401 table  */
static const EOS_INTEGER EOS_RG401 = 25;        /* The RG array in Sesame 401 table  */
static const EOS_INTEGER EOS_RL401 = 26;        /* The RL array in Sesame 401 table  */
static const EOS_INTEGER EOS_EG401 = 27;        /* The EG array in Sesame 401 table  */
static const EOS_INTEGER EOS_EL401 = 28;        /* The EL array in Sesame 401 table  */
static const EOS_INTEGER EOS_AG401 = 29;        /* The AG array in Sesame 401 table  */
static const EOS_INTEGER EOS_AL401 = 30;        /* The AL array in Sesame 401 table  */
static const EOS_INTEGER EOS_NUM_PHASES = 31;   /* The number of material phases */
static const EOS_INTEGER EOS_X_Species_Data = 32; /* The species-specific X values calculated by the mixing algorithm */
static const EOS_INTEGER EOS_Y_Species_Data = 33; /* The species-specific Y values calculated by the mixing algorithm */
static const EOS_INTEGER EOS_F_Species_Data = 34; /* The species-specific F values calculated by the mixing algorithm */
static const EOS_INTEGER EOS_dFx_Species_Data = 35;   /* The species-specific dF/dX values calculated by the mixing algorithm */
static const EOS_INTEGER EOS_dFy_Species_Data = 36;   /* The species-specific dF/dY values calculated by the mixing algorithm */
static const EOS_INTEGER EOS_nXYPairs = 37;   /* The number of (X,Y) values last passed into the the interpolator */

/* Internal meta data information constants */
/* No additional constants are currently needed here, because only table type constants are currently valid */
#define EOS_NUM_META_DATA_CONSTANTS  0

/* Internal meta data category constants */
#define EOS_NUM_META_DATA_CATEGORIES  7
//static const EOS_INTEGER EOS_Table_Type   /* defined above */
static const EOS_INTEGER EOS_Table_Name                     = 3001; /* The specified table type's descriptive name */
static const EOS_INTEGER EOS_Dependent_Var                  = 3002; /* The short string representation of the specified table type's dependent variable as listed in APPENDIX A */
static const EOS_INTEGER EOS_Independent_Var1               = 3003; /* The short string representation of the specified table type's first independent variable as listed in APPENDIX A */
static const EOS_INTEGER EOS_Independent_Var2               = 3004; /* The short string representation of the specified table type's second independent variable as listed in APPENDIX A */
static const EOS_INTEGER EOS_Sesame_Table_List              = 3005; /* The specified table type's associated SESAME table number(s) */
static const EOS_INTEGER EOS_Pressure_Balance_Table_Type    = 3006; /* The specified table type's associated pressure balance table type as used by the eos_Mix algorithms */
static const EOS_INTEGER EOS_Temperature_Balance_Table_Type = 3007; /* The specified table type's associated temperature balance table type as used by the eos_Mix algorithms */

/* Table handle specific meta data information constants */
//static const EOS_INTEGER EOS_Table_Name        /* defined above */
//static const EOS_INTEGER EOS_Dependent_Var     /* defined above */
//static const EOS_INTEGER EOS_Independent_Var1  /* defined above */
//static const EOS_INTEGER EOS_Independent_Var2  /* defined above */
static const EOS_INTEGER EOS_File_Name                      = 4001; /* The SESAME file name associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Name                  = 4002; /* The material name that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Source                = 4003; /* The material source (e.g. author) that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Date                  = 4004; /* The material creation date that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Ref                   = 4005; /* The material documentation reference(s) that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Composition           = 4006; /* The material composition that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Codes                 = 4007; /* The data generation software name(s) that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Phases                = 4008; /* The material phase name(s) that is associated with the specified table handle */
static const EOS_INTEGER EOS_Material_Classification        = 4009; /* The material classification that is associated with the specified table handle. Examples include, but are not limited to, Unknown, Unclassified, Export Controlled, etc. */
#define EOS_NUM_TABLE_META_DATA_CONSTANTS 9

#define EOS_META_DATA_STRLEN 4096 /* maximum length of string buffer required for meta data information (currently is PATH_MAX as defined on Linux) */

/* Error code constants */
static const EOS_INTEGER EOS_OK = 0;    /* No errors detected */
static const EOS_INTEGER EOS_MIN_ERROR_CODE_VALUE = 2001;       /* Minimum error code value */
static const EOS_INTEGER EOS_BAD_DERIVATIVE_FLAG = 2001;        /* Derivative is not recognized */
static const EOS_INTEGER EOS_BAD_INTERPOLATION_FLAG = 2002;     /* Interpolation is not recognized */
static const EOS_INTEGER EOS_BAD_MATERIAL_ID = 2003;    /* Material ID is zero */
static const EOS_INTEGER EOS_CONVERGENCE_FAILED = 2004; /* Iterative algorithm did not converge during inverse interpolation */
static const EOS_INTEGER EOS_NO_COMMENTS = 2005;        /* No comments available for this data table */
static const EOS_INTEGER EOS_DATA_TYPE_NOT_FOUND = 2006;        /* Data table type is not in library */
static const EOS_INTEGER EOS_FAILED = 2007;     /* Operation failed */
static const EOS_INTEGER EOS_INTERP_EXTRAPOLATED = 2008;        /* Interpolation caused extrapolation beyond data table boundaries */
static const EOS_INTEGER EOS_MATERIAL_NOT_FOUND = 2009; /* Material ID is not in library */
static const EOS_INTEGER EOS_MEM_ALLOCATION_FAILED = 2010;      /* EOS table area cannot be expanded. */
static const EOS_INTEGER EOS_NO_DATA_TABLE = 2011;      /* Data table is not in EOS table area */
static const EOS_INTEGER EOS_NO_SESAME_FILES = 2012;    /* No data library files exist */
static const EOS_INTEGER EOS_NOT_INITIALIZED = 2013;    /* EOS table area is not initialized */
static const EOS_INTEGER EOS_BAD_DATA_TYPE = 2014;      /* Data table type is not recognized */
static const EOS_INTEGER EOS_OPEN_SESAME_FILE_FAILED = 2015;    /* Could not open data file */
static const EOS_INTEGER EOS_READ_DATA_FAILED = 2016;   /* Could not load data table */
static const EOS_INTEGER EOS_READ_FILE_VERSION_FAILED = 2017;   /* Could not load version from data file */
static const EOS_INTEGER EOS_READ_MASTER_DIR_FAILED = 2018;     /* Could not load master directory */
static const EOS_INTEGER EOS_READ_MATERIAL_DIR_FAILED = 2019;   /* Could not load material directory */
static const EOS_INTEGER EOS_READ_TOTAL_MATERIALS_FAILED = 2020;        /* Could not read number of materials */
static const EOS_INTEGER EOS_INVALID_TABLE_HANDLE = 2021;       /* Invalid table handle */
static const EOS_INTEGER EOS_INVALID_SUBTABLE_INDEX = 2022;     /* Subtable index out of the range */
static const EOS_INTEGER EOS_xHi_yHi = 2023;    /* Both the x and y arguments were high. */
static const EOS_INTEGER EOS_xHi_yOk = 2024;    /* The x argument was high, the y argument was OK. */
static const EOS_INTEGER EOS_xHi_yLo = 2025;    /* The x argument was high, the y argument was low. */
static const EOS_INTEGER EOS_xOk_yLo = 2026;    /* The x argument is OK and the y argument is low. */
static const EOS_INTEGER EOS_xLo_yLo = 2027;    /* Both the x and y arguments were low. */
static const EOS_INTEGER EOS_xLo_yOk = 2028;    /* The x argument was low, the y argument was OK. */
static const EOS_INTEGER EOS_xLo_yHi = 2029;    /* The x argument was low, the y argument was OK. */
static const EOS_INTEGER EOS_xOk_yHi = 2030;    /* The x argument is OK and the y argument is high. */
static const EOS_INTEGER EOS_INVALID_OPTION_FLAG = 2031;        /* The option flag passed into eos_SetOption() is invalid. */
static const EOS_INTEGER EOS_INVALID_DATA_TYPE = 2032;  /* Operation is not defined on this data type */
static const EOS_INTEGER EOS_INVALID_SPLIT_FLAG = 2033; /* The data splitting option is invalid. */
static const EOS_INTEGER EOS_UNDEFINED = 2034;  /* The result is undefined. */
static const EOS_INTEGER EOS_NOT_ALLOCATED = 2035;      /* Memory not allocated for data */
static const EOS_INTEGER EOS_INTEGRATION_FAILED = 2036; /* Numerical integration failed or not possible */
static const EOS_INTEGER EOS_DATA_TYPE_NO_MATCH = 2037; /* Data types do not match as required for mixing */
static const EOS_INTEGER EOS_INVALID_INFO_FLAG = 2038;  /* The info flag passed into either eos_GetTableInfo() or eos_GetTableMetaData() is invalid */
static const EOS_INTEGER EOS_INVALID_CONC_SUM = 2039;   /* The sum of the supplied material concentrations does not equal 1.0 */
static const EOS_INTEGER EOS_INTERP_EXTRAP_TBAL = 2040; /* Temperature balance function extrapolated beyond data table boundaries */
static const EOS_INTEGER EOS_INTERP_EXTRAP_PBAL = 2041; /* Pressure balance function extrapolated beyond data table boundaries */
static const EOS_INTEGER EOS_CANT_MAKE_MONOTONIC = 2042;        /* Can't make data monotonic in X */
static const EOS_INTEGER EOS_CANT_INVERT_DATA = 2043;   /* Can't invert wrt first independent variable */
static const EOS_INTEGER EOS_OPEN_OUTPUT_FILE_FAILED = 2044;    /* Could not open TablesLoaded.dat or related data file */
static const EOS_INTEGER EOS_INVALID_NXYPAIRS = 2045;   /* Invalid nXYPairs value */
static const EOS_INTEGER EOS_GEN401_AND_NOT_FOUND = 2046;       /* 401 data was generated and not found */
static const EOS_INTEGER EOS_WARNING = 2047;   /* Operation has generated a warning and an associated custom message */
static const EOS_INTEGER EOS_SPLIT_FAILED = 2048; /* The data splitting algorithm failed. */
static const EOS_INTEGER EOS_INDEX_FILE_ERROR = 2049; /* The sesameFilesDir.txt file parser found a syntax error. */
static const EOS_INTEGER EOS_INVALID_INFO_CATEGORY_FLAG = 2050;       /* The info category flag passed into eos_GetMetaData() is invalid */
static const EOS_INTEGER EOS_MAX_ERROR_CODE_VALUE = 2050;       /* Maximum error code value */

#include "eos_Interface.proto.h"

#endif                          /* if not defined EOSPAC6_EOS_INTERFACE */
