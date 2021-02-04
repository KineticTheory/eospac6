/*********************************************************************
 * Header for C code to set the types used by EOSPAC
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 * NOTE: THIS HEADER FILE MUST BE KEPT IN SYNC WITH THE FOLLOWING
 *       PUBLIC INTERFACE FILES:
 *       eos_Interface.h
 *       eos_Interface.fi
 *       eos_Interface.f90
 *
 ********************************************************************/


#ifndef EOS_TYPES_H
#define EOS_TYPES_H

#include "eos_universal_types.h"

enum EOS_DATA_CLASS_enum
{
  EOS_INTERNAL,                 /* Internally-used information */
  EOS_INFORMATION,              /* General information found in 100 and 200 tables */
  EOS_TOTAL,                    /* Total EOS in 301 tables */
  EOS_ION_PLUS_COLD,            /* Ion+Cold EOS in 303 tables */
  EOS_ELECTRON,                 /* Electron EOS in 304 tables */
  EOS_ION,                      /* Ion EOS in 303 tables */
  EOS_COLD,                     /* Cold curve EOS in 306 tables */
  EOS_MASS_FRACTION,            /* Mass Fraction EOS in 321 tables */
  EOS_VAPORIZATION,             /* Vaporization data in 401 tables */
  EOS_MELT,                     /* Melt data in 411 and 412 tables */
  EOS_SHEAR_MODULUS,            /* Shear Modulus data in 431 tables */
  EOS_OPACITY,                  /* Opacity data in 500 tables */
  EOS_CONDUCTIVITY              /* Conductivity data in 600 tables */
};
typedef enum EOS_DATA_CLASS_enum EOS_DATA_CLASS;

enum EOS_TABLE_CATEGORY_enum
{
  EOS_CATEGORY0,                /* table is not inverted */
  EOS_CATEGORY1,                /* table is inverted with respect to 1st independent variable */
  EOS_CATEGORY2,                /* table is inverted with respect to 2nd independent variable */
  EOS_CATEGORY3,                /* table is merged with another function to change 1st independent variable */
  EOS_CATEGORY4,                /* table is merged with another function to change 2nd independent variable */
  EOS_INVERTED_AT_SETUP         /* table is inverted or merged with another function during setup */
};
typedef enum EOS_TABLE_CATEGORY_enum EOS_TABLE_CATEGORY;

/* record types corresponding to defined RecordType classes */
enum EOS_RECORD_TYPE_enum
{
  EOS_RECORD_TYPE1 = 1,
  EOS_RECORD_TYPE2,
  EOS_RECORD_TYPE3,
  EOS_RECORD_TYPE4,
  EOS_RECORD_TYPE5,
  EOS_RECORD_TYPE6,
  EOS_RECORD_TYPE_NONE = 0,
  EOS_RECORD_TYPE_UNKNOWN = -1
};
typedef enum EOS_RECORD_TYPE_enum EOS_RECORD_TYPE;

/* table types: */
#define EOS_NullTable  0        /* null table */
#define EOS_Comment    1        /* Descriptive Comments */
#define EOS_Info       2        /* Atomic Number, Atomic Mass, Normal Density, Solid Bulk Modulus, Exchange Coefficient */
#define EOS_Pt_DT      3        /* Total Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_D_PtT      4        /* Density (Mg/m^3) (Total Pressure (GPa)- and Temperature (K)-dependent) */
#define EOS_T_DPt      5        /* Temperature (K) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent) */
#define EOS_Pt_DUt     6        /* Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pt_DAt     7        /* Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pt_DSt     8        /* Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Ut_DT      12       /* Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DUt      14       /* Temperature (K) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Ut_DPt     15       /* Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent) */
#define EOS_Ut_DAt     16       /* Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ut_DSt     17       /* Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Ut_PtT     18       /* Total Specific-Internal-Energy (MJ/kg) (Total Pressure (GPa)- and Temperature (K)-dependent) */
#define EOS_At_DT      21       /* Total Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DAt      23       /* Temperature (K) (Density (Mg/m^3)- and Total Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_At_DPt     24       /* Total Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent) */
#define EOS_At_DUt     25       /* Total Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_At_DSt     26       /* Total Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_St_DT      30       /* Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DSt      32       /* Temperature (K) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_St_DPt     33       /* Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent) */
#define EOS_St_DUt     34       /* Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_St_DAt     35       /* Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pic_DT     39       /* Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DPic     41       /* Temperature (K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent) */
#define EOS_Pic_DUic   42       /* Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pic_DAic   43       /* Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pic_DSic   44       /* Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Uic_DT     48       /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DUic     50       /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uic_DPic   51       /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent) */
#define EOS_Uic_DAic   52       /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uic_DSic   53       /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Aic_DT     57       /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DAic     59       /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Aic_DPic   60       /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent) */
#define EOS_Aic_DUic   61       /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Aic_DSic   62       /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Sic_DT     66       /* Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DSic     68       /* Temperature (K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Sic_DPic   69       /* Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent) */
#define EOS_Sic_DUic   70       /* Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Sic_DAic   71       /* Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pe_DT      75       /* Electron Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DPe      77       /* Temperature (K) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent) */
#define EOS_Pe_DUe     78       /* Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pe_DAe     79       /* Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pe_DSe     80       /* Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Ue_DT      84       /* Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DUe      86       /* Temperature (K) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Ue_DPe     87       /* Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent) */
#define EOS_Ue_DAe     88       /* Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ue_DSe     89       /* Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Ae_DT      93       /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DAe      95       /* Temperature (K) (Density (Mg/m^3)- and Electron Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ae_DPe     96       /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent) */
#define EOS_Ae_DUe     97       /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Ae_DSe     98       /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Se_DT      102      /* Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DSe      104      /* Temperature (K) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Se_DPe     105      /* Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent) */
#define EOS_Se_DUe     106      /* Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Se_DAe     107      /* Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Piz_DT     111      /* Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DPiz     113      /* Temperature (K) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent) */
#define EOS_Piz_DUiz   114      /* Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Piz_DAiz   115      /* Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Piz_DSiz   116      /* Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Uiz_DT     120      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DUiz     122      /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Uiz_DPiz   123      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent) */
#define EOS_Uiz_DAiz   124      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Uiz_DSiz   125      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Aiz_DT     129      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DAiz     131      /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Aiz_DPiz   132      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent) */
#define EOS_Aiz_DUiz   133      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Aiz_DSiz   134      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Siz_DT     138      /* Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DSiz     140      /* Temperature (K) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Siz_DPiz   141      /* Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent) */
#define EOS_Siz_DUiz   142      /* Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Siz_DAiz   143      /* Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Pc_D       147      /* Pressure Cold Curve (GPa) (Density (Mg/m^3)-dependent) */
#define EOS_Uc_D       151      /* Specific-Internal-Energy Cold Curve (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_Ac_D       155      /* Specific-Helmholtz-Free-Energy Cold Curve (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_Pv_T       159      /* Vapor Pressure (GPa) (Temperature (K)-dependent) */
#define EOS_T_Pv       160      /* Temperature (K) (Vapor Pressure (GPa)-dependent) */
#define EOS_Pv_Dv      161      /* Vapor Pressure (GPa) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Pv_Dls     162      /* Vapor Pressure (GPa) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Pv_Uv      163      /* Vapor Pressure (GPa) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pv_Uls     164      /* Vapor Pressure (GPa) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pv_Av      165      /* Vapor Pressure (GPa) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pv_Als     166      /* Vapor Pressure (GPa) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Dv_T       167      /* Vapor Density on coexistence line (Mg/m^3) (Temperature (K)-dependent) */
#define EOS_T_Dv       168      /* Temperature (K) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Dv_Pv      169      /* Vapor Density on coexistence line (Mg/m^3) (Vapor Pressure (GPa)-dependent) */
#define EOS_Dv_Dls     170      /* Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Dv_Uv      171      /* Vapor Density on coexistence line (Mg/m^3) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Dv_Uls     172      /* Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Dv_Av      173      /* Vapor Density on coexistence line (Mg/m^3) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Dv_Als     174      /* Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Dls_T      175      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Temperature (K)-dependent) */
#define EOS_T_Dls      176      /* Temperature (K) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Dls_Pv     177      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Pressure (GPa)-dependent) */
#define EOS_Dls_Dv     178      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Dls_Uv     179      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Dls_Uls    180      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Dls_Av     181      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Dls_Als    182      /* Liquid or Solid Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uv_T       183      /* Vapor Specific-Internal-Energy (MJ/kg) (Temperature (K)-dependent) */
#define EOS_T_Uv       184      /* Temperature (K) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uv_Pv      185      /* Vapor Specific-Internal-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent) */
#define EOS_Uv_Dv      186      /* Vapor Specific-Internal-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Uv_Dls     187      /* Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Uv_Uls     188      /* Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uv_Av      189      /* Vapor Specific-Internal-Energy (MJ/kg) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uv_Als     190      /* Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uls_T      191      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Temperature (K)-dependent) */
#define EOS_T_Uls      192      /* Temperature (K) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uls_Pv     193      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent) */
#define EOS_Uls_Dv     194      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Uls_Dls    195      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Uls_Uv     196      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uls_Av     197      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uls_Als    198      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Av_T       199      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Temperature (K)-dependent) */
#define EOS_T_Av       200      /* Temperature (K) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Av_Pv      201      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent) */
#define EOS_Av_Dv      202      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Av_Dls     203      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Av_Uv      204      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Av_Uls     205      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Av_Als     206      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Als_T      207      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Temperature (K)-dependent) */
#define EOS_T_Als      208      /* Temperature (K) (Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Als_Pv     209      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent) */
#define EOS_Als_Dv     210      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Als_Dls    211      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent) */
#define EOS_Als_Uv     212      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Als_Uls    213      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Als_Av     214      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) (Vapor Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Tm_D       215      /* Melt Temperature (K) (Density (Mg/m^3)-dependent) */
#define EOS_D_Tm       216      /* Density (Mg/m^3) (Melt Temperature (K)-dependent) */
#define EOS_Tm_Pm      217      /* Melt Temperature (K) (Melt Pressure (GPa)-dependent) */
#define EOS_Tm_Um      218      /* Melt Temperature (K) (Melt Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Tm_Am      219      /* Melt Temperature (K) (Melt Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pm_D       220      /* Melt Pressure (GPa) (Density (Mg/m^3)-dependent) */
#define EOS_D_Pm       221      /* Density (Mg/m^3) (Melt Pressure (GPa)-dependent) */
#define EOS_Pm_Tm      222      /* Melt Pressure (GPa) (Melt Temperature (K)-dependent) */
#define EOS_Pm_Um      223      /* Melt Pressure (GPa) (Melt Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pm_Am      224      /* Melt Pressure (GPa) (Melt Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Um_D       225      /* Melt Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_D_Um       226      /* Density (Mg/m^3) (Melt Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Um_Tm      227      /* Melt Specific-Internal-Energy (MJ/kg) (Melt Temperature (K)-dependent) */
#define EOS_Um_Pm      228      /* Melt Specific-Internal-Energy (MJ/kg) (Melt Pressure (GPa)-dependent) */
#define EOS_Um_Am      229      /* Melt Specific-Internal-Energy (MJ/kg) (Melt Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Am_D       230      /* Melt Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_D_Am       231      /* Density (Mg/m^3) (Melt Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Am_Tm      232      /* Melt Specific-Helmholtz-Free-Energy (MJ/kg) (Melt Temperature (K)-dependent) */
#define EOS_Am_Pm      233      /* Melt Specific-Helmholtz-Free-Energy (MJ/kg) (Melt Pressure (GPa)-dependent) */
#define EOS_Am_Um      234      /* Melt Specific-Helmholtz-Free-Energy (MJ/kg) (Melt Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Tf_D       235      /* Freeze Temperature (K) (Density (Mg/m^3)-dependent) */
#define EOS_D_Tf       236      /* Density (Mg/m^3) (Freeze Temperature (K)-dependent) */
#define EOS_Tf_Pf      237      /* Freeze Temperature (K) (Freeze Pressure (GPa)-dependent) */
#define EOS_Tf_Uf      238      /* Freeze Temperature (K) (Freeze Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Tf_Af      239      /* Freeze Temperature (K) (Freeze Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pf_D       240      /* Freeze Pressure (GPa) (Density (Mg/m^3)-dependent) */
#define EOS_D_Pf       241      /* Density (Mg/m^3) (Freeze Pressure (GPa)-dependent) */
#define EOS_Pf_Tf      242      /* Freeze Pressure (GPa) (Freeze Temperature (K)-dependent) */
#define EOS_Pf_Uf      243      /* Freeze Pressure (GPa) (Freeze Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Pf_Af      244      /* Freeze Pressure (GPa) (Freeze Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uf_D       245      /* Freeze Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_D_Uf       246      /* Density (Mg/m^3) (Freeze Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Uf_Tf      247      /* Freeze Specific-Internal-Energy (MJ/kg) (Freeze Temperature (K)-dependent) */
#define EOS_Uf_Pf      248      /* Freeze Specific-Internal-Energy (MJ/kg) (Freeze Pressure (GPa)-dependent) */
#define EOS_Uf_Af      249      /* Freeze Specific-Internal-Energy (MJ/kg) (Freeze Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Af_D       250      /* Freeze Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)-dependent) */
#define EOS_D_Af       251      /* Density (Mg/m^3) (Freeze Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Af_Tf      252      /* Freeze Specific-Helmholtz-Free-Energy (MJ/kg) (Freeze Temperature (K)-dependent) */
#define EOS_Af_Pf      253      /* Freeze Specific-Helmholtz-Free-Energy (MJ/kg) (Freeze Pressure (GPa)-dependent) */
#define EOS_Af_Uf      254      /* Freeze Specific-Helmholtz-Free-Energy (MJ/kg) (Freeze Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Gs_D       255      /* Shear Modulus (Gpa) (Density (Mg/m^3)-dependent) */
#define EOS_D_Gs       256      /* Density (Mg/m^3) (Shear Modulus (Gpa)-dependent) */
#define EOS_Ogb        257      /* Calculated versus Interpolated Opacity Grid Boundary */
#define EOS_Kr_DT      258      /* Rosseland Mean Opacity (cm^2/g) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Keo_DT     261      /* Electron Conductive Opacity (Opacity Model) (cm^2/g) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Zfo_DT     264      /* Mean Ion Charge (Opacity Model) (free electrons per atom) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Kp_DT      267      /* Planck Mean Opacity (cm^2/g) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Zfc_DT     270      /* Mean Ion Charge (Conductivity Model) (free electrons per atom) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Kec_DT     273      /* Electrical Conductivity (1/s) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Ktc_DT     276      /* Thermal Conductivity (1/cm/s) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_B_DT       279      /* Thermoelectric Coefficient (1/cm^2/s) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_Kc_DT      282      /* Electron Conductive Opacity (Conductivity Model) (cm^2/g) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_V_PtT      285      /* Volume (m^3)- (Total Specific-Helmholtz-Free-Energy (MJ/kg)- and Temperature (K)-dependent)  */
#define EOS_M_DT       305      /* Mass Fraction (Density- and Temperature-dependent) */
#define EOS_Gt_DT      310      /* Total Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DGt      311      /* Temperature (K) (Density (Mg/m^3)- and Total Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pt_DGt     312      /* Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ut_DGt     313      /* Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_At_DGt     314      /* Total Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_St_DGt     315      /* Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Gt_DPt     316      /* Total Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent) */
#define EOS_Gt_DUt     317      /* Total Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Gt_DAt     318      /* Total Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Gt_DSt     319      /* Total Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Gic_DT     320      /* Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DGic     321      /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pic_DGic   322      /* Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Uic_DGic   323      /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Aic_DGic   324      /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Sic_DGic   325      /* Ion Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Gic_DPic   326      /* Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent) */
#define EOS_Gic_DUic   327      /* Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Gic_DAic   328      /* Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Gic_DSic   329      /* Ion Specific-Gibbs-Free-Energy plus Cold Curve Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Entropy plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Ge_DT      330      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DGe      331      /* Temperature (K) (Density (Mg/m^3)- and Electron Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Pe_DGe     332      /* Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ue_DGe     333      /* Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ae_DGe     334      /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Se_DGe     335      /* Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ge_DPe     336      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent) */
#define EOS_Ge_DUe     337      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent) */
#define EOS_Ge_DAe     338      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Helmholtz-Free-Energy (MJ/kg)-dependent) */
#define EOS_Ge_DSe     339      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent) */
#define EOS_Giz_DT     340      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent) */
#define EOS_T_DGiz     341      /* Temperature (K) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Piz_DGiz   342      /* Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Uiz_DGiz   343      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Aiz_DGiz   344      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Siz_DGiz   345      /* Ion Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Gibbs-Free-Energy (MJ/kg)-dependent) */
#define EOS_Giz_DPiz   346      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent) */
#define EOS_Giz_DUiz   347      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Giz_DAiz   348      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg)-dependent) */
#define EOS_Giz_DSiz   349      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Entropy Including Zero Point (MJ/kg/K)-dependent) */
#define EOS_Gc_D       350      /* Specific-Gibbs-Free-Energy Cold Curve (MJ/kg) (Density (Mg/m^3)-dependent) */

/* Variable definition constants */
#define EOS_B        10000      /* Thermoelectric Coefficient (1/cm^2/s) */
#define EOS_Ac       10001      /* Specific-Helmholtz-Free-Energy Cold Curve (MJ/kg) */
#define EOS_Ae       10002      /* Electron Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Af       10003      /* Freeze Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Aiz      10004      /* Ion Specific-Helmholtz-Free-Energy Including Zero Point (MJ/kg) */
#define EOS_Aic      10005      /* Ion Specific-Helmholtz-Free-Energy plus Cold Curve Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Als      10006      /* Liquid or Solid Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Am       10007      /* Melt Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Gs       10008      /* Shear Modulus (Gpa) */
#define EOS_At       10009      /* Total Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Av       10010      /* Vapor Specific-Helmholtz-Free-Energy (MJ/kg) */
#define EOS_Kc       10011      /* Electron Conductive Opacity (Conductivity Model) (cm^2/g) */
#define EOS_Keo      10012      /* Electron Conductive Opacity (Opacity Model) (cm^2/g) */
#define EOS_Kec      10013      /* Electrical Conductivity (1/s) */
#define EOS_Kp       10014      /* Planck Mean Opacity (cm^2/g) */
#define EOS_Kr       10015      /* Rosseland Mean Opacity (cm^2/g) */
#define EOS_Ktc      10016      /* Thermal Conductivity (1/cm/s) */
#define EOS_Pc       10017      /* Pressure Cold Curve (GPa) */
#define EOS_Pe       10018      /* Electron Pressure (GPa) */
#define EOS_Pf       10019      /* Freeze Pressure (GPa) */
#define EOS_Piz      10020      /* Ion Pressure Including Zero Point (GPa) */
#define EOS_Pic      10021      /* Ion Pressure plus Cold Curve Pressure (GPa) */
#define EOS_Pm       10022      /* Melt Pressure (GPa) */
#define EOS_Pt       10023      /* Total Pressure (GPa) */
#define EOS_Pv       10024      /* Vapor Pressure (GPa) */
#define EOS_D        10025      /* Density (Mg/m^3) */
#define EOS_Dls      10026      /* Liquid or Solid Density on coexistence line (Mg/m^3) */
#define EOS_Dv       10027      /* Vapor Density on coexistence line (Mg/m^3) */
#define EOS_Se       10028      /* Electron Specific-Entropy (MJ/kg/K) */
#define EOS_Siz      10029      /* Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) */
#define EOS_Sic      10030      /* Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) */
#define EOS_St       10031      /* Total Specific-Entropy (MJ/kg/K) */
#define EOS_T        10032      /* Temperature (K) */
#define EOS_Tf       10033      /* Freeze Temperature (K) */
#define EOS_Tm       10034      /* Melt Temperature (K) */
#define EOS_Uc       10035      /* Specific-Internal-Energy Cold Curve (MJ/kg) */
#define EOS_Ue       10036      /* Electron Specific-Internal-Energy (MJ/kg) */
#define EOS_Uf       10037      /* Freeze Specific-Internal-Energy (MJ/kg) */
#define EOS_Uiz      10038      /* Ion Specific-Internal-Energy Including Zero Point (MJ/kg) */
#define EOS_Uic      10039      /* Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) */
#define EOS_Uls      10040      /* Liquid or Solid Specific-Internal-Energy (MJ/kg) */
#define EOS_Um       10041      /* Melt Specific-Internal-Energy (MJ/kg) */
#define EOS_Ut       10042      /* Total Specific-Internal-Energy (MJ/kg) */
#define EOS_Uv       10043      /* Vapor Specific-Internal-Energy (MJ/kg) */
#define EOS_Zfc      10044      /* Mean Ion Charge (Conductivity Model) (free electrons per atom) */
#define EOS_Zfo      10045      /* Mean Ion Charge (Opacity Model) (free electrons per atom) */
#define EOS_V        10046      /* Volume (m^3) */
#define EOS_M        10047      /* Mass Fraction */
#define EOS_Gt       10048      /* Total Specific-Gibbs-Free-Energy (MJ/kg) */
#define EOS_Gic      10049      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) */
#define EOS_Ge       10050      /* Electron Specific-Gibbs-Free-Energy (MJ/kg) */
#define EOS_Giz      10051      /* Ion Specific-Gibbs-Free-Energy Including Zero Point (MJ/kg) */
#define EOS_Gc       10052      /* Specific-Gibbs-Free-Energy Cold Curve (MJ/kg) */

/* Data information constants */
#define EOS_NUM_INFO_CONSTANTS  49
#define EOS_Cmnt_Len             1      /* The length in characters of the comments available for the specified data table */
#define EOS_Exchange_Coeff       2      /* The exchange coefficient */
#define EOS_F_Convert_Factor   EOS_F_CONVERT    /* The conversion factor corresponding to the dependent variable, F(x,y) */
#define EOS_Log_Val              4      /* Non-zero if the data table is in a log10 format */
#define EOS_Material_ID          5      /* The SESAME material identification number */
#define EOS_Mean_Atomic_Mass     6      /* The mean atomic mass */
#define EOS_Mean_Atomic_Num      7      /* The mean atomic number */
#define EOS_Modulus              8      /* The solid bulk modulus */
#define EOS_Normal_Density       9      /* The normal density */
#define EOS_Table_Type          10      /* The type of data table. Corresponds to the parameters in APPENDIX B and APPENDIX C */
#define EOS_X_Convert_Factor   EOS_X_CONVERT    /* The conversion factor corresponding to the primary independent variable, x */
#define EOS_Y_Convert_Factor   EOS_Y_CONVERT    /* The conversion factor corresponding to the secondary independent variable, y */
#define EOS_MaxErrMsgLen      1024      /* The maximum length of an error message to be returned to the host code */
#define EOS_R_Array             11      /* The density array */
#define EOS_T_Array             12      /* The temperature array */
#define EOS_F_Array             13      /* The F array */
#define EOS_NR                  14      /* The number of densities */
#define EOS_NT                  15      /* The number of temperatures */
#define EOS_Rmin                16      /* The minimum density */
#define EOS_Rmax                17      /* The maximum density */
#define EOS_Tmin                18      /* The minimum temperature */
#define EOS_Tmax                19      /* The maximum temperature */
#define EOS_Fmin                20      /* The minimum F value */
#define EOS_Fmax                21      /* The maximum F value */
#define EOS_NT401               22      /* The number of temperatures in Sesame 401 table */
#define EOS_P401                23      /* The pressure array in Sesame 401 table */
#define EOS_T401                24      /* The temperature array in Sesame 401 table  */
#define EOS_RG401               25      /* The RG array in Sesame 401 table  */
#define EOS_RL401               26      /* The RL array in Sesame 401 table  */
#define EOS_EG401               27      /* The EG array in Sesame 401 table  */
#define EOS_EL401               28      /* The EL array in Sesame 401 table  */
#define EOS_AG401               29      /* The AG array in Sesame 401 table  */
#define EOS_AL401               30      /* The AL array in Sesame 401 table  */
#define EOS_NUM_PHASES          31      /* The number of material phases tabulated (i.e. number of mass fraction subtables available to EOS_M_DT */
#define EOS_X_Species_Data      32      /* The species-specific X values calculated by the mixing algorithm */
#define EOS_Y_Species_Data      33      /* The species-specific Y values calculated by the mixing algorithm */
#define EOS_F_Species_Data      34      /* The species-specific F values calculated by the mixing algorithm */
#define EOS_dFx_Species_Data    35      /* The species-specific dF/dX values calculated by the mixing algorithm */
#define EOS_dFy_Species_Data    36      /* The species-specific dF/dY values calculated by the mixing algorithm */
#define EOS_nXYPairs            37      /* The number of (X,Y) values last passed into the the interpolator */
#define EOS_X_LOWER_BOUND       38      /* The extrapolation lower bound(s) array, of extent EOS_NX, for the xVals */
#define EOS_X_UPPER_BOUND       39      /* The extrapolation upper bound(s) array, of extent EOS_NX, for the xVals */
#define EOS_Y_LOWER_BOUND       40      /* The extrapolation lower bound(s) array, of extent EOS_NY, for the yVals */
#define EOS_Y_UPPER_BOUND       41      /* The extrapolation upper bound(s) array, of extent EOS_NY, for the yVals */
#define EOS_NX                  42      /* The extent of the xVals extrapolation lower/upper bound(s) arrays */
#define EOS_NY                  43      /* The extent of the yVals extrapolation lower/upper bound(s) arrays */
#define EOS_X_BOUND_GRID        44      /* The extrapolation bound(s) grid array, of extent EOS_NX, for the xVals */
#define EOS_Y_BOUND_GRID        45      /* The extrapolation bound(s) grid array, of extent EOS_NY, for the yVals */

/* Define hidden option flags that are currently not defined in the public interface.
 *  NOTE: If you want to use these in a test code, copy these defines into your code. */
#define EOS_NUM_PRIVATE_OPTIONS 4
#define EOS_MIN_PRIVATE_OPTION_FLAG_VALUE   11000       /* Minimum private option flag value */
#define DISABLE_FTBLS_INVT_MASK   11000 /* Disable FTBLS_INVT_MASK function */
#define EOS_DEBUG_PRINT   11001 /* Enable DEBUG_PRINT function */
#define EOS_DISABLE_GHOST_NODES 11002      /* Disable the forced usage of ghost node data during interpolation */
#define EOS_ALLOW_ALL_INFO_ITEMS 11003  /* Override category restrictions related to selected table information parameters */

/* Table setup and interpolation option constants */
#define EOS_NUM_LOADING_OPTIONS 15
#define EOS_NUM_INTERPOLATION_OPTIONS 11
#define EOS_NUM_TABLE_OPTIONS   31      /* Total number of option flags, not including EOS_NUM_PRIVATE_OPTIONS */
#define EOS_TOTAL_TABLE_OPTIONS (EOS_NUM_TABLE_OPTIONS + EOS_NUM_PRIVATE_OPTIONS)
#define EOS_NUM_GENERAL_OPTIONS (EOS_TOTAL_TABLE_OPTIONS - EOS_NUM_LOADING_OPTIONS - EOS_NUM_INTERPOLATION_OPTIONS)
#define EOS_MIN_OPTION_FLAG_VALUE   1000        /* Minimum option flag value */
#define EOS_DUMP_DATA  1000     /* Write the loaded data table to a file. (overwrite) */
#define EOS_APPEND_DATA  1001   /* Write the loaded data table to a file. (append) */
#define EOS_INSERT_DATA  1002   /* Insert grid points between each original grid point with respect to all independent variables (i.e., increase grid resolution). The value of eos_SetOption parameter, tableOptionVal, is to contain the user-defined number of data points to insert between existing data points */
#define EOS_MONOTONIC_IN_X  1003        /* Enable forced monotonicity with respect to x of F(x,y) */
#define EOS_MONOTONIC_IN_Y  1004        /* Enable forced monotonicity with respect to y of F(x,y) */
#define EOS_SMOOTH  1005        /* Enable data table smoothing that imposes a linear floor on temperature dependence, forces linear temperature dependence for low temp, and forces linear density dependence for low and high dens. */
#define EOS_SPLIT_COWAN  1006   /* Allows splitting for ion-electron data table not found in the database using the cold curve plus Cowan-nuclear model for ions */
#define EOS_SPLIT_FORCED  1007  /* Forces specified splitting option for data table */
#define EOS_SPLIT_IDEAL_GAS  1008       /* Allows splitting for ion-electron data table not found in the database using the cold curve plus ideal gas model for ions */
#define EOS_SPLIT_NUM_PROP  1009        /* Allows splitting for ion-electron data table not found in the database using the cold curve plus number-proportional model for ions */
#define EOS_CHECK_ARGS  1010    /* Turn on extensive argument checking, default is off. */
#define EOS_LINEAR  1011        /* Bilinear (4-point) interpolation */
#define EOS_RATIONAL  1012      /* Birational (12-point) interpolation */
#define EOS_X_CONVERT  1013     /* Set the conversion factor used on the xVals independent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
#define EOS_Y_CONVERT  1014     /* Set the conversion factor used on the yVals independent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
#define EOS_F_CONVERT  1015     /* Set the conversion factor used on the fVals dependent variable value(s). The value of eos_SetOption parameter, tableOptionVal, is to contain the conversion factor value */
#define EOS_PT_SMOOTHING 1016   /* */
#define EOS_ADJUST_VAP_PRES 1017        /* correct the initial low density, used in conjunction with EOS_PT_SMOOTHING */
#define EOS_USE_CUSTOM_INTERP 1018      /* Use a custom inverse-interpolation algorithm that requires the setup option, EOS_PT_SMOOTHING, to be enabled for the specified table handle. This option is only valid for table types EOS_Ut_PtT and EOS_V_PtT */
#define EOS_SAVE_SPECIES_DATA 1019  /* Enable the storage of species-specific data during EOS mixing */
#define EOS_CALC_FREE_ENERGY 1020  /* Enforce the calculation of the free-energy table instead of loading it from file */
#define EOS_CREATE_TZERO 1021  /* Using linear extrapolation along each isochore , create a T=0 isotherm if it's unavailable when loading 300-series Sesame data */
#define EOS_USE_TAYLOR_FIT 1022  /* Use Taylor polynomial fit data instead of standard tabulated data */
#define EOS_USE_MAXWELL_TABLE 1023  /* Use the Maxwell data in table 311 instead of the corresponding table 301 */
#define EOS_DISCONTINUOUS_DERIVATIVES  1024 /* Enable the original linear/bilinear logic, which calculates discontinuous derivatives at the tabulated grid. This option requires the interpolation option, EOS_LINEAR, to be enabled for the specified table handle. */
#define EOS_XY_PASSTHRU  1025 /* Neither create an internal copy nor modify the xVals and yVals inputs for eos_Interpolate, eos_Mix and eos_CheckExtrap. Use host code's arrays directly -- unmodified. Overrides previously-set EOS_XY_MODIFY option. */
#define EOS_XY_MODIFY  1026 /* Do not create an internal copy of the xVals and yVals inputs for eos_Interpolate, eos_Mix and eos_CheckExtrap. Modify the xVals and yVals inputs in situ -- use host code's arrays directly. Overrides previously-set EOS_XY_PASSTHRU option. */
#define EOS_INVERT_AT_SETUP  1027 /* Table(s) are inverted at setup, which means inverse interpolation is avoided later */
#define EOS_USE_HOST_XY 1028 /* Enable EOS_XY_MODIFY, and enable logic to revert the modified xVals and yVals inputs after interpolation is completed */
#define EOS_SKIP_EXTRAP_CHECK   1029 /* All extrapolation checks are skipped unless host calls eos_CheckExtrap */
#define EOS_NORMAL_DERIVATIVES  1030  /* Return EOS partial derivatives of F with respect to both x and y.
					 This is not in the public interface anymore since it is implemented by default,
				         and it is still defined in case other similar options are implemented in the future. */

/* note: if the order of the above option definitions changes,
         EOS_LOADING_OPTION_FLAG_TO_INDEX() macro will be affected. Olga 3/1/04 */

/* Internal meta data information constants */
/* No additional constants are currently needed here, because only table type constants are currently valid */
#define EOS_NUM_META_DATA_CONSTANTS 0

/* Internal meta data category constants */
#define EOS_NUM_META_DATA_CATEGORIES 7
//#define EOS_Table_Type      /* defined above */
#define EOS_Table_Name                     3001 /* The specified table type's descriptive name */
#define EOS_Dependent_Var                  3002 /* The short string representation of the specified table type's dependent variable as listed in APPENDIX A */
#define EOS_Independent_Var1               3003 /* The short string representation of the specified table type's first independent variable as listed in APPENDIX A */
#define EOS_Independent_Var2               3004 /* The short string representation of the specified table type's second independent variable as listed in APPENDIX A */
#define EOS_Sesame_Table_List              3005 /* The specified table type's associated SESAME table number(s) */
#define EOS_Pressure_Balance_Table_Type    3006 /* The specified table type's associated pressure balance table type as used by the eos_Mix algorithms */
#define EOS_Temperature_Balance_Table_Type 3007 /* The specified table type's associated temperature balance table type as used by the eos_Mix algorithms */

/* Table handle specific meta data information constants */
#define EOS_File_Name                      4001 /* The SESAME file name associated with the specified table handle */
#define EOS_Material_Name                  4002 /* The material name that is associated with the specified table handle */
#define EOS_Material_Source                4003 /* The material source (e.g. author) that is associated with the specified table handle */
#define EOS_Material_Date                  4004 /* The material creation date that is associated with the specified table handle */
#define EOS_Material_Ref                   4005 /* The material documentation reference(s) that is associated with the specified table handle */
#define EOS_Material_Composition           4006 /* The material composition that is associated with the specified table handle */
#define EOS_Material_Codes                 4007 /* The data generation software name(s) that is associated with the specified table handle */
#define EOS_Material_Phases                4008 /* The material phase name(s) that is associated with the specified table handle */
#define EOS_Material_Classification        4009 /* The material classification that is associated with the specified table handle. Examples include, but are not limited to, Unknown, Unclassified, Export Controlled, etc. */
#define EOS_NUM_TABLE_META_DATA_CONSTANTS 9

#define EOS_META_DATA_STRLEN 4096 /* maximum length of string buffer required for meta data information */

/* Error code constants */
#define EOS_OK  0               /* No errors detected */
#define EOS_MIN_ERROR_CODE_VALUE   2001 /* Minimum error code value */
#define EOS_BAD_DERIVATIVE_FLAG  2001   /* Derivative is not recognized */
#define EOS_BAD_INTERPOLATION_FLAG  2002        /* Interpolation is not recognized */
#define EOS_BAD_MATERIAL_ID  2003       /* Material ID is zero */
#define EOS_CONVERGENCE_FAILED  2004    /* Iterative algorithm did not converge during inverse interpolation */
#define EOS_NO_COMMENTS  2005   /* No comments available for this data table */
#define EOS_DATA_TYPE_NOT_FOUND  2006   /* Data table type is not in library */
#define EOS_FAILED  2007        /* Operation failed */
#define EOS_INTERP_EXTRAPOLATED  2008   /* Interpolation caused extrapolation beyond data table boundaries */
#define EOS_MATERIAL_NOT_FOUND  2009    /* Material ID is not in library */
#define EOS_MEM_ALLOCATION_FAILED  2010 /* EOS table area cannot be expanded */
#define EOS_NO_DATA_TABLE  2011 /* Data table is not in EOS table area */
#define EOS_NO_SESAME_FILES  2012       /* No data library files exist */
#define EOS_NOT_INITIALIZED  2013       /* EOS table area is not initialized */
#define EOS_BAD_DATA_TYPE  2014 /* Data table type is not recognized */
#define EOS_OPEN_SESAME_FILE_FAILED  2015       /* Could not open data file */
#define EOS_READ_DATA_FAILED  2016      /* Could not load data table */
#define EOS_READ_FILE_VERSION_FAILED  2017      /* Could not load version from data file */
#define EOS_READ_MASTER_DIR_FAILED  2018        /* Could not load master directory */
#define EOS_READ_MATERIAL_DIR_FAILED  2019      /* Could not load material directory */
#define EOS_READ_TOTAL_MATERIALS_FAILED  2020   /* Could not read number of materials */
#define EOS_INVALID_TABLE_HANDLE  2021  /* Invalid table handle */
#define EOS_INVALID_SUBTABLE_INDEX  2022        /* Subtable index out of the range */
#define EOS_xHi_yHi  2023       /* Both the x and y arguments were high */
#define EOS_xHi_yOk  2024       /* The x argument was high, the y argument was OK */
#define EOS_xHi_yLo  2025       /* The x argument was high, the y argument was low */
#define EOS_xOk_yLo  2026       /* The x argument is OK and the y argument is low */
#define EOS_xLo_yLo  2027       /* Both the x and y arguments were low */
#define EOS_xLo_yOk  2028       /* The x argument was low, the y argument was OK */
#define EOS_xLo_yHi  2029       /* The x argument was low, the y argument was OK */
#define EOS_xOk_yHi  2030       /* The x argument is OK and the y argument is high */
#define EOS_INVALID_OPTION_FLAG  2031   /* The option flag passed into eos_SetOption() is invalid */
#define EOS_INVALID_DATA_TYPE  2032     /* Operation is not defined on this data type */
#define EOS_INVALID_SPLIT_FLAG  2033    /* The data splitting option is invalid */
#define EOS_UNDEFINED 2034      /* The result is undefined */
#define EOS_NOT_ALLOCATED 2035  /* Memory not allocated for data */
#define EOS_INTEGRATION_FAILED 2036     /* Numerical integration failed or not possible */
#define EOS_DATA_TYPE_NO_MATCH 2037     /* Data types do not match as required for mixing */
#define EOS_INVALID_INFO_FLAG  2038     /* The info flag passed into either eos_GetTableInfo() or eos_GetTableMetaData() is invalid */
#define EOS_INVALID_CONC_SUM   2039     /* The sum of the supplied material concentrations does not equal 1.0 */
#define EOS_INTERP_EXTRAP_TBAL 2040     /* Temperature balance function extrapolated beyond data table boundaries */
#define EOS_INTERP_EXTRAP_PBAL 2041     /* Pressure balance function extrapolated beyond data table boundaries */
#define EOS_CANT_MAKE_MONOTONIC   2042  /* Can't make data monotonic in X */
#define EOS_CANT_INVERT_DATA      2043  /* Can't invert wrt first independent variable */
#define EOS_OPEN_OUTPUT_FILE_FAILED 2044        /* Could not open TablesLoaded.dat or related data file */
#define EOS_INVALID_NXYPAIRS 2045       /* Invalid nXYPairs value */
#define EOS_GEN401_AND_NOT_FOUND 2046   /* 401 data was generated and not found */
#define EOS_WARNING 2047       /* Operation has generated a warning and an associated custom message */
#define EOS_SPLIT_FAILED 2048  /* The data splitting algorithm failed. */
#define EOS_INDEX_FILE_ERROR 2049 /* The sesameFilesDir.txt file parser found a syntax error. */
#define EOS_INVALID_INFO_CATEGORY_FLAG 2050 /* The info category flag passed into eos_GetMetaData() is invalid */
#define EOS_MAX_ERROR_CODE_VALUE   2050 /* Maximum error code value */
#define EOS_MAGX10_MAX_ERROR_CODE_VALUE   10000 /* (EOS_INTEGER)pow(10.0, MAX(log10(EOS_MAX_ERROR_CODE_VALUE), 1) + 1) */

#endif
/* EOS_TYPES_H */
