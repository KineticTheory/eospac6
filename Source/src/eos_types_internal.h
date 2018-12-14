/*********************************************************************
 * Header for C code to set the types used by EOSPAC
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#ifndef EOS_TYPES_INTERNAL_H
#define EOS_TYPES_INTERNAL_H

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif /*defined(HAVE_CONFIG_H)*/
#include <assert.h>
#include "eos_types.h"
#include "eos_Interpolation.h"
#include "eos_universal_types.h"
#include <math.h>
#include <float.h>
#include <stdio.h>
#ifndef _POSIX_
#define _POSIX_
#endif

/* internal generic data types, specific to a table number */
enum
{
  EOS_301_DATA = 286,
  EOS_303_DATA,
  EOS_304_DATA,
  EOS_305_DATA,
  EOS_306_DATA,
  EOS_401_DATA,
  EOS_411_DATA,
  EOS_412_DATA,
  EOS_431_DATA,
  EOS_501_DATA,
  EOS_502_DATA,
  EOS_503_DATA,
  EOS_504_DATA,
  EOS_505_DATA,
  EOS_601_DATA,
  EOS_602_DATA,
  EOS_603_DATA,
  EOS_604_DATA,
  EOS_605_DATA
};

typedef struct
{
  EOS_INTEGER eosTableType;     /* table type index for this data */
  EOS_CHAR *eosTableType_s;     /* string representation of the table type index for this data */
  EOS_INTEGER tableNum;         /* Sesame table number storing original data for this eosTableType */
  EOS_INTEGER subTableNum;      /* Sesame subtable number storing original data for this eosTableType */
  EOS_RECORD_TYPE recordType;   /* record type class used to store this eosTableType */
  EOS_TABLE_CATEGORY category;  /* category definitions shown below */
  EOS_DATA_CLASS subCategory;   /* subcategory definitions shown below */
  EOS_INTEGER depVar;           /* dependent variable type */
  EOS_INTEGER indepVar1;        /* independent variable 1 type */
  EOS_INTEGER indepVar2;        /* independent variable 2 type */
  EOS_INTEGER eosTempBalFunc;   /* Temperature balance function table type used for mixing this eosTableType */
  EOS_INTEGER eosPresBalFunc;   /* Pressure balance function table type used for mixing this eosTableType */
  EOS_INTEGER eosTableTypeRef1; /* Used for merging tables to calculate EOS_CATEGORY3 or EOS_CATEGORY4 data */
  EOS_INTEGER eosTableTypeRef2; /* Used for merging tables to calculate EOS_CATEGORY3 or EOS_CATEGORY4 data */
  EOS_BOOLEAN logAxes;          /* are data stored as Log10 values in Sesame? */
  EOS_CHAR *eosTableName;       /* descriptive text for this eosTableType */
} eos_TableData;

typedef struct
{
  EOS_INTEGER eosVarType;       /* variable type index */
  EOS_CHAR *eosVarType_s;       /* string representation of the variable type index */
  EOS_CHAR *eosVarType_short_s; /* short string representation of the variable type index */
  EOS_CHAR *eosVarDescription;  /* descriptive text for this eosTableType */
} eos_VarTypeData;

typedef union
{
  EOS_BOOLEAN bval;
  EOS_INTEGER ival;
  EOS_REAL rval;
  EOS_CHAR *cval;
} eos_OptionValue;

typedef struct
{
  EOS_INTEGER optionFlag;
  EOS_INTEGER optionType; /* 1: EOS_BOOLEAN
			     2: EOS_INTEGER
			     3: EOS_REAL
			     4: EOS_CHAR*
			  */
  eos_OptionValue optionValue;
} eos_Option;

#include "eos_DataMap.h"

#define TINY_D 1.0e-99
#define TINY_LOG_D -99.0
#define HUGE_LOG_D 99.0
#define HUGE_D 1.0e99
#define EV_TO_KELVIN (EOS_REAL)11604.85
#define UNIVERSAL_GAS_CONST (EOS_REAL)8.314472e-03 /* gas constant (kboltz/amu in kJ/kelvin/mol) */

#ifdef _EOS_INTERNAL_

eos_Option _eos_DefaultTableOptions[EOS_TOTAL_TABLE_OPTIONS];    /* initialized in eos_CreateTables() */
eos_Option *eos_DefaultTableOptions = _eos_DefaultTableOptions;    /* initialized in eos_CreateTables() */
eos_DataMap gEosDataMap;
eos_Interpolation gEosInterpolation;
EOS_CHAR *_gCustomErrorMsg[EOS_MAX_ERROR_CODE_VALUE -
                          EOS_MIN_ERROR_CODE_VALUE + 1];
EOS_CHAR **gCustomErrorMsg = _gCustomErrorMsg;

/*****************************************************************************************************************************************************
 * Map each table type to table id, subtable id, record type, table type category, table type cross-references 1 and 2, log10 status, and description.
 *
 * Data Categories:
 *   EOS_CATEGORY0  -- table is not inverted
 *   EOS_CATEGORY1  -- table is inverted with respect to 1st independent variable
 *   EOS_CATEGORY2  -- table is inverted with respect to 2nd independent variable
 *   EOS_CATEGORY3  -- table is merged with another function to change 1st independent variable
 *   EOS_CATEGORY4  -- table is merged with another function to change 2nd independent variable 
 *
 * Data Subcategories:
 *   EOS_INFORMATION    -- General information found in Sesame 100- and 200-series tables
 *   EOS_TOTAL          -- Total EOS in Sesame 301 tables
 *   EOS_ION_PLUS_COLD  -- Ion+Cold EOS in Sesame 303 tables
 *   EOS_ELECTRON       -- Electron EOS in Sesame 304 tables
 *   EOS_ION            -- Ion EOS in Sesame 303 tables
 *   EOS_COLD           -- Cold curve EOS in Sesame 306 tables
 *   EOS_VAPORIZATION   -- Vaporization data in Sesame 401 tables
 *   EOS_MELT           -- Melt data in Sesame 411 and 412 tables
 *   EOS_SHEAR_MODULUS  -- Shear Modulus data in Sesame 431 tables
 *   EOS_OPACITY        -- Opacity data in Sesame 500-series tables 
 *   EOS_CONDUCTIVITY   -- Conductivity data in Sesame 600-series tables
 *   EOS_MASS_FRACTION  -- Mass fraction data in Sesame 321 tables
 *
 * ORDER IS IMPORTANT! Do not change the order of this hash because all the base values,
 *                     eos_TableList[i].eosTableType, must be ascending like i! This ensures the
 *                     binary search for a specified eosTableType will work.
 *****************************************************************************************************************************************************/
eos_TableData _eos_TableList[] = {

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /*   0 */ {EOS_NullTable, "EOS_NullTable", 0,        0,           EOS_RECORD_TYPE_NONE, EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "null table"},
  /*   1 */ {EOS_Comment,   "EOS_Comment",   101,      0,           EOS_RECORD_TYPE4,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Descriptive Comments"},
  /*   2 */ {EOS_Info,      "EOS_Info",      201,      0,           EOS_RECORD_TYPE5,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Atomic Number, Atomic Mass, Normal Density, Solid Bulk Modulus, Exchange Coefficient"},
  /*   3 */ {EOS_Pt_DT,     "EOS_Pt_DT",     301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_TOTAL,         EOS_Pt,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pt_DT,      EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Total Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*   4 */ {EOS_D_PtT,     "EOS_D_PtT",     301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_TOTAL,         EOS_D,         EOS_Pt,        EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Total Pressure (GPa)- and Temperature (K)-dependent)"},
  /*   5 */ {EOS_T_DPt,     "EOS_T_DPt",     301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_TOTAL,         EOS_T,         EOS_D,         EOS_Pt,        EOS_T_DPt,      EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent)"},
  /*   6 */ {EOS_Pt_DUt,    "EOS_Pt_DUt",    301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Pt,        EOS_D,         EOS_Ut,        EOS_T_DUt,      EOS_Pt_DUt,     EOS_Pt_DT,        EOS_T_DUt,        EOS_FALSE, "Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*   7 */ {EOS_Pt_DAt,    "EOS_Pt_DAt",    301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Pt,        EOS_D,         EOS_At,        EOS_NullTable,  EOS_NullTable,  EOS_Pt_DT,        EOS_T_DAt,        EOS_FALSE, "Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Free-Energy (MJ/kg)-dependent)"},
  /*   8 */ {EOS_Pt_DSt,    "EOS_Pt_DSt",    301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Pt,        EOS_D,         EOS_St,        EOS_NullTable,  EOS_NullTable,  EOS_Pt_DT,        EOS_T_DSt,        EOS_FALSE, "Total Pressure (GPa) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent)"},
  /*   9 */ {EOS_Ut_DT,     "EOS_Ut_DT",     301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_TOTAL,         EOS_Ut,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pt_DT,      EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  10 */ {EOS_T_DUt,     "EOS_T_DUt",     301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_TOTAL,         EOS_T,         EOS_D,         EOS_Ut,        EOS_T_DUt,      EOS_Pt_DUt,     EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  11 */ {EOS_Ut_DPt,    "EOS_Ut_DPt",    301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Ut,        EOS_D,         EOS_Pt,        EOS_T_DPt,      EOS_NullTable,  EOS_Ut_DT,        EOS_T_DPt,        EOS_FALSE, "Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent)"},
  /*  12 */ {EOS_Ut_DAt,    "EOS_Ut_DAt",    301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Ut,        EOS_D,         EOS_At,        EOS_NullTable,  EOS_NullTable,  EOS_Ut_DT,        EOS_T_DAt,        EOS_FALSE, "Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  13 */ {EOS_Ut_DSt,    "EOS_Ut_DSt",    301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_Ut,        EOS_D,         EOS_St,        EOS_NullTable,  EOS_NullTable,  EOS_Ut_DT,        EOS_T_DSt,        EOS_FALSE, "Total Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  14 */ {EOS_Ut_PtT,    "EOS_Ut_PtT",    301,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_TOTAL,         EOS_Ut,        EOS_Pt,        EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_Ut_DT,        EOS_D_PtT,        EOS_FALSE, "Total Specific-Internal-Energy (MJ/kg) (Total Pressure (GPa)- and Temperature (K)-dependent)"},
  /*  15 */ {EOS_At_DT,     "EOS_At_DT",     301,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_TOTAL,         EOS_At,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Total Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  16 */ {EOS_T_DAt,     "EOS_T_DAt",     301,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_TOTAL,         EOS_T,         EOS_D,         EOS_At,        EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Total Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  17 */ {EOS_At_DPt,    "EOS_At_DPt",    301,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_At,        EOS_D,         EOS_Pt,        EOS_NullTable,  EOS_NullTable,  EOS_At_DT,        EOS_T_DPt,        EOS_FALSE, "Total Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent)"},
  /*  18 */ {EOS_At_DUt,    "EOS_At_DUt",    301,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_At,        EOS_D,         EOS_Ut,        EOS_NullTable,  EOS_NullTable,  EOS_At_DT,        EOS_T_DUt,        EOS_FALSE, "Total Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  19 */ {EOS_At_DSt,    "EOS_At_DSt",    301,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_At,        EOS_D,         EOS_St,        EOS_NullTable,  EOS_NullTable,  EOS_At_DT,        EOS_T_DSt,        EOS_FALSE, "Total Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /*  20 */ {EOS_St_DT,     "EOS_St_DT",     301,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_TOTAL,         EOS_St,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  21 */ {EOS_T_DSt,     "EOS_T_DSt",     301,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_TOTAL,         EOS_T,         EOS_D,         EOS_St,        EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Total Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  22 */ {EOS_St_DPt,    "EOS_St_DPt",    301,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_St,        EOS_D,         EOS_Pt,        EOS_NullTable,  EOS_NullTable,  EOS_St_DT,        EOS_T_DPt,        EOS_FALSE, "Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Pressure (GPa)-dependent)"},
  /*  23 */ {EOS_St_DUt,    "EOS_St_DUt",    301,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_St,        EOS_D,         EOS_Ut,        EOS_NullTable,  EOS_NullTable,  EOS_St_DT,        EOS_T_DUt,        EOS_FALSE, "Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  24 */ {EOS_St_DAt,    "EOS_St_DAt",    301,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_TOTAL,         EOS_St,        EOS_D,         EOS_At,        EOS_NullTable,  EOS_NullTable,  EOS_St_DT,        EOS_T_DAt,        EOS_FALSE, "Total Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Total Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  25 */ {EOS_Pic_DT,    "EOS_Pic_DT",    303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION_PLUS_COLD, EOS_Pic,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pic_DT,     EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  26 */ {EOS_T_DPic,    "EOS_T_DPic",    303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION_PLUS_COLD, EOS_T,         EOS_D,         EOS_Pic,       EOS_T_DPic,     EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent)"},
  /*  27 */ {EOS_Pic_DUic,  "EOS_Pic_DUic",  303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Pic,       EOS_D,         EOS_Uic,       EOS_T_DUic,     EOS_Pic_DUic,   EOS_Pic_DT,       EOS_T_DUic,       EOS_FALSE, "Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  28 */ {EOS_Pic_DAic,  "EOS_Pic_DAic",  303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Pic,       EOS_D,         EOS_Aic,       EOS_NullTable,  EOS_NullTable,  EOS_Pic_DT,       EOS_T_DAic,       EOS_FALSE, "Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  29 */ {EOS_Pic_DSic,  "EOS_Pic_DSic",  303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Pic,       EOS_D,         EOS_Sic,       EOS_NullTable,  EOS_NullTable,  EOS_Pic_DT,       EOS_T_DSic,       EOS_FALSE, "Ion Pressure plus Cold Curve Pressure (GPa) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  30 */ {EOS_Uic_DT,    "EOS_Uic_DT",    303,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION_PLUS_COLD, EOS_Uic,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pic_DT,     EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  31 */ {EOS_T_DUic,    "EOS_T_DUic",    303,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION_PLUS_COLD, EOS_T,         EOS_D,         EOS_Uic,       EOS_T_DUic,     EOS_Pic_DUic,   EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  32 */ {EOS_Uic_DPic,  "EOS_Uic_DPic",  303,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Uic,       EOS_D,         EOS_Pic,       EOS_T_DPic,     EOS_NullTable,  EOS_Uic_DT,       EOS_T_DPic,       EOS_FALSE, "Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent)"},
  /*  33 */ {EOS_Uic_DAic,  "EOS_Uic_DAic",  303,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Uic,       EOS_D,         EOS_Aic,       EOS_NullTable,  EOS_NullTable,  EOS_Uic_DT,       EOS_T_DAic,       EOS_FALSE, "Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  34 */ {EOS_Uic_DSic,  "EOS_Uic_DSic",  303,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Uic,       EOS_D,         EOS_Sic,       EOS_NullTable,  EOS_NullTable,  EOS_Uic_DT,       EOS_T_DSic,       EOS_FALSE, "Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  35 */ {EOS_Aic_DT,    "EOS_Aic_DT",    303,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION_PLUS_COLD, EOS_Aic,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  36 */ {EOS_T_DAic,    "EOS_T_DAic",    303,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION_PLUS_COLD, EOS_T,         EOS_D,         EOS_Aic,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  37 */ {EOS_Aic_DPic,  "EOS_Aic_DPic",  303,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Aic,       EOS_D,         EOS_Pic,       EOS_NullTable,  EOS_NullTable,  EOS_Aic_DT,       EOS_T_DPic,       EOS_FALSE, "Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent)"},
  /*  38 */ {EOS_Aic_DUic,  "EOS_Aic_DUic",  303,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Aic,       EOS_D,         EOS_Uic,       EOS_NullTable,  EOS_NullTable,  EOS_Aic_DT,       EOS_T_DUic,       EOS_FALSE, "Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  39 */ {EOS_Aic_DSic,  "EOS_Aic_DSic",  303,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Aic,       EOS_D,         EOS_Sic,       EOS_NullTable,  EOS_NullTable,  EOS_Aic_DT,       EOS_T_DSic,       EOS_FALSE, "Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /*  40 */ {EOS_Sic_DT,    "EOS_Sic_DT",    303,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION_PLUS_COLD, EOS_Sic,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  41 */ {EOS_T_DSic,    "EOS_T_DSic",    303,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION_PLUS_COLD, EOS_T,         EOS_D,         EOS_Sic,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  42 */ {EOS_Sic_DPic,  "EOS_Sic_DPic",  303,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Sic,       EOS_D,         EOS_Pic,       EOS_NullTable,  EOS_NullTable,  EOS_Sic_DT,       EOS_T_DPic,       EOS_FALSE, "Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Pressure plus Cold Curve Pressure (GPa)-dependent)"},
  /*  43 */ {EOS_Sic_DUic,  "EOS_Sic_DUic",  303,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Sic,       EOS_D,         EOS_Uic,       EOS_NullTable,  EOS_NullTable,  EOS_Sic_DT,       EOS_T_DUic,       EOS_FALSE, "Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  44 */ {EOS_Sic_DAic,  "EOS_Sic_DAic",  303,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION_PLUS_COLD, EOS_Sic,       EOS_D,         EOS_Aic,       EOS_NullTable,  EOS_NullTable,  EOS_Sic_DT,       EOS_T_DAic,       EOS_FALSE, "Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  45 */ {EOS_Pe_DT,     "EOS_Pe_DT",     304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ELECTRON,      EOS_Pe,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pe_DT,      EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Electron Pressure (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  46 */ {EOS_T_DPe,     "EOS_T_DPe",     304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ELECTRON,      EOS_T,         EOS_D,         EOS_Pe,        EOS_T_DPe,      EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent)"},
  /*  47 */ {EOS_Pe_DUe,    "EOS_Pe_DUe",    304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Pe,        EOS_D,         EOS_Ue,        EOS_T_DUe,      EOS_Pe_DUe,     EOS_Pe_DT,        EOS_T_DUe,        EOS_FALSE, "Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  48 */ {EOS_Pe_DAe,    "EOS_Pe_DAe",    304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Pe,        EOS_D,         EOS_Ae,        EOS_NullTable,  EOS_NullTable,  EOS_Pe_DT,        EOS_T_DAe,        EOS_FALSE, "Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  49 */ {EOS_Pe_DSe,    "EOS_Pe_DSe",    304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Pe,        EOS_D,         EOS_Se,        EOS_NullTable,  EOS_NullTable,  EOS_Pe_DT,        EOS_T_DSe,        EOS_FALSE, "Electron Pressure (GPa) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  50 */ {EOS_Ue_DT,     "EOS_Ue_DT",     304,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ELECTRON,      EOS_Ue,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Pe_DT,      EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  51 */ {EOS_T_DUe,     "EOS_T_DUe",     304,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ELECTRON,      EOS_T,         EOS_D,         EOS_Ue,        EOS_T_DUe,      EOS_Pe_DUe,     EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  52 */ {EOS_Ue_DPe,    "EOS_Ue_DPe",    304,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ue,        EOS_D,         EOS_Pe,        EOS_T_DPe,      EOS_NullTable,  EOS_Ue_DT,        EOS_T_DPe,        EOS_FALSE, "Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent)"},
  /*  53 */ {EOS_Ue_DAe,    "EOS_Ue_DAe",    304,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ue,        EOS_D,         EOS_Ae,        EOS_NullTable,  EOS_NullTable,  EOS_Ue_DT,        EOS_T_DAe,        EOS_FALSE, "Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  54 */ {EOS_Ue_DSe,    "EOS_Ue_DSe",    304,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ue,        EOS_D,         EOS_Se,        EOS_NullTable,  EOS_NullTable,  EOS_Ue_DT,        EOS_T_DSe,        EOS_FALSE, "Electron Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  55 */ {EOS_Ae_DT,     "EOS_Ae_DT",     304,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ELECTRON,      EOS_Ae,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Electron Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  56 */ {EOS_T_DAe,     "EOS_T_DAe",     304,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ELECTRON,      EOS_T,         EOS_D,         EOS_Ae,        EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Electron Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  57 */ {EOS_Ae_DPe,    "EOS_Ae_DPe",    304,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ae,        EOS_D,         EOS_Pe,        EOS_NullTable,  EOS_NullTable,  EOS_Ae_DT,        EOS_T_DPe,        EOS_FALSE, "Electron Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent)"},
  /*  58 */ {EOS_Ae_DUe,    "EOS_Ae_DUe",    304,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ae,        EOS_D,         EOS_Ue,        EOS_NullTable,  EOS_NullTable,  EOS_Ae_DT,        EOS_T_DUe,        EOS_FALSE, "Electron Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  59 */ {EOS_Ae_DSe,    "EOS_Ae_DSe",    304,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Ae,        EOS_D,         EOS_Se,        EOS_NullTable,  EOS_NullTable,  EOS_Ae_DT,        EOS_T_DSe,        EOS_FALSE, "Electron Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /*  60 */ {EOS_Se_DT,     "EOS_Se_DT",     304,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ELECTRON,      EOS_Se,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  61 */ {EOS_T_DSe,     "EOS_T_DSe",     304,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ELECTRON,      EOS_T,         EOS_D,         EOS_Se,        EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Electron Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  62 */ {EOS_Se_DPe,    "EOS_Se_DPe",    304,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Se,        EOS_D,         EOS_Pe,        EOS_NullTable,  EOS_NullTable,  EOS_Se_DT,        EOS_T_DPe,        EOS_FALSE, "Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Pressure (GPa)-dependent)"},
  /*  63 */ {EOS_Se_DUe,    "EOS_Se_DUe",    304,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Se,        EOS_D,         EOS_Ue,        EOS_NullTable,  EOS_NullTable,  EOS_Se_DT,        EOS_T_DUe,        EOS_FALSE, "Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  64 */ {EOS_Se_DAe,    "EOS_Se_DAe",    304,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ELECTRON,      EOS_Se,        EOS_D,         EOS_Ae,        EOS_NullTable,  EOS_NullTable,  EOS_Se_DT,        EOS_T_DAe,        EOS_FALSE, "Electron Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Electron Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  65 */ {EOS_Piz_DT,    "EOS_Piz_DT",    305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION,           EOS_Piz,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  66 */ {EOS_T_DPiz,    "EOS_T_DPiz",    305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION,           EOS_T,         EOS_D,         EOS_Piz,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent)"},
  /*  67 */ {EOS_Piz_DUiz,  "EOS_Piz_DUiz",  305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Piz,       EOS_D,         EOS_Uiz,       EOS_NullTable,  EOS_NullTable,  EOS_Piz_DT,       EOS_T_DUiz,       EOS_FALSE, "Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  68 */ {EOS_Piz_DAiz,  "EOS_Piz_DAiz",  305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Piz,       EOS_D,         EOS_Aiz,       EOS_NullTable,  EOS_NullTable,  EOS_Piz_DT,       EOS_T_DAiz,       EOS_FALSE, "Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Specific-Free-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  69 */ {EOS_Piz_DSiz,  "EOS_Piz_DSiz",  305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Piz,       EOS_D,         EOS_Siz,       EOS_NullTable,  EOS_NullTable,  EOS_Piz_DT,       EOS_T_DSiz,       EOS_FALSE, "Ion Pressure Including Zero Point (GPa) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  70 */ {EOS_Uiz_DT,    "EOS_Uiz_DT",    305,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION,           EOS_Uiz,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  71 */ {EOS_T_DUiz,    "EOS_T_DUiz",    305,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION,           EOS_T,         EOS_D,         EOS_Uiz,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  72 */ {EOS_Uiz_DPiz,  "EOS_Uiz_DPiz",  305,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Uiz,       EOS_D,         EOS_Piz,       EOS_NullTable,  EOS_NullTable,  EOS_Uiz_DT,       EOS_T_DPiz,       EOS_FALSE, "Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent)"},
  /*  73 */ {EOS_Uiz_DAiz,  "EOS_Uiz_DAiz",  305,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Uiz,       EOS_D,         EOS_Aiz,       EOS_NullTable,  EOS_NullTable,  EOS_Uiz_DT,       EOS_T_DAiz,       EOS_FALSE, "Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Free-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  74 */ {EOS_Uiz_DSiz,  "EOS_Uiz_DSiz",  305,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Uiz,       EOS_D,         EOS_Siz,       EOS_NullTable,  EOS_NullTable,  EOS_Uiz_DT,       EOS_T_DSiz,       EOS_FALSE, "Ion Specific-Internal-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  75 */ {EOS_Aiz_DT,    "EOS_Aiz_DT",    305,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION,           EOS_Aiz,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Specific-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  76 */ {EOS_T_DAiz,    "EOS_T_DAiz",    305,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION,           EOS_T,         EOS_D,         EOS_Aiz,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Specific-Free-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  77 */ {EOS_Aiz_DPiz,  "EOS_Aiz_DPiz",  305,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Aiz,       EOS_D,         EOS_Piz,       EOS_NullTable,  EOS_NullTable,  EOS_Aiz_DT,       EOS_T_DPiz,       EOS_FALSE, "Ion Specific-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent)"},
  /*  78 */ {EOS_Aiz_DUiz,  "EOS_Aiz_DUiz",  305,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Aiz,       EOS_D,         EOS_Uiz,       EOS_NullTable,  EOS_NullTable,  EOS_Aiz_DT,       EOS_T_DUiz,       EOS_FALSE, "Ion Specific-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  79 */ {EOS_Aiz_DSiz,  "EOS_Aiz_DSiz",  305,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Aiz,       EOS_D,         EOS_Siz,       EOS_NullTable,  EOS_NullTable,  EOS_Aiz_DT,       EOS_T_DSiz,       EOS_FALSE, "Ion Specific-Free-Energy Including Zero Point (MJ/kg) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /*  80 */ {EOS_Siz_DT,    "EOS_Siz_DT",    305,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_ION,           EOS_Siz,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Temperature (K)-dependent)"},
  /*  81 */ {EOS_T_DSiz,    "EOS_T_DSiz",    305,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY2, EOS_ION,           EOS_T,         EOS_D,         EOS_Siz,       EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Density (Mg/m^3)- and Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)-dependent)"},
  /*  82 */ {EOS_Siz_DPiz,  "EOS_Siz_DPiz",  305,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Siz,       EOS_D,         EOS_Piz,       EOS_NullTable,  EOS_NullTable,  EOS_Siz_DT,       EOS_T_DPiz,       EOS_FALSE, "Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Pressure Including Zero Point (GPa)-dependent)"},
  /*  83 */ {EOS_Siz_DUiz,  "EOS_Siz_DUiz",  305,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Siz,       EOS_D,         EOS_Uiz,       EOS_NullTable,  EOS_NullTable,  EOS_Siz_DT,       EOS_T_DUiz,       EOS_FALSE, "Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Internal-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  84 */ {EOS_Siz_DAiz,  "EOS_Siz_DAiz",  305,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY4, EOS_ION,           EOS_Siz,       EOS_D,         EOS_Aiz,       EOS_NullTable,  EOS_NullTable,  EOS_Siz_DT,       EOS_T_DAiz,       EOS_FALSE, "Ion Pressure Including Zero Specific-Entropy (MJ/kg/K) (Density (Mg/m^3)- and Ion Specific-Free-Energy Including Zero Point (MJ/kg)-dependent)"},
  /*  85 */ {EOS_Pc_D,      "EOS_Pc_D",      306,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_COLD,          EOS_Pc,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_Pc_D,       EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Pressure Cold Curve (GPa) (Density (Mg/m^3)-dependent)"},
  /*  86 */ {EOS_Uc_D,      "EOS_Uc_D",      306,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_COLD,          EOS_Uc,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_Pc_D,       EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Specific-Internal-Energy Cold Curve (MJ/kg) (Density (Mg/m^3)-dependent)"},
  /*  87 */ {EOS_Ac_D,      "EOS_Ac_D",      306,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_COLD,          EOS_Ac,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Specific-Free-Energy Cold Curve (MJ/kg) (Density (Mg/m^3)-dependent)"},
  /*  88 */ {EOS_Pv_T,      "EOS_Pv_T",      401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Pv,        EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Vapor Pressure (GPa) (Temperature (K)-dependent)"},
  /*  89 */ {EOS_T_Pv,      "EOS_T_Pv",      401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Vapor Pressure (GPa)-dependent)"},
  /*  90 */ {EOS_Pv_Dv,     "EOS_Pv_Dv",     401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Dv,         EOS_FALSE, "Vapor Pressure (GPa) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /*  91 */ {EOS_Pv_Dls,    "EOS_Pv_Dls",    401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Dls,        EOS_FALSE, "Vapor Pressure (GPa) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /*  92 */ {EOS_Pv_Uv,     "EOS_Pv_Uv",     401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Uv,         EOS_FALSE, "Vapor Pressure (GPa) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  93 */ {EOS_Pv_Uls,    "EOS_Pv_Uls",    401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Uls,        EOS_FALSE, "Vapor Pressure (GPa) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /*  94 */ {EOS_Pv_Av,     "EOS_Pv_Av",     401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Av,         EOS_FALSE, "Vapor Pressure (GPa) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  95 */ {EOS_Pv_Als,    "EOS_Pv_Als",    401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Pv,        EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pv_T,         EOS_T_Als,        EOS_FALSE, "Vapor Pressure (GPa) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /*  96 */ {EOS_Dv_T,      "EOS_Dv_T",      401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Dv,        EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Temperature (K)-dependent)"},
  /*  97 */ {EOS_T_Dv,      "EOS_T_Dv",      401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /*  98 */ {EOS_Dv_Pv,     "EOS_Dv_Pv",     401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Pv,         EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Vapor Pressure (GPa)-dependent)"},
  /*  99 */ {EOS_Dv_Dls,    "EOS_Dv_Dls",    401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Dls,        EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 100 */ {EOS_Dv_Uv,     "EOS_Dv_Uv",     401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Uv,         EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 101 */ {EOS_Dv_Uls,    "EOS_Dv_Uls",    401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Uls,        EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 102 */ {EOS_Dv_Av,     "EOS_Dv_Av",     401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Av,         EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 103 */ {EOS_Dv_Als,    "EOS_Dv_Als",    401,      2,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dv,        EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dv_T,         EOS_T_Als,        EOS_FALSE, "Vapor Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 104 */ {EOS_Dls_T,     "EOS_Dls_T",     401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Dls,       EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Temperature (K)-dependent)"},
  /* 105 */ {EOS_T_Dls,     "EOS_T_Dls",     401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /* 106 */ {EOS_Dls_Pv,    "EOS_Dls_Pv",    401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Pv,         EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Pressure (GPa)-dependent)"},
  /* 107 */ {EOS_Dls_Dv,    "EOS_Dls_Dv",    401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Dv,         EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /* 108 */ {EOS_Dls_Uv,    "EOS_Dls_Uv",    401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Uv,         EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 109 */ {EOS_Dls_Uls,   "EOS_Dls_Uls",   401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Uls,        EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 110 */ {EOS_Dls_Av,    "EOS_Dls_Av",    401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Av,         EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 111 */ {EOS_Dls_Als,   "EOS_Dls_Als",   401,      3,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Dls,       EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Dls_T,        EOS_T_Als,        EOS_FALSE, "Liquid or Solid Density on coexistence line (Mg/m^3) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 112 */ {EOS_Uv_T,      "EOS_Uv_T",      401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Uv,        EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Temperature (K)-dependent)"},
  /* 113 */ {EOS_T_Uv,      "EOS_T_Uv",      401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 114 */ {EOS_Uv_Pv,     "EOS_Uv_Pv",     401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Pv,         EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent)"},
  /* 115 */ {EOS_Uv_Dv,     "EOS_Uv_Dv",     401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Dv,         EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /* 116 */ {EOS_Uv_Dls,    "EOS_Uv_Dls",    401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Dls,        EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /* 117 */ {EOS_Uv_Uls,    "EOS_Uv_Uls",    401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Uls,        EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 118 */ {EOS_Uv_Av,     "EOS_Uv_Av",     401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Av,         EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 119 */ {EOS_Uv_Als,    "EOS_Uv_Als",    401,      4,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uv,        EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uv_T,         EOS_T_Als,        EOS_FALSE, "Vapor Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 120 */ {EOS_Uls_T,     "EOS_Uls_T",     401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Uls,       EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Temperature (K)-dependent)"},
  /* 121 */ {EOS_T_Uls,     "EOS_T_Uls",     401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 122 */ {EOS_Uls_Pv,    "EOS_Uls_Pv",    401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Pv,         EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent)"},
  /* 123 */ {EOS_Uls_Dv,    "EOS_Uls_Dv",    401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Dv,         EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /* 124 */ {EOS_Uls_Dls,   "EOS_Uls_Dls",   401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Dls,        EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /* 125 */ {EOS_Uls_Uv,    "EOS_Uls_Uv",    401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Uv,         EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 126 */ {EOS_Uls_Av,    "EOS_Uls_Av",    401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Av,         EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 127 */ {EOS_Uls_Als,   "EOS_Uls_Als",   401,      5,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Uls,       EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uls_T,        EOS_T_Als,        EOS_FALSE, "Liquid or Solid Specific-Internal-Energy (MJ/kg) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 128 */ {EOS_Av_T,      "EOS_Av_T",      401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Av,        EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Temperature (K)-dependent)"},
  /* 129 */ {EOS_T_Av,      "EOS_T_Av",      401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 130 */ {EOS_Av_Pv,     "EOS_Av_Pv",     401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Pv,         EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent)"},
  /* 131 */ {EOS_Av_Dv,     "EOS_Av_Dv",     401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Dv,         EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},
  /* 132 */ {EOS_Av_Dls,    "EOS_Av_Dls",    401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Dls,        EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /* 133 */ {EOS_Av_Uv,     "EOS_Av_Uv",     401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Uv,         EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 134 */ {EOS_Av_Uls,    "EOS_Av_Uls",    401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Uls,        EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 135 */ {EOS_Av_Als,    "EOS_Av_Als",    401,      6,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Av,        EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Av_T,         EOS_T_Als,        EOS_FALSE, "Vapor Specific-Free-Energy (MJ/kg) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 136 */ {EOS_Als_T,     "EOS_Als_T",     401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_VAPORIZATION,  EOS_Als,       EOS_T,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Temperature (K)-dependent)"},
  /* 137 */ {EOS_T_Als,     "EOS_T_Als",     401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY1, EOS_VAPORIZATION,  EOS_T,         EOS_Als,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Temperature (K) (Liquid or Solid Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 138 */ {EOS_Als_Pv,    "EOS_Als_Pv",    401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Pv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Pv,         EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Vapor Pressure (GPa)-dependent)"},
  /* 139 */ {EOS_Als_Dv,    "EOS_Als_Dv",    401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Dv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Dv,         EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Vapor Density on coexistence line (Mg/m^3)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 140 */ {EOS_Als_Dls,   "EOS_Als_Dls",   401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Dls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Dls,        EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Liquid or Solid Density on coexistence line (Mg/m^3)-dependent)"},
  /* 141 */ {EOS_Als_Uv,    "EOS_Als_Uv",    401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Uv,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Uv,         EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Vapor Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 142 */ {EOS_Als_Uls,   "EOS_Als_Uls",   401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Uls,       EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Uls,        EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Liquid or Solid Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 143 */ {EOS_Als_Av,    "EOS_Als_Av",    401,      7,           EOS_RECORD_TYPE2,     EOS_CATEGORY3, EOS_VAPORIZATION,  EOS_Als,       EOS_Av,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Als_T,        EOS_T_Av,         EOS_FALSE, "Liquid or Solid Specific-Free-Energy (MJ/kg) (Vapor Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 144 */ {EOS_Tm_D,      "EOS_Tm_D",      411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Tm,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Melt Temperature (K) (Density (Mg/m^3)-dependent)"},
  /* 145 */ {EOS_D_Tm,      "EOS_D_Tm",      411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Tm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Melt Temperature (K)-dependent)"},
  /* 146 */ {EOS_Tm_Pm,     "EOS_Tm_Pm",     411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tm,        EOS_Pm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tm_D,         EOS_D_Pm,         EOS_FALSE, "Melt Temperature (K) (Melt Pressure (GPa)-dependent)"},
  /* 147 */ {EOS_Tm_Um,     "EOS_Tm_Um",     411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tm,        EOS_Um,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tm_D,         EOS_D_Um,         EOS_FALSE, "Melt Temperature (K) (Melt Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 148 */ {EOS_Tm_Am,     "EOS_Tm_Am",     411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tm,        EOS_Am,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tm_D,         EOS_D_Am,         EOS_FALSE, "Melt Temperature (K) (Melt Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 149 */ {EOS_Pm_D,      "EOS_Pm_D",      411,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Pm,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Melt Pressure (GPa) (Density (Mg/m^3)-dependent)"},
  /* 150 */ {EOS_D_Pm,      "EOS_D_Pm",      411,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Pm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Melt Pressure (GPa)-dependent)"},
  /* 151 */ {EOS_Pm_Tm,     "EOS_Pm_Tm",     411,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pm,        EOS_Tm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pm_D,         EOS_D_Tm,         EOS_FALSE, "Melt Pressure (GPa) (Melt Temperature (K)-dependent)"},
  /* 152 */ {EOS_Pm_Um,     "EOS_Pm_Um",     411,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pm,        EOS_Um,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pm_D,         EOS_D_Um,         EOS_FALSE, "Melt Pressure (GPa) (Melt Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 153 */ {EOS_Pm_Am,     "EOS_Pm_Am",     411,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pm,        EOS_Am,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pm_D,         EOS_D_Am,         EOS_FALSE, "Melt Pressure (GPa) (Melt Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 154 */ {EOS_Um_D,      "EOS_Um_D",      411,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Um,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Melt Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)-dependent)"},
  /* 155 */ {EOS_D_Um,      "EOS_D_Um",      411,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Um,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Melt Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 156 */ {EOS_Um_Tm,     "EOS_Um_Tm",     411,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Um,        EOS_Tm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Um_D,         EOS_D_Tm,         EOS_FALSE, "Melt Specific-Internal-Energy (MJ/kg) (Melt Temperature (K)-dependent)"},
  /* 157 */ {EOS_Um_Pm,     "EOS_Um_Pm",     411,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Um,        EOS_Pm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Um_D,         EOS_D_Pm,         EOS_FALSE, "Melt Specific-Internal-Energy (MJ/kg) (Melt Pressure (GPa)-dependent)"},
  /* 158 */ {EOS_Um_Am,     "EOS_Um_Am",     411,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Um,        EOS_Am,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Um_D,         EOS_D_Am,         EOS_FALSE, "Melt Specific-Internal-Energy (MJ/kg) (Melt Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 159 */ {EOS_Am_D,      "EOS_Am_D",      411,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Am,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Melt Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 160 */ {EOS_D_Am,      "EOS_D_Am",      411,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Am,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Melt Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 161 */ {EOS_Am_Tm,     "EOS_Am_Tm",     411,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Am,        EOS_Tm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Am_D,         EOS_D_Tm,         EOS_FALSE, "Melt Specific-Free-Energy (MJ/kg) (Melt Temperature (K)-dependent)"},
  /* 162 */ {EOS_Am_Pm,     "EOS_Am_Pm",     411,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Am,        EOS_Pm,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Am_D,         EOS_D_Pm,         EOS_FALSE, "Melt Specific-Free-Energy (MJ/kg) (Melt Pressure (GPa)-dependent)"},
  /* 163 */ {EOS_Am_Um,     "EOS_Am_Um",     411,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Am,        EOS_Um,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Am_D,         EOS_D_Um,         EOS_FALSE, "Melt Specific-Free-Energy (MJ/kg) (Melt Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 164 */ {EOS_Tf_D,      "EOS_Tf_D",      412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Tf,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Freeze Temperature (K) (Density (Mg/m^3)-dependent)"},
  /* 165 */ {EOS_D_Tf,      "EOS_D_Tf",      412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Tf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Freeze Temperature (K)-dependent)"},
  /* 166 */ {EOS_Tf_Pf,     "EOS_Tf_Pf",     412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tf,        EOS_Pf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tf_D,         EOS_D_Pf,         EOS_FALSE, "Freeze Temperature (K) (Freeze Pressure (GPa)-dependent)"},
  /* 167 */ {EOS_Tf_Uf,     "EOS_Tf_Uf",     412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tf,        EOS_Uf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tf_D,         EOS_D_Uf,         EOS_FALSE, "Freeze Temperature (K) (Freeze Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 168 */ {EOS_Tf_Af,     "EOS_Tf_Af",     412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Tf,        EOS_Af,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Tf_D,         EOS_D_Af,         EOS_FALSE, "Freeze Temperature (K) (Freeze Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 169 */ {EOS_Pf_D,      "EOS_Pf_D",      412,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Pf,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Freeze Pressure (GPa) (Density (Mg/m^3)-dependent)"},
  /* 170 */ {EOS_D_Pf,      "EOS_D_Pf",      412,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Pf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Freeze Pressure (GPa)-dependent)"},
  /* 171 */ {EOS_Pf_Tf,     "EOS_Pf_Tf",     412,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pf,        EOS_Tf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pf_D,         EOS_D_Tf,         EOS_FALSE, "Freeze Pressure (GPa) (Freeze Temperature (K)-dependent)"},
  /* 172 */ {EOS_Pf_Uf,     "EOS_Pf_Uf",     412,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pf,        EOS_Uf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pf_D,         EOS_D_Uf,         EOS_FALSE, "Freeze Pressure (GPa) (Freeze Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 173 */ {EOS_Pf_Af,     "EOS_Pf_Af",     412,      2,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Pf,        EOS_Af,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Pf_D,         EOS_D_Af,         EOS_FALSE, "Freeze Pressure (GPa) (Freeze Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 174 */ {EOS_Uf_D,      "EOS_Uf_D",      412,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Uf,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Freeze Specific-Internal-Energy (MJ/kg) (Density (Mg/m^3)-dependent)"},
  /* 175 */ {EOS_D_Uf,      "EOS_D_Uf",      412,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Uf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Freeze Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 176 */ {EOS_Uf_Tf,     "EOS_Uf_Tf",     412,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Uf,        EOS_Tf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uf_D,         EOS_D_Tf,         EOS_FALSE, "Freeze Specific-Internal-Energy (MJ/kg) (Freeze Temperature (K)-dependent)"},
  /* 177 */ {EOS_Uf_Pf,     "EOS_Uf_Pf",     412,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Uf,        EOS_Pf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uf_D,         EOS_D_Pf,         EOS_FALSE, "Freeze Specific-Internal-Energy (MJ/kg) (Freeze Pressure (GPa)-dependent)"},
  /* 178 */ {EOS_Uf_Af,     "EOS_Uf_Af",     412,      3,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Uf,        EOS_Af,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Uf_D,         EOS_D_Af,         EOS_FALSE, "Freeze Specific-Internal-Energy (MJ/kg) (Freeze Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 179 */ {EOS_Af_D,      "EOS_Af_D",      412,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_MELT,          EOS_Af,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Freeze Specific-Free-Energy (MJ/kg) (Density (Mg/m^3)-dependent)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 180 */ {EOS_D_Af,      "EOS_D_Af",      412,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_MELT,          EOS_D,         EOS_Af,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Freeze Specific-Free-Energy (MJ/kg)-dependent)"},
  /* 181 */ {EOS_Af_Tf,     "EOS_Af_Tf",     412,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Af,        EOS_Tf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Af_D,         EOS_D_Tf,         EOS_FALSE, "Freeze Specific-Free-Energy (MJ/kg) (Freeze Temperature (K)-dependent)"},
  /* 182 */ {EOS_Af_Pf,     "EOS_Af_Pf",     412,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Af,        EOS_Pf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Af_D,         EOS_D_Pf,         EOS_FALSE, "Freeze Specific-Free-Energy (MJ/kg) (Freeze Pressure (GPa)-dependent)"},
  /* 183 */ {EOS_Af_Uf,     "EOS_Af_Uf",     412,      4,           EOS_RECORD_TYPE1,     EOS_CATEGORY3, EOS_MELT,          EOS_Af,        EOS_Uf,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_Af_D,         EOS_D_Uf,         EOS_FALSE, "Freeze Specific-Free-Energy (MJ/kg) (Freeze Specific-Internal-Energy (MJ/kg)-dependent)"},
  /* 184 */ {EOS_Gs_D,      "EOS_Gs_D",      431,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_SHEAR_MODULUS, EOS_Gs,        EOS_D,         EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Shear Modulus (Gpa) (Density (Mg/m^3)-dependent)"},
  /* 185 */ {EOS_D_Gs,      "EOS_D_Gs",      431,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_SHEAR_MODULUS, EOS_D,         EOS_Gs,        EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Density (Mg/m^3) (Shear Modulus (Gpa)-dependent)"},
  /* 186 */ {EOS_Ogb,       "EOS_Ogb",       501,      1,           EOS_RECORD_TYPE3,     EOS_CATEGORY0, EOS_OPACITY,       EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Calculated versus Interpolated Opacity Grid Boundary"},
  /* 187 */ {EOS_Kr_DT,     "EOS_Kr_DT",     502,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_OPACITY,       EOS_Kr,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfo_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Rosseland Mean Opacity (cm^2/g) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 188 */ {EOS_Keo_DT,    "EOS_Keo_DT",    503,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_OPACITY,       EOS_Keo,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfo_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Electron Conductive Opacity (Opacity Model) (cm^2/g) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 189 */ {EOS_Zfo_DT,    "EOS_Zfo_DT",    504,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_OPACITY,       EOS_Zfo,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfo_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Mean Ion Charge (Opacity Model) (free electrons per atom) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 190 */ {EOS_Kp_DT,     "EOS_Kp_DT",     505,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_OPACITY,       EOS_Kp,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfo_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Planck Mean Opacity (cm^2/g) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 191 */ {EOS_Zfc_DT,    "EOS_Zfc_DT",    601,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_CONDUCTIVITY,  EOS_Zfc,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfc_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Mean Ion Charge (Conductivity Model) (free electrons per atom) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 192 */ {EOS_Kec_DT,    "EOS_Kec_DT",    602,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_CONDUCTIVITY,  EOS_Kec,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfc_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Electrical Conductivity (1/s) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 193 */ {EOS_Ktc_DT,    "EOS_Ktc_DT",    603,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_CONDUCTIVITY,  EOS_Ktc,       EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfc_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Thermal Conductivity (1/cm/s) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 194 */ {EOS_B_DT,      "EOS_B_DT",      604,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_CONDUCTIVITY,  EOS_B,         EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfc_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Thermoelectric Coefficient (1/cm^2/s) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 195 */ {EOS_Kc_DT,     "EOS_Kc_DT",     605,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_CONDUCTIVITY,  EOS_Kc,        EOS_D,         EOS_T,         EOS_NullTable,  EOS_Zfc_DT,     EOS_NullTable,    EOS_NullTable,    EOS_TRUE,  "Electron Conductive Opacity (Conductivity Model) (cm^2/g) (Density (Mg/m^3)- and Temperature (eV)-dependent)"},
  /* 196 */ {EOS_V_PtT,     "EOS_V_PtT",     301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY1, EOS_TOTAL,         EOS_V,         EOS_Pt,        EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_Ut_DT,        EOS_D_PtT,        EOS_FALSE, "Specific-Volume (m^3/Mg) (Total Pressure (GPa)- and Temperature (K)-dependent)"},
  /* 197 */ {EOS_301_DATA,  "EOS_301_DATA",  301,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 301 table data types (all subtables)"},
  /* 198 */ {EOS_303_DATA,  "EOS_303_DATA",  303,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 303 table data types (all subtables)"},
  /* 199 */ {EOS_304_DATA,  "EOS_304_DATA",  304,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 304 table data types (all subtables)"},

  /*         eosTableType   *eosTableType_s  tableNum  subTableNum  recordType            category       subCategory        depVar         indepVar1      indepVar2      eosTempBalFunc  eosPresBalFunc  eosTableTypeRef1  eosTableTypeRef2  logAxes    *eosTableName */
  /* 200 */ {EOS_305_DATA,  "EOS_305_DATA",  305,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 305 table data types (all subtables)"},
  /* 201 */ {EOS_306_DATA,  "EOS_306_DATA",  306,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 306 table data types (all subtables)"},
  /* 202 */ {EOS_401_DATA,  "EOS_401_DATA",  401,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 401 table data types (all subtables)"},
  /* 203 */ {EOS_411_DATA,  "EOS_411_DATA",  411,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 411 table data types (all subtables)"},
  /* 204 */ {EOS_412_DATA,  "EOS_412_DATA",  412,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 412 table data types (all subtables)"},
  /* 205 */ {EOS_431_DATA,  "EOS_431_DATA",  431,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 431 table data types (all subtables)"},
  /* 206 */ {EOS_501_DATA,  "EOS_501_DATA",  501,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 501 table data types (all subtables)"},
  /* 207 */ {EOS_502_DATA,  "EOS_502_DATA",  502,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 502 table data types (all subtables)"},
  /* 208 */ {EOS_503_DATA,  "EOS_503_DATA",  503,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 503 table data types (all subtables)"},
  /* 209 */ {EOS_504_DATA,  "EOS_504_DATA",  504,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 504 table data types (all subtables)"},
  /* 210 */ {EOS_505_DATA,  "EOS_505_DATA",  505,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 505 table data types (all subtables)"},
  /* 211 */ {EOS_601_DATA,  "EOS_601_DATA",  601,      1,           EOS_RECORD_TYPE2,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 601 table data types (all subtables)"},
  /* 212 */ {EOS_602_DATA,  "EOS_602_DATA",  602,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 602 table data types (all subtables)"},
  /* 213 */ {EOS_603_DATA,  "EOS_603_DATA",  603,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 603 table data types (all subtables)"},
  /* 214 */ {EOS_604_DATA,  "EOS_604_DATA",  604,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 604 table data types (all subtables)"},
  /* 215 */ {EOS_605_DATA,  "EOS_605_DATA",  605,      1,           EOS_RECORD_TYPE1,     EOS_CATEGORY0, EOS_INFORMATION,   EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Generic internal 605 table data types (all subtables)"},
  /* 216 */ {EOS_M_DT,      "EOS_M_DT",      321,      1,           EOS_RECORD_TYPE6,     EOS_CATEGORY0, EOS_MASS_FRACTION, EOS_M,         EOS_D,         EOS_T,         EOS_NullTable,  EOS_NullTable,  EOS_NullTable,    EOS_NullTable,    EOS_FALSE, "Mass Fraction (Density- and Temperature-dependent)"}

};
eos_TableData *eos_TableList = _eos_TableList;
EOS_INTEGER MAX_TYPES = sizeof(_eos_TableList)/sizeof(_eos_TableList[0]);
EOS_INTEGER *eos_TableListReverseMap = NULL;

/* Variable definition constants */
eos_VarTypeData _eos_VarList[] = {
  {0,       "UNKNOWN", "-",   "unknown"},
  {EOS_B,   "EOS_B",   "B",   "Thermoelectric Coefficient (1/cm^2/s)"},
  {EOS_Ac,  "EOS_Ac",  "Ac",  "Specific-Free-Energy Cold Curve (MJ/kg)"},
  {EOS_Ae,  "EOS_Ae",  "Ae",  "Electron Specific-Free-Energy (MJ/kg)"},
  {EOS_Af,  "EOS_Af",  "Af",  "Freeze Specific-Free-Energy (MJ/kg)"},
  {EOS_Aiz, "EOS_Aiz", "Aiz", "Ion Specific-Free-Energy Including Zero Point (MJ/kg)"},
  {EOS_Aic, "EOS_Aic", "Aic", "Ion Specific-Free-Energy plus Cold Curve Specific-Free-Energy (MJ/kg)"},
  {EOS_Als, "EOS_Als", "Als", "Liquid or Solid Specific-Free-Energy (MJ/kg)"},
  {EOS_Am,  "EOS_Am",  "Am",  "Melt Specific-Free-Energy (MJ/kg)"},
  {EOS_Gs,  "EOS_Gs",  "Gs",  "Shear Modulus (Gpa)"},
  {EOS_At,  "EOS_At",  "At",  "Total Specific-Free-Energy (MJ/kg)"},
  {EOS_Av,  "EOS_Av",  "Av",  "Vapor Specific-Free-Energy (MJ/kg)"},
  {EOS_Kc,  "EOS_Kc",  "Kc",  "Electron Conductive Opacity (Conductivity Model) (cm^2/g)"},
  {EOS_Keo, "EOS_Keo", "Keo", "Electron Conductive Opacity (Opacity Model) (cm^2/g)"},
  {EOS_Kec, "EOS_Kec", "Kec", "Electrical Conductivity (1/s)"},
  {EOS_Kp,  "EOS_Kp",  "Kp",  "Planck Mean Opacity (cm^2/g)"},
  {EOS_Kr,  "EOS_Kr",  "Kr",  "Rosseland Mean Opacity (cm^2/g)"},
  {EOS_Ktc, "EOS_Ktc", "Ktc", "Thermal Conductivity (1/cm/s)"},
  {EOS_Pc,  "EOS_Pc",  "Pc",  "Pressure Cold Curve (GPa)"},
  {EOS_Pe,  "EOS_Pe",  "Pe",  "Electron Pressure (GPa)"},
  {EOS_Pf,  "EOS_Pf",  "Pf",  "Freeze Pressure (GPa)"},
  {EOS_Piz, "EOS_Piz", "Piz", "Ion Pressure Including Zero Point (GPa)"},
  {EOS_Pic, "EOS_Pic", "Pic", "Ion Pressure plus Cold Curve Pressure (GPa)"},
  {EOS_Pm,  "EOS_Pm",  "Pm",  "Melt Pressure (GPa)"},
  {EOS_Pt,  "EOS_Pt",  "Pt",  "Total Pressure (GPa)"},
  {EOS_Pv,  "EOS_Pv",  "Pv",  "Vapor Pressure (GPa)"},
  {EOS_D,   "EOS_D",   "D",   "Density (Mg/m^3)"},
  {EOS_Dls, "EOS_Dls", "Dls", "Liquid or Solid Density on coexistence line (Mg/m^3)"},
  {EOS_Dv,  "EOS_Dv",  "Dv",  "Vapor Density on coexistence line (Mg/m^3)"},
  {EOS_Se,  "EOS_Se",  "Se",  "Electron Specific-Entropy (MJ/kg/K)"},
  {EOS_Siz, "EOS_Siz", "Siz", "Ion Pressure Including Zero Specific-Entropy (MJ/kg/K)"},
  {EOS_Sic, "EOS_Sic", "Sic", "Ion Pressure plus Cold Curve Specific-Entropy (MJ/kg/K)"},
  {EOS_St,  "EOS_St",  "St",  "Total Specific-Entropy (MJ/kg/K)"},
  {EOS_T,   "EOS_T",   "T",   "Temperature (K)"},
  {EOS_Tf,  "EOS_Tf",  "Tf",  "Freeze Temperature (K)"},
  {EOS_Tm,  "EOS_Tm",  "Tm",  "Melt Temperature (K)"},
  {EOS_Uc,  "EOS_Uc",  "Uc",  "Specific-Internal-Energy Cold Curve (MJ/kg)"},
  {EOS_Ue,  "EOS_Ue",  "Ue",  "Electron Specific-Internal-Energy (MJ/kg)"},
  {EOS_Uf,  "EOS_Uf",  "Uf",  "Freeze Specific-Internal-Energy (MJ/kg)"},
  {EOS_Uiz, "EOS_Uiz", "Uiz", "Ion Specific-Internal-Energy Including Zero Point (MJ/kg)"},
  {EOS_Uic, "EOS_Uic", "Uic", "Ion Specific-Internal-Energy plus Cold Curve Specific-Internal-Energy (MJ/kg)"},
  {EOS_Uls, "EOS_Uls", "Uls", "Liquid or Solid Specific-Internal-Energy (MJ/kg)"},
  {EOS_Um,  "EOS_Um",  "Um",  "Melt Specific-Internal-Energy (MJ/kg)"},
  {EOS_Ut,  "EOS_Ut",  "Ut",  "Total Specific-Internal-Energy (MJ/kg)"},
  {EOS_Uv,  "EOS_Uv",  "Uv",  "Vapor Specific-Internal-Energy (MJ/kg)"},
  {EOS_Zfc, "EOS_Zfc", "Zfc", "Mean Ion Charge (Conductivity Model) (free electrons per atom)"},
  {EOS_Zfo, "EOS_Zfo", "Zfo", "Mean Ion Charge (Opacity Model) (free electrons per atom)"},
  {EOS_V,   "EOS_V",   "V",   "Volume (m^3)"},
  {EOS_M,   "EOS_M",   "M",   "Mass Fraction"}
};
eos_VarTypeData *eos_VarList = _eos_VarList;
EOS_INTEGER MAX_VARS = sizeof(_eos_VarList)/sizeof(_eos_VarList[0]);

EOS_INTEGER _eos_OptionFlags[EOS_TOTAL_TABLE_OPTIONS] = {
  EOS_DUMP_DATA,                /* include public option flags */
  EOS_APPEND_DATA,
  EOS_INSERT_DATA,
  EOS_MONOTONIC_IN_X,
  EOS_MONOTONIC_IN_Y,
  EOS_SMOOTH,
  EOS_SPLIT_COWAN,
  EOS_SPLIT_FORCED,
  EOS_SPLIT_IDEAL_GAS,
  EOS_SPLIT_NUM_PROP,
  EOS_CHECK_ARGS,
  EOS_LINEAR,
  EOS_RATIONAL,
  EOS_X_CONVERT,
  EOS_Y_CONVERT,
  EOS_F_CONVERT,
  EOS_PT_SMOOTHING,
  EOS_ADJUST_VAP_PRES,
  EOS_USE_CUSTOM_INTERP,
  EOS_SAVE_SPECIES_DATA,
  EOS_CALC_FREE_ENERGY,
  EOS_CREATE_TZERO,
  EOS_USE_TAYLOR_FIT,
  EOS_USE_MAXWELL_TABLE,
  EOS_DISCONTINUOUS_DERIVATIVES,
  EOS_XY_PASSTHRU,
  EOS_XY_MODIFY,
  EOS_INVERT_AT_SETUP,
  EOS_NORMAL_DERIVATIVES,
  EOS_DISABLE_GHOST_NODES,      /* include private option flags after all others */
  EOS_DEBUG_PRINT,              /* include private option flags after all others */
  EOS_ALLOW_ALL_INFO_ITEMS      /* include private option flags after all others */
};
EOS_INTEGER *eos_OptionFlags = _eos_OptionFlags;

EOS_CHAR* _eos_OptionFlags_str[EOS_TOTAL_TABLE_OPTIONS] = {
  "EOS_DUMP_DATA",                /* include public option flags */
  "EOS_APPEND_DATA",
  "EOS_INSERT_DATA",
  "EOS_MONOTONIC_IN_X",
  "EOS_MONOTONIC_IN_Y",
  "EOS_SMOOTH",
  "EOS_SPLIT_COWAN",
  "EOS_SPLIT_FORCED",
  "EOS_SPLIT_IDEAL_GAS",
  "EOS_SPLIT_NUM_PROP",
  "EOS_CHECK_ARGS",
  "EOS_LINEAR",
  "EOS_RATIONAL",
  "EOS_X_CONVERT",
  "EOS_Y_CONVERT",
  "EOS_F_CONVERT",
  "EOS_PT_SMOOTHING",
  "EOS_ADJUST_VAP_PRES",
  "EOS_USE_CUSTOM_INTERP",
  "EOS_SAVE_SPECIES_DATA",
  "EOS_CALC_FREE_ENERGY",
  "EOS_CREATE_TZERO",
  "EOS_USE_TAYLOR_FIT",
  "EOS_USE_MAXWELL_TABLE",
  "EOS_DISCONTINUOUS_DERIVATIVES",
  "EOS_XY_PASSTHRU",
  "EOS_XY_MODIFY",
  "EOS_INVERT_AT_SETUP",
  "EOS_NORMAL_DERIVATIVES",
  "EOS_DISABLE_GHOST_NODES",      /* include private option flags after all others */
  "EOS_DEBUG_PRINT",              /* include private option flags after all others */
  "EOS_ALLOW_ALL_INFO_ITEMS"      /* include private option flags after all others */
};
EOS_CHAR** eos_OptionFlags_str = _eos_OptionFlags_str;

/* note: this depends heavily on the order of option flag definition in eos_types.h 
   Also change if new options are added. Might need to rethink this later. Olga 3/1/04 */

#else //if not _EOS_INTERNAL_
extern eos_TableData *eos_TableList;
extern EOS_INTEGER *eos_TableListReverseMap;
extern eos_VarTypeData *eos_VarList;
extern eos_DataMap gEosDataMap;
extern eos_Interpolation gEosInterpolation;
extern EOS_CHAR **gCustomErrorMsg;
extern eos_Option *eos_DefaultTableOptions;
extern EOS_INTEGER *eos_OptionFlags;
extern EOS_CHAR** eos_OptionFlags_str;
#endif

/* The EOS_IS_PRESSURE_VARIABLE macro determines if 't' is one of the following pressure variables:
   EOS_Pc, EOS_Pe, EOS_Pf, EOS_Piz, EOS_Pic, EOS_Pm, EOS_Pt, EOS_Pv */
#define EOS_IS_PRESSURE_VARIABLE(t) (t==EOS_Pc   || \
				     t==EOS_Pe   || \
				     t==EOS_Pf   || \
				     t==EOS_Piz  || \
				     t==EOS_Pic  || \
				     t==EOS_Pm   || \
				     t==EOS_Pt   || \
				     t==EOS_Pv)

/* The EOS_IS_TYPE_DEPRECATED macro is used to identify data types that are deprecated.
   Once the data types are actually deleted, they need to be removed from this macro. */
#define  EOS_IS_TYPE_DEPRECATED(o) (EOS_FALSE)

#define EOS_IS_DERIVATIVETYPE_INTERPOLATION_OPTION(o) (o==EOS_NORMAL_DERIVATIVES || \
						       o==EOS_DISCONTINUOUS_DERIVATIVES || \
						       o==EOS_XY_PASSTHRU || \
						       o==EOS_XY_MODIFY)
#define EOS_IS_INTERPOLATIONTYPE_OPTION(o) (o==EOS_LINEAR ||		\
                                            o==EOS_RATIONAL ||		\
                                            o==EOS_USE_CUSTOM_INTERP || \
					    o==EOS_SAVE_SPECIES_DATA ||	\
                                            o==EOS_DISABLE_GHOST_NODES)
#define EOS_IS_INTERPOLATION_OPTION(o) (EOS_IS_DERIVATIVETYPE_INTERPOLATION_OPTION(o) || \
                                        EOS_IS_INTERPOLATIONTYPE_OPTION(o))

#define EOS_IS_LOADING_OPTION(o) (o == EOS_PT_SMOOTHING || \
				  o == EOS_ADJUST_VAP_PRES || \
                                  o == EOS_INSERT_DATA || \
                                  o == EOS_MONOTONIC_IN_X || \
                                  o == EOS_MONOTONIC_IN_Y || \
                                  o == EOS_SMOOTH || \
                                  o == EOS_SPLIT_COWAN || \
                                  o == EOS_SPLIT_FORCED || \
                                  o == EOS_SPLIT_IDEAL_GAS || \
                                  o == EOS_SPLIT_NUM_PROP || \
                                  o == EOS_CALC_FREE_ENERGY || \
                                  o == EOS_CREATE_TZERO || \
                                  o == EOS_USE_TAYLOR_FIT || \
				  o == EOS_USE_MAXWELL_TABLE || \
				  o == EOS_INVERT_AT_SETUP)

#define EOS_IS_GENERAL_OPTION(o) (o == EOS_DUMP_DATA || \
                                  o == EOS_APPEND_DATA || \
                                  o == EOS_CHECK_ARGS || \
                                  o == EOS_X_CONVERT || \
                                  o == EOS_Y_CONVERT || \
                                  o == EOS_F_CONVERT || \
                                  o == EOS_DEBUG_PRINT || \
				  o == EOS_ALLOW_ALL_INFO_ITEMS)

#define EOS_OPTION_FLAG_TO_INDEX(o) (((o) >= EOS_MIN_PRIVATE_OPTION_FLAG_VALUE) \
                                     ? ((o) - EOS_MIN_PRIVATE_OPTION_FLAG_VALUE + EOS_NUM_TABLE_OPTIONS) \
                                     : ((o) - EOS_MIN_OPTION_FLAG_VALUE))
#define EOS_LOADING_OPTION_FLAG_TO_INDEX(f)  ((f == EOS_INSERT_DATA)? 0 : \
					     ((f == EOS_MONOTONIC_IN_X)? 1 : \
					     ((f == EOS_MONOTONIC_IN_Y)? 2 : \
					     ((f == EOS_SMOOTH)? 3 :	\
					     ((f == EOS_SPLIT_COWAN)? 4 : \
					     ((f == EOS_SPLIT_FORCED)? 5 : \
					     ((f == EOS_SPLIT_IDEAL_GAS)? 6 : \
					     ((f == EOS_SPLIT_NUM_PROP)? 7: \
					     ((f == EOS_PT_SMOOTHING)? 8: \
					     ((f == EOS_ADJUST_VAP_PRES)? 9: \
					     ((f == EOS_CALC_FREE_ENERGY)? 10: \
					     ((f == EOS_CREATE_TZERO)? 11: \
					     ((f == EOS_USE_TAYLOR_FIT)? 12 : \
					     ((f == EOS_USE_MAXWELL_TABLE)? 13 : \
					     ((f == EOS_INVERT_AT_SETUP)? 14 : 15)))))))))))))))
#define EOS_GENERAL_OPTION_FLAG_TO_INDEX(o) ((o == EOS_DUMP_DATA)? 0 :	\
					    ((o == EOS_APPEND_DATA)? 1 : \
					    ((o == EOS_CHECK_ARGS)? 2 : \
					    ((o == EOS_X_CONVERT)? 3 : \
					    ((o == EOS_Y_CONVERT)? 4 : \
					    ((o == EOS_F_CONVERT)? 5 : \
					    ((o == EOS_DEBUG_PRINT)? 6 : \
					    ((o == EOS_ALLOW_ALL_INFO_ITEMS)? 7 : 8))))))))
#define EOS_GENERAL_INDEX_TO_OPTION_FLAG(i) ((i == 0)? EOS_DUMP_DATA : \
					    ((i == 1)? EOS_APPEND_DATA: \
                                            ((i == 2)? EOS_CHECK_ARGS : \
                                            ((i == 3)? EOS_X_CONVERT : \
                                            ((i == 4)? EOS_Y_CONVERT : \
                                            ((i == 5)? EOS_F_CONVERT : \
                                            ((i == 6)? EOS_DEBUG_PRINT : \
                                            ((i == 7)? EOS_ALLOW_ALL_INFO_ITEMS : EOS_ALLOW_ALL_INFO_ITEMS))))))))

#define EOS_ALLOW_CAT0_ONLY_INFO_ITEM(i) (((i == EOS_R_Array) || \
					   (i == EOS_T_Array) || \
					   (i == EOS_F_Array) || \
					   (i == EOS_NR) || \
					   (i == EOS_NT) || \
					   (i == EOS_NUM_PHASES) || \
					   (i == EOS_Rmin) || \
					   (i == EOS_Rmax) || \
					   (i == EOS_Tmin) || \
					   (i == EOS_Tmax) || \
					   (i == EOS_Fmin) || \
					   (i == EOS_Fmax) || \
					   (i == EOS_NT401) || \
					   (i == EOS_P401) || \
					   (i == EOS_T401) || \
					   (i == EOS_RG401) || \
					   (i == EOS_RL401) || \
					   (i == EOS_EG401) || \
					   (i == EOS_EL401)) ? EOS_TRUE : EOS_FALSE \
					 )

/* var orders for conversion factors */
#define X_Y_F 0
#define X_U_F 1
#define X_Y_U 2
#define U_Y_F 3
#define X_F   4
#define U_F   5
#define X_U   6

#define MIN(x,y) (((x)<(y))? (x) : (y))
#define MAX(x,y) (((x)>(y))? (x) : (y))
#define ABS(x)  (((x)>0)? (x) : -1*(x))
#define FABS(x) (((x)>0)? (x) : (EOS_REAL)-1.0*(x))
#define SIGN(x) (((x)>=0)? 1: -1)        /* determine the sign of a number */
#define FLOOR(x) (MAX(fabs(x),TINY_D) * (EOS_REAL) SIGN(x))     /* impose minimum value on x while retaining its sign */
#define BOUND(mn,mx,x) MAX(MIN(x,mx),mn)        /* impose minimum and maximum values on x */

#ifdef _EOS_INTERNAL_

/*!
 * The following function is used to build a reverse map, eos_TableListReverseMap[], for eos_TableList[]
 */
void _eos_SetTableListReverseMap(void) {

  if ( ! eos_TableListReverseMap ) {
    EOS_INTEGER i;
    EOS_INTEGER max_idx = -1;

    /* find largest table type constant value */
    for (i = 0; i < MAX_TYPES; i++)
      max_idx = MAX(max_idx, eos_TableList[i].eosTableType);

    /* allocate reverse map
     * (must include max_idx+1 elements to account for all values eosTableType=0 through eosTableType=max_idx) */
    eos_TableListReverseMap = malloc((max_idx+1) * sizeof(EOS_INTEGER));

    /* initialize reverse map */
    for (i = 0; i <= max_idx; i++)
      eos_TableListReverseMap[i] = -1;

    /* populate reverse map */
    for (i = 0; i < MAX_TYPES; i++)
      eos_TableListReverseMap[eos_TableList[i].eosTableType] = i;
  }
}

#else /* ! _EOS_INTERNAL_ */

EOS_INTEGER __eos_TypeToListIndex__(EOS_INTEGER t, EOS_CHAR *caller);
void _eos_SetTableListReverseMap(void);

extern EOS_INTEGER MAX_TYPES;
extern EOS_INTEGER MAX_VARS;

#endif /* _EOS_INTERNAL_ */

//#define EOS_TYPE_TO_LIST_INDEX __eos_TypeToListIndex__
#define EOS_TYPE_TO_LIST_INDEX(t,c) eos_TableListReverseMap[t]

#define EOS_TYPE_TO_STRING(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_STRING")].eosTableType_s)
#define EOS_TYPE_TO_TAB_NUM(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_TAB_NUM")].tableNum)
#define EOS_TYPE_TO_SUB_TAB_NUM(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_SUB_TAB_NUM")].subTableNum)
#define EOS_TYPE_TO_RECORD_TYPE(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_RECORD_TYPE")].recordType)
#define EOS_CATEGORY(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_CATEGORY")].category)
#define EOS_SUBCATEGORY(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_DATA_CLASSIFICATION")].subCategory)
#define EOS_EOS_TABLE_TYPE_REF1(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_EOS_TABLE_TYPE_REF1")].eosTableTypeRef1)
#define EOS_EOS_TABLE_TYPE_REF2(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_EOS_TABLE_TYPE_REF2")].eosTableTypeRef2)
#define EOS_TYPE_TO_LOG_AXES(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_LOG_AXES")].logAxes)
#define EOS_TYPE_TO_TAB_NAME(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_TAB_NAME")].eosTableName)
#define EOS_TYPE_TO_DEP_VAR(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_DEP_VAR")].depVar)
#define EOS_TYPE_TO_INDEP_VAR1(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_INDEP_VAR1")].indepVar1)
#define EOS_TYPE_TO_INDEP_VAR2(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_INDEP_VAR2")].indepVar2)
#define EOS_TYPE_TO_PRES_BAL_FUNC(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_PRES_BAL_FUNC")].eosPresBalFunc)
#define EOS_TYPE_TO_TEMP_BAL_FUNC(t) (eos_TableList[EOS_TYPE_TO_LIST_INDEX(t, "EOS_TYPE_TO_TEMP_BAL_FUNC")].eosTempBalFunc)
#define EOS_IS_ONE_DIM_TYPE(t) (((EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable && EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) || \
				 (EOS_TYPE_TO_INDEP_VAR1 (dataType) == EOS_NullTable && EOS_TYPE_TO_INDEP_VAR2 (dataType) != EOS_NullTable)) \
				? EOS_TRUE : EOS_FALSE)

/* DBL_MAX_10_EXP and DBL_MIN_10_EXP are defined in float.h as defined by POSIX:
      DBL_MAX_10_EXP = Maximum integer such that 10 raised to that number is a representable
                       floating-point number.
      DBL_MIN_10_EXP = Minimum negative integer such that 10 raised to that number is a
                       representable floating-point number.
*/
#define EOS_CHECK_PRODUCT(a,b) (((log10(FABS(FLOOR(a))) + log10(FABS(FLOOR(b)))) < (DBL_MAX_10_EXP - 2) && \
                                (log10(FABS(FLOOR(a))) + log10(FABS(FLOOR(b)))) > (DBL_MIN_10_EXP)) \
				? EOS_TRUE : EOS_FALSE) /* determine if product is a valid number */
#define EOS_IS_PRODUCT_LT_MIN(a,b) ((log10(FABS(FLOOR(a))) + log10(FABS(FLOOR(b)))) > (DBL_MIN_10_EXP) \
				    ? EOS_TRUE : EOS_FALSE) /* determine if product is too small */
#define EOS_IS_PRODUCT_GT_MAX(a,b) ((log10(FABS(FLOOR(a))) + log10(FABS(FLOOR(b)))) < (DBL_MAX_10_EXP - 2) \
				    ? EOS_TRUE : EOS_FALSE) /* determine if product is too big */

typedef struct
{                               /* global values used by Newton-Bisection or returned by eos_GetMachinePrecision */
  EOS_INTEGER gotMachinePrecision;      /* Flag indicating if eos_GetMachinePrecision has been called */
  EOS_REAL eps;                 /* The smallest positive floating-point number such
                                   that (1.0 + eps) > 1, is referred to as the
                                   "floating-point precision." */
  EOS_REAL epsneg;              /* The smallest positive floating-point number such
                                   that (1.0 - eps) < 1, is another way of defining
                                   the "floating-point precision." */
  EOS_REAL maxErr;              /* Newton convergence tolerence */
  EOS_INTEGER maxIter;          /* Newton maximum iterations */
} NRmachPrecData;

#define IS_EXTRAPOLATED(e) ((e) == EOS_INTERP_EXTRAPOLATED || (e) == EOS_xHi_yOk || (e) == EOS_xHi_yLo || (e) == EOS_xHi_yHi || (e) == EOS_xLo_yOk || (e) == EOS_xLo_yLo || (e) == EOS_xLo_yHi || (e) == EOS_xOk_yHi || (e) == EOS_xOk_yLo)
#define EXTRAP_ERROR_TO_TEXT(i) ((i==EOS_xHi_yHi) ? "xHi_yHi" : \
                                 (i==EOS_xHi_yOk) ? "xHi_yOk" : \
                                 (i==EOS_xHi_yLo) ? "xHi_yLo" : \
                                 (i==EOS_xOk_yLo) ? "xOk_yLo" : \
                                 (i==EOS_xLo_yLo) ? "xLo_yLo" : \
                                 (i==EOS_xLo_yOk) ? "xLo_yOk" : \
                                 (i==EOS_xLo_yHi) ? "xLo_yHi" : \
                                 (i==EOS_xOk_yHi) ? "xOk_yHi" : \
                                 (i==EOS_OK     ) ? "           " : \
                                 "UNDEFINED"                      \
                                )
#endif /*EOS_TYPES_INTERNAL_H */
