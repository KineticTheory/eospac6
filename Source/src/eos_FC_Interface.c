/*********************************************************************
 * Class Name : eos_Interface
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <string.h>
#include "eos_wrappers.h"
#include "eos_types_internal.h"
#include "eos_Data.h"
#include "eos_DataMap.h"

#include "eos_Utils.h"
#include "eos_SesUtils.h"

/************************************************************************
 *
 * If needed, define macros that are used to enable/disable double-
 * precision rounding according to IEEE 754.
 *
 * The following problem description is taken from section 8.6 of
 * An Introduction to GCC - for the GNU compilers gcc and g++
 * by Brian J. Gough, foreword by Richard M. Stallman
 * Paperback (6"x9"), 124 pages
 * ISBN 0954161793
 * http://www.network-theory.co.uk/docs/gccintro/index.html
 *
 * The IEEE-754 standard defines the bit-level behavior of floating-point
 * arithmetic operations on all modern processors. This allows numerical
 * programs to be ported between different platforms with identical results,
 * in principle. In practice, there are often minor variations caused by
 * differences in the order of operations (depending on the compiler and
 * optimization level) but these are generally not significant.
 * However, more noticeable discrepancies can be seen when porting numerical
 * programs between x86 systems and other platforms, because the the x87
 * floating point unit (FPU) on x86 processors computes results using extended
 * precision internally (the values being converted to double precision only
 * when they are stored to memory). In contrast, processors such as SPARC,
 * PA-RISC, Alpha, MIPS and POWER/PowerPC work with native double-precision
 * values throughout. The differences between these implementations
 * lead to changes in rounding and underflow/overflow behavior, because
 * intermediate values have a greater relative precision and exponent range
 * when computed in extended precision. In particular, comparisons
 * involving extended precision values may fail where the equivalent double
 * precision values would compare equal.
 * To avoid these incompatibilities, the x87 FPU also offers a hardware
 * double-precision rounding mode. In this mode the results of each extended-
 * precision floating-point operation are rounded to double precision in the
 * floating-point registers by the FPU. It is important to note that the
 * rounding only affects the precision, not the exponent range, so the result
 * is a hybrid double-precision format with an extended range of exponents. 
 * On BSD systems such as FreeBSD, NetBSD and OpenBSD, the hardware double-
 * precision rounding mode is the default, giving the greatest compatibility
 * with native double precision platforms. On x86 GNU/Linux systems the
 * default mode is extended precision (with the aim of providing increased
 * accuracy). To enable the double-precision rounding mode it is necessary
 * to override the default setting on per-process basis using the FLDCW
 * "floating-point load control-word" machine instruction.
 *
 * Simple macros, which apply the solution described above, are defined
 * below for various platforms:
 *
 * x86_SetPrecision
 * x86_RestorePrecision
 *
 * These macros were taken from the following location:
 * http://www.fortran-2000.com/ArnaudRecipes/CompilerTricks.html#x86_PrecMode
 *
 ************************************************************************/
#if defined(_WIN32)
# include <float.h>
# ifdef SINGLE
#  define _CW_PREC PC_24
# else
#  define _CW_PREC PC_53
# endif
# define x86_SetPrecision \
  unsigned int _oldcw_pc; \
  _oldcw_pc = _control87(0,0) & MCW_PC; \
  _control87(_CW_PREC,MCW_PC)
# define x86_RestorePrecision \
  _control87(_oldcw_pc,MCW_PC)

#elif defined(i386) && defined(__FreeBSD__)
# include <floatingpoint.h>
# ifdef SINGLE
#  define _CW_PREC PC_PS
# else
#  define _CW_PREC PC_PD
# endif
# define x86_SetPrecision \
  fp_prec_t _oldcw_pc; \
  _oldcw_pc = fpgetprec(); \
  fpsetprec(_CW_PREC) \
# define x86_RestorePrecision \
  fpsetprec(_oldcw_pc)

#elif defined(i386) && defined(__GNUC__)
# ifdef SINGLE
#  define _CW_PREC _FPU_SINGLE
# else
#  define _CW_PREC _FPU_DOUBLE
# endif
# if defined(linux)
#  include <fpu_control.h>
# else
#  define _FPU_EXTENDED 0x300
#  define _FPU_DOUBLE   0x200
#  define _FPU_SINGLE   0x0
#  define _FPU_GETCW(cw) __asm__ __volatile__("fnstcw %0" : "=m" (*&cw))
#  define _FPU_SETCW(cw) __asm__ __volatile__("fldcw %0" : : "m" (*&cw))
#  define fpu_control_t unsigned int
# endif
# define x86_SetPrecision \
  fpu_control_t _oldcw_pc; \
  { fpu_control_t _cw; \
    _FPU_GETCW(_cw); \
    _oldcw_pc = _cw & _FPU_EXTENDED; \
    _cw = (_cw & ~_FPU_EXTENDED) | _CW_PREC; \
    _FPU_SETCW(_cw); \
  }
# define x86_RestorePrecision \
  { fpu_control_t _cw; \
    _FPU_GETCW(_cw); \
    _cw = (_cw & ~_FPU_EXTENDED) | _oldcw_pc; \
    _FPU_SETCW(_cw); \
  }

#else
# define x86_SetPrecision
# define x86_RestorePrecision
#endif

/* UNTESTED, see "ieee_flags" for an alternative */
#if 0 && defined(i386) && defined(__sun) && \
    (defined(__SUNPRO_C) || defined(__SUNPRO_CC)) 
# include <fenv.h>
# ifdef SINGLE
#  define _CW_PREC FE_FLTPREC
# else
#  define _CW_PREC FE_DBLPREC
# endif
# define x86_SetPrecision \
  int _oldcw_pc; \
  _oldcw_pc = fegetprec(); \
  fesetprec(_CW_PREC) \
# define x86_RestorePrecision \
  fesetprec(_oldcw_pc)
#endif


#ifdef __cplusplus
extern "C"
{
#endif

  static EOS_BOOLEAN firstTime = EOS_TRUE;
  static EOS_INTEGER numInterpolationOptions = 0;

  /*************************************************************************
   *
   * Function Name: eos_CheckExtrap
   *
   * Description:
   * If the error code returned by eos_Interp or eos_Mix is
   * EOS_INTERP_EXTRAPOLATED, this routine allows the user to determine
   * which (x,y) pairs caused extrapolation and in which direction (high or
   * low), it occurred.
   *
   * Input Values:
   * EOS_INTEGER tableHandle
   * EOS_INTEGER nXYPairs
   * EOS_REAL    xVals[nXYPairs]
   * EOS_REAL    yVals[nXYPairs]
   *
   * Returned Values:
   * EOS_INTEGER xyBounds[nXYPairs]
   * EOS_INTEGER errorCode
   * 
   *************************************************************************/
  void FUNC_INTER eos_CheckExtrap (EOS_INTEGER *tableHandle,
                                   EOS_INTEGER *nXYPairs,
                                   EOS_REAL *xVals,
                                   EOS_REAL *yVals,
                                   EOS_INTEGER *xyBounds,
                                   EOS_INTEGER *errorCode)
  {
    EOS_REAL xconv, yconv, *X, *Y;
    EOS_INTEGER i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;
/*     eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode); */

    if (*nXYPairs <= 0)
    {
      *errorCode = EOS_INVALID_NXYPAIRS;
      return;
    }

    X = (EOS_REAL *) malloc (*nXYPairs * sizeof (EOS_REAL));
    Y = (EOS_REAL *) malloc (*nXYPairs * sizeof (EOS_REAL));

    /* process x-conversion option */
    xconv =
      eos_getRealOptionFromTableHandle (*tableHandle, EOS_X_CONVERT,
                                        errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      /* update error code with table handle */
      *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
      return;
    }

    /* process y-conversion option */
    yconv =
      eos_getRealOptionFromTableHandle (*tableHandle, EOS_Y_CONVERT,
                                        errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      /* update error code with table handle */
      *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
      return;
    }

    /* now apply supplied conversion factors to convert input to Sesame default units */
    for (i = 0; i < *nXYPairs; i++) {
      X[i] = xVals[i] / FLOOR (xconv);
      Y[i] = yVals[i] / FLOOR (yconv);
    }

    eos_CheckExtrapEosInterpolation (&gEosInterpolation, *tableHandle,
                                     *nXYPairs, X, Y, xyBounds, errorCode);

    if (X)
      EOS_FREE (X);
    if (Y)
      EOS_FREE (Y);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_CreateTables
   *
   * Description:
   * The eos_CreateTables function allocates all memory to store the
   * specified data tables.
   *
   * Input Values:
   * EOS_INTEGER nTables
   * EOS_INTEGER tableType[nTables]
   * EOS_INTEGER matID[nTables]
   *
   * Returned Value:
   * EOS_INTEGER *tableHandles[nTables]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_CreateTables (EOS_INTEGER *nTables,
                                    EOS_INTEGER tableType[],
                                    EOS_INTEGER matID[],
                                    EOS_INTEGER tableHandles[],
                                    EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (firstTime) {

      numInterpolationOptions = 0;
      for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
        eos_DefaultTableOptions[i].optionFlag = eos_OptionFlags[i];

        if (EOS_IS_INTERPOLATION_OPTION
            (eos_DefaultTableOptions[i].optionFlag))
          numInterpolationOptions++;

	/* initialize union values */
	eos_DefaultTableOptions[i].optionValue.ival = (EOS_INTEGER) 0;
	eos_DefaultTableOptions[i].optionValue.rval = (EOS_REAL) 0.0;
	eos_DefaultTableOptions[i].optionValue.bval = (EOS_BOOLEAN) EOS_FALSE;

	/* set defaults */
        switch (eos_DefaultTableOptions[i].optionFlag) {
        case EOS_INSERT_DATA:
	  eos_DefaultTableOptions[i].optionType = _INTEGER_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.ival = (EOS_INTEGER) 0;
          break;
        case EOS_ADJUST_VAP_PRES:
	  eos_DefaultTableOptions[i].optionType = _REAL_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.rval = (EOS_REAL) 0.0;
          break;
        case EOS_X_CONVERT:
        case EOS_Y_CONVERT:
        case EOS_F_CONVERT:
	  eos_DefaultTableOptions[i].optionType = _REAL_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.rval = (EOS_REAL) 1;
          break;
        default:
	  eos_DefaultTableOptions[i].optionType = _BOOLEAN_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.bval =
            (EOS_BOOLEAN) EOS_FALSE;
        }
      }

      for (i = 0; i < EOS_MAX_ERROR_CODE_VALUE - EOS_MIN_ERROR_CODE_VALUE + 1;
           i++) {
        gCustomErrorMsg[i] = NULL;
      }

      eos_ConstructEosDataMap (&gEosDataMap);
      eos_ConstructEosInterpolation (&gEosInterpolation);

      _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

      firstTime = EOS_FALSE;

    }

    eos_CreateTablesEosDataMap (&gEosDataMap, *nTables, tableType, matID,
                                tableHandles, EOS_TRUE, 1, EOS_FALSE, -1, errorCode);

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_DestroyAll
   *
   * Description:
   * The eos_Clenup function releases all memory and cache used internaly in EOSPAC
   * called at the END of execution.
   *
   * Input Values:
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_DestroyAll (EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    *errorCode = EOS_OK;

    for (i = 0; i < EOS_MAX_ERROR_CODE_VALUE - EOS_MIN_ERROR_CODE_VALUE + 1;
         i++) {
      if (gCustomErrorMsg[i])
        EOS_FREE (gCustomErrorMsg[i]);
    }

    eos_DestroyEosDataMap (&gEosDataMap);
    eos_DestroyEosInterpolation (&gEosInterpolation);
    *errorCode = eos_SesCleanFileCache();
    firstTime = EOS_TRUE;

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }


  /*************************************************************************
   *
   * Function Name: eos_DestroyTables
   *
   * Description:
   * The eos_DestroyTables function releases all memory associated with the
   * specified data tables.
   *
   * Input Values:
   * EOS_INTEGER nTables
   * EOS_INTEGER tableHandles[nTables]
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_DestroyTables (EOS_INTEGER *nTables,
                                     EOS_INTEGER tableHandles[],
                                     EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;
    for (i = 0; i < *nTables; i++)
      eos_HandleErrorEosDataMap (&gEosDataMap, tableHandles[i], *errorCode);

    eos_DestroyTablesEosDataMap (&gEosDataMap, *nTables, tableHandles,
                                 errorCode);

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetErrorCode
   *
   * Description:
   * The eos_GetErrorCode function returns the most recent error code for a
   * specified tableHandle an error is indicated to have occured.
   *
   * Input Value:
   * INTEGER tableHandle
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/

  void FUNC_INTER eos_GetErrorCode (EOS_INTEGER *tableHandle,
                                    EOS_INTEGER *errorCode)
  {

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    *errorCode = EOS_OK;
    *errorCode = eos_GetErrorCodeEosDataMap (&gEosDataMap, *tableHandle);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetErrorMessage
   *
   * Input Value:
   * EOS_INTEGER errorCode
   *
   * Returned Value:
   * EOS_CHAR    *errorMsg  - max message length is EOS_MaxErrMsgLen
   *
   *************************************************************************/
  void FUNC_INTER eos_GetErrorMessage (EOS_INTEGER *errorCode,
                                       EOS_CHAR errorMsg[EOS_MaxErrMsgLen])
  {
    int i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    for (i = 0; i < EOS_MaxErrMsgLen; i++) {
      errorMsg[i] = ' ';
    }
    sprintf (errorMsg, "%s", eos_GetErrorMsg (*errorCode));

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetPackedTables
   *
   * Description:
   * The eos_GetPackedTables function fills a character array with the
   * specified data table's data. This is the function to extract the data
   * tables from EOSPAC to allow multithreaded codes to share the data.
   * Before calling this routine the host code must call
   * eos_GetPackedTablesSize to determine packedTablesSize, the total number
   * of bytes required to contain the data associated with the specified
   * data tables, allowing the host code to allocate adequate storage.
   *
   * Input Values:
   * EOS_INTEGER nTables
   * EOS_INTEGER tableHandles[nTables]
   *
   * Returned Values:
   * EOS_CHAR    *packedTables[packedTablesSize]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_GetPackedTables (EOS_INTEGER *nTables,
                                       EOS_INTEGER tableHandles[],
                                       EOS_CHAR *packedTables,
                                       EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i, j, sizePacked = 0;
    EOS_BOOLEAN optVal;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;
    for (i = 0; i < *nTables; i++)
      eos_HandleErrorEosDataMap (&gEosDataMap, tableHandles[i], *errorCode);

    /* now pack the tables */
    eos_GetPackedTablesEosDataMap (&gEosDataMap, *nTables, tableHandles,
                                   packedTables, &sizePacked, errorCode);

    /* now pack interpolation options! */
    for (i = 0; i < *nTables; i++) {
      for (j = 0; j < EOS_TOTAL_TABLE_OPTIONS; j++) {
        if (EOS_IS_INTERPOLATION_OPTION
            (eos_DefaultTableOptions[j].optionFlag)) {
          eos_GetOptionEosInterpolation (&gEosInterpolation, tableHandles[i],
                                         eos_DefaultTableOptions[j].optionFlag,
					 &optVal, errorCode);
          if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
            optVal = EOS_FALSE; /* pack false for now for invalid options */
          memcpy (packedTables + sizePacked,
                  &(eos_DefaultTableOptions[j].optionFlag),
                  sizeof (EOS_INTEGER));

          sizePacked += sizeof (EOS_INTEGER);
          memcpy (packedTables + sizePacked, &(optVal), sizeof (EOS_BOOLEAN));

          sizePacked += sizeof (EOS_BOOLEAN);
        }
      }
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetPackedTablesSize
   *
   * Description:
   * The eos_GetPackedTablesSize function calculates the total number of
   * bytes required to contain the data associated with the specified data
   * tables. This routine is used with the eos_GetPackedTables routine.
   *
   * Input Values:
   * EOS_INTEGER nTables
   * EOS_INTEGER tableHandles[nTables]
   *
   * Returned Values:
   * EOS_INTEGER *packedTablesSize
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_GetPackedTablesSize (EOS_INTEGER *nTables,
                                           EOS_INTEGER tableHandles[],
                                           EOS_INTEGER *packedTablesSize,
                                           EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i, sizePacked = 0;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;
    for (i = 0; i < *nTables; i++)
      eos_HandleErrorEosDataMap (&gEosDataMap, tableHandles[i], *errorCode);

    /* initialize variables */
    *packedTablesSize = 0;

    /* get the size of the packed tables */
    eos_GetPackedTablesEosDataMap (&gEosDataMap, *nTables, tableHandles,
                                   NULL, &sizePacked, errorCode);
    *packedTablesSize += sizePacked;

    /* now get the size of packed  interpolation options! */
    for (i = 0; i < *nTables; i++) {
      sizePacked += numInterpolationOptions * sizeof (EOS_INTEGER);
      sizePacked += numInterpolationOptions * sizeof (EOS_BOOLEAN);
    }
    *packedTablesSize += sizePacked;

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetMetaData
   *
   * Description:
   * The eos_GetMetaData routine is returns a string representation of a specified
   * internal meta datum. Unlike most other Data routines, this routine does not
   * reference a specific data table handle. Information is requested by passing a
   * pair of parameters to the routine that returns the corresponding meta data as
   * a character string.
   *
   * Input Values:
   * EOS_INTEGER infoItem
   * EOS_INTEGER infoItemCategory
   *
   * Returned Values:
   * EOS_CHAR    infoStr[EOS_META_DATA_STRLEN]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/

  void FUNC_INTER eos_GetMetaData (EOS_INTEGER *infoItem, EOS_INTEGER *infoItemCategory,
				   EOS_CHAR *infoStr, EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i, j;
    EOS_CHAR *s;
    EOS_INTEGER eos_TableListReverseMap_size;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (! eos_TableListReverseMap) _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

    eos_TableListReverseMap_size = sizeof(eos_TableListReverseMap)/sizeof(eos_TableListReverseMap[0]);

    if ( *infoItem > EOS_NullTable
	 && *infoItem <= eos_TableListReverseMap_size
	 && eos_TableListReverseMap[*infoItem] < 0 ) {
      /* currently infoItem value is limited to a valid table type */
      *errorCode = EOS_INVALID_INFO_FLAG;
      return;
    }

    switch (*infoItemCategory) {

    case EOS_Table_Type:
      sprintf( infoStr, "%s", EOS_TYPE_TO_STRING(*infoItem) );
      break;

    case EOS_Table_Name:
      sprintf( infoStr, "%s", EOS_TYPE_TO_TAB_NAME(*infoItem) );
      break;

    case EOS_Dependent_Var:
      j = EOS_TYPE_TO_DEP_VAR(*infoItem);
      i = get_VarListIndex(j);
      s = get_VarStr(j, 1);
      sprintf( infoStr, "%s", s);
      break;

    case EOS_Independent_Var1:
      j = EOS_TYPE_TO_INDEP_VAR1(*infoItem);
      i = get_VarListIndex(j);
      s = get_VarStr(j, 1);
      sprintf( infoStr, "%s", s);
      break;

    case EOS_Independent_Var2:
      j = EOS_TYPE_TO_INDEP_VAR2(*infoItem);
      i = get_VarListIndex(j);
      s = get_VarStr(j, 1);
      sprintf( infoStr, "%s", s);
      break;

    case EOS_Sesame_Table_List:
      if ( *infoItem == EOS_Comment ) {
	sprintf( infoStr, "101, 102, ... 199" );
      }
      else if ( *infoItem == EOS_NullTable ) {
	sprintf( infoStr, "-" );
      }
      else {
	sprintf( infoStr, "%d", EOS_TYPE_TO_TAB_NUM(*infoItem) );
      }
      break;

    case EOS_Pressure_Balance_Table_Type:
      i = EOS_TYPE_TO_PRES_BAL_FUNC(*infoItem);
      if ( i == EOS_NullTable ) {
	sprintf( infoStr, "-" );
      }
      else {
	sprintf( infoStr, "%s", EOS_TYPE_TO_STRING(i) );
      }
      break;

    case EOS_Temperature_Balance_Table_Type:
      i = EOS_TYPE_TO_TEMP_BAL_FUNC(*infoItem);
      if ( i == EOS_NullTable ) {
	sprintf( infoStr, "-" );
      }
      else {
	sprintf( infoStr, "%s", EOS_TYPE_TO_STRING(i) );
      }
      break;

    default:
      *errorCode = EOS_INVALID_INFO_CATEGORY_FLAG;
      break;

    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetTableMetaData
   *
   * Description:
   * The eos_GetTableMetaData routine is returns a string representation of a specified
   * meta datum associated with a table handle. Information is requested by passing a
   * table handle and a requested info item flag to the routine. The corresponding meta
   * data is returned as a character string.
   *
   * Input Values:
   * EOS_INTEGER tableHandle
   * EOS_INTEGER infoItem
   *
   * Returned Values:
   * EOS_CHAR    infoStr[EOS_META_DATA_STRLEN]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/

  void FUNC_INTER eos_GetTableMetaData (EOS_INTEGER *tableHandle, EOS_INTEGER *infoItem,
					EOS_CHAR *infoStr, EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i, j;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (!eos_IsHandleValidEosDataMap (&gEosDataMap, *tableHandle)) {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, EOS_INVALID_TABLE_HANDLE);
      return;
    }

    /* initialize output buffer to blanks; terminate with NULL */
    memset(infoStr, '\0', EOS_META_DATA_STRLEN-1);
    infoStr[0] = '\0';

    switch (*infoItem) {

    case EOS_Table_Type:
    case EOS_Table_Name:
    case EOS_Dependent_Var:
    case EOS_Independent_Var1:
    case EOS_Independent_Var2:
      /* get table type */
      i = gEosDataMap.tableHandlesMap[*tableHandle];
      j = gEosDataMap.tableTypes[i];

      /* get meta data for associated table type */
      eos_GetMetaData (&j, infoItem, infoStr, errorCode);
      break;

    default:
      eos_GetTableMetaDataEosDataMap (&gEosDataMap, *tableHandle, *infoItem, infoStr, errorCode);
      break;

    }

    /* initialize output buffer to blanks; terminate with NULL */
    //memset(infoStr[MIN(strlen(infoStr)+1, EOS_META_DATA_STRLEN-1)], ' ', EOS_META_DATA_STRLEN-1);

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetTableInfo
   *
   * Description:
   * The eos_GetTableInfo routine is returns the values of requested
   * information about data table members. Unlike most other Data routines,
   * this routine works on only a single data table. Information is
   * requested by passing a list of parameters to the routine that returns
   * the requested information in the same order.
   *
   * Input Values:
   * EOS_INTEGER *tableHandle
   * EOS_INTEGER *numInfoItems
   * EOS_INTEGER infoItems[numInfoItems]
   *
   * Returned Values:
   * EOS_REAL    infoVals[numInfoItems]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/

  void FUNC_INTER eos_GetTableInfo (EOS_INTEGER *tableHandle, EOS_INTEGER *numInfoItems,
				    EOS_INTEGER *infoItems, EOS_REAL *infoVals,
				    EOS_INTEGER *errorCode)
  {
    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    eos_GetTableInfoEosDataMap (&gEosDataMap, *tableHandle, *numInfoItems,
                                infoItems, infoVals, errorCode);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

 /*************************************************************************
  *
  * Function eos_GetTableCmnts
  * Description:
  * The eos_GetTableCmnts routine returns the comments available about the
  * requested data table. This routine works on only a single data table.
  * Before calling this routine the host code must call eos_GetTableInfo to
  * find out the length of the comments, lenCmnts, allowing the host code
  * to allocate adequate storage.
  * Works on only a type 4 data table.
  *
  * Input Values:
  * EOS_INTEGER *tableHandle
  * EOS_CHAR *cmntStr  - ALLOCATED string to hold table comments, the assumption
  *                      is made that the user has called eos_GettableInfo() for the
  *                      same table handle, and have allocated the comment string to
  *                      be the size returned from eos_GettableInfo().
  *
  * Returned Values:
  * EOS_INTEGER *errorCode
  *
  *************************************************************************/
  void FUNC_INTER eos_GetTableCmnts (EOS_INTEGER *tableHandle, EOS_CHAR *cmntStr,
				     EOS_INTEGER *errorCode)
  {
    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    eos_GetTableCmntsEosDataMap (&gEosDataMap, *tableHandle, cmntStr, errorCode);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_Interpolate
   *
   * Description:
   * The eos_Interpolate function provides interpolated values for a single
   * material using a table handle associated with data stored within a
   * data table.
   * Before calling this routine the host code may need to call eos_SetOption
   * so the desired interpolation options can be changed from the documented
   * defaults.
   *
   *	The input arguments are:
   *		EOS_INTEGER tableHandle	  This is a scalar EOS_INTEGER handle to a particular data table. 
   *		EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.
   *		EOS_REAL xVals[nXYPairs]  array of the primary independent variable values to use during interpolation. 
   *		EOS_REAL yVals[nXYPairs]  array of the secondary independent variable values to use during interpolation. 
   *
   *	The output arguments are:
   *		EOS_REAL fVals[nXYPairs]  array of the interpolated data corresponding to x and y. 
   *		EOS_REAL dFx[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to x. 
   *		EOS_REAL dFy[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to y. 
   *		EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
   *
   *************************************************************************/
  void FUNC_INTER eos_Interpolate (EOS_INTEGER *tableHandle,
                                   EOS_INTEGER *nXYPairs,
                                   EOS_REAL *xVals,
                                   EOS_REAL *yVals,
                                   EOS_REAL *fVals,
                                   EOS_REAL *dFx,
                                   EOS_REAL *dFy, EOS_INTEGER *errorCode)
  {

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (*nXYPairs <= 0) {
      *errorCode = EOS_INVALID_NXYPAIRS;
      return;
    }

    /* Interpolate the data */
    eos_InterpolateEosInterpolation (&gEosInterpolation, *tableHandle,
                                     *nXYPairs, xVals, yVals, fVals, dFx, dFy,
                                     NULL, errorCode);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_LoadTables
   *
   * Description:
   * The eos_LoadTables fills a collection of data tables 
   * with the requested data tables from SESAME.
   * Before calling this routine the host code may need to call eos_SetOption
   * so the desired set up options can be changed from the documented
   * defaults.
   *
   * Input Values:
   * EOS_INTEGER nTables
   * EOS_INTEGER tableType[nTables]
   * EOS_INTEGER matID[nTables]
   *
   * Returned Values:
   * EOS_INTEGER *tableHandles[nTables]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/

  void FUNC_INTER eos_LoadTables (EOS_INTEGER *nTables,
                                  EOS_INTEGER tableHandles[],
                                  EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode for each table handle, unless it's EOS_MATERIAL_NOT_FOUND. */
    *errorCode = EOS_OK;
    for (i = 0; i < *nTables; i++) {
      if (eos_GetStandardErrorCodeFromCustomErrorCode(gEosDataMap.errorCodes[tableHandles[i]]) != EOS_MATERIAL_NOT_FOUND)
	eos_HandleErrorEosDataMap (&gEosDataMap, tableHandles[i], *errorCode);
    }

    eos_LoadTablesEosDataMap (&gEosDataMap, *nTables, tableHandles, errorCode);

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_Mix
   *
   * Description:
   * The mixed material interpolation uses established EOSPAC data tables
   * and returns interpolated data of mixed materials requested by the host
   * code. This routine is the typical way to generate mixed material data
   * using the data tables' member data tables. The data tables to be mixed
   * must be of the same table type. An error code is returned if the table
   * type is not valid for mixing (EOS_NullTable, EOS_Info, etc.). The
   * eos_Mix routine will provide interpolated values corresponding to
   * mixtures of materials in pressure (or pressure and temperature)
   * equilibrium.
   * Before calling this routine the host code may need to call eos_SetOption
   * so the desired interpolation and/or mixing options can be changed from
   * the documented defaults.
   *
   * Input Values:
   * EOS_INTEGER *nTables
   * EOS_INTEGER tableHandles[nTables]
   * EOS_INTEGER *nXYPairs
   * EOS_REAL concInMix[nTables]
   * EOS_REAL    xVals[nXYPairs]
   * EOS_REAL    yVals[nXYPairs]
   *
   * Returned Values:
   * EOS_REAL    fVals[nXYPairs]
   * EOS_REAL    dFx[nXYPairs]
   * EOS_REAL    dFy[nXYPairs]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_Mix (EOS_INTEGER *nTables, EOS_INTEGER *tableHandles,
                           EOS_INTEGER *nXYPairs, EOS_REAL *concInMix,
                           EOS_REAL *xVals, EOS_REAL *yVals, EOS_REAL *fVals,
                           EOS_REAL *dFx, EOS_REAL *dFy,
                           EOS_INTEGER *errorCode)
  {
    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (*nXYPairs <= 0) {
      *errorCode = EOS_INVALID_NXYPAIRS;
      return;
    }

    /* Interpolate the mixture data */
    eos_MixInterpolate (&gEosInterpolation, *nTables, tableHandles, *nXYPairs,
                        concInMix, xVals, yVals, fVals, dFx, dFy, errorCode);

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_SetPackedTables
   *
   * Description:
   * The eos_SetPackedTables function fills the specified data tables with
   * data tables stored as a character array. Typically this is used to
   * insert the data tables into EOSPAC after a multithread code has
   * shared the data tables extracted by eos_GetPackedTables.
   *
   * Input Values:
   * EOS_INTEGER *nTables
   * EOS_INTEGER tableHandles[nTables]
   * EOS_INTEGER *packedTablesSize
   *
   * Returned Value:
   * EOS_CHAR    packedTables[packedTablesSize]
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_SetPackedTables (EOS_INTEGER *nTables,
                                       EOS_INTEGER *packedTablesSize,
                                       EOS_CHAR *packedTables,
                                       EOS_INTEGER tableHandles[],
                                       EOS_INTEGER *errorCode)
  {
    EOS_INTEGER i, j, sizeUnpacked = 0, optFlag;
    EOS_BOOLEAN optValue;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (firstTime) {
      numInterpolationOptions = 0;
      for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++) {
        eos_DefaultTableOptions[i].optionFlag = eos_OptionFlags[i];

        if (EOS_IS_INTERPOLATION_OPTION
            (eos_DefaultTableOptions[i].optionFlag))
          numInterpolationOptions++;

        switch (eos_DefaultTableOptions[i].optionFlag) {
        case EOS_INSERT_DATA:
	  eos_DefaultTableOptions[i].optionType = _INTEGER_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.ival = (EOS_INTEGER) 0;
          break;
        case EOS_ADJUST_VAP_PRES:
	  eos_DefaultTableOptions[i].optionType = _REAL_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.rval = (EOS_REAL) 0.0;
          break;
        case EOS_X_CONVERT:
        case EOS_Y_CONVERT:
        case EOS_F_CONVERT:
	  eos_DefaultTableOptions[i].optionType = _REAL_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.rval = (EOS_REAL) 1;
          break;
        default:
	  eos_DefaultTableOptions[i].optionType = _BOOLEAN_TYPE_INDEX;
          eos_DefaultTableOptions[i].optionValue.bval =
            (EOS_BOOLEAN) EOS_FALSE;
        }
      }

      for (i = 0; i < EOS_MAX_ERROR_CODE_VALUE - EOS_MIN_ERROR_CODE_VALUE + 1;
           i++) {
        gCustomErrorMsg[i] = NULL;
      }

      eos_ConstructEosDataMap (&gEosDataMap);
      eos_ConstructEosInterpolation (&gEosInterpolation);

      _eos_SetTableListReverseMap(); /* build reverse lookup for eos_TableList[] */

      firstTime = EOS_FALSE;

    }

    /* now unpack the tables */
    eos_SetPackedTablesEosDataMap (&gEosDataMap, *nTables, tableHandles,
                                   packedTables, &sizeUnpacked, errorCode);

    /* now unpack interpolation options! */
    for (i = 0; i < *nTables; i++) {

      for (j = 0; j < numInterpolationOptions; j++) {
        memcpy (&optFlag, packedTables + sizeUnpacked, sizeof (EOS_INTEGER));

        sizeUnpacked += sizeof (EOS_INTEGER);
        memcpy (&(optValue), packedTables + sizeUnpacked,
                sizeof (EOS_BOOLEAN));

        sizeUnpacked += sizeof (EOS_BOOLEAN);
        if (optValue)
	  eos_SetOptionEosInterpolation (&gEosInterpolation, tableHandles[i],
					 optFlag, optValue, errorCode);
        if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        }                       /* ignore invalid options for now */
      }
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_SetOption
   *
   * Description:
   * The eos_SetOption function allows the host code to set specific
   * options to be applied to data associated with a given tableHandle.
   *
   * Input Values:
   * EOS_INTEGER tableHandle
   * EOS_INTEGER tableOption
   * EOS_REAL    tableOptionVal  optional, only for numeric options 
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_SetOption (EOS_INTEGER *tableHandle,
                                 const EOS_INTEGER *tableOption,
                                 const EOS_REAL *tableOptionVal,
                                 EOS_INTEGER *errorCode)
  {

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;

    if (EOS_IS_INTERPOLATION_OPTION (*tableOption))
      eos_SetOptionEosInterpolation (&gEosInterpolation, *tableHandle,
                                     *tableOption, EOS_TRUE, errorCode);
    else
      eos_SetOptionEosDataMap (&gEosDataMap, *tableHandle, *tableOption,
                               *tableOptionVal, -1, errorCode);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_ResetOption
   *
   * Description:
   * The eos_SetOption function allows the host code to reset to default specific
   * options to be applied to data associated with a given tableHandle.
   *
   * Input Values:
   * EOS_INTEGER tableHandle
   * EOS_INTEGER tableOption
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_ResetOption (EOS_INTEGER *tableHandle,
                                   const EOS_INTEGER *tableOption,
                                   EOS_INTEGER *errorCode)
  {

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    /* Initialize errorCode */
    *errorCode = EOS_OK;
    eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);

    if (EOS_IS_INTERPOLATION_OPTION (*tableOption))
      eos_ResetOptionEosInterpolation (&gEosInterpolation, *tableHandle,
                                       *tableOption, errorCode);
    else
      eos_ResetOptionEosDataMap (&gEosDataMap, *tableHandle, *tableOption, -1,
                                 errorCode);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */
  }

  /*************************************************************************
   *
   * Function Name: eos_GetMaxDataFileNameLength
   *
   * Description:
   * The eos_GetMaxDataFileNameLength function allows the host code to query
   * the maximum file name length allowed on the current machine.
   *
   * Returned Value:
   * EOS_INTEGER *max_length
   *
   *************************************************************************/
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 255
#endif
  void FUNC_INTER eos_GetMaxDataFileNameLength (EOS_INTEGER *max_length)
  {
    *max_length = PATH_MAX;
  }

  /*************************************************************************
   *
   * Function Name: eos_SetDataFileName
   *
   * Description:
   * The eos_SetDataFileName function allows the host code to set a specific
   * file name for the specified table handle.
   *
   * Input Values:
   * EOS_INTEGER *tableHandle -- EOS_INTEGER : scalar table handle
   * EOS_INTEGER *matID       -- EOS_INTEGER : scalar Sesame material ID
   * EOS_INTEGER *tableType   -- EOS_INTEGER : scalar table type
   * EOS_CHAR    *fileName    -- EOS_CHAR*   : character string containing file name
   *
   * Returned Value:
   * EOS_INTEGER *errorCode
   *
   *************************************************************************/
  void FUNC_INTER eos_SetDataFileName (EOS_INTEGER *tableHandle,
				       EOS_INTEGER *matID,
				       EOS_INTEGER *tableType,
				       EOS_CHAR *fileName,
				       EOS_INTEGER *errorCode,
				       long int fileName_len)
  {
    EOS_INTEGER i, tableHandle0;
    eos_Data *eosData0 = NULL, *eosData = NULL;
    EOS_CHAR *_fn = NULL;
    EOS_BOOLEAN fileIndexChanged = EOS_FALSE, createNewObject = EOS_FALSE, fileCached = EOS_FALSE;
    EOS_INTEGER fileCachedIndex = -1;


    *errorCode = EOS_OK;

    /* Is handle invalid with associated setup errorcode */
    if (! eos_IsHandleValid(*tableHandle) && gEosDataMap.errorCodes[*tableHandle] != EOS_OK) {
      *errorCode = gEosDataMap.errorCodes[*tableHandle];
      eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);
      return;
    }

    /* if tableHandle is invalid, reset tableHandlesMap entry */
    if (! eos_IsHandleValid(*tableHandle)) {
      gEosDataMap.tableHandlesMap[*tableHandle] = _EOS_NEWLY_INITIALIZED_TABLE_HANDLE_VALUE;
    }

    /* create local copy of fileName */
    _fn = (EOS_CHAR*) malloc((PATH_MAX+1)*sizeof(EOS_CHAR));
    if (! _fn) {
      *errorCode = EOS_FAILED;
      eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);
      *errorCode = eos_SetCustomErrorMsg (*tableHandle, *errorCode,
					  "EOS_FAILED: Internal temporary memory allocation failure");
      return;
    }

    /* Issue warning and exit if eosData->isLoaded */
    if (gEosDataMap.dataObjects && gEosDataMap.tableHandlesMap[*tableHandle] >= 0)
      eosData = gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[*tableHandle]];
    if (eosData && eosData->isLoaded) {
      *errorCode = EOS_WARNING;
      eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);
      *errorCode = eos_SetCustomErrorMsg (*tableHandle, *errorCode,
					  "EOS_WARNING: Could not reset data file name, because data is already loaded for the specified handle, %d",
					  *tableHandle);
      EOS_FREE(_fn);
      return;
    }

    /* create a local copy of fileName to ensure a valid C-string with max length of PATH_MAX */
    for (i = MIN(fileName_len-1,PATH_MAX-1); i >= 0; i--) {
      if (fileName[i] == ' ') continue;
      else                    break;
    }
    //if (i+1 < PATH_MAX && fileName[i+1] == ' ') i++; /* correct for Fortran strings */
    i++;
    strncpy(_fn, fileName, i);
    _fn[i] = '\0'; /* terminate string */

    /* Ensure the file exists and is valid */
    if (! _eos_fileExistsAndValid(_fn)) {
      *errorCode = EOS_OPEN_SESAME_FILE_FAILED;
      eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);
      *errorCode = eos_SetCustomErrorMsg (*tableHandle, *errorCode,
					  "EOS_OPEN_SESAME_FILE_FAILED: Could not open data file, because it does not exist");
      EOS_FREE(_fn);
      return;
    }

    /* determine if the fileName already exists in sesameFiles[] */
    for (i = 0; i < sesameFilesL; i++) {
      if (!strcmp(sesameFiles[i], _fn)) /* files are identical */
	break;
    }

    if (i < sesameFilesL) { /* file is already cached */
      int j;
      fileCachedIndex = i;
      for (j = 0; j < gEosDataMap.nAlloc; j++) { /* find existing object that uses file, _fn */
	if (gEosDataMap.tableHandlesMap[j] < 0 ||
	    gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[j]] == NULL) continue;
	eosData0 = gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[j]]; /* save pointer */
	if (eosData0->dataFileIndex == i) {
	  tableHandle0 = j;
	  break;
	}
      }
      if (j < gEosDataMap.nAlloc && eos_IsHandleValid (tableHandle0)) {
	eosData = _eos_RemapEosDataMap (&gEosDataMap, *tableHandle, tableHandle0, NULL, NULL, NULL, 0, -1, errorCode);
	for (i = 0; i < sesameFilesL; i++) {
	  if (!strcmp(sesameFiles[i], _fn)) { /* files are identical */
	    if (! eosData) continue;
	    if (eosData->dataFileIndex != i) fileIndexChanged = EOS_TRUE;
	    eosData->dataFileIndex = i;
	    break;
	  }
	}
      }
      else {
	if (fileCachedIndex < sesameFilesL && fileCachedIndex >= 0)
	  fileCached = EOS_TRUE;
	createNewObject = EOS_TRUE;
      }

    }
    else { /* file not cached */

      /* if fileName is not longer than PATH_MAX, then push onto sesameFiles[] and
       * increment sesameFilesL.
       */
      if (strlen (_fn) <= PATH_MAX) {
	if (! sesameFiles)
	  sesameFiles = (EOS_CHAR **) malloc (sizeof (EOS_CHAR *) * (sesameFilesL + 1));
	else
	  sesameFiles = (EOS_CHAR **) realloc (sesameFiles, sizeof (EOS_CHAR *) * (sesameFilesL + 1));
	sesameFiles[sesameFilesL] =
	  (EOS_CHAR *) malloc ((MIN(fileName_len,PATH_MAX) + 1) * sizeof (EOS_CHAR));
	strcpy (sesameFiles[sesameFilesL], _fn);
	sesameFilesL += 1;
      }
      else {
	*errorCode = EOS_OPEN_SESAME_FILE_FAILED;
        eos_HandleErrorEosDataMap (&gEosDataMap, *tableHandle, *errorCode);
        *errorCode = eos_SetCustomErrorMsg (*tableHandle, *errorCode,
					    "EOS_OPEN_SESAME_FILE_FAILED: Could not open data file, because its length > %d characters",
					    PATH_MAX);
	EOS_FREE(_fn);
	return;
      }

      createNewObject = EOS_TRUE;

    }

    if (createNewObject) {

      EOS_INTEGER fileIndex = sesameFilesL-1;

      /* Store pointer to current eosData */
      tableHandle0 = *tableHandle;
      if (eos_IsHandleValid(*tableHandle) && gEosDataMap.tableHandlesMap[*tableHandle] >= 0)
	eosData0 = gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[*tableHandle]]; /* save pointer */

      /* Update cached data */
      if (fileCached) fileIndex = fileCachedIndex;
      eos_CreateTablesEosDataMap (&gEosDataMap, 1, tableType, matID, tableHandle,
				  EOS_TRUE, 1, EOS_TRUE, fileIndex, errorCode);

      eosData = gEosDataMap.dataObjects[gEosDataMap.tableHandlesMap[*tableHandle]]; /* update pointer */
      if (eosData) {
	if (eosData->dataFileIndex != (sesameFilesL - 1)) fileIndexChanged = EOS_TRUE;
	if (! fileCached) eosData->dataFileIndex = sesameFilesL - 1;

	if (eosData0 && eosData != eosData0) { /* Cleanup eosData0 if necessary */
	  eosData0->refCounter--;
	  if (eosData0->refCounter <= 0) {
	    eos_DestroyEosData (eosData0);
	    eos_FreeEosData (&eosData0, eosData0->recordType);
	  }
	}
      }

    }

    if (eosData)
      eosData->userDefinedDataFile = EOS_TRUE; /* indicate user-defined file name */

    if (fileIndexChanged) { /* update dataFileOffset since the fileIndex has been reset */

      if (eosData) /* get the file offset, index, data size for reading data */
	eosData->SetFileIndexes (eosData, *tableHandle);
      else
	*errorCode = EOS_READ_DATA_FAILED;

    }

    EOS_FREE(_fn);

    /* update error code with table handle */
    *errorCode = eos_SetCustomErrorCode(*tableHandle, *errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = EOS_OK;

  }

  /*************************************************************************
   *
   * Function Name: eos_ErrorCodesEqual
   *
   * Description:
   * The eos_ErrorCodesEqual function allows the host code to determine if
   * two errorcodes are equivalent while ignoring the table handle component.
   *
   * Input Values:
   * EOS_INTEGER *err1     -- EOS_INTEGER : scalar error code
   * EOS_INTEGER *err2     -- EOS_INTEGER : scalar error code
   *
   * Returned Value:
   * EOS_BOOLEAN *result   -- EOS_BOOLEAN : scalar result
   *
   *************************************************************************/
  void FUNC_INTER eos_ErrorCodesEqual (EOS_INTEGER *err1, EOS_INTEGER *err2, EOS_BOOLEAN *result)
  {
    *result = EOS_FALSE;
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err1) ==
	eos_GetStandardErrorCodeFromCustomErrorCode(*err2)) *result = EOS_TRUE;
  }

#ifndef NO_EOS_SETDATAFILENAME_CWRAPPER

  void FUNC_INTER eos_SetDataFileName_Cwrapper (EOS_INTEGER *tableHandle,
						EOS_INTEGER *matID,
						EOS_INTEGER *tableType,
						EOS_CHAR *fileName,
						EOS_INTEGER *errorCode)
  {
    long int fileName_len = strlen(fileName);
    eos_SetDataFileName (tableHandle, matID, tableType, fileName, errorCode, fileName_len);
  }

#endif

/************************************************************************
 * 
 * This function is used to perform time queries using clock() and time()
 * C functions.
 * 
 * Returned Values:
 * EOS_REAL    *wctime    - wall clock time (seconds) elapsed since time
 *                          state was reset
 * EOS_REAL    *cputime   - cpu clock time (seconds) elapsed since time
 *                          state was reset
 * EOS_REAL    *cpucycles - cpu clock time (seconds) elapsed since time
 *                          state was reset
 * EOS_INTEGER *err       - error flag
 *
 * Input Value:
 * EOS_BOOLEAN *reset     - EOS_TRUE : reset time state stored here
 *                          EOS_FALSE: retrieve times since upon last reset
 * 
 ************************************************************************/
#include <time.h>

#ifndef WIN32

#include <sys/times.h>
#include <unistd.h>
#define _TIMES_ times
#define _TIME_T_ struct tms
#define _CYCLES_PER_SECOND_ sysconf(_SC_CLK_TCK)

#elif defined __CYGWIN__

#include <sys/times.h>
#include <unistd.h>
#define _TIMES_ times
#define _TIME_T_ struct tms
#define _CYCLES_PER_SECOND_ sysconf(_SC_CLK_TCK)

#else

#define _TIMES_ time
#define _TIME_T_ time_t
#define _CYCLES_PER_SECOND_ CLOCKS_PER_SEC

#endif

  void FUNC_INTER eos_Time (EOS_BOOLEAN *reset, EOS_REAL *wctime,
                            EOS_REAL *cputime, EOS_REAL *cpucycles,
                            EOS_INTEGER *err)
  {
    static EOS_REAL wc_start;
    static EOS_REAL cpucyc_start;

    static long cycles_per_sec = 0;
    _TIME_T_ buffer;

    clock_t cpu_clock_start;
    clock_t cpu_clock_end;

    x86_SetPrecision;  /* use double-precision rounding on x86 architectures */

    *err = EOS_OK;

    if (cycles_per_sec == 0) {
      cycles_per_sec = _CYCLES_PER_SECOND_;
    }

    if (*reset) {
      wc_start = (EOS_REAL) time (NULL);
      cpucyc_start = (EOS_REAL) _TIMES_ (&buffer);

      cpu_clock_start = clock();
      *cputime = (EOS_REAL) cpu_clock_start;

    }
    else {
      *wctime = (EOS_REAL) time (NULL) - wc_start;
      *cpucycles = (EOS_REAL) _TIMES_ (&buffer) - cpucyc_start;

      cpu_clock_end = clock();
      *cputime = ((EOS_REAL) cpu_clock_end - *cputime) / ((EOS_REAL) CLOCKS_PER_SEC);

    }

    x86_RestorePrecision;  /* reset extended-double-precision rounding on x86 architectures */

    return;
  }

#ifndef EOSPAC6_VERSION_FUNCTIONS_REDEFINED
  static const EOS_CHAR *eos_version_info_msg =
  "No version information has been defined for this build of EOSPAC 6.";
  void FUNC_INTER eos_GetVersionLength (EOS_INTEGER *length)
  {
    /* include space for the '\0' character */
    *length = strlen (eos_version_info_msg) + 1;
  }

  void FUNC_INTER eos_GetVersion (EOS_CHAR *version)
  {
    strcpy (version, eos_version_info_msg);
    return;
  }
#endif

#ifdef __cplusplus
}
#endif
