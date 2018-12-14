/*********************************************************************
 * Class Name : eos_ErrorHandler
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>

#define _EOS_ERRORHANDLER_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_ErrorHandler.h"

#include "eos_Utils.h"

/************************************************************************
 * 
 * eos_ErrorHandler class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_ErrorHandler **me  - this pointer (pointer to the instance of type eos_ErrorHandler)
 * 
 ************************************************************************/
void eos_DestroyEosErrorHandler (eos_ErrorHandler *me)
{
}

/************************************************************************
 * 
 * eos_ErrorHandler class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_ErrorHandler *me         - this pointer (pointer to the instance of type eos_RecordType1
 * 
 ************************************************************************/
void eos_ConstructEosErrorHandler (eos_ErrorHandler *me)
{
  me->HandleError = NULL;       /* pure virtual function */
}

/************************************************************************
 * 
 * method eos_GetErrorMsg
 * 
 * Returned Values: char * error message string
 *
 * Input Value:
 * EOS_INTEGER      errorCode
 * 
 ************************************************************************/
EOS_CHAR *eos_GetErrorMsg (EOS_INTEGER errorCode)
{
  EOS_INTEGER th;
  EOS_INTEGER standard_errorCode;
  EOS_CHAR    *msg = NULL;

  standard_errorCode = eos_GetStandardErrorCodeFromCustomErrorCode(errorCode);

  if (standard_errorCode == EOS_OK)
    return "No errors detected";

  if (standard_errorCode < EOS_MIN_ERROR_CODE_VALUE ||
      standard_errorCode > EOS_MAX_ERROR_CODE_VALUE)
    return "Invalid error code";

  th = eos_GetHandleFromCustomErrorCode(errorCode);

  msg = eos_GetCustomErrorMsg (th, standard_errorCode);
  if (th >= 0 && strlen(msg) > 0)
    return msg;

  switch (standard_errorCode) {
  case EOS_BAD_DERIVATIVE_FLAG:
    return "EOS_BAD_DERIVATIVE_FLAG: Derivative is not recognized";
  case EOS_BAD_INTERPOLATION_FLAG:
    return "EOS_BAD_INTERPOLATION_FLAG: Interpolation is not recognized";
  case EOS_BAD_MATERIAL_ID:
    return "EOS_BAD_MATERIAL_ID: Material ID is zero";
  case EOS_CONVERGENCE_FAILED:
    return "EOS_CONVERGENCE_FAILED: Iterative algorithm did not converge during inverse interpolation";
  case EOS_NO_COMMENTS:
    return "EOS_NO_COMMENTS: No comments available for this data table";
  case EOS_DATA_TYPE_NOT_FOUND:
    return "EOS_DATA_TYPE_NOT_FOUND: Data table type is not in library";
  case EOS_FAILED:
    return "EOS_FAILED: Operation failed";
  case EOS_INTERP_EXTRAPOLATED:
    return
      "EOS_INTERP_EXTRAPOLATED: Interpolation caused extrapolation beyond data table boundaries";
  case EOS_MATERIAL_NOT_FOUND:
    return "EOS_MATERIAL_NOT_FOUND: Material ID is not in library";
  case EOS_MEM_ALLOCATION_FAILED:
    return "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded.";
  case EOS_NO_DATA_TABLE:
    return "EOS_NO_DATA_TABLE: Data table is not in EOS table area";
  case EOS_NO_SESAME_FILES:
    return "EOS_NO_SESAME_FILES: No data library files exist";
  case EOS_NOT_INITIALIZED:
    return "EOS_NOT_INITIALIZED: EOS table area is not initialized";
  case EOS_BAD_DATA_TYPE:
    return "EOS_BAD_DATA_TYPE: Data table type is not recognized";
  case EOS_OPEN_SESAME_FILE_FAILED:
    return "EOS_OPEN_SESAME_FILE_FAILED: Could not open data file";
  case EOS_READ_DATA_FAILED:
    return "EOS_READ_DATA_FAILED: Could not load data table";
  case EOS_READ_FILE_VERSION_FAILED:
    return
      "EOS_READ_FILE_VERSION_FAILED: Could not load version from data file";
  case EOS_READ_MASTER_DIR_FAILED:
    return "EOS_READ_MASTER_DIR_FAILED: Could not load master directory";
  case EOS_READ_MATERIAL_DIR_FAILED:
    return "EOS_READ_MATERIAL_DIR_FAILED: Could not load material directory";
  case EOS_READ_TOTAL_MATERIALS_FAILED:
    return
      "EOS_READ_TOTAL_MATERIALS_FAILED: Could not read number of materials";
  case EOS_INVALID_TABLE_HANDLE:
    return "EOS_INVALID_TABLE_HANDLE: Invalid table handle";
  case EOS_INVALID_SUBTABLE_INDEX:
    return "EOS_INVALID_SUBTABLE_INDEX: Subtable index out of the range";
  case EOS_INVALID_OPTION_FLAG:
    return
      "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid";
  case EOS_INVALID_DATA_TYPE:
    return
      "EOS_INVALID_DATA_TYPE: Operation is not defined on this data type";
  case EOS_INVALID_SPLIT_FLAG:
    return "EOS_INVALID_SPLIT_FLAG: The data splitting option is invalid";
  case EOS_UNDEFINED:
    return "EOS_UNDEFINED: The result is undefined";
  case EOS_NOT_ALLOCATED:
    return "EOS_NOT_ALLOCATED: Memory not allocated for data";
  case EOS_INTEGRATION_FAILED:
    return
      "EOS_INTEGRATION_FAILED: Numerical integration failed or not possible";
  case EOS_DATA_TYPE_NO_MATCH:
    return
      "EOS_DATA_TYPE_NO_MATCH: Data types do not match as required for mixing";
  case EOS_INVALID_INFO_FLAG:
    return
      "EOS_INVALID_INFO_FLAG: The info flag passed into either eos_GetTableInfo() or eos_GetTableMetaData() is invalid";
  case EOS_INVALID_CONC_SUM:
    return
      "EOS_INVALID_CONC_SUM: The sum of the supplied material concentrations does not equal 1.0";
  case EOS_INTERP_EXTRAP_TBAL:
    return
      "EOS_INTERP_EXTRAP_TBAL: Temperature balance function extrapolated beyond data table boundaries";
  case EOS_INTERP_EXTRAP_PBAL:
    return
      "EOS_INTERP_EXTRAP_PBAL: Pressure balance function extrapolated beyond data table boundaries";
  case EOS_CANT_MAKE_MONOTONIC:
    return "EOS_CANT_MAKE_MONOTONIC: Can't make data monotonic in X";
  case EOS_CANT_INVERT_DATA:
    return
      "EOS_CANT_INVERT_DATA: Can't invert wrt first independent variable";
  case EOS_OPEN_OUTPUT_FILE_FAILED:
    return
      "EOS_OPEN_OUTPUT_FILE_FAILED: Could not open TablesLoaded.dat or related data file";
  case EOS_INVALID_NXYPAIRS:
    return "EOS_INVALID_NXYPAIRS: Invalid nXYPairs value";
  case EOS_GEN401_AND_NOT_FOUND:
    return "EOS_GEN401_AND_NOT_FOUND: 401 data was generated and not found";
  case EOS_WARNING:
    return "EOS_WARNING: Operation may have expectedly resulted in invalid data";
  case EOS_SPLIT_FAILED:
    return "EOS_SPLIT_FAILED: The data splitting algorithm failed";
  case EOS_INDEX_FILE_ERROR:
    return "EOS_INDEX_FILE_ERROR: The sesameFilesDir.txt file parser found a syntax error";
  case EOS_INVALID_INFO_CATEGORY_FLAG:
    return "EOS_INVALID_INFO_CATEGORY_FLAG: The info category flag passed into eos_GetMetaData() is invalid";
  default:
    return "Unknown error";
  }
}
