/*********************************************************************
 * Class Name : eos_RecordType2Interp
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "eos_types_internal.h"

#define _EOS_RECORDTYPE2_INTERNAL_PROTOTYPES
#include "eos_RecordType2.h"

#define _EOS_RECORDTYPE2INTERP_INTERNAL_PROTOTYPES
#include "eos_RecordType2Interp.h"

#include "eos_Utils.h"

#include "eos_Interpolation.h"

//#define DEBUG

/* static const EOS_REAL ZERO = (EOS_REAL) 0; */

/***********************************************************************/
/*!
 * \brief Function eos_InterpolateRecordType1 (helping function for eos_InterpolateEosInterpolation().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                 to x. 
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                 of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - unused
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 *
 * The input arguments are:
 * void *ptr                 a pointer to a eos_RecordType1 instance from which to extract the data. 
 * EOS_INTEGER th            table Handle
 * EOS_INTEGER dataType      dataType
 * EOS_INTEGER nXYPairs      total number of pairs of independent variable values provided for interpolation.
 * EOS_REAL srchX[nXYPairs]  array of the primary independent variable values to use during interpolation. 
 * EOS_REAL srchY[nXYPairs]  unused
 *
 * \return none
 *
 ***********************************************************************/
void eos_InterpolateRecordType2 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_REAL *fVals,
                                 EOS_REAL *dFx, EOS_REAL *dFy,
                                 EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  EOS_INTEGER nX, cat, subTableNum;
  EOS_REAL *X, *F, *F2 = NULL;
  eos_Data *eosData;
  eos_RecordType2 *me;
  EOS_CHAR *errMsg = NULL;
  eos_HashTable1D* F_ht = NULL;

  *errorCode = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType2 *) eosData;
  cat = EOS_CATEGORY (dataType);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  nX = me->NT;

  /* make sure the data is record type 2 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE2) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  _eos_GetDataRecordType2 (me, &X, &F, &F_ht, subTableNum);
  if (cat == EOS_CATEGORY3)
    _eos_GetDataRecordType2 (me, &X, &F2, NULL,
                             EOS_TYPE_TO_SUB_TAB_NUM (EOS_EOS_TABLE_TYPE_REF2
                                                      (dataType)));

  /* Perform 1-D interpolation required by dataType */
  _eos_InterpolateRecordType_1D (EOS_TRUE, nX, X, F, F2, th, dataType,
                                 nXYPairs, srchX, fVals, dFx, me->T_ht, F_ht, xyBounds,
                                 errorCode, &errMsg);
  if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
  EOS_FREE(errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);

  return;

}

/* eos_CheckExtrap
If the EOS_INTERP_EXTRAPOLATED error code is returned by either eos_Interp or eos_Mix, this routine allows the user 
to determine which X caused extrapolation and in which direction (high or low), it occurred.
The input arguments are
xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.

The output arguments are 
xyBounds	This is an array of size nXYPairs elements that returns EOS_OK if extrapolation did not occur. If extrapolation occurred the variable and direction are determined from Table 2.
errorCode	This is a scalar EOS_INTEGER to contain an error code.

In the case that eos_Mix returned EOS_INTERP_EXTRAPOLATED as an error code, an additional series of steps must be performed to determine which tableHandle(s) correspond to the extrapolation error:
1.	For each tableHandle sent to eos_Mix, call eos_GetErrorCode and, optionally, eos_GetErrorMessage.
2.	For each of these tableHandles, call eos_CheckExtrap to determine one of codes listed in Table 2.
Table 2.	Extrapolation return codes.
Code	Definition
EOS_OK	No extrapolation occurred.
EOS_xHi_yOk	The x argument was high.
EOS_xLo_yOk	The x argument was low.
 */

void eos_CheckExtrapRecordType2 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, tabInd2, nX, cat, subTableNum;
  EOS_REAL *X, *F;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals;
  eos_RecordType2 *me;

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap) 
    return;

  *errorCode = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType2 *) eosData;
  cat = EOS_CATEGORY (dataType);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  nX = me->NT;

  /* make sure the data is record type 2 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE2) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  _eos_GetDataRecordType2 (me, &X, &F, NULL, subTableNum);

  xVals = srchX;
  /* srchY is unused here and should be NULL */

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      for (i = 0; i < nXYPairs; i++) {
        if (xVals[i] < X[0])
          xyBounds[i] = EOS_xLo_yOk;
        else if (xVals[i] > X[nX - 1])
          xyBounds[i] = EOS_xHi_yOk;
        else
          xyBounds[i] = EOS_OK;
      }
      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      /* means that the user provides F of a function that is defined as x(F)
         so effectively the x and y user has given us are F and Y */

      for (i = 0; i < nXYPairs; i++) {
        /* x is really f, find it among stored f values  */
        if (xVals[i] < F[0])
          xyBounds[i] = EOS_xLo_yOk;
        else if (xVals[i] > F[nX - 1])
          xyBounds[i] = EOS_xHi_yOk;
        else
          xyBounds[i] = EOS_OK;
      }                         /* nXYPairs loop */
      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these valuables */
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* call checkExtrap recursively, allow for passing of only X into 
         eos_CheckExtrapRecordType2 (), checking the second table is enough */
      me->eosData.tmpVarOrder = X_U;
      eos_CheckExtrapRecordType2 (me, th, tabInd2, nXYPairs, xVals, NULL,
                                  xyBounds, errorCode);
      break;
    }
  default:
    {
      err = EOS_INVALID_TABLE_HANDLE;
      break;
    }
  }                             /* switch statement */


  if (srchX != xVals)
    EOS_FREE (xVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
    return;
  }
}
