/*********************************************************************
 * Class Name : eos_RecordType6Interp
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#ifndef _POSIX_
#define _POSIX_
#endif
#include <sys/types.h>
#include <sys/stat.h>

#include "eos_types_internal.h"

#define _EOS_RECORDTYPE6_INTERNAL_PROTOTYPES
#include "eos_RecordType6.h"

#define _EOS_RECORDTYPE6INTERP_INTERNAL_PROTOTYPES
#include "eos_RecordType6Interp.h"

#include "eos_Utils.h"

#include "eos_Interpolation.h"

/* static const EOS_REAL ONE = (EOS_REAL) 1; */
static const EOS_REAL ZERO = (EOS_REAL) 0;
/* static const EOS_REAL EOS_MIN_MASS_FRACTION = (EOS_REAL) 1.0e-8; */

/*	function _eos_InterpolateRecordType6 (helping function for eos_InterpolateEosInterpolation() 

	The input arguments are:
		void *ptr                 a pointer to a eos_RecordType6 instance from which to extract the data. 
		EOS_INTEGER th            table Handle
		EOS_INTEGER subTableNum   which subtable/phase to use for interpolation
		EOS_INTEGER varOrder      order of variables
		EOS_INTEGER dataType      dataType
		EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.
		EOS_REAL srchX[nXYPairs]  array of the primary independent variable values to use during interpolation.
		EOS_REAL srchY[nXYPairs]  array of the secondary independent variable values to use during interpolation.

	The output arguments are:
		EOS_REAL fVals[nXYPairs]  array of the interpolated data corresponding to x and y. 
		EOS_REAL dFx[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to x. 
		EOS_REAL dFy[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to y. 
		EOS_REAL dFCx[nXYPairs]	  optional array of the interpolated partial derivatives of cold curve with respect to x. 
		EOS_REAL dFx0[nXYPairs]	  optional array of the interpolated partial derivatives of cat 0 F wrt x (for inverse functions mostly). 
		EOS_REAL dFy0[nXYPairs]	  optional array of the interpolated partial derivatives of cat 0 F wrt y (for inverse functions mostly). 
		EOS_INTEGER *xyBounds     interpolation errors per xy-pair
		EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
*/

void _eos_InterpolateRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER subTableNum,
                                  EOS_INTEGER varOrder, EOS_INTEGER dataType,
                                  EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                  EOS_REAL *srchY, EOS_REAL *fVals,
                                  EOS_REAL *dFx, EOS_REAL *dFy,
                                  EOS_INTEGER *xyBounds,
                                  EOS_INTEGER *errorCode)
{
  EOS_INTEGER i, doRational =
    0, err, nX, nY, cat, *ixv, *iyv, *xyBounds2;
  EOS_REAL *X, *Y, **F, *xVals, *yVals;
  eos_Data *eosData;
  eos_RecordType6 *me;

  *errorCode = EOS_OK;

  if (nXYPairs <= 0) {
    *errorCode = EOS_FAILED;
    return;
  }

  err = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType6 *) eosData;

  /* make sure the data is record type 6 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE6) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  cat = EOS_CATEGORY (dataType);

  /* get the size of the data */
  eos_GetSizeRecordType6 (me, &nX, &nY);

  if (me->eosData.eos_IsRequiredDataLoaded && ! me->eosData.eos_IsRequiredDataLoaded(me, dataType)) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* initialize F, dFx, dFy to zero */
  for (i = 0; i < nXYPairs; i++) {
    fVals[i] = ZERO;
    dFx[i] = ZERO;
    dFy[i] = ZERO;
  }

  _eos_GetDataRecordType6 (me, &X, &Y, &F, subTableNum);

  xVals = srchX;
  yVals = srchY;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {

      /* force LINEAR interpolation */
      gEosInterpolation.interpolationDataList[th]->interpolationType = EOS_LINEAR;

      doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);
      if (doRational) {

        /* .... perform table searches to load indices and spacings of nearest
           .... data table x,y values to x,searchYVals. */

        iyv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
        ixv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of X near which X points are */
        xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER)); /* xy-bounds for y */

        _eos_srchdf (nXYPairs, yVals, 1, nY, Y, 1, iyv, me->T_ht,
                     xyBounds, errorCode);
        _eos_srchdf (nXYPairs, xVals, 1, nX, X, 1, ixv, me->R_ht,
                     xyBounds2, errorCode);

        for (i = 0; i < nXYPairs; i++)
          xyBounds[i] = _eos_CombineExtrapErrors (xyBounds2[i], xyBounds[i]);

        eos_BiRationalInterpolate (nXYPairs, nX, nY, X, Y, F, ixv, iyv, xVals, yVals,
                                   fVals, dFx, dFy, xyBounds, &err);

        EOS_FREE (xyBounds2);
        EOS_FREE (ixv);
        EOS_FREE (iyv);

      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_BiLineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
                               nXYPairs, nX, nY, X, Y, F, 0, xVals, yVals, fVals,
                               dFx, dFy, me->R_ht, me->T_ht, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;

      break;
    }

  default:
    {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
      break;
    }
  } /* end switch (cat) */

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (srchY != yVals)
    EOS_FREE (yVals);

}

/***********************************************************************/
/*!
 * \brief Function eos_InterpolateRecordType1 (helping function for eos_InterpolateEosInterpolation().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                 to x and y. 
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                 of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                 of fVals with respect to y.
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
 * EOS_REAL srchY[nXYPairs]  array of the secondary independent variable values to use during interpolation. 
 *
 * \return none
 *
 ***********************************************************************/
void eos_InterpolateRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_REAL *fVals,
                                 EOS_REAL *dFx, EOS_REAL *dFy,
                                 EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{

  EOS_INTEGER numPhases = 1, subTableNum;
  EOS_REAL *F, *dFx_ = dFx, *dFy_ = dFy;
  eos_RecordType6 *me;
  EOS_CHAR *errMsg = NULL;

  *errorCode = EOS_OK;

  me = (eos_RecordType6 *) ptr;

  if (dataType == EOS_M_DT) {

    /* The sizes of srchX, srchY, fVals, dFx, and dFy are NOT equal so new logic is required. */

    /* fVals is allocated to contain nXYPairs*numPhases values; therefore,
       interpolate for each material phase prior to applying data constraints.
     */
    numPhases = _eos_GetNumberOfPhases(ptr, errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);

    /* no derivatives are returned to the host code for EOS_M_DT,
       so temporary arrays are allocated here to simplify integration with
       existing interpolation functions. */
    dFx_ = (EOS_REAL *) malloc(nXYPairs * sizeof(EOS_REAL));
    dFy_ = (EOS_REAL *) malloc(nXYPairs * sizeof(EOS_REAL));

    for (subTableNum = 1; subTableNum <= numPhases; subTableNum++) {

      F = &(fVals[(subTableNum-1)*nXYPairs]);
      _eos_InterpolateRecordType6 (ptr, th, subTableNum, me->eosData.varOrder, dataType, nXYPairs,
				   srchX, srchY, F, dFx_, dFy_, xyBounds, errorCode);

    }

    /* apply data constraints to interpolated values. */
    eos_ApplyMassFracConstraintsRecordType6 (ptr, th, dataType, nXYPairs,
					     fVals, errorCode);

    /* free temporary arrays */
    EOS_FREE(dFx_);
    EOS_FREE(dFy_);

  } /*end if (dataType == EOS_M_DT)*/

}

/* eos_CheckExtrap
If the EOS_INTERP_EXTRAPOLATED error code is returned by either eos_Interp or eos_Mix, this routine allows the user 
to determine which (x,y) pairs caused extrapolation and in which direction (high or low), it occurred.
The input arguments are
xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.
yVals	This is an array of the secondary independent variable values to use during interpolation. There are nXYPairs elements in yVals.

The output arguments are 
xyBounds	This is an array of size nXYPairs elements that returns EOS_OK if extrapolation did not occur. If extrapolation occurred the variable and direction are determined from Table 2.
errorCode	This is a scalar EOS_INTEGER to contain an error code.

In the case that eos_Mix returned EOS_INTERP_EXTRAPOLATED as an error code, an additional series of steps must be performed to determine which tableHandle(s) correspond to the extrapolation error:
1.	For each tableHandle sent to eos_Mix, call eos_GetErrorCode and, optionally, eos_GetErrorMessage.
2.	For each of these tableHandles, call eos_CheckExtrap to determine one of codes listed in Table 2.
Table 2.	Extrapolation return codes.
Code	Definition
EOS_OK	No extrapolation occurred.
EOS_xHi_yHi	Both the x and y arguments were high.
EOS_xHi_yOk	The x argument was high, the y argument was OK.
EOS_xHi_yLo	The x argument was high, the y argument was low.
EOS_xOk_yLo	The x argument is OK and the y argument is low.
EOS_xLo_yLo	Both the x and y arguments were low.
EOS_xLo_yOk	The x argument was low, the y argument was OK.
EOS_xLo_yHi	The x argument was low, the y argument was OK.
   EOS_xOk_yHi	The x argument is OK and the y argument is high.
 */
void eos_CheckExtrapRecordType6 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, nX, nY, cat, subTableNum;
  EOS_REAL *X, *Y, **F;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals, *yVals;
  eos_RecordType6 *me;
  EOS_BOOLEAN isOneDimDatatype = EOS_FALSE;

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap) 
    return;

  *errorCode = EOS_OK;

  eosData = (eos_Data *) ptr;
  me = (eos_RecordType6 *) eosData;
  cat = EOS_CATEGORY (dataType);
  subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  /* get the size of the data */
  eos_GetSizeRecordType6 (me, &nX, &nY);

  /* make sure the data is record type 6 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE6) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  if (me->eosData.eos_IsRequiredDataLoaded && ! me->eosData.eos_IsRequiredDataLoaded(me, dataType)) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* is the datatpe a function with one independent variable? */
  isOneDimDatatype = EOS_IS_ONE_DIM_TYPE (dataType);

  _eos_GetDataRecordType6 (me, &X, &Y, &F, subTableNum);

  xVals = srchX;
  yVals = srchY;


  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      for (i = 0; i < nXYPairs; i++) {
        if (! isOneDimDatatype && yVals[i] < Y[0]) {
          if (xVals[i] < X[0])
            xyBounds[i] = EOS_xLo_yLo;
          else if (xVals[i] > X[nX - 1])
            xyBounds[i] = EOS_xHi_yLo;
          else
            xyBounds[i] = EOS_xOk_yLo;
        }
        else if (! isOneDimDatatype && yVals[i] > Y[nY - 1]) {
          if (xVals[i] < X[0])
            xyBounds[i] = EOS_xLo_yHi;
          else if (xVals[i] > X[nX - 1])
            xyBounds[i] = EOS_xHi_yHi;
          else
            xyBounds[i] = EOS_xOk_yHi;
        }
        else {                  /* y is either OK or not considered */

          if (xVals[i] < X[0])
            xyBounds[i] = EOS_xLo_yOk;
          else if (xVals[i] > X[nX - 1])
            xyBounds[i] = EOS_xHi_yOk;
          else
            xyBounds[i] = EOS_OK;
        }
      }
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
  if (srchY != yVals)
    EOS_FREE (yVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
    return;
  }
}
