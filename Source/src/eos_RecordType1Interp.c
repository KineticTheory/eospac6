/*********************************************************************
 * Class Name : eos_RecordType1Interp
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

#define _EOS_RECORDTYPE1_INTERNAL_PROTOTYPES
#include "eos_RecordType1.h"
 
#define _EOS_RECORDTYPE1INTERP_INTERNAL_PROTOTYPES
#include "eos_RecordType1Interp.h"

#include "eos_RecordType2.h"
#include "eos_RecordType2Interp.proto.h"

#include "eos_Utils.h"
 
#include "eos_Data.h"
#include "eos_Interpolation.h"

#undef MY_DEBUG
 
static const EOS_REAL ONE = (EOS_REAL) 1;
/* static const EOS_REAL TWO = (EOS_REAL) 2; */
/* static const EOS_REAL THREE = (EOS_REAL) 3; */
/* static const EOS_REAL FOUR = (EOS_REAL) 4; */
/* static const EOS_REAL FIVE = (EOS_REAL) 5; */
/* static const EOS_REAL SIX = (EOS_REAL) 6; */
/* static const EOS_REAL SEVEN = (EOS_REAL) 7; */
/* static const EOS_REAL EIGHT = (EOS_REAL) 8; */
/* static const EOS_REAL NINE = (EOS_REAL) 9; */
static const EOS_REAL ZERO = (EOS_REAL) 0;

/***********************************************************************/
/*!
 * \brief Function _eos_InterpolateRecordType1 (helping function for
 *  eos_InterpolateRecordType1().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                            to x and y.
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to y.
 * \param[out]   dFCx[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cold curve with respect to x.
 * \param[out]   dFx0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt x (for inverse
 *                                            functions mostly).
 * \param[out]   dFy0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt y (for inverse
 *                                            functions mostly).
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table Handle
 * \param[in]    varOrder        - EOS_INTEGER : order of variables
 * \param[in]    dataType        - EOS_INTEGER : dataType
 * \param[in]    nXYPairs        - EOS_INTEGER : total number of pairs of independent variable values provided for interpolation.
 * \param[in]    srchX[nXYPairs] - EOS_REAL : array of the primary independent variable values to use during interpolation.
 * \param[in]    srchY[nXYPairs] - EOS_REAL : array of the secondary independent variable values to use during interpolation.
 *
 * \return none
 *
 ***********************************************************************/
void _eos_InterpolateRecordType1 (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER varOrder, EOS_INTEGER dataType,
                                  EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                  EOS_REAL *srchY, EOS_REAL *fVals,
                                  EOS_REAL *dFx, EOS_REAL *dFy,
                                  EOS_REAL *dFCx, EOS_REAL *dFx0,
                                  EOS_REAL *dFy0, EOS_INTEGER *xyBounds,
                                  EOS_INTEGER *errorCode)
{
  /* macros to enable debugging output in this function;
     used to make algorithm logic more easily read */
  //#define _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1

  EOS_INTEGER i, ind, doRational = 0,
    err, tabInd1, tabInd2, nX, nY, cat, subTableNum, subTableNum2, tableNum, *xyBounds2;
  EOS_REAL *X, *Y, **F, **F2, *uVals, *dUy, *dUx, *dFCx1, *dUCx, *dYx, *dXy,
    *coldCurve, *newColdCurve = NULL, *newColdCurve_dFdx = NULL,
    *newFVals, *xVals, *yVals, *null_val = NULL;
  
  eos_HashTable2D *F_ht;

  eos_Data *eosData;
  eos_RecordType1 *me;
  EOS_BOOLEAN isPtSmooth = EOS_FALSE, useCustomInterp = EOS_FALSE;
  EOS_INTEGER nGhostData=0;

  EOS_REAL *xtbls, *ytbls, **ftbls; /* temporary arrays to include ghost node data */
  EOS_BOOLEAN optVal = EOS_FALSE;
  EOS_CHAR *errMsg = NULL;
  eos_ExtrapolationBoundsEosDataMap *extrapolationBounds = eos_GetExtrapolationBoundsEosDataMap(&gEosDataMap, th);

  EOS_INTEGER nxtbl, nytbl;

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;

#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
  char fn[50] = "eos_RationalInterpolate_RecordType1.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  *errorCode = EOS_OK;

  if (nXYPairs <= 0) {
    *errorCode = EOS_FAILED;
    return;
  }

  err = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;

  eos_HashTable1D *X_ht = me->R_hashTable;
  eos_HashTable1D *Y_ht = me->T_hashTable;

  if (me->isInvertedAtSetup) {
    cat = EOS_INVERTED_AT_SETUP;
    subTableNum = 1;
  }
  else {
    cat = EOS_CATEGORY (dataType);
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  }
  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);
  /* note, when the data is log 10 - cold curve, we need to take the indep. var into this form when inverting */
  tableNum = EOS_TYPE_TO_TAB_NUM (dataType);

  if (me->eosData.eos_IsRequiredDataLoaded && ! me->eosData.eos_IsRequiredDataLoaded(me, dataType)) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, &F_ht, subTableNum);

  /* initialize F, dFx, dFy to zero */
  for (i = 0; i < nXYPairs; i++) {
    fVals[i] = ZERO;
    dFx[i] = ZERO;
    dFy[i] = ZERO;
    if (dFCx)
      dFCx[i] = ZERO;
    if (dFx0)
      dFx0[i] = ZERO;
    if (dFy0)
      dFy0[i] = ZERO;
  }

  /* Use custom interpolator if EOS_PT_SMOOTHING is set for th */
  isPtSmooth =
    eos_getBoolOptionFromTableHandle (th, EOS_PT_SMOOTHING, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  useCustomInterp =
    eos_getBoolOptionFromTableHandle (th, EOS_USE_CUSTOM_INTERP, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  if (isPtSmooth && useCustomInterp &&
      (dataType == EOS_V_PtT || dataType == EOS_Ut_PtT)) {
    _eos_SesameInvTRecordType1 (me, dataType, nXYPairs, srchX, srchY,
                                fVals, dFx, dFy, xyBounds, errorCode, &errMsg);
    if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  /* get EOS_DISABLE_GHOST_NODES setting for this handle */
  eos_GetOptionEosInterpolation (&gEosInterpolation, th, EOS_DISABLE_GHOST_NODES, &optVal, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)  {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

  if ((!me->isInvertedAtSetup || me->useTmpGhostData) && ! me->nGhostData && (nX > 1) && (nY > 1)) {
    /* add "ghost node" data prior to interpolation */
    nGhostData = 1;
    if (EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable
        && EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) {
      /* use a NULL pointer for Y input array if operating on a 1-D table */
      _eos_CreateGhostData (EOS_FALSE, nGhostData, nX, nY, X, NULL, F, NULL,
                            &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errMsg);
      if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
      EOS_FREE(errMsg);
    }
    else if (EOS_TYPE_TO_INDEP_VAR1 (dataType) == EOS_NullTable
             && EOS_TYPE_TO_INDEP_VAR2 (dataType) != EOS_NullTable) {
      /* use a NULL pointer for X input array if operating on a 1-D table */
      _eos_CreateGhostData (EOS_FALSE, nGhostData, nX, nY, NULL, Y, F, NULL,
                            &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errMsg);
      if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
      EOS_FREE(errMsg);
    }
    else {
      /* operating on a 2-D table */
      _eos_CreateGhostData (EOS_FALSE, nGhostData, nX, nY, X, Y, F, NULL,
                            &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errMsg);
      if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
      EOS_FREE(errMsg);
    }
    if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)  {
      nGhostData = 0;
      ((eos_ErrorHandler *) me)->HandleError (me, th, err);
      return;
    }
    nX = nxtbl;
    nY = nytbl;
    X = xtbls;
    Y = ytbls;
    F = ftbls;
  }
  else {
    nGhostData = me->nGhostData;
  }

  /* Check if 1-D interpolation is required by dataType */
  if (EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable
      && EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) {

    /* fetch cross-reference data if category 3 interpolation */
    if (cat == EOS_CATEGORY3) {
      ind = EOS_EOS_TABLE_TYPE_REF2(dataType);
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (ind);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, &F_ht, subTableNum2);
    }
    else
      F2 = &null_val;

    /* interpolate */
    _eos_InterpolateRecordType_1D (EOS_FALSE, nX, X, *F, *F2, th, dataType,
                                   nXYPairs, srchX, fVals, dFx, X_ht, NULL, xyBounds,
                                   errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  else if (EOS_TYPE_TO_INDEP_VAR1 (dataType) == EOS_NullTable
           && EOS_TYPE_TO_INDEP_VAR2 (dataType) != EOS_NullTable) {

    /* fetch cross-reference data if category 3 interpolation */
    if (cat == EOS_CATEGORY3) {
      ind = EOS_EOS_TABLE_TYPE_REF2(dataType);
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (ind);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, &F_ht, subTableNum2);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, &F_ht, subTableNum);
    }
    else
      F2 = &null_val;

    /* interpolate */
    _eos_InterpolateRecordType_1D (EOS_FALSE, nY, Y, *F, *F2, th, dataType,
                                   nXYPairs, srchX, fVals, dFx, Y_ht, NULL, xyBounds,
                                   errorCode, &errMsg);
    if (errMsg) *errorCode = eos_SetCustomErrorMsg(th, *errorCode, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }


  /* Perform 2-D interpolation as required by dataType */
  xVals = srchX;
  yVals = srchY;

  switch (cat) {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
  case EOS_INVERTED_AT_SETUP:  /* indicates the table is already inverted during setup */
    {

      if (cat != EOS_INVERTED_AT_SETUP) {

        /* send in the first row = cold curve */
        if ((tableNum == 301 || tableNum == 303) && coldCurve) {
          /* interpolate cold curve at the given density */
          newColdCurve = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
          newColdCurve_dFdx = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
          _eos_InterpolateRecordType_1D (EOS_FALSE, nX, X, coldCurve, NULL, th, dataType,
                                         nXYPairs, xVals, newColdCurve, newColdCurve_dFdx, X_ht, NULL,
                                         xyBounds, &err, &errMsg);
          if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
          EOS_FREE(errMsg);
          for (i = 0; i < nXYPairs; i++) {
            if (xyBounds[i] == EOS_UNDEFINED) {
              newColdCurve[i] = coldCurve[0];
              newColdCurve_dFdx[i] = ZERO;
              err = EOS_OK;
            }
          }
        }

      }
      else {

        /* Selectively apply the required transformation of independent data values, so that yVals[]
         * contains data that is compatible with the data stored in the me->T[] array. */
        switch (EOS_CATEGORY(dataType)) {
        case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
        case EOS_CATEGORY4:          /* indicates the table is inverted with respect to 2nd independent variable
                                      * and also merged with another function */
          {
            newColdCurve      = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
            newColdCurve_dFdx = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
            yVals = (EOS_REAL*)malloc(nXYPairs*sizeof(EOS_REAL));

	    
            if (doRational)
              eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->ny, 1, 0,
                                       extrapolationBounds->x, extrapolationBounds->yLo,
                                       xVals, newColdCurve, newColdCurve_dFdx, 'y', NULL, xyBounds, &err);
            else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
              eos_LineInterpolate (EOS_TRUE, nXYPairs, extrapolationBounds->ny, 1, 0,
                                   extrapolationBounds->x, &(extrapolationBounds->yLo),
                                   xVals, newColdCurve, newColdCurve_dFdx, 'y', NULL, xyBounds, &err);
	  
            for (i = 0; i < nXYPairs; i++) {
	      yVals[i] = srchY[i] - newColdCurve[i];
	    }
            switch (EOS_TYPE_TO_INDEP_VAR2(dataType)) {
              /* special cases for pressure tables that include cold curve data */
            case EOS_Pt:
            case EOS_Pic:
              /* divide by xVals[] */
              for (i = 0; i < nXYPairs; i++) yVals[i] /= FLOOR(xVals[i]);
              break;
            default:
              // do nothing
              break;
            }
          }
          break;
        default:
          // do nothing
          break;
        }

      }

      if (doRational) {

        eos_RationalInterpolateXY (nXYPairs, xVals, yVals, dFx, dFy, fVals,
                                   nX, nY, X, Y, F, coldCurve, nGhostData, X_ht, Y_ht, xyBounds, &err);

      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) { /* interpolate linearly instead */
#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
        sprintf(fn, "eos_LinearInterpolate_RecordType1.indices");
        iyv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
        ixv = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));       /* indexes of X near which X points are */
        xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER)); /* xy-bounds for y */

        _eos_srchdf (nXYPairs, yVals, 1, nY, Y, 1, iyv, Y_ht,
                     xyBounds, errorCode);
        _eos_srchdf (nXYPairs, xVals, 1, nX, X, 1, ixv, X_ht,
                     xyBounds2, errorCode);
        _eos_DumpIndicesToFile (fn, mode, nXYPairs, X, Y, F,
                                ixv, iyv, NULL, NULL);
        EOS_FREE (xyBounds2);
        EOS_FREE (ixv);
        EOS_FREE (iyv);
#endif
        eos_BiLineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
                               nXYPairs, nX, nY, X, Y, F, nGhostData, xVals, yVals, fVals,
                               dFx, dFy, X_ht, Y_ht, xyBounds, &err);
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED) {
        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          EOS_FREE (newColdCurve);
          EOS_FREE (newColdCurve_dFdx);
        }
        break;
      }

      if (cat != EOS_INVERTED_AT_SETUP) {

        for (i = 0; i < nXYPairs; i++) {

          /* the interpolated values are given as F-coldCurve, make them into F */
          if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
            //OLgaLog10             fVals[i] = (tableNum == 301 || tableNum == 303)?  pow(10.0, fVals[i]) + newColdCurve[i] : pow(10.0, fVals[i]) ;
            fVals[i] = fVals[i] + newColdCurve[i];
            // DAP -- Add dFcold/dx to dF/dx since interpolator was only using thermal data
            dFx[i] = dFx[i] + newColdCurve_dFdx[i];
            if (dFCx)
              dFCx[i] = newColdCurve_dFdx[i];
          }
          else if (dFCx)
            dFCx[i] = ZERO;

          if (dFx0)
            dFx0[i] = dFx[i];
          if (dFy0){
            dFy0[i] = dFy[i];
	  }
          if (!skipExtrap && xyBounds[i])
            *errorCode = EOS_INTERP_EXTRAPOLATED;

        }

        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          EOS_FREE (newColdCurve);
          EOS_FREE (newColdCurve_dFdx);
        }

      }
      else {

        /* check extrapolation to override incorrect values returned by interpolator wrappers above */
        if (!skipExtrap) me->eosData.CheckExtrap(ptr, th, dataType, nXYPairs, xVals, yVals, xyBounds, &err);

        /* appropriately correct derivatives for cat. 2 or 4 results */
        switch (EOS_CATEGORY(dataType)) {
        case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
        case EOS_CATEGORY4:          /* indicates the table is merged with another function */
          {

            switch (EOS_TYPE_TO_INDEP_VAR2(dataType)) {
              /* special cases for tables that include cold curve data */
            case EOS_Pt:
            case EOS_Pic:
              for (i = 0; i < nXYPairs; i++) {
                dFx[i] = dFx[i] - (dFy[i] / xVals[i]) * (yVals[i] + newColdCurve_dFdx[i]); /* correct dFx */
                dFy[i] = dFy[i] / FLOOR(xVals[i]);                                         /* correct dFy */
              }
              break;
            case EOS_Ut:
            case EOS_Uic:
            case EOS_At:
            case EOS_Aic:
              for (i = 0; i < nXYPairs; i++) dFx[i] = dFx[i] - dFy[i] * newColdCurve_dFdx[i]; /* correct dFx */
              break;
            default:
              // do nothing
              break;
            }

          }
          break;
        default:
          // do nothing
          break;
        }

      }
#ifdef _EOS_DUMP_INDEX_DATA_INTERPOLATE_RECORDTYPE1
      mode = appendMode; /* reset mode for indices output file */
#endif

      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
    {
      /* means that the user provides y and F of a function that is defined as x( F,y)
         so effectively the x and y user has given us are F and Y */

      /* do inverse interpolation with respect to first var, x
         NOTE: dFx and dFx are dF/dx and dF/dy for category 0 table,
         so they must be transformed below. */
      if (doRational) {
        err = EOS_OK;
        eos_InverseRationalInterpolateFY (nXYPairs, yVals, xVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, coldCurve, nGhostData, X_ht, Y_ht, F_ht,
                                          xyBounds, &err, &errMsg);
        if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
        EOS_FREE(errMsg);
      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_InverseBilinearInterpolateFY (nXYPairs, yVals, xVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, coldCurve, Y_ht, F_ht, nGhostData,
                                          xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED)
        break;

      for (i = 0; i < nXYPairs; i++) {

        /* if EOS_CANT_INVERT_DATA, then potential div-by-zero */
        if (xyBounds[i] == EOS_CANT_INVERT_DATA) {
          dFx[i] = dFy[i] = fVals[i] = 1.0e99;
          continue;
        }

        /* optionally return dF/dx and dF/dy  for later use to
           transform partial derivatives */
        if (dFx0)
          dFx0[i] = dFx[i];
        if (dFy0)
          dFy0[i] = dFy[i];

        /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
           This is a temporary kludge to prevent SIGFPE. */
        /* dFy in this case needs to be transformed to dx/dy */
        if (EOS_CHECK_PRODUCT(dFy[i], -ONE/dFx[i]))
          dFy[i] =(-ONE/FLOOR(dFx[i]))*dFy[i];
        else
          dFy[i] = SIGN(dFy[i]) * SIGN(-ONE/FLOOR(dFx[i])) *
            (EOS_IS_PRODUCT_GT_MAX(dFy[i], -ONE/FLOOR(dFx[i])) ? DBL_MAX : DBL_MIN);

        /*calculate dx/dF*/
        dFx[i] = ONE / FLOOR(dFx[i]);

        /* compute EOS_V_PtT differently: v = 1/r, dv/dP = -(1/r^2) * dr/dP = -1/xVals[i]^2 * dFx[i] */
        if (dataType == EOS_V_PtT) {
          fVals[i] = ONE / MAX (TINY_D, fVals[i]);

          /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
             This is a temporary kludge to prevent SIGFPE. */
          if (EOS_CHECK_PRODUCT(fVals[i], fVals[i]) &&
              EOS_CHECK_PRODUCT(fVals[i]*fVals[i],dFx[i]))
            dFx[i] = -ONE * pow (fVals[i], 2.0) * dFx[i];
          else
            dFx[i] = -ONE * SIGN(dFx[i]) *
              (EOS_IS_PRODUCT_GT_MAX(fVals[i], fVals[i]) ||
               EOS_IS_PRODUCT_GT_MAX(fVals[i]*fVals[i],dFx[i]) ? DBL_MAX : DBL_MIN);
        }
      }
      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
    {
      /* do inverse interpolation with respect to second var */
      /* means that the user provides x and F of a function that is defined as y( x,F)
         so effectively the x and y user has given us are X and F */

      EOS_BOOLEAN invertAtSetup = EOS_FALSE;
      invertAtSetup = eos_getBoolOptionFromTableHandle (th, EOS_INVERT_AT_SETUP, &err);

      /* send in the first row = cold curve */
      if (! invertAtSetup &&
          (tableNum == 301 || tableNum == 303) && coldCurve) {
        /* interpolate cold curve given density */
        newColdCurve = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        newColdCurve_dFdx =
          (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

        if (doRational)
          eos_RationalInterpolate (EOS_FALSE, nXYPairs, nX, 1, 0, X, coldCurve, xVals,
                                   newColdCurve, newColdCurve_dFdx, 'y', NULL,
                                   xyBounds, &err);
        else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) { /* interpolate linearly instead */
          eos_LineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err),
                               nXYPairs, nX, 1, 0, X, &coldCurve,
                               xVals, newColdCurve, newColdCurve_dFdx, 'y', X_ht,
                               xyBounds, &err);
          for (i = 0; i < nXYPairs; i++) {
            if (xyBounds[i] == EOS_UNDEFINED) {
              newColdCurve[i] = coldCurve[0];
              newColdCurve_dFdx[i] = ZERO;
              err = EOS_OK;
            }
          }
        }
      }

      /* need to take yVals which are in fact F-vals into the same form as stored in the table */
      newFVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      for (i = 0; i < nXYPairs; i++)
        // OlgaLog10    newFVals[i] = (tableNum == 301 || tableNum == 303)?  log10(yVals[i] - newColdCurve[i]) : log10(yVals[i]);
        newFVals[i] = ((tableNum == 301 || tableNum == 303)
                       && newColdCurve) ? yVals[i] -
          newColdCurve[i] : yVals[i];

      /* do inverse interpolation with respect to second var */
      if (doRational) {
        err = EOS_OK;
        eos_InverseRationalInterpolateXF (nXYPairs, xVals, newFVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, X_ht, Y_ht, F_ht, nGhostData, xyBounds, dataType, invertAtSetup,
                                          &err, &errMsg);
        if (errMsg) err = eos_SetCustomErrorMsg(th, err, "%s", errMsg);
         EOS_FREE(errMsg);
      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
        eos_InverseBilinearInterpolateXF (nXYPairs, xVals, newFVals, dFx, dFy,
                                          fVals, nX, nY, X, Y, F, X_ht, F_ht, nGhostData, xyBounds,
                                          &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED) {
        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          EOS_FREE (newColdCurve);
          EOS_FREE (newColdCurve_dFdx);
        }
        EOS_FREE (newFVals);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {

        /* if EOS_CANT_INVERT_DATA, then potential div-by-zero */
        if (xyBounds[i] == EOS_CANT_INVERT_DATA) {
          dFx[i] = dFy[i] = fVals[i] = 1.0e99;
          continue;
        }

        /* optionally return dF/dx and dF/dy  for later use to
           transform partial derivatives */
        if (dFy0)
          dFy0[i] = dFy[i];

        /* transform dFy from dF/dy to dy/dF */
        dFy[i] = ONE/FLOOR(dFy[i]);

        if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
          if (dFx0)
            dFx0[i] = dFx[i] + newColdCurve_dFdx[i];

          /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
             This is a temporary kludge to prevent SIGFPE. */
          if (EOS_CHECK_PRODUCT(-(dFx[i] + newColdCurve_dFdx[i]), dFy[i]))
            dFx[i] = -(dFx[i] + newColdCurve_dFdx[i]) * dFy[i];
          else
            dFx[i] = -(SIGN(dFx[i] + newColdCurve_dFdx[i])) * SIGN(dFy[i]) *
              (EOS_IS_PRODUCT_GT_MAX(-(dFx[i] + newColdCurve_dFdx[i]), dFy[i]) ? DBL_MAX : DBL_MIN);

          if (dFCx)
            dFCx[i] = newColdCurve_dFdx[i];
        }
        else {
          if (dFx0)
            dFx0[i] = dFx[i];

          /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
             This is a temporary kludge to prevent SIGFPE. */
          if (EOS_CHECK_PRODUCT(dFx[i], dFy[i]))
            dFx[i] = -dFx[i] * dFy[i];
          else
            dFx[i] = -(SIGN(dFx[i])) * SIGN(dFy[i]) *
              (EOS_IS_PRODUCT_GT_MAX(dFx[i], dFy[i]) ? DBL_MAX : DBL_MIN);

          if (dFCx)
            dFCx[i] = ZERO;
        }

      }

      if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
        EOS_FREE (newColdCurve);
        EOS_FREE (newColdCurve_dFdx);
      }
      EOS_FREE (newFVals);
      break;
    }
  case EOS_CATEGORY4:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these variables */
      tabInd1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need F(X,Y) given F(X, U) and U(X, Y) */
      /* 1. find U using second table, 2. find F using 1st table. */
      uVals = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dYx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUCx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dFCx1 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

      _eos_InterpolateRecordType1 (eosData, th, X_U_F, tabInd2, nXYPairs,
                                   xVals, yVals, uVals, dFx /*dUx */ , dUy,
                                   dUCx, dYx, NULL, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUy);
        EOS_FREE (uVals);
        EOS_FREE (dUCx);
        EOS_FREE (dFCx1);
        EOS_FREE (dYx);
        EOS_FREE (xyBounds2);
        break;
      }
      _eos_InterpolateRecordType1 (eosData, th, X_Y_U, tabInd1, nXYPairs,
                                   xVals, uVals, fVals, dFx, dFy /*dFu */ ,
                                   dFCx1, NULL, NULL, xyBounds2, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUy);
        EOS_FREE (uVals);
        EOS_FREE (dUCx);
        EOS_FREE (dFCx1);
        EOS_FREE (dYx);
        EOS_FREE (xyBounds2);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {
        /* derivative dFy = dFu * dUy; dFx = dFx - dFy * dUCx */

        /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
           This is a temporary kludge to prevent SIGFPE. */
        if (EOS_CHECK_PRODUCT(dUy[i], dFy[i]))
          dFy[i] = dUy[i] * dFy[i] /*dFu */ ;
        else
          dFy[i] = SIGN(dUy[i]) * SIGN(dFy[i]) *
            (EOS_IS_PRODUCT_GT_MAX(dUy[i], dFy[i]) ? DBL_MAX : DBL_MIN);

        /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
           This is a temporary kludge to prevent SIGFPE. */
        if (EOS_CHECK_PRODUCT(dYx[i], dFy[i]))
          dFx[i] = dFx[i] - dFy[i] * dYx[i];
        else
          dFx[i] = SIGN(dYx[i]) * SIGN(dFy[i]) *
            (EOS_IS_PRODUCT_GT_MAX(dYx[i], dFy[i]) ? DBL_MAX : DBL_MIN);

        /* combine extrapolation codes */
	
	if (!skipExtrap)  xyBounds[i] = _eos_CombineExtrapErrors (xyBounds[i], xyBounds2[i]);
      }
      EOS_FREE (dUy);
      EOS_FREE (uVals);
      EOS_FREE (dUCx);
      EOS_FREE (dFCx1);
      EOS_FREE (dYx);
      EOS_FREE (xyBounds2);
      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these variables */
      tabInd1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need F(X,Y) given F(U, Y) and U(X, Y) */
      /* 1. find U using second table, 2. find F using 1st table. */
      uVals = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dXy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      xyBounds2 = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
      _eos_InterpolateRecordType1 (eosData, th, U_Y_F, tabInd2, nXYPairs,
                                   xVals, yVals, uVals, dUx, dFy /*dUy */ ,
                                   NULL, NULL, dXy, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        EOS_FREE (dXy);
        EOS_FREE (xyBounds2);
        break;
      }
      _eos_InterpolateRecordType1 (eosData, th, X_Y_U, tabInd1, nXYPairs,
                                   uVals, yVals, fVals, dFx /*dFu */ , dFy,
                                   NULL, NULL, NULL, xyBounds2, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_UNDEFINED) {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        EOS_FREE (dXy);
        EOS_FREE (xyBounds2);
        break;
      }

      for (i = 0; i < nXYPairs; i++) {
        /* derivative dFx = dFu * dUx */

        /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
           This is a temporary kludge to prevent SIGFPE. */
        if (EOS_CHECK_PRODUCT(dUx[i], dFx[i]))
          dFx[i] = dUx[i] * dFx[i] /*dFu */ ;
        else
          dFx[i] = SIGN(dUx[i]) * SIGN(dFx[i]) *
            (EOS_IS_PRODUCT_GT_MAX(dUx[i], dFx[i]) ? DBL_MAX : DBL_MIN);

        /* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
           This is a temporary kludge to prevent SIGFPE. */
        if (EOS_CHECK_PRODUCT(dXy[i], dFx[i]))
          dFy[i] = dFy[i] - dFx[i] * dXy[i];
        else
          dFy[i] = SIGN(dXy[i]) * SIGN(dFx[i]) *
            (EOS_IS_PRODUCT_GT_MAX(dXy[i], dFx[i]) ? DBL_MAX : DBL_MIN);

        /* combine extrapolation codes */
        if (!skipExtrap) xyBounds[i] = _eos_CombineExtrapErrors (xyBounds[i], xyBounds2[i]);
      }
      EOS_FREE (dUx);
      EOS_FREE (uVals);
      EOS_FREE (dXy);
      EOS_FREE (xyBounds2);
      break;
    }
  default:
    {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
      return;
      break;
    }
  }

  /* Finally, check for extrapolation using the most current and correct method */
  if (!skipExtrap && me->eosData.CheckExtrap)
    me->eosData.CheckExtrap(ptr, th, dataType, nXYPairs, xVals, yVals, xyBounds, errorCode);

  if (srchX != xVals)
    EOS_FREE (xVals);
  if (srchY != yVals)
    EOS_FREE (yVals);
  EOS_FREE (newColdCurve);
  EOS_FREE (newColdCurve_dFdx);

 if ((!me->isInvertedAtSetup || me->useTmpGhostData) && ! me->nGhostData && (nX > 1) && (nY > 1)) {
    /* free arrays, which were allocated within _eos_CreateGhostData above */
    *errorCode = _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, NULL);
  }
}

/***********************************************************************/
/*!
 * \brief Function _eos_InterpolateRecordType1GpuLimited (helping function for
 *  eos_InterpolateRecordType1().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                            to x and y.
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to y.
 * \param[out]   dFCx[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cold curve with respect to x.
 * \param[out]   dFx0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt x (for inverse
 *                                            functions mostly).
 * \param[out]   dFy0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt y (for inverse
 *                                            functions mostly).
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table Handle
 * \param[in]    varOrder        - EOS_INTEGER : order of variables
 * \param[in]    dataType        - EOS_INTEGER : dataType
 * \param[in]    nXYPairs        - EOS_INTEGER : total number of pairs of independent variable values provided for interpolation.
 * \param[in]    srchX[nXYPairs] - EOS_REAL : array of the primary independent variable values to use during interpolation.
 * \param[in]    srchY[nXYPairs] - EOS_REAL : array of the secondary independent variable values to use during interpolation.
 *
 * \return none
 *
 ***********************************************************************/

void _eos_InterpolateRecordType1GpuLimited (void *ptr, EOS_INTEGER th,
                                  EOS_INTEGER varOrder, EOS_INTEGER dataType,
                                  EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                  EOS_REAL *srchY, EOS_REAL *fVals,
                                  EOS_REAL *dFx, EOS_REAL *dFy,
                                  EOS_REAL *dFCx, EOS_REAL *dFx0,
                                  EOS_REAL *dFy0, EOS_INTEGER *xyBounds,
                                  EOS_INTEGER *errorCode)
{

#ifdef DO_OFFLOAD
  EOS_INTEGER i, ind, doRational = 0,
    err, nX, nY, cat, subTableNum, subTableNum2, tableNum;
  EOS_REAL *X, *Y, **F, **F2,
    *coldCurve, *newColdCurve = NULL, *newColdCurve_dFdx = NULL, *null_val = NULL;

  eos_Data *eosData;
  eos_RecordType1 *me;
  EOS_BOOLEAN isPtSmooth = EOS_FALSE, useCustomInterp = EOS_FALSE;
  EOS_INTEGER nGhostData=0;

  EOS_BOOLEAN optVal = EOS_FALSE;
  EOS_CHAR *errMsg = NULL;
  eos_ExtrapolationBoundsEosDataMap *extrapolationBounds = eos_GetExtrapolationBoundsEosDataMap(&gEosDataMap, th);

  doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

  *errorCode = EOS_OK;
  if (nXYPairs <= 0) {
    *errorCode = EOS_FAILED;
    return;
  }

  err = EOS_OK;
  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;

  if (me->isInvertedAtSetup) {
    cat = EOS_INVERTED_AT_SETUP;
    subTableNum = 1;
  }
  else {
    cat = EOS_CATEGORY (dataType);
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  }
  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);
  /* note, when the data is log 10 - cold curve, we need to take the indep. var into this form when inverting */
  tableNum = EOS_TYPE_TO_TAB_NUM (dataType);

  if (me->eosData.eos_IsRequiredDataLoaded && ! me->eosData.eos_IsRequiredDataLoaded(me, dataType)) {
    *errorCode = EOS_DATA_TYPE_NOT_FOUND;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  
  /* Fetch data array pointers */
  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, NULL, subTableNum);

    /* Use custom interpolator if EOS_PT_SMOOTHING is set for th */
  isPtSmooth =
    eos_getBoolOptionFromTableHandle (th, EOS_PT_SMOOTHING, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  useCustomInterp =
    eos_getBoolOptionFromTableHandle (th, EOS_USE_CUSTOM_INTERP, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }
  if (isPtSmooth && useCustomInterp &&
		  (dataType == EOS_V_PtT || dataType == EOS_Ut_PtT)) {
	  assert(EOS_FALSE && "This setup is not ported to GPU");
  }

  /* get EOS_DISABLE_GHOST_NODES setting for this handle */
  eos_GetOptionEosInterpolation (&gEosInterpolation, th, EOS_DISABLE_GHOST_NODES, &optVal, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)  {
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  if ((!me->isInvertedAtSetup || me->useTmpGhostData) && ! me->nGhostData && (nX > 1) && (nY > 1)) {
	  assert(EOS_FALSE && "This setup is not ported to GPU");
  }
  else {
    nGhostData = me->nGhostData;
  }

  /* Check if 1-D interpolation is required by dataType */
  if (EOS_TYPE_TO_INDEP_VAR1 (dataType) != EOS_NullTable
      && EOS_TYPE_TO_INDEP_VAR2 (dataType) == EOS_NullTable) {
    /* fetch cross-reference data if category 3 interpolation */
    if (cat == EOS_CATEGORY3) {
      ind = EOS_EOS_TABLE_TYPE_REF2(dataType);
      subTableNum2 = EOS_TYPE_TO_SUB_TAB_NUM (ind);
      _eos_GetDataRecordType1 (me, &X, &Y, &F2, &coldCurve, NULL, subTableNum2);
    }
    else
      F2 = &null_val;

    /* interpolate */
    _eos_InterpolateRecordType_1D_GpuLimited (EOS_FALSE, nX, X, F, F2, th, dataType,
                                   nXYPairs, srchX, fVals, dFx, xyBounds,
                                   errorCode, &errMsg);
    return;
  }
  else if (EOS_TYPE_TO_INDEP_VAR1 (dataType) == EOS_NullTable
           && EOS_TYPE_TO_INDEP_VAR2 (dataType) != EOS_NullTable) {
	  assert(EOS_FALSE && "This setup is not ported to GPU");
  }


  EOS_BOOLEAN isValidCat = (cat==EOS_CATEGORY0 || cat==EOS_INVERTED_AT_SETUP);
  assert(isValidCat && "This setup is not ported to GPU");

  /*unsupported cats: EOS_CATEGORY1/2/4/3  when !inverted-at-setup*/

  /* Selectively apply the required transformation of independent data values, so that yVals[]
   * contains data that is compatible with the data stored in the me->T[] array. */

  EOS_BOOLEAN isValidCat2 = (EOS_CATEGORY(dataType)==EOS_CATEGORY2 || EOS_CATEGORY(dataType)==EOS_CATEGORY4);
 
  
 
  assert(isValidCat2 && "This setup not ported to GPU");

  int h = omp_get_initial_device();
  int t = omp_get_default_device();
  newColdCurve = omp_target_alloc(nXYPairs * sizeof (EOS_REAL),t);
  newColdCurve_dFdx =omp_target_alloc(nXYPairs * sizeof (EOS_REAL),t);
 
  int yind = gEosDataMap.yinds[th];
  int xarrind = gEosDataMap.xarrinds[th];
  EOS_REAL* eb_yLo=&gEosDataMap.eb_yLo[yind];
  EOS_REAL* eb_x=&gEosDataMap.eb_x[xarrind];      
 
  

  if (doRational)
    eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->ny, 1, 0,
			     eb_x, eb_yLo,
			     srchX, newColdCurve, newColdCurve_dFdx, 'y', NULL, xyBounds, &err);
  
  else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */{
    EOS_BOOLEAN useOrigDeriv=EOS_TRUE;

    if (useOrigDeriv) 
      _eos_LineInterpolateWithOriginalDerivatives(nXYPairs, extrapolationBounds->ny, 1, 0,eb_x, 
						  eb_yLo,srchX, newColdCurve, 
						  newColdCurve_dFdx, 'y', NULL, xyBounds, &err);
    else 
      _eos_LineInterpolateWithContinuousDerivatives(nXYPairs, extrapolationBounds->ny, 1, 0,eb_x, 
						  eb_yLo,srchX, newColdCurve, 
						  newColdCurve_dFdx, 'y', NULL, xyBounds, &err);
  }

#pragma omp target is_device_ptr(srchY, newColdCurve)
  {
    #pragma omp teams distribute parallel for
    for (i = 0; i < nXYPairs; i++) {
      srchY[i] = srchY[i] - newColdCurve[i];
   }
  }

  if (EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pt || EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pic) {
    /* special cases for pressure tables that include cold curve data */
#pragma omp target is_device_ptr(srchY, srchX)
    {
#pragma omp teams distribute parallel for
      for (i = 0; i < nXYPairs; i++) srchY[i] /= FLOOR(srchX[i]);
    }
  }

  if (doRational)
    eos_RationalInterpolateXY (nXYPairs, srchX, srchY, dFx, dFy, fVals,
			       nX, nY, X, Y, F, coldCurve, nGhostData, NULL, NULL, xyBounds, &err);
  else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err))  {/* interpolate linearly instead */
    if (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err))
      _eos_BiLineInterpolateWithOriginalDerivatives(nXYPairs, nX, nY, X, Y, F, nGhostData, srchX, srchY, fVals,
			  dFx, dFy, NULL, NULL, xyBounds, &err);
    else
      _eos_BiLineInterpolateWithContinuousDerivatives(nXYPairs, nX, nY, X, Y, F, nGhostData, srchX, srchY, fVals,
			  dFx, dFy, NULL, NULL, xyBounds, &err);
  }
  if (cat != EOS_INVERTED_AT_SETUP) {

    if ((tableNum == 301 || tableNum == 303) && newColdCurve) {
      assert(EOS_FALSE && "This setup is not ported to GPU");
    } else {
#pragma omp target is_device_ptr(dFCx,dFx0,dFy0)
      {
#pragma omp teams distribute parallel for
	for (i = 0; i < nXYPairs; i++) {
	  if (dFCx) dFCx[i] = ZERO;
	  if (dFx0) dFx0[i] = dFx[i];
	  if (dFy0) dFy0[i] = dFy[i];
	}
      }
    }
  } else {

    /* appropriately correct derivatives for cat. 2 or 4 results */
    if (EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pt || EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pic){
#pragma omp target is_device_ptr(dFx,dFy,srchX,srchY,newColdCurve_dFdx)
      {
#pragma omp teams distribute parallel for
	for (i = 0; i < nXYPairs; i++) {
	  dFx[i] = dFx[i] - (dFy[i] / srchX[i]) * (srchY[i] + newColdCurve_dFdx[i]); /* correct dFx */
	  dFy[i] = dFy[i] / FLOOR(srchX[i]);                                         /* correct dFy */
	}
      }
    } else if (EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Ut || EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Uic ||
	       EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_At || EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Aic) {
#pragma omp target is_device_ptr(dFx,dFy, newColdCurve_dFdx)
      {
#pragma omp teams distribute parallel for
	for (i = 0; i < nXYPairs; i++) dFx[i] = dFx[i] - dFy[i] * newColdCurve_dFdx[i]; /* correct dFx */
      }
    }
  }


  if (EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pt || EOS_TYPE_TO_INDEP_VAR2(dataType)==EOS_Pic) {
              /* revert special cases for pressure tables that include cold curve data */
#pragma omp target is_device_ptr(srchY, srchX)
    {
#pragma omp teams distribute parallel for
	  for (i = 0; i < nXYPairs; i++) srchY[i] *= FLOOR(srchX[i]);
    }
  }
#pragma omp target is_device_ptr(srchY, newColdCurve)
  {
#pragma omp teams distribute parallel for
    for (i = 0; i < nXYPairs; i++) srchY[i] = srchY[i] + newColdCurve[i];
  }
  omp_target_free(newColdCurve, t);
  omp_target_free(newColdCurve_dFdx, t);

#endif
}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_Pt_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = - T->DerivativeScalar(T, 1, 0, x, y);

  /* partial derivatives */
  *dFx = (x * x) *T->DerivativeScalar(T, 2, 0, x, y);
  *dFy = - T->DerivativeScalar(T, 1, 1, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_Ut_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = T->EvaluateScalar(T, x, y) - y * T->DerivativeScalar(T, 0, 1, x, y);

  /* partial derivatives */
  *dFx = - (x * x) *(T->DerivativeScalar(T, 1, 0, x, y) - y * T->DerivativeScalar(T, 1, 1, x, y));
  *dFy = - y * T->DerivativeScalar(T, 0, 2, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTaylorFor_dataType_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = T->EvaluateScalar(T, x, y);

  /* partial derivatives */
  *dFx = -(x * x) *T->DerivativeScalar(T, 1, 0, x, y);
  *dFy = T->DerivativeScalar(T, 0, 1, x, y);

  return EOS_OK;
}

EOS_INTEGER _eos_EvaluateTransformedTaylorFor_St_DT (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy)
{
  /* initialize */
  *f = *dFx = *dFy = 0;

  /* function */
  *f = - T->DerivativeScalar(T, 0, 1, x, y);

  /* partial derivatives */
  *dFx = (x * x) * T->DerivativeScalar(T, 1, 1, x, y);
  *dFy = - T->DerivativeScalar(T, 0, 2, x, y);

  return EOS_OK;
}

/***********************************************************************/
/*!
 * \brief Function _eos_EvaluateTaylorRecordType1 (helping function for
 *  eos_InterpolateRecordType1().
 *
 * \param[out]   fVals[nXYPairs] - EOS_REAL : array of the interpolated data corresponding
 *                                            to x and y.
 * \param[out]   dFx[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to x.
 * \param[out]   dFy[nXYPairs]   - EOS_REAL : array of the interpolated partial derivatives
 *                                            of fVals with respect to y.
 * \param[out]   dFCx[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cold curve with respect to x.
 * \param[out]   dFx0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt x (for inverse
 *                                            functions mostly).
 * \param[out]   dFy0[nXYPairs]  - EOS_REAL : optional array of the interpolated partial
 *                                            derivatives of cat 0 F wrt y (for inverse
 *                                            functions mostly).
 * \param[out]   *xyBounds       - EOS_INTEGER : interpolation errors per xy-pair
 * \param[out]   errorCode       - EOS_INTEGER : error code of the interpolation:
 *                                               EOS_INTERP_EXTRAPOLATED or EOS_OK
 * \param[in]    *ptr            - void : data object pointer;
 *                                        internally recast to eos_RecordType1*
 * \param[in]    th              - EOS_INTEGER : table Handle
 * \param[in]    dataType        - EOS_INTEGER : dataType
 * \param[in]    nXYPairs        - EOS_INTEGER : total number of pairs of independent variable values provided for interpolation.
 * \param[in]    srchX[nXYPairs] - EOS_REAL : array of the primary independent variable values to use during interpolation.
 * \param[in]    srchY[nXYPairs] - EOS_REAL : array of the secondary independent variable values to use during interpolation.
 *
 * \return none
 *
 ***********************************************************************/
void _eos_EvaluateTaylorRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                     EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                     EOS_REAL *srchY, EOS_REAL *fVals,
                                     EOS_REAL *dFx, EOS_REAL *dFy,
                                     EOS_REAL *dFCx, EOS_REAL *dFx0,
                                     EOS_REAL *dFy0, EOS_INTEGER *xyBounds,
                                     EOS_INTEGER *errorCode)
{
  eos_RecordType1 *me = (eos_RecordType1 *) ptr;

  EOS_INTEGER subTableNum = -1, my_subTableNum = -1;
  EOS_INTEGER k;

  my_subTableNum = EOS_TYPE_TO_SUB_TAB_NUM(dataType) - 1;
  if (me->Taylor_objects[my_subTableNum])
    subTableNum = my_subTableNum;
  else
    subTableNum = 2; /* transform Helmholtz free energy according to dataType */

  if (my_subTableNum >= 0 && *(me->_eos_EvaluateTaylor)[my_subTableNum]) {

    /* Evaluate Taylor polynomial (currently only for EOS_Pt, EOS_Ut, EOS_At, or EOS_St functions) */
    EOS_INTEGER i, *ix_low = NULL, *iy_low = NULL;
    EOS_INTEGER err = EOS_OK;
    eos_Taylor *T = NULL;
    EOS_REAL *inverse_srchX = NULL;
    EOS_REAL xMin, yMin, xMax, yMax;

    /* locate indexes of X, Y */
    ix_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
    assert(ix_low != NULL);
    iy_low = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
    assert(iy_low != NULL);

    inverse_srchX = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
    assert(inverse_srchX != NULL);
    for (i=0;i<nXYPairs;i++) { /* invert X values for search; impose TINY_D as minimum */
      inverse_srchX[i] = 1.0 / MAX(srchX[i], TINY_D);
    }

    // TODO: Should the n args be +1?
    _eos_srchdf (nXYPairs, inverse_srchX, 1, me->M + 1, me->TX, 1, ix_low, NULL, xyBounds, &err);
    _eos_srchdf (nXYPairs, srchY, 1, me->N + 1, me->TY, 1, iy_low, NULL, xyBounds, &err);

    /* evaluate */
    for (i=0;i<nXYPairs;i++) {
      k = ix_low[i] + iy_low[i] * me->M;
      T = (me->Taylor_objects[subTableNum])[k];

      //printf("ix_low=%d\tiy_low=%d\tk=%d\ta=%e\tb=%e\tx=%e\ty=%e\n",ix_low[i],iy_low[i],k,T->a,T->b,inverse_srchX[i],srchY[i]);

      /* evaluate function and partial derivatives based upon dataType */
      err = (*(me->_eos_EvaluateTaylor)[my_subTableNum])(T, inverse_srchX[i], srchY[i], &fVals[i], &dFx[i], &dFy[i]);
    }

    if (! err) {
      /* fix xyBounds[] array content since ranges in me->TX and/or me->TY do not represent to original data ranges */
      T = (me->Taylor_objects[subTableNum])[0];
      xMin = (me->tabulated_rhozero_exists) ? 0.0 : T->tx_interval[0];
      yMin = T->ty_interval[0];
      T = (me->Taylor_objects[subTableNum])[(me->M - 1) + (me->N - 1) * me->M];
      xMax = T->tx_interval[1];
      yMax = T->ty_interval[1];
      EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
      if (!skipExtrap) *errorCode = _eos_CheckExtrapEosInterpolationGeneric(xMin, xMax, yMin, yMax, nXYPairs, srchX, srchY, xyBounds);
    }
    else {
      *errorCode = err;
    }

    EOS_FREE (iy_low);
    EOS_FREE (ix_low);
    EOS_FREE (inverse_srchX);

  }
  else {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
  }

  ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
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
void eos_InterpolateRecordType1 (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType,
                                 EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                 EOS_REAL *srchY, EOS_REAL *fVals,
                                 EOS_REAL *dFx, EOS_REAL *dFy,
                                 EOS_INTEGER *xyBounds,
                                 EOS_INTEGER *errorCode)
{
  eos_OptionValue *optVal = NULL;
  eos_RecordType1 *me;
  me = (eos_RecordType1 *) ptr;

  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    ((eos_ErrorHandler *) me)->HandleError (me, th, *errorCode);
    return;
  }

  optVal = _eos_getOptionEosData (&(me->eosData), EOS_USE_TAYLOR_FIT);

  if (optVal->bval) {

    _eos_EvaluateTaylorRecordType1 (ptr, th, dataType, nXYPairs, srchX,
				    srchY, fVals, dFx, dFy, NULL, NULL, NULL,
				    xyBounds, errorCode);

  }
  else {

    EOS_INTEGER err = EOS_OK;
    EOS_BOOLEAN invertAtSetup = EOS_FALSE;
    /* DAP (2017-06-01) - Uncommenting the following line will create several test failures */
    //invertAtSetup =
      eos_getBoolOptionFromTableHandle (th, EOS_INVERT_AT_SETUP, &err);

    if (me->isInvertedAtSetup || invertAtSetup)
      eos_SetOptionEosInterpolation (&gEosInterpolation, th, EOS_DISABLE_GHOST_NODES, EOS_TRUE, errorCode);

    /* Interpolate tabular data */



#ifdef DO_OFFLOAD
    EOS_BOOLEAN  useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;

    if (!useGpuData)
#endif /* DO_OFFLOAD */
    {
      _eos_InterpolateRecordType1 (ptr, th, me->eosData.varOrder, dataType, nXYPairs, srchX,
				 srchY, fVals, dFx, dFy, NULL, NULL, NULL,
				 xyBounds, errorCode);
    }
#ifdef DO_OFFLOAD
    else
    {
      _eos_InterpolateRecordType1GpuLimited (ptr, th, me->eosData.varOrder, dataType, nXYPairs, srchX,
				 srchY, fVals, dFx, dFy, NULL, NULL, NULL,
				 xyBounds, errorCode);
    }
#endif /* DO_OFFLOAD */
  }
}

/***********************************************************************/
/*!
 * \brief If the EOS_INTERP_EXTRAPOLATED error code is returned by either
 *  eos_Interp or eos_Mix, this routine allows the user to determine which
 *  (x,y) pairs caused extrapolation and in which direction (high or low),
 *  it occurred.
 *
 * The input arguments are
 * xVals	This is an array of the primary independent variable values to use during interpolation. There are nXYPairs elements in xVals.
 * yVals	This is an array of the secondary independent variable values to use during interpolation. There are nXYPairs elements in yVals.
 *
 * \param[out]   xyBounds  - EOS_INTEGER : This is an array of size nXYPairs elements that
 *                                         returns EOS_OK if extrapolation did not occur.
 *                                         If extrapolation occurred the variable and direction
 *                                         are determined from Table 2.
 * \param[out]   errorCode - EOS_INTEGER : This is a scalar EOS_INTEGER to contain an error code.
 *                                         In the case that eos_Mix returned
 *                                         EOS_INTERP_EXTRAPOLATED as an error code,
 *                                         an additional series of steps must be performed
 *                                         to determine which tableHandle(s) correspond to
 *                                         the extrapolation error:
 *
 *                                         -# For each tableHandle sent to eos_Mix, call
 *                                            eos_GetErrorCode and, optionally, eos_GetErrorMessage.
 *                                         -# For each of these tableHandles, call eos_CheckExtrap
 *                                            to determine one of codes listed in Table 2.
 *\code
 *               Table 2. Extrapolation return codes.
 *               Code Definition
 *               EOS_OK	No extrapolation occurred.
 *               EOS_xHi_yHi	Both the x and y arguments were high.
 *               EOS_xHi_yOk	The x argument was high, the y argument was OK.
 *               EOS_xHi_yLo	The x argument was high, the y argument was low.
 *               EOS_xOk_yLo	The x argument is OK and the y argument is low.
 *               EOS_xLo_yLo	Both the x and y arguments were low.
 *               EOS_xLo_yOk	The x argument was low, the y argument was OK.
 *               EOS_xLo_yHi	The x argument was low, the y argument was OK.
 *               EOS_xOk_yHi	The x argument is OK and the y argument is high.
 \endcode
 *
 * \return none
 *
 ***********************************************************************/
void eos_CheckExtrapRecordType1_using_extrapolationBounds (void *ptr, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER nXYPairs,
                                                           EOS_REAL *srchX, EOS_REAL *srchY, EOS_INTEGER *xyBounds, EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER i, nX, nY, cat, subTableNum;
  EOS_REAL *X, *Y, **F, *coldCurve;
  EOS_INTEGER err = EOS_OK;
  EOS_REAL *xVals, *yVals;
  eos_RecordType1 *me;
  EOS_BOOLEAN isOneDimDatatype = EOS_FALSE, doRational;
  eos_ExtrapolationBoundsEosDataMap *extrapolationBounds = eos_GetExtrapolationBoundsEosDataMap(&gEosDataMap, th);

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap) 
    return;

  *errorCode = EOS_OK;

  eosData = (eos_Data *) ptr;
  me = (eos_RecordType1 *) eosData;

  assert (extrapolationBounds->stored);

  if (me->isInvertedAtSetup) {
    subTableNum = 1;
  }
  else {
    subTableNum = EOS_TYPE_TO_SUB_TAB_NUM (dataType);
  }

  cat = EOS_CATEGORY (dataType);

  /* get the size of the data */
  eos_GetSizeRecordType1 (me, &nX, &nY);

  /* make sure the data is record type 1 */
  if (EOS_TYPE_TO_RECORD_TYPE (dataType) != EOS_RECORD_TYPE1) {
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

  _eos_GetDataRecordType1 (me, &X, &Y, &F, &coldCurve, NULL, subTableNum);

  xVals = srchX;
  yVals = (! isOneDimDatatype) ? srchY : NULL;

  switch (cat)
  {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      if (! isOneDimDatatype) {

        EOS_INTEGER xFlag, yFlag;
        for (i = 0; i < nXYPairs; i++) {
          xFlag = yFlag = EOS_OK;
          if (xVals[i] < extrapolationBounds->xLo[0])
            xFlag = EOS_xLo_yOk;
          else if (xVals[i] > extrapolationBounds->xHi[0])
            xFlag = EOS_xHi_yOk;
          if (yVals[i] < extrapolationBounds->yLo[0])
            yFlag = EOS_xLo_yOk;
          else if (yVals[i] > extrapolationBounds->yHi[0])
            yFlag = EOS_xHi_yOk;
          xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
        }

      }
      else {                  /* y is either OK or not considered */

        for (i = 0; i < nXYPairs; i++) {
          if (xVals[i] < extrapolationBounds->xLo[0])
            xyBounds[i] = EOS_xLo_yOk;
          else if (xVals[i] > extrapolationBounds->xHi[0])
            xyBounds[i] = EOS_xHi_yOk;
          else
            xyBounds[i] = EOS_OK;
        }

      }
      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to 1st independent variable */
  case EOS_CATEGORY3:          /* indicates the table is a merging of a CATEGORY1 table and a CATEGORY0 table */
    {
      /* means that the user provides y and F of a function that is defined as x( F,y)
         so effectively the x and y user has given us are F and Y */
      if (! isOneDimDatatype) {

        EOS_REAL *fValsLo = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        EOS_REAL *fValsHi = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        EOS_INTEGER xFlag, yFlag;

        doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

        if (doRational) {
          eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->nx, 1, 0,
                                   extrapolationBounds->x, extrapolationBounds->xLo,
                                   yVals, fValsLo, NULL, 'y', NULL, xyBounds, &err);
          eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->nx, 1, 0,
                                   extrapolationBounds->x, extrapolationBounds->xHi,
                                   yVals, fValsHi, NULL, 'y', NULL, xyBounds, &err);
        }
        else {
          eos_LineInterpolate (EOS_TRUE, nXYPairs, extrapolationBounds->nx, 1, 0,
                               extrapolationBounds->x, &(extrapolationBounds->xLo),
                               yVals, fValsLo, NULL, 'y', NULL, xyBounds, &err);
          eos_LineInterpolate (EOS_TRUE, nXYPairs, extrapolationBounds->nx, 1, 0,
                               extrapolationBounds->x, &(extrapolationBounds->xHi),
                               yVals, fValsHi, NULL, 'y', NULL, xyBounds, &err);
        }

        for (i = 0; i < nXYPairs; i++) {
          xFlag = yFlag = EOS_OK;
          if (xVals[i] < fValsLo[i])
            xFlag = EOS_xLo_yOk;
          else if (xVals[i] > fValsHi[i])
            xFlag = EOS_xHi_yOk;
          if (yVals[i] < extrapolationBounds->yLo[0])
            yFlag = EOS_xOk_yLo;
          else if (yVals[i] > extrapolationBounds->yHi[0])
            yFlag = EOS_xOk_yHi;
          xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
        }

        EOS_FREE(fValsLo);
        EOS_FREE(fValsHi);

      }
      else {                  /* y is either OK or not considered */

        if (extrapolationBounds->stored) {

          for (i = 0; i < nXYPairs; i++) {
            if (xVals[i] < extrapolationBounds->xLo[0])
              xyBounds[i] = EOS_xLo_yOk;
            else if (xVals[i] > extrapolationBounds->xHi[0])
              xyBounds[i] = EOS_xHi_yOk;
            else
              xyBounds[i] = EOS_OK;
          }

        }
        else {

          for (i = 0; i < nXYPairs; i++) {
            if (xVals[i] < F[0][0])
              xyBounds[i] = EOS_xLo_yOk;
            else if (xVals[i] > F[0][nX - 1])
              xyBounds[i] = EOS_xHi_yOk;
            else
              xyBounds[i] = EOS_OK;
          }

        }

      }
      break;
    }
  case EOS_CATEGORY2:          /* indicates the table is inverted with respect to 2nd independent variable */
  case EOS_CATEGORY4:          /* indicates the table is a merging of a CATEGORY2 table and a CATEGORY0 table */
    {
      /* means that the user provides x and F of a function that is defined as y( x,F)
         so effectively the x and y user has given us are X and F */

      if (! isOneDimDatatype) {

        EOS_REAL *fValsLo = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        EOS_REAL *fValsHi = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
        EOS_INTEGER xFlag, yFlag;

        doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);

        if (doRational) {
          eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->ny, 1, 0,
                                   extrapolationBounds->x, extrapolationBounds->yLo,
                                   xVals, fValsLo, NULL, 'y', NULL, xyBounds, &err);
          eos_RationalInterpolate (EOS_FALSE, nXYPairs, extrapolationBounds->ny, 1, 0,
                                   extrapolationBounds->x, extrapolationBounds->yHi,
                                   xVals, fValsHi, NULL, 'y', NULL, xyBounds, &err);
        }
        else {
          eos_LineInterpolate (EOS_TRUE, nXYPairs, extrapolationBounds->ny, 1, 0,
                               extrapolationBounds->x, &(extrapolationBounds->yLo),
                               xVals, fValsLo, NULL, 'y', NULL, xyBounds, &err);
          eos_LineInterpolate (EOS_TRUE, nXYPairs, extrapolationBounds->ny, 1, 0,
                               extrapolationBounds->x, &(extrapolationBounds->yHi),
                               xVals, fValsHi, NULL, 'y', NULL, xyBounds, &err);
        }

        for (i = 0; i < nXYPairs; i++) {
          xFlag = yFlag = EOS_OK;
          if (xVals[i] < extrapolationBounds->xLo[0])
            xFlag = EOS_xLo_yOk;
          else if (xVals[i] > extrapolationBounds->xHi[0])
            xFlag = EOS_xHi_yOk;
          if (yVals[i] < fValsLo[i])
            yFlag = EOS_xOk_yLo;
          else if (yVals[i] > fValsHi[i])
            yFlag = EOS_xOk_yHi;
          xyBounds[i] = _eos_CombineExtrapErrors (xFlag, yFlag);
        }

        EOS_FREE(fValsLo);
        EOS_FREE(fValsHi);

      }
      else {                  /* y is either OK or not considered */

        if (extrapolationBounds->stored) {

          for (i = 0; i < nXYPairs; i++) {
            if (xVals[i] < extrapolationBounds->xLo[0])
              xyBounds[i] = EOS_xLo_yOk;
            else if (xVals[i] > extrapolationBounds->xHi[0])
              xyBounds[i] = EOS_xHi_yOk;
            else
              xyBounds[i] = EOS_OK;
          }

        }
        else {

          for (i = 0; i < nXYPairs; i++) {
            if (xVals[i] < F[0][0])
              xyBounds[i] = EOS_xLo_yOk;
            else if (xVals[i] > F[0][nX - 1])
              xyBounds[i] = EOS_xHi_yOk;
            else
              xyBounds[i] = EOS_OK;
          }

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
  if (! isOneDimDatatype && srchY != yVals)
    EOS_FREE (yVals);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, th, err);
    *errorCode = err;
    return;
  }
}
