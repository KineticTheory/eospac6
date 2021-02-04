/*********************************************************************
 * Class Name : eos_Interpolation
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#define _EOS_INTERPOLATION_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_Interpolation.h"

#include "eos_Utils.h"
#include "eos_Data.h"
#include "eos_DataMap.h"
#include "eos_RecordType1.h"
#include "eos_RecordType2.h"
#include "eos_RecordType6.h"
#include "eos_HashTable.h"

NRmachPrecData _eos_machinePrecisionData = {
  0,                            // gotMachinePrecision
  0.0,                          // eps
  0.0,                          // epsneg
  1.0e-04,                      // maxErr (default from original es4invt logic)
  20                            // maxIter (default from original es4invt logic)
};

void eos_GetMachinePrecision (EOS_REAL *eps, EOS_REAL *epsneg)
{

#ifdef SHOW_MACH_PRECISION
  printf("eos_GetMachinePrecision is estimating this machine's precision.\n");
#endif

  EOS_REAL one = (EOS_REAL) (1);
  EOS_REAL two = (EOS_REAL) (2);

  // determine *eps
  *eps = one;
  do {
    *eps /= two;
  } while (one + (*eps / two) > one);

  // determine *epsneg
  *epsneg = one;
  do {
    *epsneg /= two;
  } while (one - (*epsneg / two) < one);

#ifdef SHOW_MACH_PRECISION
  printf (" eps_POSIX=%.16g\n", eps_POSIX);
  printf ("      *eps=%g\n", *eps);
  printf ("   *epsneg=%g\n", *epsneg);
  printf ("rel. diff.=%g\n", (*eps - *epsneg) / (*eps));
#endif
}

/************************************************************************
 * 
 * eos_Data class destructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Data **me  - this pointer (pointer to the instance of type eos_Data)
 * 
 ************************************************************************/
void eos_DestroyEosInterpolation (eos_Interpolation *me)
{
  int i;

  eos_DestroyEosAccess (&(me->eosAccess));

  /* Deallocate memory */
  for (i = 0; i < me->numberOfHandles; i++) {

    /* Deallocate struct(s) arrays */
    EOS_FREE (me->interpolationDataList[i]->xyBounds);
    EOS_FREE (me->interpolationDataList[i]->xSpecies);
    EOS_FREE (me->interpolationDataList[i]->ySpecies);
    EOS_FREE (me->interpolationDataList[i]->FSpecies);
    EOS_FREE (me->interpolationDataList[i]->dFxSpecies);
    EOS_FREE (me->interpolationDataList[i]->dFySpecies);

    /* Deallocate array of struct(s) */
    EOS_FREE (me->interpolationDataList[i]);

  }

  EOS_FREE (me->interpolationDataList);

  me->numberOfHandles = 0;
}

/************************************************************************
 * 
 * eosInterpolation class constructor
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_Interpolation *me  - this pointer (pointer to the instance of type eos_eosInterpolation
 * 
 ************************************************************************/
void eos_ConstructEosInterpolation (eos_Interpolation *me)
{
  eos_ConstructEosAccess ((eos_Access *) me);

  me->numberOfHandles = 0;
  me->interpolationDataList = NULL;

#ifdef DEBUG
  ((eos_ErrorHandler *) me)->HandleError = eos_HandleErrorEosInterpolation;     /* derived virtual function */
#endif
}

/************************************************************************
 * 
 * virtual method function that handles specific error code.
 * 
 * Returned Values: none
 *
 * Input Value:
 * eos_ErrorHandler *me  - pointer to an instance of type eos_ErrorHandler.
 * 
 ************************************************************************/
void eos_HandleErrorEosInterpolation (void *ptr, EOS_INTEGER errorCode)
{
  eos_ErrorHandler *me = (eos_ErrorHandler *) ptr;
  me->errorCode = errorCode;
#ifdef DEBUG
  printf ("eos_Interpolation: error is: %s\n", eos_GetErrorMsg (errorCode));
#endif
}

/************************************************************************/
/* local helping functions */
/*!**********************************************************************
 *
 * function _eos_srchdf
 * This routine returns a vector of index locations J of the 
 * vector Y in the table X such that X(J(I)).LE.Y(I).LT.X(J(I)+1).   
 * Given an increasing table X(1).LT.X(2).LT.....LT.X(M),    
 * interval indices 1,2,...,M-1 correspond to [X(1),X(2)),   
 * [X(2),X(3)),...,[X(M-1),X(M)).    
        
 * If the values  of Y gathered are all interior to the table of X's; that is,      
 * X[0].LE.Y[M]).LT.X(M), I=1,n, the extrapolation flag is set.    
 * 
 * On entry  
 * 
 *  my   = the number of elements in the vector Y.    
 *  y[]  = a vector of length 'my' whose locations in the
 *         X table are to be determined.      
 *  iy   = unused integer
 *  n    = the number of elements in the table X.     
 *  x[]  = a table of increasing elements to be searched in.
 *  ix   = step size to traverse array x[]
 *  x_ht = hashtable for x, pass NULL to use hunt and locate search (slower)
 * 
 * On return 
 *
 *  EOS_INTEGER *errorCode    scalar error code
 *  EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 * 
 *  EOS_INTEGER J[] = a vector of length my of integers locating the position     
 *                    of each element of y[] in the x[] table:       
 *                    x[j[i]] <= y[i] < x[j[i]+1], i=1...my
 ********************************************************************************/
#ifdef DEBUG_EOS_SRCHDF
static EOS_INTEGER _eos_srchdf_counter = 0;
#endif


void _eos_srchdf (EOS_INTEGER my, EOS_REAL* y, EOS_INTEGER iy,
                  EOS_INTEGER n, EOS_REAL* x, EOS_INTEGER ix,
                  EOS_INTEGER* j, eos_HashTable1D* x_ht, EOS_INTEGER *xyBounds,
                  EOS_INTEGER *errorCode)
{
  EOS_INTEGER i;
#ifdef DEBUG_EOS_SRCHDF
  _eos_srchdf_counter++;
#endif
  *errorCode = EOS_OK;

  EOS_BOOLEAN useGpuData=EOS_FALSE;
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;

#ifdef DO_OFFLOAD
    useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;
#endif

  eos_Search (x, n, ix /* step */, y, my, j, xyBounds, x_ht, errorCode);

  if (!skipExtrap && !useGpuData){
    for (i = 0; i < my; i++) {
      if (IS_EXTRAPOLATED (eos_GetStandardErrorCodeFromCustomErrorCode(xyBounds[i])))
	*errorCode = EOS_INTERP_EXTRAPOLATED;
    }
  }

#ifdef DEBUG_EOS_SRCHDF
#ifndef DO_OFFLOAD
  printf("\n# fcmp_ignore: _eos_srchdf_counter = %d\n# fcmp_ignore: _eos_srchdf: %10s  %10s %23s %23s\n", _eos_srchdf_counter, "i", "j[i]", "x[j[i]]", "y[i]");
  for (i = 0; i < my; i++) {
    /* if (i==0 || i==100) */
    printf("# fcmp_ignore: _eos_srchdf: %10d. %10d %23.15e %23.15e n=%d my=%d ix=%d xyBounds[i]=%d\n", i, j[i], x[j[i]], y[i], n, my, ix, xyBounds[i]);
  }
  printf("\n");
#endif
#endif


}

/*!**********************************************************************
 *
 * function _eos_CombineExtrapErrors
 * This function takes two extrapolation error codes, and combines them
 * into a single error code. This is necessary due to the 1-D search
 * algorithms that are often used on 2-D data tables.
 * 
 * Returned Value: 
 * EOS_INTEGER      - newly-combined extrapolation error code
 *
 * Input Values:
 * EOS_INTEGER xErr - extrapolation error code from 1-D search of X[] data
 * EOS_INTEGER yErr - extrapolation error code from 1-D search of Y[] data
 *
 ************************************************************************/
EOS_INTEGER _eos_CombineExtrapErrors (EOS_INTEGER xErr_in, EOS_INTEGER yErr_in)
{
  EOS_INTEGER xErr = eos_GetStandardErrorCodeFromCustomErrorCode(xErr_in);
  EOS_INTEGER yErr = eos_GetStandardErrorCodeFromCustomErrorCode(yErr_in);

  if (xErr==yErr) return xErr;

  if (xErr!=EOS_OK        && yErr==EOS_OK       )
    return xErr;

  if (xErr==EOS_OK        && yErr!=EOS_OK       ) {
    if (yErr == EOS_xHi_yOk)
      return EOS_xOk_yHi;
    else if (yErr == EOS_xLo_yOk)
      return EOS_xOk_yLo;
    else
      return yErr;
  }
  if (xErr==EOS_xHi_yOk   && (yErr==EOS_xHi_yOk ||
                              yErr==EOS_xOk_yHi ||
                              yErr==EOS_xHi_yHi))
    return EOS_xHi_yHi;

  if (xErr==EOS_xLo_yOk   && (yErr==EOS_xLo_yOk ||
                              yErr==EOS_xOk_yLo ||
                              yErr==EOS_xLo_yLo))
    return EOS_xLo_yLo;

  if (xErr==EOS_xHi_yOk   && (yErr==EOS_xLo_yOk ||
                              yErr==EOS_xOk_yLo ||
                              yErr==EOS_xHi_yLo))
    return EOS_xHi_yLo;

  if (xErr==EOS_xLo_yOk   && (yErr==EOS_xHi_yOk ||
                              yErr==EOS_xOk_yHi ||
                              yErr==EOS_xLo_yHi))
    return EOS_xLo_yHi;

  if (xErr==EOS_UNDEFINED || yErr==EOS_UNDEFINED) return EOS_UNDEFINED;

  /* default case */
  return EOS_OK;
}

/*!**********************************************************************
 *
 * function _eos_SplitExtrapErrors
 * This function takes one extrapolation error code, and split it
 * into two error codes.
 * 
 * Input Value:
 * EOS_INTEGER xyBounds - extrapolation error code to be split into x,y
 *                        components
 *
 * Output Values: 
 * EOS_INTEGER *xErr    - extrapolation error code w.r.t. x component
 * EOS_INTEGER *yErr    - extrapolation error code w.r.t. y component
 *
 ************************************************************************/
void _eos_SplitExtrapErrors (EOS_INTEGER xyBounds, EOS_INTEGER *xErr, EOS_INTEGER *yErr)
{

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap) 
    return;
  *xErr = *yErr = EOS_OK;

  switch (xyBounds) {
  case EOS_OK:
  case EOS_xLo_yOk:
  case EOS_xHi_yOk: *yErr = EOS_OK; break;

  case EOS_xOk_yLo:
  case EOS_xLo_yLo:
  case EOS_xHi_yLo: *yErr = EOS_xOk_yLo; break;

  case EOS_xOk_yHi:
  case EOS_xLo_yHi:
  case EOS_xHi_yHi: *yErr = EOS_xOk_yHi; break;

  default: *yErr = EOS_UNDEFINED; break;
  }
  switch(xyBounds) {
  case EOS_OK:
  case EOS_xOk_yLo:
  case EOS_xOk_yHi: *xErr = EOS_OK; break;

  case EOS_xLo_yOk:
  case EOS_xLo_yLo:
  case EOS_xLo_yHi: *xErr = EOS_xLo_yOk; break;

  case EOS_xHi_yOk:
  case EOS_xHi_yLo:
  case EOS_xHi_yHi: *xErr = EOS_xHi_yOk; break;

  default: *xErr = EOS_UNDEFINED; break;
  }
}

/*!**************************************************************************************************************** 
   function _eos_DumpIndicesToFile
   
   Inputs:
   char        *fn         filename to which output is to be written
   char        *mode       write mode for output file
   EOS_INTEGER nsrch       number of elements in ix_low, iy_low, ix_high, and iy_high arrays
   EOS_REAL    *xtbls      array of X data
   EOS_REAL    *ytbls      array of Y data
   EOS_REAL    **ftbls     array of F(x,y) data
   EOS_INTEGER *ix_low     resulting index array of size npoints of indexes in xtbls array
   EOS_INTEGER *iy_low     resulting index array of size npoints of indexes in ytbls array
   EOS_INTEGER *ix_high    resulting index array of size npoints of indexes in xtbls array
   EOS_INTEGER *iy_high    resulting index array of size npoints of indexes in ytbls array
   EOS_REAL    *lowerbound array of lower bounds of target solutions
   EOS_REAL    *upperbound array of upper bounds of target solutions
 *********************************************************************************************************************/
void _eos_DumpIndicesToFile (char *fn, char *mode,
                             EOS_INTEGER nsrch,
                             EOS_REAL *xtbls, EOS_REAL *ytbls, EOS_REAL **ftbls,
                             EOS_INTEGER *ix_low, EOS_INTEGER *iy_low,
                             EOS_REAL *lowerbound, EOS_REAL *upperbound)
{
  int j;
  FILE *fp;

  fp = fopen (fn, mode);

  if (lowerbound == NULL || upperbound == NULL)
    fprintf(fp,"%-5s %3s %3s %23s %23s %23s\n",
            "i","ix","iy","xtbl[ix]","ytbl[iy]","ftbl[iy][ix]");
  else
    fprintf(fp,"%-5s %3s %3s %23s %23s %23s %23s %23s\n",
            "i","ix","iy","xtbl[ix]","ytbl[iy]","ftbl[iy][ix]","lowerbound","upperbound");

  for (j=0; j<nsrch; j++) {
    fprintf(fp,"%-5d %3d %3d",j,ix_low[j],iy_low[j]);
    if (lowerbound == NULL || upperbound == NULL)
      fprintf(fp," %23.15e %23.15e %23.15e\n",
              xtbls[ix_low[j]],ytbls[iy_low[j]],ftbls[iy_low[j]][ix_low[j]]);
    else
      fprintf(fp," %23.15e %23.15e %23.15e %23.15e %23.15e\n",
              xtbls[ix_low[j]],ytbls[iy_low[j]],ftbls[iy_low[j]][ix_low[j]],
              lowerbound[j],upperbound[j]);
  }

  fprintf(fp,"\n");
  if (fp) fclose(fp);
}

/*!**************************************************************************************************************** 
   function eos_SearchIndices_YF
   Takes an array of values of Y and corresponding values of F(x,y) and for each y, F(x,y) pair, finds the closest
   x_low, x_high and y_low, y_high so that F(x,y) is inside the rectangle F(x_low, y_low), F(x_low, y_high),
   F(x_high, y_low, F(x_high, y_high).

   Assumption is made that F(x,y) is monotonic, so there is UNIQUE interval in X for which F(x,y) interval
   contains F0 for interval in Y containing Y0.

   Inputs:
   EOS_REAL    Y0[]        array of the known values of Y
   EOS_REAL    F0[]        array of the corresponding values of F(x,y), so that F(X0[i], IY[i]) = F0; i=0,npoints-1
   EOS_INTEGER npoints     number of given points in X0 and F0
   EOS_REAL    X[]         array of X values   
   EOS_REAL    Y[]         array of Y values
   EOS_REAL    F[][]       array of F(x,y) values
   EOS_INTEGER nx          dimension of X
   EOS_INTEGER ny          dimension of Y
   EOS_INTEGER nGhostData  number of "ghost" data points added at each isotherm/isochore extremum
                           (which are used to smooth interpolation at table boundaries)
   EOS_REAL coldCurve[]    optional input cold curve that was subtracted from F before log10 is taken
   eos_HashTable1D Y_ht    hashtable for Y values (can be NULL)
   eos_HashTable2D F_ht    hashtable for F values (can be NULL)
   EOS_INTEGER adjustF     flag, whether or not the F0 is in the same form as F

   Outputs:
   EOS_INTEGER  Low_IY0[]   resulting index array of size npoints of indexes of Y such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER  Low_IX0[]   resulting index array of size npoints of indexes of X such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1

   EOS_INTEGER  High_IY0[]  resulting index array of size npoints of indexes of Y such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER  High_IX0[]  resulting index array of size npoints of indexes of X such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER *xyBounds     interpolation errors per xy-pair
   EOS_INTEGER *err;        output error code
 *********************************************************************************************************************/
#ifdef DEBUG_EOS_SEARCHINDICES_YF
static EOS_INTEGER eos_SearchIndices_YF_counter = 0;
#endif
void eos_SearchIndices_YF (EOS_REAL Y0[], EOS_REAL F0[], EOS_INTEGER npoints,
                           EOS_REAL X[], EOS_REAL Y[], EOS_REAL **F,
                           EOS_INTEGER nx, EOS_INTEGER ny, EOS_INTEGER nGhostData,
                           EOS_INTEGER Low_IY0[], EOS_INTEGER Low_IX0[],
                           EOS_INTEGER High_IY0[], EOS_INTEGER High_IX0[],
                           EOS_REAL *coldCurve, eos_HashTable1D* Y_ht, eos_HashTable2D* F_ht, EOS_INTEGER adjustF,
                           EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER err1 = EOS_OK, err2 = EOS_OK, i;
  *err = EOS_OK;

  for (i = 0; i < npoints; i++) {                                                                           
    xyBounds[i] = EOS_OK;
  }

  /* initialize indexes */
  for (i = 0; i < npoints; i++) {
    Low_IX0[i]  = 0;
    High_IX0[i] = 0;
    Low_IY0[i]  = 0;
    High_IY0[i] = 0;
  }

  /* first find y-range, assume the values of YX0[] are INCREASING */
  eos_Search (&(Y[nGhostData]), ny-nGhostData*2, 1, Y0, npoints, Low_IY0, xyBounds, Y_ht, &err1);
  for (i = 0; i < npoints; i++) {
    Low_IY0[i] += nGhostData; /* increment index since eos_Search output is offset */
    High_IY0[i] = Low_IY0[i] + 1;
  }
  /* now find f-range along Low_Y, High_Y, it will give us indexes of x */
  for (i = 0; i < npoints; i++) {
    eos_Search (&(F[High_IY0[i]][nGhostData]), nx-nGhostData*2, 1,
      &(F0[i]), 1, &(High_IX0[i]), xyBounds,
      F_ht == NULL ? NULL : &F_ht->rowTables[High_IY0[i]], &err2); /* upper bound */
    eos_Search (&(F[Low_IY0[i]][nGhostData]),  nx-nGhostData*2, 1,
      &(F0[i]), 1, &(Low_IX0[i]), xyBounds,
      F_ht == NULL ? NULL : &F_ht->rowTables[Low_IY0[i]], &err1); /* lower bound */
    Low_IX0[i]  += nGhostData; /* increment index since eos_Search output is offset */
    High_IX0[i] += nGhostData; /* increment index since eos_Search output is offset */
    xyBounds[i] = EOS_OK; /* disable xyBounds[i] assignment in this function --
                             leave it to the parent function's control */
  }

#ifdef DEBUG_EOS_SEARCHINDICES_YF
  printf("\n# fcmp_ignore: eos_SearchIndices_YF_counter = %d\n# fcmp_ignore: eos_SearchIndices_YF: %10s  %11s %11s %11s %11s\n",
         eos_SearchIndices_YF_counter, "i", "Low_IX0[i]", "High_IX0[i]", "Low_IY0[i]", "High_IY0[i]");
  for (i = 0; i < npoints; i++) {
    /* if (i==0 || i==100) */
    printf("# fcmp_ignore: eos_SearchIndices_YF: %10d. %11d %11d %11d %11d npoints=%d\n", i, Low_IX0[i], High_IX0[i], Low_IY0[i], High_IY0[i], npoints);
  }
  printf("\n");
#endif

}

//#define C_CRANFILL_ORIGINAL 1
//#define DISABLE_FLOOR_CHECK
/*************************************************************************************
 *       eos_RationalInterpolate4
 *       purpose   -- interpolate F(x) for value of x given 4 known points closest to x.
 *       _create_ghostdata = input  boolean enables _create_ghostdata logic in eos_RationalInterpolate4
 *       srchX = input value, x, used to interpolate F.
 *       F[4] = input array containing 4 tabulated F values.
 *       X[4] = input array containing 4 tabulated X values.
 *       fvalv    = output pointer containing interpolated value of function F(x)
 *       dfvalv   = output pointer OPTIONAL, containing the derivative of function F(x)
 *************************************************************************************/
void eos_RationalInterpolate4 (EOS_BOOLEAN _create_ghostdata, EOS_REAL *srchX, EOS_REAL *X, EOS_REAL *F,
                               EOS_REAL *fvalv, EOS_REAL *dfvalv, EOS_INTEGER ninds) {

  EOS_INTEGER i;
  EOS_BOOLEAN use_df = (dfvalv==NULL?EOS_FALSE:EOS_TRUE);


#ifdef DO_OFFLOAD
  EOS_BOOLEAN  useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;
  /* input value, srchX, marked as * corresponds to target values, fvalv and dfvalv */
#pragma omp target if(useGpuData) is_device_ptr(srchX, F, X, fvalv, dfvalv) 
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif
  for (i=0; i<ninds; i++){
    EOS_REAL X0, X1, X2, X3;
    EOS_REAL ftg1, ftg2, ftg3, ftg4;
    if (_create_ghostdata && srchX[i] < X[1+i*4]) {
      /* Xghost        X[0]   *    X[1]        X[2]        X[3] */
      /* Fghost        F[0]   *    F[1]        F[2]        F[3] */
      X0 = 2*X[0+i*4] - X[1+i*4]; /* assume increasing order */
      X1 = X[0+i*4];
      X2 = X[1+i*4];
      X3 = X[2+i*4];
      ftg1 = F[0];
      ftg2 = F[1];
      ftg3 = F[0] + (X0 - X1) / (X2 - X1) * (F[1] - F[0]); /* DAP 2020-05-12: This is verified for consistency with _eos_CreateGhostData logic */
      ftg4 = F[2];
    }
    else if (_create_ghostdata && srchX[i] >= X[2+i*4]) {
      /* X[0]        X[1]        X[2]    *   X[3]        Xghost */
      /* F[0]        F[1]        F[2]    *   F[3]        Fghost */
      X0 = X[1+i*4];
      X1 = X[2+i*4];
      X2 = X[3+i*4];
      X3 = X[3+i*4] + (X[3+i*4] - X[2+i*4]); /* assume increasing order */
      ftg1 = F[2+i*4];
      ftg2 = F[3+i*4];
      ftg3 = F[1+i*4];
      ftg4 = F[2+i*4] + (X3 - X1) / (X2 - X1) * (F[3+i*4] - F[2+i*4]); /* DAP 2020-05-12: This is verified for consistency with _eos_CreateGhostData logic */
    }
    else {
      /* X[0]        X[1]    *   X[2]        X[3] */
      X0 = X[0+i*4];
      X1 = X[1+i*4];
      X2 = X[2+i*4];
      X3 = X[3+i*4];
      ftg1 = F[1+i*4];
      ftg2 = F[2+i*4];
      ftg3 = F[0+i*4];
      ftg4 = F[3+i*4];
    }

    /* interpolate values of eos function f(x,y) and derivative. */
    EOS_REAL dxg1 = X1 - X0;
    EOS_REAL dxg2 = X2 - X1;
#ifndef C_CRANFILL_ORIGINAL
    EOS_REAL dxg3 = X3 - X2;
#ifdef DISABLE_FLOOR_CHECK
    EOS_REAL denom1 = dxg2;
#else
    EOS_REAL denom1 = FLOOR(dxg2);
#endif
    EOS_REAL rxv = (srchX[i] - X1) / denom1;
#else
    EOS_REAL dxg3 = X3 - X1;
    EOS_REAL rxv = (srchX[i] - X1) / dxg2;
#endif

#ifndef C_CRANFILL_ORIGINAL
#ifdef DISABLE_FLOOR_CHECK
    denom1 = dxg2;
#else
    denom1 = FLOOR(dxg2);
#endif
    EOS_REAL rx3 = -dxg1 / denom1;
    EOS_REAL rx4 = dxg3 / denom1 + 1.0;
#else
    EOS_REAL rx3 = -dxg1 / dxg2;
    EOS_REAL rx4 = dxg3 / dxg2;
#endif

    EOS_REAL sxv = (rx3 > rxv) ? rx3 : rxv;

    sxv = (rx4 < sxv) ? rx4 : sxv;
  /* sxv  =  MIN (rx4, MAX (rx3, rxv)); */

    EOS_REAL txv = (rxv > (EOS_REAL) 0.0) ? rxv : (EOS_REAL) 0.0;
    txv = (txv < (EOS_REAL) 1.0) ? txv : (EOS_REAL) 1.0;
  /*txv  =  MIN (1.0, MAX (0.0, rxv)); */
    
    EOS_REAL fx1 = ftg2 - ftg1;
#ifndef C_CRANFILL_ORIGINAL
#ifdef DISABLE_FLOOR_CHECK
    denom1 = rx3;
    EOS_REAL denom2 = (EOS_REAL) 1.0 - rx3;
#else
    denom1 = FLOOR(rx3);
    EOS_REAL denom2 = FLOOR((EOS_REAL) 1.0 - rx3);
#endif
    EOS_REAL fxx1 = (fx1 - (ftg3 - ftg1) / denom1) / denom2;

#ifdef DISABLE_FLOOR_CHECK
    denom1 = rx4;
    denom2 = (EOS_REAL) 1.0 - rx4;
#else
    denom1 = FLOOR(rx4);
    denom2 = FLOOR((EOS_REAL) 1.0 - rx4);
#endif
    EOS_REAL gxx1 = (fx1 - (ftg4 - ftg1) / denom1) / denom2 - fxx1;
    
#else
    EOS_REAL fxx1 = (fx1 - (ftg3 - ftg1) / rx3) / ((EOS_REAL) 1.0 - rx3);
    EOS_REAL gxx1 = (fx1 - (ftg4 - ftg1) / rx4) / ((EOS_REAL) 1.0 - rx4) - fxx1;
#endif

    EOS_REAL wx1 = txv * FABS (fxx1);
    wx1 = wx1 / (wx1 + ((EOS_REAL) 1.0 - txv) * FABS (fxx1 + gxx1) + TINY_D);
    fxx1 = fxx1 + wx1 * gxx1;
    fx1 = fx1 - ((EOS_REAL) 1.0 - sxv - sxv) * fxx1;
    fvalv[i] = ftg1 + rxv * fx1 - sxv * sxv * fxx1;
  /* find and derivative wrt y of gathered eos data table y,f values. */
#ifndef C_CRANFILL_ORIGINAL
    if (use_df) {
#ifdef DISABLE_FLOOR_CHECK
      denom1 = dxg2;
#else
      denom1 = FLOOR(dxg2);
#endif
      dfvalv[i] = (fx1 - wx1 * ((EOS_REAL) 1.0 - wx1) * gxx1) / denom1;
    }
#else
    if (use_df)
      dfvalv[i] = (fx1 - wx1 * ((EOS_REAL) 1.0 - wx1) * gxx1) / dxg2;
#endif
  }
#ifdef DO_OFFLOAD
  }
#endif

}

/*****************************************************************************
 *     purpose   -- 1. Original logic performs vector uni-rational-
 *                     function (4-point) search-interpolation on eos
 *                     function f(x,y) holding x fixed. Uses linear
 *                     extrapolation.
 *                  2. New logic performs vector uni-rational-
 *                     function (4-point) search-interpolation on eos
 *                     function f(x,y) holding y fixed. Uses linear
 *                     extrapolation.
 *       _create_ghostdata = input  boolean enables _create_ghostdata logic in eos_RationalInterpolate4
 *       nsrch    = input  integer length of search vectors.
 *       nxtbl    = input  integer number of data table x values (1 if x is fixed).
 *       nytbl    = input  integer number of data table y values (1 if y is fixed).
 *       ixtbl    = input  integer index of data table x (y if y is fixed) value.
 *       ytbls    = input  real array containing data table y values. (x if y is fixed)
 *       ftbls    = input  real array containing data table f values.
 *       yvalv    = input  real search vector containing y (x if x is fixed) values.
 *       fvalv    = output real array containing interpolated values of
 *                         eos function f(x,y) 
 *       dfvalv   = output OPTIONAL real array containing the derivatives of
 *                         eos function f(x,y) 
 *       fixedVar = input  lower-case character
 *                         identifying independent variable to hold
 *                         fixed:
 *                         ='x' holds x fixed (original/default logic)
 *                         ='y' holds y fixed (new logic)
 *       ytbls_ht = input  hashtable for ytbls array, can be NULL
 *       EOS_INTEGER output *xyBounds     interpolation errors per xy-pair
 *       EOS_INTEGER *err  = output error code
 *
 *     ORIGINAL/DEFAULT LOGIC:
 *     index map of gathered eos data table f values (ftg) when
 *     x values fixed --
 *                   /
 *       (f indices) /
 *                   /  (y indices)
 *       /////////////     jy-1 ->  jy+0 ->  jy+1 ->  jy+2 ->
 *
 *           (x index)
 *            jx+0 ->        3        1        2        4
 *
 *     NEW LOGIC:
 *     index map of gathered eos data table f values (ftg) when
 *     y values fixed --
 *                   /
 *       (f indices) /
 *                   /  (y index)
 *       /////////////     jy+0
 *
 *         (x indices)
 *            jx-1 ->        3
 *            jx+0 ->        1
 *            jx+1 ->        2
 *            jx+2 ->        4
 *
 ********************************************************************/
void eos_RationalInterpolate (EOS_BOOLEAN _create_ghostdata,
                              EOS_INTEGER nsrch, EOS_INTEGER nxtbl,
                              EOS_INTEGER nytbl, EOS_INTEGER ixtbl,
                              EOS_REAL *ytbls, EOS_REAL *ftbls,
                              EOS_REAL *yvalv, EOS_REAL *fvalv,
                              EOS_REAL *dfvalv, char fixedVar,
                              eos_HashTable1D *ytbls_ht,
                              EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER err1, jx, jv, j, vect_size;
  EOS_REAL *interpX, *interpF;

  EOS_INTEGER *iyv;

#ifdef DO_OFFLOAD
  int h, t;
#endif
  *err = EOS_OK;

  // if (4-point) search-interpolation is impossible, return.
  vect_size = (fixedVar == 'y') ? nxtbl : nytbl;
  if (vect_size < 4) {
    return;
  }

#ifdef DO_OFFLOAD
  EOS_BOOLEAN  useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;

  t = omp_get_default_device();
  h = omp_get_initial_device();
#endif /* DO_OFFLOAD */

#ifdef DO_OFFLOAD
  if (!useGpuData)
#endif /* DO_OFFLOAD */
  {
    iyv = (EOS_INTEGER*) malloc (nsrch * sizeof (EOS_INTEGER));
    interpX = (EOS_REAL*) malloc (4*nsrch * sizeof (EOS_REAL));
    interpF = (EOS_REAL*) malloc (4*nsrch * sizeof (EOS_REAL));
  }
#ifdef DO_OFFLOAD
  else
  { 
    iyv = omp_target_alloc(nsrch * sizeof (EOS_INTEGER), t);
    interpF = omp_target_alloc(4*nsrch * sizeof (EOS_REAL), t);
    interpX = omp_target_alloc(4*nsrch * sizeof (EOS_REAL), t);
  }
#endif /* DO_OFFLOAD */

  
  /* perform vector table searches to load search vectors containing
     indices and spacings of nearest data table y values to yvalv. */
  _eos_srchdf (nsrch, yvalv, 1, vect_size, ytbls, 1, iyv, ytbls_ht, xyBounds, &err1);
  

  /* ignore error, might be only one point */

  /* gather nearest eos data table f values. */
  if (fixedVar == 'x')
    jx = (nxtbl - 1 < ixtbl) ? nxtbl - 1 : ixtbl;
  else
    jx = (nytbl - 1 < ixtbl) ? nytbl - 1 : ixtbl;
  if (jx <= 0)
    jx = 0;


#ifdef DO_OFFLOAD
#pragma omp target if (useGpuData) is_device_ptr(interpF, interpX, iyv, ftbls, ytbls)
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif
    for (jv = 0; jv < nsrch; jv++) {
      EOS_INTEGER jy = iyv[jv] - 1;
      while (jy > vect_size - 4)
	jy--;                     /* adjust so that there are 2 values on the right */
      if (jy < 0)
	jy = 0;
      for (j = 0; j < 4; j++) {
	interpX[jv*4+j] = ytbls[jy + j];
	if (fixedVar == 'x') {
	  interpF[jv*4+j] = ftbls[(jx) + (jy + j) * nxtbl];
	} else                      /* 'y' */ {
	  interpF[jv*4+j] = ftbls[(jy + j) + (jx) * nxtbl];
	}
      }
    }                             /* 1, nsrch loop */
#ifdef DO_OFFLOAD
  }
#endif /* DO_OFFLOAD */

  eos_RationalInterpolate4 (_create_ghostdata, &(yvalv[0]), &(interpX[0]), &(interpF[0]), &(fvalv[0]), &(dfvalv[0]), nsrch);


#ifdef DO_OFFLOAD
  if(!useGpuData)
#endif /* DO_OFFLOAD */
  {
    free(iyv); free(interpF); free(interpX);
  }
#ifdef DO_OFFLOAD
  else
  {
    omp_target_free(iyv, t);
    omp_target_free(interpF, t);
    omp_target_free(interpX, t);
  }
#endif /* DO_OFFLOAD */

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
    *err = err1;
}

/****************************************************************************************************** 
   function eos_SearchIndices_XF
   Takes an array of values of X and corresponding values of F(x,y) and for each x, F(x,y) pair, finds the closest
   x_low, x_high and y_low, y_high so that F(x,y) is inside the rectangle F(x_low, y_low), F(x_low, y_high),
   F(x_high, y_low, F(x_high, y_high).

   Assumption is made that F(x,y) is monotonic, so there is UNIQUE interval in Y for which F(x,y) interval
   contains F0 for interval in X containing X0.

   Inputs:
   EOS_REAL    X0[]        array of the known values of X
   EOS_REAL    F0[]        array of the corresponding values of F(x,y), so that F(X0[i], IY[i]) = F0; i=0,npoints-1
   EOS_INTEGER npoints     number of given points in X0 and F0
   EOS_REAL    X[]         array of X values
   EOS_REAL    Y[]         array of Y values
   EOS_REAL    F[][]       array of F(x,y) values
   EOS_INTEGER nx          dimension of X
   EOS_INTEGER ny          dimension of Y
   EOS_INTEGER nGhostData  number of "ghost" data points added at each isotherm/isochore extremum
                           (which are used to smooth interpolation at table boundaries)
   EOS_REAL coldCurve[]    optional input cold curve that was subtracted from F before log10 is taken
   eos_HashTable1D X_ht    hashtable for X values (can be NULL)
   eos_HashTable2D F_ht    hashtable for F values (can be NULL)
   EOS_INTEGER adjustF     flag, whether or not the F0 is in the same form as F

   Outputs:
   EOS_INTEGER  Low_IY0[]   resulting index array of size npoints of indexes of Y such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER  Low_IX0[]   resulting index array of size npoints of indexes of X such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1

   EOS_INTEGER  High_IY0[]  resulting index array of size npoints of indexes of Y such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER  High_IX0[]  resulting index array of size npoints of indexes of X such that 
                            F(X[Low_IX0[i], Y[Low_IY[i]]) <= F0[i] <= F(X[High_IX0[i], Y[High_IY[i]]); i=0,npoints-1
   EOS_INTEGER *xyBounds     interpolation errors per xy-pair
   EOS_INTEGER *err         output error code
**************************************************************************************************/
#ifdef DEBUG_EOS_SEARCHINDICES_XF
static EOS_INTEGER eos_SearchIndices_XF_counter = 0;
#endif
void eos_SearchIndices_XF (EOS_REAL X0[], EOS_REAL F0[], EOS_INTEGER npoints,
                           EOS_REAL X[], EOS_REAL Y[], EOS_REAL **F,
                           EOS_INTEGER nx, EOS_INTEGER ny, EOS_INTEGER nGhostData,
                           EOS_INTEGER Low_IY0[], EOS_INTEGER Low_IX0[],
                           EOS_INTEGER High_IY0[], EOS_INTEGER High_IX0[],
                           EOS_REAL *coldCurve, eos_HashTable1D* X_ht, eos_HashTable2D* F_ht, EOS_INTEGER adjustF,
                           EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER err1 = EOS_OK, err2 = EOS_OK, i;
  *err = EOS_OK;


  /* initialize indexes */
  for (i = 0; i < npoints; i++) {
    Low_IX0[i]  = 0;
    High_IX0[i] = 0;
    Low_IY0[i]  = 0;
    High_IY0[i] = 0;
  }

  /* first find x-range, assume the values of IX0[] are INCREASING */
  eos_Search (&(X[nGhostData]), nx-nGhostData*2, 1, X0, npoints, Low_IX0, xyBounds, X_ht, &err1);
  for (i = 0; i < npoints; i++) {
    Low_IX0[i] += nGhostData; /* increment index since eos_Search output is offset */
    High_IX0[i] = Low_IX0[i] + 1;
  }

  /* now find f-range along Low_X, High_X, it will give us indexes of y */
  for (i = 0; i < npoints; i++) {
    eos_Search (&(F[nGhostData][High_IX0[i]]), ny-nGhostData*2, nx,
      &(F0[i]), 1, &(High_IY0[i]), xyBounds,
      F_ht == NULL ? NULL : &F_ht->colTables[High_IX0[i]], &err2); /* upper bound */
    eos_Search (&(F[nGhostData][Low_IX0[i]]),  ny-nGhostData*2, nx,
      &(F0[i]), 1, &(Low_IY0[i]),  xyBounds,
      F_ht == NULL ? NULL : &F_ht->colTables[Low_IX0[i]], &err1); /* lower bound */
    Low_IY0[i]  += nGhostData; /* increment index since eos_Search output is offset */
    High_IY0[i] += nGhostData; /* increment index since eos_Search output is offset */
    xyBounds[i] = EOS_OK; /* disable xyBounds[i] assignment in this function --
                             leave it to the parent function's control */
  }

#ifdef DEBUG_EOS_SEARCHINDICES_XF
  printf("\n# fcmp_ignore: eos_SearchIndices_XF_counter = %d\n# fcmp_ignore: eos_searchindices_xf: %10s  %11s %11s %11s %11s\n",
         eos_SearchIndices_XF_counter, "i", "Low_IX0[i]", "High_IX0[i]", "Low_IY0[i]", "High_IY0[i]");
  for (i = 0; i < npoints; i++) {
    /* if (i==0 || i==100) */
    printf("# fcmp_ignore: eos_SearchIndices_XF: %10d. %11d %11d %11d %11d npoints=%d\n", i, Low_IX0[i], High_IX0[i], Low_IY0[i], High_IY0[i], npoints);
  }
  printf("\n");
#endif

}

/*******************************************************************************
	function eos_InterpolateEosInterpolation
	The eos_InterpolateEosInterpolation() routine provides interpolated values for a single material using a
    table handle associated with  data stored within an data table.

	The input arguments are:
		EOS_INTEGER tableHandle	  This is a scalar EOS_INTEGER handle to a particular data table. 
		EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.
		EOS_REAL xVals[nXYPairs]  array of the primary independent variable values to use during interpolation. 
		EOS_REAL yVals[nXYPairs]  array of the secondary independent variable values to use during interpolation. 
		EOS_INTEGER *dt           optional data Type to overwrite, can be NULL

	The output arguments are:
		EOS_REAL fVals[nXYPairs]  array of the interpolated data corresponding to x and y. 
		EOS_REAL dFx[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to x. 
		EOS_REAL dFy[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to y. 
		EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
*********************************************************************************/
void eos_InterpolateEosInterpolation (eos_Interpolation *me,
                                      EOS_INTEGER tableHandle,
                                      EOS_INTEGER nXYPairs, EOS_REAL *xVals,
                                      EOS_REAL *yVals, EOS_REAL *fVals,
                                      EOS_REAL *dFx, EOS_REAL *dFy,
                                      EOS_INTEGER *dt, EOS_INTEGER *errorCode)
{
  EOS_REAL xconv, yconv, fconv, *X, *Y;
  EOS_INTEGER i, xErr, yErr, *xyBounds;
  EOS_BOOLEAN logAxes = EOS_FALSE;

  eos_Data *eosData;
  EOS_INTEGER dataType, err = EOS_OK;

  *errorCode = EOS_OK;

  if (tableHandle < 0 ||
      tableHandle >= gEosDataMap.nAlloc ||
      ! eos_IsHandleValid (tableHandle)) { /* tableHandle is invalid */
    *errorCode = EOS_INVALID_TABLE_HANDLE;
    return;
  }

  /* Set machine precision data */
  if (!_eos_machinePrecisionData.gotMachinePrecision) {
    /* determine the current machine's floating point precision */
    _eos_machinePrecisionData.gotMachinePrecision = 1;
    eos_GetMachinePrecision (&_eos_machinePrecisionData.eps, &_eos_machinePrecisionData.epsneg);
    _eos_machinePrecisionData.maxIter = 100;
    _eos_machinePrecisionData.maxErr = pow (MAX(_eos_machinePrecisionData.eps, _eos_machinePrecisionData.epsneg), 0.75);
  }

  /* get eos Data pointer and the dataType from global data map */
  eosData = eos_GetEosDataEosDataMap (&gEosDataMap, tableHandle, &dataType, &err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, err);
    *errorCode = err;
    return;
  }

  EOS_BOOLEAN skipExtrap;
  eos_GetOptionEosInterpolation (me, tableHandle, EOS_SKIP_EXTRAP_CHECK, &skipExtrap, &err);
  gEosDataMap.skipExtrapCheck =  skipExtrap;

  if (dt)
    dataType = *dt;

#ifdef DO_OFFLOAD
  EOS_BOOLEAN useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
#endif /* DO_OFFLOAD */

  /* if useGpuData then xyBounds is not valid.
     allocated xyBounds list to store per point error specific to extrapolation */
  if (me->interpolationDataList[tableHandle]->nAlloc == 0)
    me->interpolationDataList[tableHandle]->xyBounds =
      (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
  else if (me->interpolationDataList[tableHandle]->nAlloc < nXYPairs)
    me->interpolationDataList[tableHandle]->xyBounds =
      (EOS_INTEGER *) realloc (me->interpolationDataList[tableHandle]->
                               xyBounds, nXYPairs * sizeof (EOS_INTEGER));
  me->interpolationDataList[tableHandle]->nXYPairs = nXYPairs;
  me->interpolationDataList[tableHandle]->nAlloc = nXYPairs;
  me->interpolationDataList[tableHandle]->lastErrorCode = EOS_OK;
  for (i = 0; i < nXYPairs; i++)
    me->interpolationDataList[tableHandle]->xyBounds[i] = EOS_OK;
    
  /* process x-conversion option */
  xconv =
    eos_getRealOptionFromTableHandle (tableHandle, EOS_X_CONVERT, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
    return;

  /* process y-conversion option */
  yconv =
    eos_getRealOptionFromTableHandle (tableHandle, EOS_Y_CONVERT, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
    return;

  /* process f-conversion option */
  fconv =
    eos_getRealOptionFromTableHandle (tableHandle, EOS_F_CONVERT, errorCode);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
    return;

  /* Is data stored as log10 values? */
  logAxes = EOS_TYPE_TO_LOG_AXES(dataType);

 
#ifdef DO_OFFLOAD
  if (!useGpuData)
#endif /* DO_OFFLOAD */
  {
    if ( me->interpolationDataList[tableHandle]->enableXYpassthru
         || me->interpolationDataList[tableHandle]->enableXYmodify ) {

      /* use host's arrays directly */
      X = xVals;
      Y = yVals;
      
    }
    else {

      /* Allocate temporary memory */
      X = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      Y = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      
    }
    if ( ! me->interpolationDataList[tableHandle]->enableXYpassthru ) {
      
      /* Apply supplied conversion factors to convert input to Sesame default units.
         Enforce temperature and density minimums. */
      for (i = 0; i < nXYPairs; i++) {
        if (! logAxes && EOS_TYPE_TO_INDEP_VAR1(dataType) == EOS_D && xVals[i] < 0.0)
          X[i] = 0.0;
        else
          X[i] = xVals[i] / FLOOR (xconv);
        if (! logAxes && EOS_TYPE_TO_INDEP_VAR2(dataType) == EOS_T && yVals[i] < 0.0)
          Y[i] = 0.0;
        else
          Y[i] = yVals[i] / FLOOR (yconv);
      }
      
      /* Convert input to log10 values for 500 and 600 Sesame tables if necessary */
      if (logAxes) {
        for (i = 0; i < nXYPairs; i++) {
          X[i] = log10 (MAX (TINY_D, X[i]));
          Y[i] = log10 (MAX (TINY_D, Y[i]));
        }
      }
      
    }
    
    { /* Interpolate the data according to recordType */
      if (eosData->recordType == EOS_RECORD_TYPE6) {
        for (i = 0; i < nXYPairs; i++) {
          fVals[i] = 0.0;
          /* dFx and dFy are ignored for this record type */
        }
      }
      else {
        for (i = 0; i < nXYPairs; i++) {
          fVals[i] = 0.0;
          dFx[i] = 0.0;
          dFy[i] = 0.0;
        }
      }
      if (eosData->Interpolate)
        eosData->Interpolate (eosData, tableHandle, dataType, nXYPairs, X, Y, fVals, dFx, dFy,
                              me->interpolationDataList[tableHandle]->xyBounds, &err);
      else /* wrong record type */
        err = EOS_INVALID_DATA_TYPE;
    }

    /* Convert output from log10 values for 500 and 600 Sesame tables if necessary */
    if (logAxes) {
      for (i = 0; i < nXYPairs; i++) {
        fVals[i] = pow (10.0, MIN (HUGE_LOG_D, fVals[i]));
      }
      if (eosData->recordType != EOS_RECORD_TYPE6) {
        for (i = 0; i < nXYPairs; i++) {
          dFx[i] = fVals[i] / MAX (TINY_D, xVals[i]) * dFx[i];
          dFy[i] = fVals[i] / MAX (TINY_D, yVals[i]) * dFy[i];
        }
      }
    }
    
    /* Apply supplied conversion factors to convert output to host code units */
    if (fconv != 1.0) {
      for (i = 0; i < nXYPairs; i++) {
        fVals[i] *= fconv;
      }
      if (eosData->recordType != EOS_RECORD_TYPE6) {
        for (i = 0; i < nXYPairs; i++) {
          dFx[i] *= fconv;
          dFy[i] *= fconv;
        }
      }
    }
    
    if (eosData->recordType != EOS_RECORD_TYPE6) {
      if (xconv != 1.0) {
        for (i = 0; i < nXYPairs; i++)
          dFx[i] /= FLOOR (xconv);
      }
      if (yconv != 1.0) {
        for (i = 0; i < nXYPairs; i++)
          dFy[i] /= FLOOR (yconv);
      }
    }

    EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;

    if (!skipExtrap){
      /* Set extrapolation error codes if input less than temperature
	 or density minimum. */
      xyBounds = me->interpolationDataList[tableHandle]->xyBounds;
      for (i = 0; i < nXYPairs; i++) {
	
	_eos_SplitExtrapErrors (xyBounds[i], &xErr, &yErr);
	
	if (EOS_TYPE_TO_INDEP_VAR1(dataType) == EOS_D && xVals[i] < 0.0)
	  xErr = EOS_xLo_yOk;
	if (EOS_TYPE_TO_INDEP_VAR2(dataType) == EOS_T && yVals[i] < 0.0)
	  yErr = EOS_xOk_yLo;
	
	if (xyBounds[i] != EOS_CANT_INVERT_DATA)
	  xyBounds[i] = _eos_CombineExtrapErrors (xErr, yErr);
	
	if (xyBounds[i] != EOS_OK) {
	  err = EOS_INTERP_EXTRAPOLATED;
	  me->interpolationDataList[tableHandle]->lastErrorCode = err;
	}
      }
    }
    if ( ! me->interpolationDataList[tableHandle]->enableXYpassthru
         && ! me->interpolationDataList[tableHandle]->enableXYmodify ) {
      
      if (X)
        EOS_FREE (X);
      if (Y)
        EOS_FREE (Y);
      
    }

    if ( ! me->interpolationDataList[tableHandle]->enableXYpassthru
         && me->interpolationDataList[tableHandle]->enableXYmodify
         && me->interpolationDataList[tableHandle]->useHostXY) {

      if (logAxes) {
        /* Convert previously-modified input to 10^x and 10^y values for 500 and 600 Sesame tables if necessary */
        for (i = 0; i < nXYPairs; i++) {
          xVals[i] = pow(10.0, (MAX (TINY_D, X[i])));
          yVals[i] = pow(10.0, (MAX (TINY_D, Y[i])));
        }
      }
      else {
        /* Apply supplied conversion factors to convert previously-modified input back to host units.
           NOTE: Previously-enforced temperature and density minimums cannot be undone. */
        for (i = 0; i < nXYPairs; i++) {
          xVals[i] = X[i] * FLOOR (xconv);
          yVals[i] = Y[i] * FLOOR (yconv);
        }
      }

    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, err);
      *errorCode = err;
      return;
    }
  }
#ifdef DO_OFFLOAD
  else
  {
    EOS_BOOLEAN isRecType6 = (eosData->recordType == EOS_RECORD_TYPE6);
    if ( ! me->interpolationDataList[tableHandle]->enableXYpassthru ) {
            
      EOS_BOOLEAN var1 = EOS_TYPE_TO_INDEP_VAR1(dataType) == EOS_D;
      EOS_BOOLEAN var2 = EOS_TYPE_TO_INDEP_VAR2(dataType) == EOS_T;
      int t = omp_get_default_device();
      if (var1 || (var2 || logAxes)) {
        EOS_REAL fxconv=FLOOR(xconv);
        EOS_REAL fyconv=FLOOR(yconv);
        EOS_REAL tinyD=TINY_D; tinyD=log10(tinyD); 
#pragma omp target is_device_ptr(xVals,yVals) 
        {
#pragma omp teams distribute parallel for
          for (i = 0; i < nXYPairs; i++) {
            if (! logAxes && var1 && xVals[i] < 0.0)
              xVals[i] = 0.0;
            else if (xconv!=1.0)
              xVals[i] = xVals[i] / fxconv;
            if (! logAxes && var2 && yVals[i] < 0.0)
              yVals[i] = 0.0;
            else if (yconv != 1.0)
              yVals[i] = yVals[i] / fyconv;
	    
            // Convert input to log10 values for 500 and 600 Sesame tables if necessary 
            if (logAxes) {
              xVals[i] = log10 (xVals[i]); if (tinyD>xVals[i]) xVals[i]=tinyD;
              yVals[i] = log10 (yVals[i]); if (tinyD>yVals[i]) yVals[i]=tinyD;
            }

          }      
        }
      }
    }

#pragma omp target is_device_ptr(fVals)
    {
#pragma omp teams distribute parallel for
      for (i = 0; i < nXYPairs; i++) {
        fVals[i] = 0.0;
      }
    }

    if (!isRecType6) {
#pragma omp target is_device_ptr(dFy,dFx)      
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++) {
          dFx[i] = 0.0;
          dFy[i] = 0.0;
        }
      }
    }
    if (eosData->Interpolate)
      eosData->Interpolate (eosData, tableHandle, dataType, nXYPairs, xVals, yVals, fVals, dFx, dFy,
                            me->interpolationDataList[tableHandle]->xyBounds, &err);
    else /* wrong record type */
      err = EOS_INVALID_DATA_TYPE;
   
    /* Convert output from log10 values for 500 and 600 Sesame tables if necessary */
    if (logAxes) {
#pragma omp target is_device_ptr(fVals)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++) {
          fVals[i] = pow (10.0, MIN (HUGE_LOG_D, fVals[i]));
        }
      }
      if (!isRecType6) {
#pragma omp target is_device_ptr(dFy,dFx, fVals, xVals, yVals)      
        {
#pragma omp teams distribute parallel for
          for (i = 0; i < nXYPairs; i++) {
            dFx[i] = fVals[i] / MAX (TINY_D, xVals[i]) * dFx[i];
            dFy[i] = fVals[i] / MAX (TINY_D, yVals[i]) * dFy[i];
          }
        }
      }
    }
  
    
    /* Apply supplied conversion factors to convert output to host code units */
    if (fconv != 1.0) {
#pragma omp target is_device_ptr(fVals)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++) {
          fVals[i] *= fconv;
        }
      }
      
      if (!isRecType6) {
#pragma omp target is_device_ptr(dFy,dFx)      
        {
#pragma omp teams distribute parallel for
          for (i = 0; i < nXYPairs; i++) {
            dFx[i] *= fconv;
            dFy[i] *= fconv;
          }
        }
      }
    }
    
  
    EOS_BOOLEAN revert = ( ! me->interpolationDataList[tableHandle]->enableXYpassthru
                           && me->interpolationDataList[tableHandle]->enableXYmodify
                           && me->interpolationDataList[tableHandle]->useHostXY);
    if (revert && xconv!=1.0) {
#pragma omp target is_device_ptr(xVals)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++){
          xVals[i] *= FLOOR(xconv);
        }
      }
    }
    if (revert && yconv!=1.0) {
#pragma omp target is_device_ptr(yVals)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++){
          yVals[i] *= FLOOR(yconv);
        }
      }
    }

    if (!isRecType6 && xconv != 1.0) {
#pragma omp target is_device_ptr(dFx)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++){
          dFx[i] /= FLOOR (xconv);
        }
      }
    }
    
    if (!isRecType6 && yconv != 1.0) {
#pragma omp target is_device_ptr(dFy)
      {
#pragma omp teams distribute parallel for
        for (i = 0; i < nXYPairs; i++){
          dFy[i] /= FLOOR (yconv);
        }
      }
    }

    if (logAxes) {
      /* Convert previously-modified input to 10^x and 10^y values for 500 and 600 Sesame tables if necessary */
#pragma omp target is_device_ptr(xVals, yVals)
      {
#pragma omp teams distribute parallel for
	for (i = 0; i < nXYPairs; i++) {
	  xVals[i] = pow(10.0, (MAX (TINY_D, xVals[i])));
	  yVals[i] = pow(10.0, (MAX (TINY_D, yVals[i])));
	}
      }
    }
  }
#endif /* DO_OFFLOAD */
}

/*!**************************************************************
 * function _eos_CheckExtrapCategory0
 *
 * Populate xyBounds array with appropriate extrapolation error
 * codes.
 *
 * Input Variables:
 * EOS_INTEGER nGhostData - number of ghost nodes added to original table
 * EOS_INTEGER nxtbl      - size of xtbls array
 * EOS_INTEGER nytbl      - size of ytbls array
 * EOS_REAL    *xtbls     - array of tabulated X data
 * EOS_REAL    *ytbls     - array of tabulated Y data
 * EOS_INTEGER nsrch      - size of xvals, yvals and xyBounds arrays
 * EOS_REAL    *xvals     - array of X values to check for extrapolation
 * EOS_REAL    *yvals     - (optional) array of Y values to check for extrapolation
 *
 * Output Variables:
 * EOS_INTEGER *xyBounds  - array of extrapolation error codes
 * EOS_INTEGER *err       - error code
 *
 ****************************************************************/
void _eos_CheckExtrapCategory0(EOS_INTEGER nGhostData,
                               EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                               EOS_REAL *xtbls, EOS_REAL *ytbls,
                               EOS_INTEGER nsrch, EOS_REAL *xvals, EOS_REAL *yvals,
                               EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  int j;
  EOS_INTEGER xErr, yErr, nG;
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap)
    return;
  nG = MAX(0, nGhostData);

  *err = EOS_OK;
  for (j=0; j<nsrch; j++) {
    xErr = yErr = EOS_OK;
    if (xvals[j] < MIN(xtbls[nG], xtbls[nxtbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xLo_yOk;
    }
    if (xvals[j] > MAX(xtbls[nG], xtbls[nxtbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xHi_yOk;
    }

    if (! yvals) { /* y is not considered */
      xyBounds[j] = xErr;
      continue;
    }

    if (yvals[j] < MIN(ytbls[nG], ytbls[nytbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yLo;
    }
    if (yvals[j] > MAX(ytbls[nG], ytbls[nytbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yHi;
    }
    /* combine xErr and yErr */
    xyBounds[j] = _eos_CombineExtrapErrors (xErr, yErr);
  }
}

/*!**************************************************************
 * function _eos_CheckExtrapCategory1
 *
 * Populate xyBounds array with appropriate extrapolation error
 * codes.
 *
 * @param[in] use_discontinuous_derivatives  
 *            specify which helper function to use
 * @param[in] nGhostData    number of ghost nodes added to original table
 * @param[in] iy_low        y index into ytbls that is lower bound of yvals
 * @param[in] doRational    use the birational interpolator for bounds check
 * @param[in] nxtbl         size of xtbls array
 * @param[in] nytbl         size of ytbls array
 * @param[in] xtbls         array of tabulated X data
 * @param[in] ytbls         array of tabulated Y data
 * @param[in] ftbls         array of tabulated F data
 * @param[in] nsrch         size of xvals, yvals and xyBounds arrays
 * @param[in] fvals         array of F values to check for extrapolation
 * @param[in] yvals         array of Y values to check for extrapolation
 * @param[in] xtbls_ht      hashtable for xtbls values (can be NULL)
 * @param[in] ytbls_ht      hashtable for ytbls values (can be NULL)
 *
 * @param[out] xyBounds     array of extrapolation error codes
 * @param[out] err          error code
 *
 ****************************************************************/
void _eos_CheckExtrapCategory1(EOS_BOOLEAN use_discontinuous_derivatives,
                               EOS_INTEGER nGhostData, EOS_INTEGER *iy_low,
                               EOS_BOOLEAN doRational,
                               EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                               EOS_REAL *xtbls, EOS_REAL *ytbls, EOS_REAL **ftbls,
                               EOS_INTEGER nsrch, EOS_REAL *fvals, EOS_REAL *yvals,
                               eos_HashTable1D *xtbls_ht, eos_HashTable1D* ytbls_ht,
                               EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  int i, j;
  EOS_INTEGER xErr, yErr, nG, min_index, max_index;
  EOS_REAL *fvals1, *fvals2, *dFx, *dFy, *min_x, *max_x, **_ftbls_;
  EOS_INTEGER *ix_low1, *ix_low2;

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap)
    return;
  ix_low1 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_low2 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  min_x = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  max_x = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals1 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals2 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  dFx = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  dFy = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  
  nG = MAX(0, nGhostData);

  if (doRational) { /* use birational */
    min_index = 1;
    max_index = nxtbl - 3;
  }
  else { /* use bilinear */
    min_index = nG;
    max_index = nxtbl - 2 - nG;
  }
  for (j=0; j<nsrch; j++) { /* initialize arrays */
    min_x[j] = MIN(xtbls[nG], xtbls[nxtbl-1-nG]);
    max_x[j] = MAX(xtbls[nG], xtbls[nxtbl-1-nG]);
    ix_low1[j] = (xtbls[nG] < xtbls[nxtbl-1-nG]) ? min_index : max_index;
    ix_low2[j] = (xtbls[nG] > xtbls[nxtbl-1-nG]) ? min_index : max_index;
  }

  if (doRational) { /* use birational */
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low1, iy_low,
                               min_x, yvals, fvals1, dFx, dFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low2, iy_low,
                               max_x, yvals, fvals2, dFx, dFy, xyBounds, err);
  }
  else { /* use bilinear */
    _ftbls_ = (EOS_REAL **) malloc ((nytbl-2*nG) * sizeof (EOS_REAL*));
    for (j=0; j<nytbl-2*nG; j++)
      _ftbls_[j] = (EOS_REAL *) malloc ((nxtbl-2*nG) * sizeof (EOS_REAL));
    for (j=0; j<nytbl-2*nG; j++)
      for (i=0; i<nxtbl-2*nG; i++)
        _ftbls_[j][i] = ftbls[j+nG][i+nG];

    eos_BiLineInterpolate (use_discontinuous_derivatives,
                           nsrch, nxtbl-2*nG, nytbl-2*nG, xtbls+nG, ytbls+nG, _ftbls_, nG,
                           min_x, yvals, fvals1, dFx, dFy, xtbls_ht, ytbls_ht, xyBounds, err);
    eos_BiLineInterpolate (use_discontinuous_derivatives,
                           nsrch, nxtbl-2*nG, nytbl-2*nG, xtbls+nG, ytbls+nG, _ftbls_, nG,
                           max_x, yvals, fvals2, dFx, dFy, xtbls_ht, ytbls_ht, xyBounds, err);

    for (j=0; j<nytbl-2*nG; j++)
      EOS_FREE(_ftbls_[j]);
    EOS_FREE(_ftbls_);
  }

  for (j=0; j<nsrch; j++) {
    xErr = yErr = EOS_OK;
    if (fvals[j] < MIN(fvals1[j], fvals2[j])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xLo_yOk;
    }
    if (fvals[j] > MAX(fvals1[j], fvals2[j])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xHi_yOk;
    }
    
    if (yvals[j] < MIN(ytbls[nG], ytbls[nytbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yLo;
    }
    if (yvals[j] > MAX(ytbls[nG], ytbls[nytbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yHi;
    }
    /* combine xErr and yErr */
    xyBounds[j] = _eos_CombineExtrapErrors (xErr, yErr);
  }

  EOS_FREE(ix_low1);
  EOS_FREE(ix_low2);
  EOS_FREE(min_x);
  EOS_FREE(max_x);
  EOS_FREE(fvals1);
  EOS_FREE(fvals2);
  EOS_FREE(dFx);
  EOS_FREE(dFy);
}

/*!**************************************************************
 * function _eos_CheckExtrapCategory2
 *
 * Populate xyBounds array with appropriate extrapolation error
 * codes.
 *
 * Input Variables:
 * EOS_BOOLEAN use_discontinuous_derivatives - input - specify which helper function to use
 * EOS_INTEGER nGhostData - number of ghost nodes added to original table
 * EOS_INTEGER *ix_low    - x index into xtbls that is lower bound of xvals
 * EOS_BOOLEAN doRational - use the birational interpolator for bounds check
 * EOS_INTEGER nxtbl      - size of xtbls array
 * EOS_INTEGER nytbl      - size of ytbls array
 * EOS_REAL    *xtbls     - array of tabulated X data
 * EOS_REAL    *ytbls     - array of tabulated Y data
 * EOS_REAL    **ftbls    - array of tabulated F data
 * EOS_INTEGER nsrch      - size of xvals, yvals and xyBounds arrays
 * EOS_REAL    *xvals     - array of X values to check for extrapolation
 * EOS_REAL    *fvals     - array of F values to check for extrapolation
 * eos_HashTable1D *xtbls_ht - hashtable for xtbls (can be NULL)
 * eos_HashTable1D *ytbls_ht - hashtable for ytbls (can be NULL)
 *
 * Output Variables:
 * EOS_INTEGER *xyBounds  - array of extrapolation error codes
 * EOS_INTEGER *err       - error code
 *
 ****************************************************************/
void _eos_CheckExtrapCategory2(EOS_BOOLEAN use_discontinuous_derivatives,
                               EOS_INTEGER nGhostData, EOS_INTEGER *ix_low,
                               EOS_BOOLEAN doRational,
                               EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                               EOS_REAL *xtbls, EOS_REAL *ytbls, EOS_REAL **ftbls,
                               EOS_INTEGER nsrch, EOS_REAL *xvals, EOS_REAL *fvals,
                               eos_HashTable1D* xtbls_ht, eos_HashTable1D* ytbls_ht,
                               EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  int i, j;
  EOS_INTEGER xErr, yErr, nG, min_index, max_index;
  EOS_REAL *fvals1, *fvals2, *dFx, *dFy, *min_y, *max_y, **_ftbls_;
  EOS_INTEGER *iy_low1, *iy_low2;

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap)
    return;
  *err = EOS_OK;

  iy_low1 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  iy_low2 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  min_y = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  max_y = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals1 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals2 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  dFx = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  dFy = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));

  nG = MAX(0, nGhostData);

  if (doRational) { /* use birational */
    min_index = 1;
    max_index = nytbl - 3;
  }
  else { /* use bilinear */
    min_index = nG;
    max_index = nytbl - 2 - nG;
  }
  for (j=0; j<nsrch; j++) { /* initialize arrays */
    min_y[j] = MIN(ytbls[nG], ytbls[nytbl-1-nG]);
    max_y[j] = MAX(ytbls[nG], ytbls[nytbl-1-nG]);
    iy_low1[j] = (ytbls[nG] < ytbls[nytbl-1-nG]) ? min_index : max_index;
    iy_low2[j] = (ytbls[nG] > ytbls[nytbl-1-nG]) ? min_index : max_index;
  }

  if (doRational) { /* use birational */
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low1,
                               xvals, min_y, fvals1, dFx, dFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low2,
                               xvals, max_y, fvals2, dFx, dFy, xyBounds, err);
  }
  else { /* use bilinear */
    _ftbls_ = (EOS_REAL **) malloc ((nytbl-2*nG) * sizeof (EOS_REAL*));
    for (j=0; j<nytbl-2*nG; j++)
      _ftbls_[j] = (EOS_REAL *) malloc ((nxtbl-2*nG) * sizeof (EOS_REAL));
    for (j=0; j<nytbl-2*nG; j++)
      for (i=0; i<nxtbl-2*nG; i++)
        _ftbls_[j][i] = ftbls[j+nG][i+nG];

    eos_BiLineInterpolate (use_discontinuous_derivatives,
                           nsrch, nxtbl-2*nG, nytbl-2*nG, xtbls+nG, ytbls+nG, _ftbls_, nG,
                           xvals, min_y, fvals1, dFx, dFy, xtbls_ht, ytbls_ht, xyBounds, err);
    eos_BiLineInterpolate (use_discontinuous_derivatives,
                           nsrch, nxtbl-2*nG, nytbl-2*nG, xtbls+nG, ytbls+nG, _ftbls_, nG,
                           xvals, max_y, fvals2, dFx, dFy, xtbls_ht, ytbls_ht, xyBounds, err);

    for (j=0; j<nytbl-2*nG; j++)
      EOS_FREE(_ftbls_[j]);
    EOS_FREE(_ftbls_);
  }
  for (j=0; j<nsrch; j++) {
    xErr = yErr = EOS_OK;
    if (xvals[j] < MIN(xtbls[nG], xtbls[nxtbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xLo_yOk;
    }
    if (xvals[j] > MAX(xtbls[nG], xtbls[nxtbl-1-nG])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xHi_yOk;
    }

    if (fvals[j] < MIN(fvals1[j], fvals2[j])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yLo;
    }
    if (fvals[j] > MAX(fvals1[j], fvals2[j])) {
      *err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yHi;
    }
    /* combine xErr and yErr */
    xyBounds[j] = _eos_CombineExtrapErrors (xErr, yErr);
  }

  EOS_FREE(iy_low1);
  EOS_FREE(iy_low2);
  EOS_FREE(min_y);
  EOS_FREE(max_y);
  EOS_FREE(fvals1);
  EOS_FREE(fvals2);
  EOS_FREE(dFx);
  EOS_FREE(dFy);
}

/*!**************************************************************
 * function _eos_CheckExtrap
 *
 * Populate xyBounds array with appropriate extrapolation error codes given general
 * independent variable value bounds.
 *
 * Input Variables:
 * EOS_REAL    xMin       - Minimum valid value for xvals[i]
 * EOS_REAL    xMax       - Maximum valid value for xvals[i]
 * EOS_REAL    yMin       - Minimum valid value for yvals[i]
 * EOS_REAL    yMax       - Maximum valid value for yvals[i]
 * EOS_INTEGER nsrch      - size of xvals, yvals and xyBounds arrays
 * EOS_REAL    *xvals     - array of X values to check for extrapolation
 * EOS_REAL    *yvals     - (optional) array of Y values to check for extrapolation
 *
 * Output Variables:
 * EOS_INTEGER *xyBounds  - array of extrapolation error codes
 * EOS_INTEGER *err       - error code
 *
 ****************************************************************/
EOS_INTEGER _eos_CheckExtrapEosInterpolationGeneric(EOS_REAL xMin, EOS_REAL xMax, EOS_REAL yMin, EOS_REAL yMax,
                                                    EOS_INTEGER nsrch, EOS_REAL *xvals, EOS_REAL *yvals,
                                                    EOS_INTEGER *xyBounds)
{
  int j;
  EOS_INTEGER xErr, yErr;
  EOS_INTEGER err = EOS_OK;
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;

  if (skipExtrap)
    return err;

  for (j=0; j<nsrch; j++) {
    xErr = yErr = EOS_OK;
    if (xvals[j] < xMin) {
      err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xLo_yOk;
    }
    if (xvals[j] > xMax) {
      err = EOS_INTERP_EXTRAPOLATED;
      xErr = EOS_xHi_yOk;
    }

    if (! yvals) { /* y is not considered */
      xyBounds[j] = xErr;
      continue;
    }

    if (yvals[j] < yMin) {
      err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yLo;
    }
    if (yvals[j] > yMax) {
      err = EOS_INTERP_EXTRAPOLATED;
      yErr = EOS_xOk_yHi;
    }
    /* combine xErr and yErr */
    xyBounds[j] = _eos_CombineExtrapErrors (xErr, yErr);
  }

  return err;
}

/****************************************************************
 eos_CheckExtrap
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
*******************************************************************/
void eos_CheckExtrapEosInterpolation (eos_Interpolation *me,
                                      EOS_INTEGER tableHandle,
                                      EOS_INTEGER nXYPairs, EOS_REAL *srchX,
                                      EOS_REAL *srchY, EOS_INTEGER *xyBounds,
                                      EOS_INTEGER *errorCode)
{
  eos_Data *eosData;
  EOS_INTEGER dataType, i, err = EOS_OK;
  *errorCode = EOS_OK;

  
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (skipExtrap) 
    return;

  /* first check last interpolation error for this handle. If the error was not EOS_OK,
     then return the cached in xyBounds values */
  if (0 && eos_GetStandardErrorCodeFromCustomErrorCode(me->interpolationDataList[tableHandle]->lastErrorCode) != EOS_OK &&
      nXYPairs == me->interpolationDataList[tableHandle]->nXYPairs) {
    for (i = 0; i < nXYPairs; i++)
      xyBounds[i] = me->interpolationDataList[tableHandle]->xyBounds[i];
    return;
  }

  /* get eos Data pointer and the dataType from global data map */
  eosData =
    eos_GetEosDataEosDataMap (&gEosDataMap, tableHandle, &dataType, &err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, err);
    *errorCode = err;
    return;
  }

  { /* Check extrapolation according to recordType */
    if (eosData->CheckExtrap)
      eosData->CheckExtrap (eosData, tableHandle, dataType,
                            nXYPairs, srchX, srchY, xyBounds, &err);
    else /* wrong record type */
      err = EOS_INVALID_DATA_TYPE;
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK) {
    ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, err);
    *errorCode = err;
    return;
  }
}

/************************************************************************
 * 
 * function eos_LineInterpolate
 *  
 * Purpose   -- performs vector uni-linear (2-point)
 *              Wrapper around _eos_LineInterpolateWithContinuousDerivatives
 *              and _eos_LineInterpolateWithOriginalDerivatives helper functions
 *
 * Returned Values: none
 *
 * Input Values:
 * EOS_BOOLEAN use_discontinuous_derivatives input  specify which helper function to use
 * EOS_INTEGER numZones                      input  integer number of zones being processed.
 * EOS_INTEGER numXVals                      input  integer number of data table x values. 
 * EOS_INTEGER numYVals                      input  integer number of data table y values. 
 * EOS_INTEGER fixedIndex                    input  integer index of fixed row/column. 
 * EOS_REAL    YValues[numYVals]             input  real array containing data table non-fixed var values values.
 * EOS_REAL    FVals[numXVals][numYVals]     input  real array containing data table f values.
 * EOS_REAL    searchYVals[numZones]         input  real search vector containing srach non-fixed var values.
 * EOS_REAL    searchFVals[numZones]         output real array containing interpolated values of
 *                                           eos function f(x,y) 
 * EOS_REAL    searchDFy[numZones]           derivative wrt non-fixed var
 * EOS_CHAR    fixedVar;                     which var i s fixed: 'x' or 'y'
 * eos_HashTable1D *YValues_ht               hashtable for YValues (can be NULL)
 * EOS_INTEGER *xyBounds                     interpolation errors per xy-pair
 * EOS_INTEGER *err                          output error code
 * 
 ************************************************************************/
void eos_LineInterpolate (EOS_BOOLEAN use_discontinuous_derivatives,
                          EOS_INTEGER numZones, EOS_INTEGER numXVals,
                          EOS_INTEGER numYVals, EOS_INTEGER fixedIndex,
                          EOS_REAL *YValues, EOS_REAL **FVals,
                          EOS_REAL *searchYVals, EOS_REAL *searchFVals,
                          EOS_REAL *searchDFy, EOS_CHAR fixedVar,
                          eos_HashTable1D* YValues_ht,
                          EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  if (use_discontinuous_derivatives)
    _eos_LineInterpolateWithOriginalDerivatives (numZones, numXVals, numYVals, fixedIndex,
                                                 YValues, FVals, searchYVals, searchFVals,
                                                 searchDFy, fixedVar, YValues_ht, xyBounds, err);
  else
    _eos_LineInterpolateWithContinuousDerivatives (numZones, numXVals, numYVals, fixedIndex,
                                                   YValues, FVals, searchYVals, searchFVals,
                                                   searchDFy, fixedVar, YValues_ht, xyBounds, err);
}

/************************************************************************
 * 
 * function _eos_LineInterpolateWithContinuousDerivatives
 *  
 * Purpose   -- helper function performs vector uni-linear (2-point)
 *              search-interpolation on eos function f(x,y)
 *              holding 1 variable fixed.  uses linear extrapolation.
 *              Implements new continous-derivatives.
 *
 * Returned Values: none
 *
 * Input Values:
 * EOS_INTEGER numZones                      input  integer number of zones being processed.
 * EOS_INTEGER numXVals                      input  integer number of data table x values. 
 * EOS_INTEGER numYVals                      input  integer number of data table y values. 
 * EOS_INTEGER fixedIndex                    input  integer index of fixed row/column. 
 * EOS_REAL    YValues[numYVals]             input  real array containing data table non-fixed var values values.
 * EOS_REAL    FVals[numXVals][numYVals]     input  real array containing data table f values.
 * EOS_REAL    searchYVals[numZones]         input  real search vector containing srach non-fixed var values.
 * EOS_REAL    searchFVals[numZones]         output real array containing interpolated values of
 *                                           eos function f(x,y) 
 * EOS_REAL    searchDFy[numZones]           derivative wrt non-fixed var
 * EOS_CHAR    fixedVar;                     which var i s fixed: 'x' or 'y'
 * eos_HashTable1D *YValues_ht               hashtable for YValues (can be NULL)
 * EOS_INTEGER *xyBounds                     interpolation errors per xy-pair
 * EOS_INTEGER *err                          output error code
 * 
 ************************************************************************/

void _eos_LineInterpolateWithContinuousDerivatives (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                                    EOS_INTEGER numYVals, EOS_INTEGER fixedIndex,
                                                    EOS_REAL *YValues, EOS_REAL **FVals,
                                                    EOS_REAL *searchYVals, EOS_REAL *searchFVals,
                                                    EOS_REAL *searchDFy, EOS_CHAR fixedVar, eos_HashTable1D* YValues_ht,
                                                    EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER j, *iyv, err1, jfixed;
  *err = EOS_OK;

  EOS_BOOLEAN useGpuData = EOS_FALSE;

#ifdef DO_OFFLOAD
  EOS_INTEGER t;
  useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
  t = omp_get_default_device();
  if (!useGpuData) {
#endif
    iyv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
#ifdef DO_OFFLOAD
  } else{
    iyv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
  }
#endif


  /* if (2-point) search-interpolation is impossible, return. */
  if (fixedVar == 'x' && numYVals <= 1)
    return;
  if (fixedVar == 'y' && numXVals <= 1)
    return;

  /*
    perform vector table searches to load search vectors containing
    indices and spacings of nearest data table y values to input searchYVals.
  */
  if (fixedVar == 'x') {
    _eos_srchdf (numZones, searchYVals, 1, numYVals, YValues, 1, iyv, YValues_ht,
                 xyBounds, &err1);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(err1) != EOS_OK)
      *err = err1;
    /* ignore error, it might be for one of the poiints only */
    jfixed = (numXVals < fixedIndex) ? numXVals - 1 : fixedIndex;
  }
  else {                        /* y is fixed */

    _eos_srchdf (numZones, searchYVals, 1, numXVals, YValues, 1, iyv, YValues_ht,
                 xyBounds, &err1);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(err1) != EOS_OK)
      *err = err1;
    /* ignore error, it might be for one of the points only */
    jfixed = (numYVals < fixedIndex) ? numYVals - 1 : fixedIndex;
  }

  if (jfixed < 0)
    jfixed = 0;
  /*
    .... interpolate values of eos function f(x,y) and derivative.
  */



#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(YValues, searchYVals, FVals, searchFVals, searchDFy, iyv)
  {
#pragma omp teams distribute parallel for                                                      
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif
    for (j = 0; j < numZones; j++) {
    /*    
        if (xyBounds[j] == EOS_UNDEFINED) {
   		searchFVals[j] = 0.0;
    	continue;
    	}
    */

      EOS_INTEGER go_upwards, offset;
      EOS_REAL dyg, ryv, delta_f, stag_y1, stag_y2, dfdy1, dfdy2;
      EOS_REAL ftg0, ftg1, ftg2, ftg3;
      EOS_REAL *FVals_flat = !useGpuData?&FVals[0][0]:&FVals[0];
      /* compute fractionals and differentials */
      dyg = YValues[iyv[j] + 1] - YValues[iyv[j]];
      ryv = (searchYVals[j] - YValues[iyv[j]]) / dyg;
      if (fixedVar == 'x') {
	if(iyv[j]>0)
	  ftg0 = FVals_flat[(iyv[j] - 1)*numXVals+jfixed]; /* left side of previous interval */
			    ftg1 = FVals_flat[iyv[j]*numXVals+jfixed];     /* left side of interval */
			    ftg2 = FVals_flat[(iyv[j] + 1)*numXVals+jfixed]; /* right side of interval */
			    if(iyv[j]<numYVals-2)
			      ftg3 = FVals_flat[(iyv[j] + 2)*numXVals+jfixed]; /* right side of next interval */
			    }
	  else {                      /* y is fixed */
	    if(iyv[j]>0)
	      ftg0 = FVals_flat[jfixed*numXVals+iyv[j] - 1]; /* left side of previous interval */
	    ftg1 = FVals_flat[jfixed*numXVals+iyv[j]];     /* left side of interval */
	    ftg2 = FVals_flat[jfixed*numXVals+iyv[j] + 1]; /* right side of interval */
	    if(iyv[j]<numXVals-2)
	      ftg3 = FVals_flat[jfixed*numXVals+iyv[j] + 2]; /* right side of next interval */
	  }
	
	/* interpolate value of function only. */
	delta_f = ftg2 - ftg1;         /* dF */
	searchFVals[j] = ftg1 + ryv * delta_f; /* F0 + (dY'/dY) * dF */
	/* derivative wrt y. */
	
	if (searchDFy) {
	  /*determine whether to use staggered cell upward or downward*/
	  go_upwards = searchYVals[j] >= (YValues[iyv[j] + 1] + YValues[iyv[j]])/2;
	  offset=((go_upwards && iyv[j]<(fixedVar=='x'?numYVals:numXVals)-2) || iyv[j]==0); /* note: 0 or 1 */
	  /* if(offset^go_upwards) warn(extrapolation); */
	  stag_y1 = (YValues[iyv[j] + offset - 1] + YValues[iyv[j] + offset])/2;
	  stag_y2 = (YValues[iyv[j] + offset] + YValues[iyv[j] + offset + 1])/2;
	  EOS_REAL ftgo  = (offset==0?ftg0:ftg1);
	  EOS_REAL ftgo1 = (offset==0?ftg1:ftg2);
	  EOS_REAL ftgo2 = (offset==0?ftg2:ftg3);
	  dfdy1 = (ftgo1 - ftgo )/(YValues[iyv[j] + offset] - YValues[iyv[j] + offset - 1]);
	  dfdy2 = (ftgo2 - ftgo1)/(YValues[iyv[j] + offset + 1] - YValues[iyv[j] + offset]);
	  
	  searchDFy[j] = ((stag_y2 - searchYVals[j]) * dfdy1 + (searchYVals[j] - stag_y1) * dfdy2)/(stag_y2 - stag_y1);  /* dF/dY */
	}
      }
#ifdef DO_OFFLOAD
    }
#endif

#ifdef DO_OFFLOAD
    if (!useGpuData) {
#endif /* DO_OFFLOAD */
      EOS_FREE (iyv);
#ifdef DO_OFFLOAD
    } else {
      omp_target_free(iyv, t);
    }
#endif /* DO_OFFLOAD */
}

/************************************************************************
 * 
 * function _eos_LineInterpolateWithOriginalDerivatives
 *  
 * Purpose   -- helper function performs vector uni-linear (2-point)
 *              search-interpolation on eos function f(x,y)
 *              holding 1 variable fixed.  uses linear extrapolation.
 *              Implements original discontinous-derivatives.
 *              The EOS_DISCONTINUOUS_DERIVATIVES option enables
 *              usage of this function when EOS_LINEAR is specified.
 *
 *
 * Returned Values: none
 *
 * Input Values:
 * EOS_INTEGER numZones                      input  integer number of zones being processed.
 * EOS_INTEGER numXVals                      input  integer number of data table x values. 
 * EOS_INTEGER numYVals                      input  integer number of data table y values. 
 * EOS_INTEGER fixedIndex                    input  integer index of fixed row/column. 
 * EOS_REAL    YValues[numYVals]             input  real array containing data table non-fixed var values values.
 * EOS_REAL    FVals[numXVals][numYVals]     input  real array containing data table f values.
 * EOS_REAL    searchYVals[numZones]         input  real search vector containing srach non-fixed var values.
 * EOS_REAL    searchFVals[numZones]         output real array containing interpolated values of
 *                                           eos function f(x,y) 
 * EOS_REAL    searchDFy[numZones]           derivative wrt non-fixed var
 * EOS_CHAR    fixedVar;                     which var i s fixed: 'x' or 'y'
 * eos_HashTable1D *YValues_ht               hashtable for YValues (can be NULL)
 * EOS_INTEGER *xyBounds                     interpolation errors per xy-pair
 * EOS_INTEGER *err                          output error code
 * 
 ************************************************************************/
void _eos_LineInterpolateWithOriginalDerivatives (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                                  EOS_INTEGER numYVals, EOS_INTEGER fixedIndex,
                                                  EOS_REAL *YValues, EOS_REAL **FVals,
                                                  EOS_REAL *searchYVals, EOS_REAL *searchFVals,
                                                  EOS_REAL *searchDFy, EOS_CHAR fixedVar, eos_HashTable1D* YValues_ht,
                                                  EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER j, jfixed, *iyv, err1;
  EOS_BOOLEAN useGpuData = EOS_FALSE;
  *err = EOS_OK;

#ifdef DO_OFFLOAD
  EOS_INTEGER t;
  useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
  t = omp_get_default_device();
  if (!useGpuData) {
#endif
    iyv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
#ifdef DO_OFFLOAD
  } else {
    iyv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
  }
#endif /* DO_OFFLOAD */
  
  /* if (2-point) search-interpolation is impossible, return. */
  if (fixedVar == 'x' && numYVals <= 1)
    return;
  if (fixedVar == 'y' && numXVals <= 1)
    return;

  /*
    perform vector table searches to load search vectors containing
    indices and spacings of nearest data table y values to input searchYVals.
  */
  if (fixedVar == 'x') {
    _eos_srchdf (numZones, searchYVals, 1, numYVals, YValues, 1, iyv, YValues_ht,
                 xyBounds, &err1);
    if (err1 != EOS_OK)
      *err = err1;
    /* ignore error, it might be for one of the poiints only */
    jfixed = (numXVals < fixedIndex) ? numXVals - 1 : fixedIndex;
  }
  else {                        /* y is fixed */

    _eos_srchdf (numZones, searchYVals, 1, numXVals, YValues, 1, iyv, YValues_ht,
                 xyBounds, &err1);
    if (err1 != EOS_OK)
      *err = err1;
    /* ignore error, it might be for one of the poiints only */
    jfixed = (numYVals < fixedIndex) ? numYVals - 1 : fixedIndex;
  }

  if (jfixed < 0)
    jfixed = 0;
  /*
    .... interpolate values of eos function f(x,y) and derivative.
  */


#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(YValues, iyv, searchYVals, FVals, searchFVals, searchDFy)
  {
    #pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif

  for (j = 0; j < numZones; j++) {

    EOS_REAL *FVals_flat = !useGpuData?&FVals[0][0]:&FVals[0];
    EOS_REAL ftg1, ftg2, ftg3;
    /*
      if (xyBounds[j] == EOS_UNDEFINED) {
      searchFVals[j] = 0.0;
      continue;
    }
    */

    /* compute fractionals and differentials */
    EOS_REAL dyg = YValues[iyv[j] + 1] - YValues[iyv[j]];
    EOS_REAL ryv = (searchYVals[j] - YValues[iyv[j]]) / dyg;
    if (fixedVar == 'x') {
      ftg1 = FVals_flat[iyv[j] * numXVals + jfixed];     /* left side of interval */
      ftg2 = FVals_flat[(iyv[j] + 1) * numXVals + jfixed]; /* right side of interval */
    }
    else {                      /* y is fixed */
      ftg1 = FVals_flat[jfixed * numXVals + iyv[j]];     /* left side of interval */
      ftg2 = FVals_flat[jfixed * numXVals + iyv[j] + 1]; /* right side of interval */
    }

    ftg3 = ftg2 - ftg1;

    /* interpolate value of function only. */
    ftg3 = ftg2 - ftg1;         /* dF */
    searchFVals[j] = ftg1 + ryv * ftg3; /* F0 + (dY'/dY) * dF */

    if (searchDFy) {
      /* derivative wrt y. */
      searchDFy[j] = ftg3 / dyg;  /* dF/dY */
    }
  }
#ifdef DO_OFFLOAD
  }
#endif /* DO_OFFLOAD */

#ifdef DO_OFFLOAD
  if (!useGpuData) {
#endif /* DO_OFFLOAD */
  EOS_FREE (iyv);
#ifdef DO_OFFLOAD
  } else {
    omp_target_free(iyv, t);
  }
#endif /* DO_OFFLOAD */

}

/*********************************************************************************************
     eos_BiLineInterpolate()
     purpose   -- performs vector bi-linear (4-point)
                  search-interpolation on eos function f(x,y).
                  uses linear extrapolation.
                  Wrapper around _eos_BiLineInterpolateWithContinuousDerivatives
                  and _eos_BiLineInterpolateWithOriginalDerivatives helper functions
     arguments --
       EOS_BOOLEAN use_discontinuous_derivatives    input  specify which helper function to use
       numZones    = input  integer number of zones being processed.
       numXVals    = input  integer number of data table x values.
       numYVals    = input  integer number of data table y values.
       XValues    = input  real(r8kind) array containing data table x values.
       YValues    = input  real(r8kind) array containing data table y values.
       FValues    = input  real(r8kind) array containing data table f values.
       searchXVals    = input  real(r8kind) search vector containing x values.
       searchYVals    = input  real(r8kind) search vector containing y values.
       searchFVals    = output real(r8kind) array containing interpolated values of eos function f(x,y) 
       searchDFx[numZones]           derivative wrt x
       searchDFy[numZones]           derivative wrt y
       XValues_ht    = input  hashtable for XValues (can be NULL)
       YValues_ht    = input  hashtable for YValues (can be NULL)
	   EOS_INTEGER *xyBounds     output - interpolation errors per xy-pair
************************************************************************************************/
void eos_BiLineInterpolate (EOS_BOOLEAN use_discontinuous_derivatives,
                            EOS_INTEGER numZones, EOS_INTEGER numXVals,
                            EOS_INTEGER numYVals, EOS_REAL *XValues,
                            EOS_REAL *YValues, EOS_REAL **FValues, EOS_INTEGER nGhostData,
                            EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                            EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                            EOS_REAL *searchDFy, eos_HashTable1D* XValues_ht, eos_HashTable1D* YValues_ht, EOS_INTEGER *xyBounds,
                            EOS_INTEGER *err)
{
  if (use_discontinuous_derivatives)
    _eos_BiLineInterpolateWithOriginalDerivatives (numZones, numXVals, numYVals, XValues, YValues,
                                                   FValues, nGhostData, searchXVals, searchYVals, searchFVals,
                                                   searchDFx, searchDFy, XValues_ht, YValues_ht, xyBounds, err);
  else
    _eos_BiLineInterpolateWithContinuousDerivatives (numZones, numXVals, numYVals, XValues, YValues,
                                                     FValues, nGhostData, searchXVals, searchYVals, searchFVals,
                                                     searchDFx, searchDFy, XValues_ht, YValues_ht, xyBounds, err);

}

/*********************************************************************************************
     _eos_BiLineInterpolateWithContinuousDerivatives()
     purpose   -- performs vector bi-linear (4-point) search-interpolation on eos function f(x,y).
                  uses linear extrapolation.

     arguments --
       numZones    = input  integer number of zones being processed.
       numXVals    = input  integer number of data table x values.
       numYVals    = input  integer number of data table y values.
       XValues    = input  real(r8kind) array containing data table x values.
       XValues_ht = input  hashtable for XValues array
       YValues    = input  real(r8kind) array containing data table y values.
       YValues_ht = input  hashtable for YValues array
       FValues    = input  real(r8kind) array containing data table f values.
       searchXVals    = input  real(r8kind) search vector containing x values.
       searchYVals    = input  real(r8kind) search vector containing y values.
       searchFVals    = output real(r8kind) array containing interpolated values of eos function f(x,y) 
       searchDFx[numZones]           derivative wrt x
       searchDFy[numZones]           derivative wrt y
       XValues_ht    = input  hashtable for XValues (can be NULL)
       YValues_ht    = input  hashtable for YValues (can be NULL)
	   EOS_INTEGER *xyBounds     output - interpolation errors per xy-pair
************************************************************************************************/
void _eos_BiLineInterpolateWithContinuousDerivatives (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                                      EOS_INTEGER numYVals, EOS_REAL *XValues,
                                                      EOS_REAL *YValues, EOS_REAL **FValues, EOS_INTEGER nGhostData,
                                                      EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                                                      EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                                                      EOS_REAL *searchDFy, eos_HashTable1D* XValues_ht, eos_HashTable1D* YValues_ht, EOS_INTEGER *xyBounds,
                                                      EOS_INTEGER *err)
{
  EOS_INTEGER err1, *iyv, *ixv, j, *xyBounds2;


  *err = EOS_OK;
  EOS_BOOLEAN useGpuData=EOS_FALSE;

#ifdef DO_OFFLOAD
  EOS_INTEGER t;
  useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
  t = omp_get_default_device();
  if (!useGpuData) {
#endif
      iyv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
      ixv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indices of X near which X points are */
#ifdef DO_OFFLOAD
    } else {
     ixv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
     iyv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
    }
#endif /* DO_OFFLOAD */

  *err = EOS_OK;

  if(!useGpuData) xyBounds2 = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));

  /* if (2-point) search-interpolation is impossible, return. */
  if (numYVals <= 1)
    return;

  /* .... perform vector table searches to load search vectors containing
     .... indices and spacings of nearest data table x,y values to x,searchYVals. */

  _eos_srchdf (numZones, searchXVals, 1, numXVals, XValues, 1, ixv, XValues_ht, xyBounds, &err1);
  /* ignore error, it might be for one point only */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err1) != EOS_OK)
    *err = err1;

  _eos_srchdf (numZones, searchYVals, 1, numYVals, YValues, 1, iyv, YValues_ht, useGpuData?xyBounds:xyBounds2, &err1);
  /* ignore error, it might be for one point only */
  if (eos_GetStandardErrorCodeFromCustomErrorCode(err1) != EOS_OK)
    *err = err1;

  /* ensure proper initial placement of bilinear stencil at table edges while ignoring ghost data */

#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(ixv, iyv)
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif

  for (j = 0; j < numZones; j++) {
    ixv[j] = MIN(ixv[j], numXVals-2-nGhostData);
    iyv[j] = MIN(iyv[j], numYVals-2-nGhostData);
  }
#ifdef DO_OFFLOAD
  }
#endif

  /* .... interpolate values of eos function f(x,y) and derivatives. */
  eos_bilinearInterpolate(XValues, numXVals, YValues, numYVals, FValues, ixv, iyv, numZones,searchXVals, searchYVals, searchFVals);

  eos_BilinearDerivatives(XValues, YValues, FValues, numXVals, numYVals, ixv, nGhostData, iyv, nGhostData, numZones, searchXVals, searchYVals,searchDFx, searchDFy);
  

#ifdef DO_OFFLOAD
  if (!useGpuData){
#endif
    EOS_FREE (iyv);
    EOS_FREE (ixv);
    EOS_FREE (xyBounds2)
#ifdef DO_OFFLOAD
  } else {
    omp_target_free(ixv, t);
    omp_target_free(iyv, t);
  }
#endif
    
}

/*********************************************************************************************
     _eos_BiLineInterpolateWithOriginalDerivatives()
     purpose   -- performs vector bi-linear (4-point) search-interpolation on eos function f(x,y).
                  uses linear extrapolation.

     arguments --
       numZones    = input  integer number of zones being processed.
       numXVals    = input  integer number of data table x values.
       numYVals    = input  integer number of data table y values.
       XValues    = input  real(r8kind) array containing data table x values.
       XValues_ht = input  hashtable for XValues array
       YValues    = input  real(r8kind) array containing data table y values.
       YValues_ht = input  hashtable for YValues array
       FValues    = input  real(r8kind) array containing data table f values.
       searchXVals    = input  real(r8kind) search vector containing x values.
       searchYVals    = input  real(r8kind) search vector containing y values.
       searchFVals    = output real(r8kind) array containing interpolated values of eos function f(x,y) 
       searchDFx[numZones]           derivative wrt x
       searchDFy[numZones]           derivative wrt y
       XValues_ht    = input  hashtable for XValues (can be NULL)
       YValues_ht    = input  hashtable for YValues (can be NULL)
	   EOS_INTEGER *xyBounds     output - interpolation errors per xy-pair
************************************************************************************************/
void _eos_BiLineInterpolateWithOriginalDerivatives (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                                    EOS_INTEGER numYVals, EOS_REAL *XValues,
                                                    EOS_REAL *YValues, EOS_REAL **FValues, EOS_INTEGER nGhostData,
                                                    EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                                                    EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                                                    EOS_REAL *searchDFy, eos_HashTable1D* XValues_ht, eos_HashTable1D* YValues_ht, EOS_INTEGER *xyBounds,
                                                    EOS_INTEGER *err)
{
  EOS_INTEGER err1, j, *iyv, *ixv, *xyBounds2;
  *err = EOS_OK;

  EOS_BOOLEAN useGpuData = EOS_FALSE;
  if (!useGpuData) xyBounds2 = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));
#ifdef DO_OFFLOAD
  EOS_INTEGER t;
  useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
  t = omp_get_default_device();
  if (!useGpuData) {
#endif
    iyv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indexes of Y near which Y points are */
    ixv = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));       /* indexes of X near which X points are */
#ifdef DO_OFFLOAD
  } else {
    iyv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
    ixv = omp_target_alloc(numZones*sizeof(EOS_INTEGER),t);
  }
#endif

  /* if (2-point) search-interpolation is impossible, return. */
  if (numYVals <= 1)
    return;

  /* .... perform vector table searches to load search vectors containing
     .... indices and spacings of nearest data table x,y values to x,searchYVals. */

  _eos_srchdf (numZones, searchXVals, 1, numXVals, XValues, 1, ixv, XValues_ht, xyBounds, &err1);
  /* ignore error, it might be for one point only */
  if (err1 != EOS_OK)
    *err = err1;

  _eos_srchdf (numZones, searchYVals, 1, numYVals, YValues, 1, iyv, YValues_ht, useGpuData?xyBounds:xyBounds2, &err1);
  /* ignore error, it might be for one point only */
  if (err1 != EOS_OK)
    *err = err1;

  /* ensure proper initial placement of bilinear stencil at table edges while ignoring ghost data */
  /* for (j = 0; j < numZones; j++) { */
  /*   ixv[j] = MAX(MIN(ixv[j], numXVals-2-nGhostData), nGhostData+1); */
  /*   iyv[j] = MAX(MIN(iyv[j], numYVals-2-nGhostData), nGhostData+1); */
  /* } */

  /* .... interpolate values of eos function f(x,y) and derivatives. */

#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(FValues, YValues, XValues, searchFVals, searchXVals, searchYVals)
  {
    #pragma omp teams distribute parallel for                                                       
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif

  for (j = 0; j < numZones; j++) {
    EOS_REAL *FValues_flat = !useGpuData?&FValues[0][0]:&FValues[0];

    /*
      xyBounds[j] = _eos_CombineExtrapErrors (xyBounds[j], xyBounds2[j]);
      if (xyBounds[j] == EOS_UNDEFINED) {
      searchFVals[j] = 0.0;
      continue;
      }
    */
    EOS_REAL dxg, dyg, ryv, rxv, ftg11, ftg12, ftg21, ftg22, fx1, fy1, fxy1;
    EOS_INTEGER ix = ixv[j];
    EOS_INTEGER iy = iyv[j];
    /* compute fractionals and differentials */
    dxg = XValues[ix + 1] - XValues[ix];
    rxv = (searchXVals[j] - XValues[ix]) / dxg;
    dyg = YValues[iy + 1] - YValues[iy];
    ryv = (searchYVals[j] - YValues[iy]) / dyg;

    /* gather nearest eos data table f values. */
    EOS_INTEGER i0 = iy * numXVals;
    EOS_INTEGER i1 = i0 + numXVals;
    ftg11 = FValues_flat[i0 + ix];
    ftg21 = FValues_flat[i0 + ix + 1];
    ftg12 = FValues_flat[i1 + ix];
    ftg22 = FValues_flat[i1 + ix + 1];

    /* interpolate value of function only. */
    fx1 = ftg21 - ftg11;
    fy1 = ftg12 - ftg11;
    fxy1 = ftg22 - ftg12 - fx1;
    searchFVals[j] = ftg11 + rxv * fx1 + ryv * (fy1 + rxv * fxy1);

    /* derivative wrt x, y. */
    searchDFy[j] = (fy1 + rxv * fxy1) / dyg;    /* dF/dY */
    searchDFx[j] = (fx1 + ryv * fxy1) / dxg;    /* dF/dX */
  }
#ifdef DO_OFFLOAD
  }
#endif

#ifdef DO_OFFLOAD
  if (!useGpuData) {
#endif /* DO_OFFLOAD */
    EOS_FREE (ixv);
    EOS_FREE (iyv);
    EOS_FREE (xyBounds2);
#ifdef DO_OFFLOAD
  } else {
    omp_target_free(ixv, t);
    omp_target_free(iyv, t);
  }
#endif /* DO_OFFLOAD */
}

/*********************************************************************************************
     eos_bilinearInterpolate()
     purpose   -- performs vector bi-linear (4-point)
                  search-interpolation on eos function f(x,y).
                  uses linear extrapolation.
     arguments --

     searchFVals    = output real(r8kind) array containing interpolated value of eos function f(x,y) 
     
************************************************************************************************/
void eos_bilinearInterpolate(EOS_REAL* XValues, EOS_INTEGER numXVals, EOS_REAL* YValues, 
			     EOS_INTEGER numYVals, EOS_REAL** FValues, EOS_INTEGER* ixv, 
			     EOS_INTEGER* iyv, EOS_INTEGER numZones,EOS_REAL* searchXVals, 
			     EOS_REAL* searchYVals, EOS_REAL* searchFVals)
{

  EOS_INTEGER j;
  EOS_BOOLEAN useGpuData = EOS_FALSE;


#ifdef DO_OFFLOAD
  useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;
#pragma omp target if(useGpuData) is_device_ptr(FValues, ixv, iyv, XValues, YValues, searchXVals,searchYVals,searchFVals)
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif
  for(j=0; j<numZones; j++) {
 
  EOS_REAL *FValues_flat = !useGpuData?&FValues[0][0]:&FValues[0];
  
    /*
      xyBounds[j] = _eos_CombineExtrapErrors (xyBounds[j], xyBounds2[j]);
      if (xyBounds[j] == EOS_UNDEFINED) {
      searchFVals[j] = 0.0;
      continue;
      }
    */
  
    /* gather nearest eos data table f values. */
  
    EOS_INTEGER ix = ixv[j];
    EOS_INTEGER iy = iyv[j];
    EOS_INTEGER i0 = iy * numXVals;
    EOS_INTEGER i1 = i0 + numXVals;
    EOS_REAL ftg11 = FValues_flat[i0 + ix];
    EOS_REAL ftg21 = FValues_flat[i0 + ix + 1];
    EOS_REAL ftg12 = FValues_flat[i1 + ix];
    EOS_REAL ftg22 = FValues_flat[i1 + ix + 1];

    EOS_REAL x1 = XValues[ix];
    EOS_REAL x2 = XValues[ix + 1];
    EOS_REAL y1 = YValues[iy];
    EOS_REAL y2 = YValues[iy + 1];
    
    EOS_REAL X = (searchXVals[j]-x1)/(x2-x1);
    EOS_REAL Y = (searchYVals[j]-y1)/(y2-y1);
    searchFVals[j] = (1 - Y)*(1 - X)*ftg11 + (1 - Y)*X*ftg21 + Y*(1 - X)*ftg12 + Y*X*ftg22;
  // deprecated
  /*   EOS_REAL fx1; */
  /*   fx1 = ftg21 - ftg11; */
  /*   fy1 = ftg12 - ftg11; */
  /*   fxy1 = ftg22 - ftg12 - fx1; */
  /*   searchDFy[j] = (fy1 + rxv * fxy1) / dyg;    /\* dF/dY *\/ */
  /*   searchDFx[j] = (fx1 + ryv * fxy1) / dxg;    /\* dF/dX *\/ */
  /*   printf("case %i: old answer <%g,%g>\n(", j, searchDFx[j], searchDFy[j]); */

  }
#ifdef DO_OFFLOAD
  }
#endif

}

/*************************************************************************************
      eos_BilinearDerivatives()
       purpose -- calculates the derivatives with respect to x and y of F using a first
       order algorithm which provides continuous first derivatives

       arguments --
       XValues    = input  real(r8kind) array containing data table x values.
       YValues    = input  real(r8kind) array containing data table y values.
       FValues    = input  real(r8kind) array containing data table f values.
       numXVals    = input  integer number of data table x values.
       numYVals    = input  integer number of data table y values.
       ixv    = input  array of lowest x value of cell in which the search point lies
       iyv    = input array of lowest y value of cell in which the search point lies
       numZones    = input number of points in searchXVal/searchYVal and output
       searchXVal    = input  real number, x value.
       searchYVal    = input  real number, y value.
       searchDFx, searchDFy  = output  arrays dF/dx, dF/dy

**************************************************************************************/
void eos_BilinearDerivatives(EOS_REAL *XValues, EOS_REAL *YValues, EOS_REAL **FValues, 
			     EOS_INTEGER numXVals, EOS_INTEGER numYVals,
                             EOS_INTEGER* ixv, EOS_INTEGER min_ixv, EOS_INTEGER* iyv, 
			     EOS_INTEGER min_iyv, EOS_INTEGER numZones, EOS_REAL* searchXVal, 
			     EOS_REAL* searchYVal, EOS_REAL *searchDFx, EOS_REAL *searchDFy)
{

  EOS_INTEGER j;

  EOS_BOOLEAN useGpuData = EOS_FALSE;

#ifdef DO_OFFLOAD
  useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;
#pragma omp target if(useGpuData) is_device_ptr(FValues, ixv, iyv, XValues, YValues, searchXVal,searchYVal, searchDFx, searchDFy)
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif

    for (j=0; j<numZones; j++){
      EOS_REAL x0, x1, x2, x3, y0, y1, y2, y3, X, Y, ftg11, ftg21, 
	ftg12, ftg22, ftg01, ftg02, ftg31, ftg32, ftg13, ftg23, dfx12, 
	ftg10, ftg20, dfx11, dfx21, dfx22, 
	dfy21, dfy11, dfy12, dfy22, dfxX, dfxY, dfyX, dfyY;
      EOS_BOOLEAN go_right, go_up;
      EOS_REAL *FValues_flat = !useGpuData?&FValues[0][0]:&FValues[0];

      x1 = XValues[ixv[j]];
      x2 = XValues[ixv[j] + 1];
      y1 = YValues[iyv[j]];
      y2 = YValues[iyv[j] + 1];
      
      X = (searchXVal[j]-x1)/(x2-x1);
      Y = (searchYVal[j]-y1)/(y2-y1);
      
      /* gather nearest eos data table f values. */
      ftg11 = FValues_flat[iyv[j] * numXVals + ixv[j]];
      ftg21 = FValues_flat[iyv[j] * numXVals + ixv[j] + 1];
      ftg12 = FValues_flat[(iyv[j] + 1) * numXVals + ixv[j]];
      ftg22 = FValues_flat[(iyv[j] + 1) * numXVals + ixv[j] + 1];
      
      /* Pick left or right direction based on position of search point,
	 but don't leave the grid!  This assumes that the grid is at
	 least 2 x 2. */
      
      /* Note: we may need to set an extrapolation flag here. */
      go_right = (searchXVal[j] >= (x1+x2)/2)?EOS_TRUE:EOS_FALSE;
      if((go_right && ixv[j]<numXVals-2-min_ixv) || ixv[j]==min_ixv) {
	/* use cell to right */
	/* The (ixv<numXVals-2-min_ixv) criteria assumes that there are min_ixv points to ignore (i.e., nGhostData) */
	/* First calculate the derivatives in the x direction along the top and bottom edges of the cell */
	x3 = XValues[ixv[j]+2];
	ftg31 = FValues_flat[iyv[j] * numXVals + ixv[j]+2];
	ftg32 = FValues_flat[(iyv[j]+1) * numXVals + ixv[j]+2];
	dfx12 = (ftg22 - ftg12)/FLOOR(x2 - x1);
	dfx11 = (ftg21 - ftg11)/FLOOR(x2 - x1);
	/*Now calculate the derivative along the edges of the cell to the right of the current cell. */
	dfx21 = (ftg31-ftg21)/FLOOR(x3-x2);
	dfx22 = (ftg32-ftg22)/FLOOR(x3-x2);
	dfxX = (searchXVal[j] - (x1+x2)/2)/FLOOR((x2+x3)/2 - (x1+x2)/2);
      }
      else {
	/* use cell to left */
	x0 = XValues[ixv[j]-1];
	ftg01 = FValues_flat[iyv[j] * numXVals + ixv[j]-1];
	ftg02 = FValues_flat[(iyv[j]+1) * numXVals + ixv[j]-1];
	/*Calculate the derivatives in the x direction along the top and bottom edges of the cell*/
	dfx22 = (ftg22 - ftg12)/FLOOR(x2 - x1);
	dfx21 = (ftg21 - ftg11)/FLOOR(x2 - x1);
	/*Now calculate the derivative along the edges of the cell to the left of the current cell. */
	dfx12 = (ftg12-ftg02)/FLOOR(x1-x0);
	dfx11 = (ftg11-ftg01)/FLOOR(x1-x0);
	dfxX = (searchXVal[j] - (x0+x1)/2)/FLOOR((x1+x2)/2 - (x0+x1)/2);
      }
      dfxY = Y;
      
      /* Note: we may need to set an extrapolation flag here. */
      go_up=(searchYVal[j] >= (y1+y2)/2)?EOS_TRUE:EOS_FALSE;
      if((go_up && iyv[j]<numYVals-2-min_iyv) || iyv[j]==min_iyv) {
	/* use cell above */
	/* The (iyv<numYVals-2-min_iyv) criteria assumes that there are min_iyv points to ignore (i.e., nGhostData) */
	/* First calculate the derivatives in the y direction along the left and right edges of the cell */
	//    printf("up");
	y3 = YValues[iyv[j]+2];
	ftg13 = FValues_flat[(iyv[j]+2) * numXVals + ixv[j]];
	ftg23 = FValues_flat[(iyv[j]+2) * numXVals + ixv[j]+1];
	dfy21 = (ftg22 - ftg21)/FLOOR(y2 - y1);
	dfy11 = (ftg12 - ftg11)/FLOOR(y2 - y1);
	/*Now calculate the derivative along the edges of the cell above the current cell. */
	dfy12 = (ftg13-ftg12)/FLOOR(y3-y2);
	dfy22 = (ftg23-ftg22)/FLOOR(y3-y2);
	dfyY = (searchYVal[j] - (y1+y2)/2)/FLOOR((y2+y3)/2 - (y1+y2)/2);
      }
      else {
	/* use cell below */
	y0 = YValues[iyv[j]-1];
	ftg10 = FValues_flat[(iyv[j]-1) * numXVals + ixv[j]];
	ftg20 = FValues_flat[(iyv[j]-1) * numXVals + ixv[j]+1];
	/*Calculate the derivatives in the y direction along the left and right edges of the cell*/
	dfy22 = (ftg22 - ftg21)/FLOOR(y2 - y1);
	dfy12 = (ftg12 - ftg11)/FLOOR(y2 - y1);
	/*Now calculate the derivative along the edges of the cell below the current cell. */
	dfy21 = (ftg21-ftg20)/FLOOR(y1-y0);
	dfy11 = (ftg11-ftg10)/FLOOR(y1-y0);
	dfyY = (searchYVal[j] - (y0+y1)/2)/FLOOR((y1+y2)/2 - (y0+y1)/2);
      }
      dfyX = X;
      
      /*Now do a bilinear interpolation over the new cell.*/
      /*derivative of F wrt x*/
      searchDFx[j]= (1 - dfxY)*(1 - dfxX)*dfx11 + (1 - dfxY)*dfxX*dfx21 + dfxY*(1 - dfxX)*dfx12 + dfxY*dfxX*dfx22;
      /*derivative of F wrt y*/
      searchDFy[j] = (1 - dfyY)*(1 - dfyX)*dfy11 + (1 - dfyY)*dfyX*dfy21 + dfyY*(1 - dfyX)*dfy12 + dfyY*dfyX*dfy22; 
    }
#ifdef DO_OFFLOAD
  }
#endif

}

/*************************************************************************************
 *     eos_InverseBilinearInterpolateFY()
 *
 *     purpose   --  Given arrays of f and y find values of x.
 *
 *     arguments --
 *       numZones = input number of search Y,F points
 *       searchY  = input search Y values
 *       searchF  = input search F values
 *       searchDFx = output, the partial derivatives.
 *       searchDFy = output, the partial derivatives.
 *       resultX  = output of corresponding X values of size numZones (allocated by caller)
 *       numXVals = input  integer number of data table x values.
 *       numYVals = input  integer number of data table y values.
 *       X        = in/out real array containing data table x values.
 *       Y        = in/out real array containing data table y values.
 *       F        = in/out real 2d array containing data table f values.
 *  EOS_INTEGER nGhostData  number of "ghost" data points added at each isotherm/isochore extremum
 *                          (which are used to smooth interpolation at table boundaries)
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 *       coldCurve = optional rational input cold curve that was subtracted from F
 *       Y_ht     = hashtable for searchY
 *       F_ht     = hashtable for searchF
 **************************************************************************************/
void eos_InverseBilinearInterpolateFY (EOS_INTEGER numZones,
                                       EOS_REAL *searchY, EOS_REAL *searchF,
                                       EOS_REAL *searchDFx,
                                       EOS_REAL *searchDFy, EOS_REAL *resultX,
                                       EOS_INTEGER numXVals,
                                       EOS_INTEGER numYVals, EOS_REAL *X,
                                       EOS_REAL *Y, EOS_REAL **F, EOS_REAL *coldCurve, 
                                       eos_HashTable1D *Y_ht, eos_HashTable2D *F_ht,
                                       EOS_INTEGER nGhostData,
                                       EOS_INTEGER *xyBounds,
                                       EOS_INTEGER *err)
{
  /* macros to enable debugging output in this function */
//#define _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEFY

  EOS_INTEGER i, *iy_low, *iy_high, *ix_low, *ix_high, haveCC;
  EOS_REAL F11, F22, F12, F21, X1, X2, Y1, Y2;
  EOS_REAL derivs[2];
  EOS_REAL *F_flat = &F[0][0];
#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEFY
  char *fn = "eos_InverseBilinearInterpolateFY.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  *err = EOS_OK;

  haveCC = (coldCurve == NULL) ? 0 : 1;

  iy_low = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));    /* which cell in Y direction is near Y point given (lower)*/
  iy_high = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));   /* which cell in Y direction is near Y point given (upper)*/
  ix_low = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));    /* which cell in X direction is near X point given (lower)*/
  ix_high = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));   /* which cell in X direction is near X point given (upper)*/

  eos_SearchIndices_YF (searchY, searchF, numZones, X, Y, F, numXVals,
                        numYVals, nGhostData, iy_low, ix_low, iy_high, ix_high, coldCurve,
                        Y_ht, F_ht, haveCC, xyBounds, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
    return;

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEFY
  _eos_DumpIndicesToFile (fn, mode, numZones, X, Y, F, ix_low, iy_low, NULL, NULL);
  mode = appendMode; /* reset mode for indices output file */
#endif

 
#ifndef DISABLE_OMP_SIMD_PRAGMA 
#pragma omp simd 
#endif 
  for (i = 0; i < numZones; i++) {

    F11 =
      (haveCC) ? F_flat[iy_low[i] * numXVals + ix_low[i]] +
      coldCurve[ix_low[i]] : F_flat[iy_low[i] * numXVals + ix_low[i]];
    F22 =
      (haveCC) ? F_flat[(iy_low[i]+1) * numXVals + ix_low[i]+1] +
      coldCurve[ix_low[i]+1] : F_flat[(iy_low[i]+1)  * numXVals + ix_low[i]+1];
    F21 =
      (haveCC) ? F_flat[iy_low[i] * numXVals + ix_low[i]+1] +
      coldCurve[ix_low[i]+1] : F_flat[iy_low[i] * numXVals + ix_low[i]+1];
    F12 =
      (haveCC) ? F_flat[(iy_low[i]+1) * numXVals + ix_low[i]] +
      coldCurve[ix_low[i]] : F_flat[(iy_low[i]+1) * numXVals + ix_low[i]];
    X1 = X[ix_low[i]];
    X2 = X[ix_low[i]+1];
    Y1 = Y[iy_low[i]];
    Y2 = Y[iy_low[i]+1];

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEFY
    {
      FILE *fp;
      fp = fopen (fn, mode);
      fprintf(fp,"%-5d %3d %3d %23.15e %23.15e %23.15e F11=%23.15e F12=%23.15e F21=%23.15e F22=%23.15e X1=%23.15e X2=%23.15e Y1=%23.15e Y2=%23.15e\n",
              i,ix_low[i],iy_low[i],X[ix_low[i]],Y[iy_low[i]],F[iy_low[i]][ix_low[i]],F11,F12,F21,F22,X1,X2,Y1,Y2);
      if (fp) fclose(fp);
    }
#endif

    /* exact inversion finite difference equation */
    resultX[i] =
      ((X1 - X2)*(Y1 - Y2)*searchF[i] + F12*X2*(Y1 - searchY[i]) +
       F22*X1*(-Y1 + searchY[i]) - F21*X1*(-Y2 + searchY[i]) + F11*X2*(-Y2 + searchY[i])) /
      FLOOR(F12*(Y1 - searchY[i]) + F22*(-Y1 + searchY[i]) + F11*(-Y2 + searchY[i]) - F21*(-Y2 + searchY[i]));
  }
    /* Now use resultY and searchX to determine dF/dx, dF/dy and then convert to dX/df
       and dX/dy later in  eos_RecordType1.c interpolate function under
       CATEGORY1(all partial derivatives) */
  eos_BilinearDerivatives(X, Y, F, numXVals, numYVals, ix_low, nGhostData, iy_low, nGhostData, numZones, resultX, searchY,searchDFx, searchDFy);

  EOS_FREE (ix_low);
  EOS_FREE (iy_low);
  EOS_FREE (ix_high);
  EOS_FREE (iy_high);

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEFY
  mode = appendMode; /* reset mode for indices output file */
#endif
}

/*****************************************************************************************
 *     eos_InverseBilinearInterpolateXF()
 *
 *     purpose   --  Given arrays of x and f find values of y.
 *
 *     arguments --
 *       numZones = input number of search Y,F points
 *       searchX  = input search Y values
 *       searchF  = input search F values
 *       searchDFx = output, the partial derivatives.
 *       searchDFy = output, the partial derivatives.
 *       resultY  = output of corresponding X values of size numZones (allocated by caller)
 *       numXVals = input  integer number of data table x values.
 *       numYVals = input  integer number of data table y values.
 *       X        = in/out real array containing data table x values.
 *       Y        = in/out real array containing data table y values.
 *       F        = in/out 2d real array containing data table f values.
 *       X_ht     = input hashtable for X array (can be NULL)
 *       Y_ht     = input hashtable for Y array (can be NULL)
 *  EOS_INTEGER nGhostData  number of "ghost" data points added at each isotherm/isochore extremum
 *                          (which are used to smooth interpolation at table boundaries)
 *       NOTE: coldCurve is already subtracted from searchF values.
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 ******************************************************************************************/
void eos_InverseBilinearInterpolateXF (EOS_INTEGER numZones,
                                       EOS_REAL *searchX, EOS_REAL *searchF,
                                       EOS_REAL *searchDFx,
                                       EOS_REAL *searchDFy, EOS_REAL *resultY,
                                       EOS_INTEGER numXVals,
                                       EOS_INTEGER numYVals, EOS_REAL *X,
                                       EOS_REAL *Y, EOS_REAL **F, eos_HashTable1D* X_ht,
                                       eos_HashTable2D* F_ht, EOS_INTEGER nGhostData,
                                       EOS_INTEGER *xyBounds,
                                       EOS_INTEGER *err)
{
  /* macros to enable debugging output in this function */
//#define _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEXF

  EOS_INTEGER i, *iy_low, *iy_high, *ix_low, *ix_high;
  EOS_REAL F11, F22, F12, F21, X1, X2, Y1, Y2, derivs[2];
#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEXF
  char *fn = "eos_InverseBilinearInterpolateXF.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif
  EOS_REAL *F_flat = &F[0][0];
  *err = EOS_OK;


  iy_low = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));    /* indexes of Y near which Y points are */
  iy_high = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));   /* indexes of X near which X points are */
  ix_low = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));    /* indexes of Y near which Y points are */
  ix_high = (EOS_INTEGER *) malloc (numZones * sizeof (EOS_INTEGER));   /* indexes of X near which X points are */

  eos_SearchIndices_XF (searchX, searchF, numZones, X, Y, F, numXVals,
                        numYVals, nGhostData, iy_low, ix_low, iy_high, ix_high, NULL, X_ht, F_ht, 0,
                        xyBounds, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
    return;

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEXF
  _eos_DumpIndicesToFile (fn, mode, numZones, X, Y, F, ix_low, iy_low, NULL, NULL);
  mode = appendMode; /* reset mode for indices output file */
#endif

#ifndef DISABLE_OMP_SIMD_PRAGMA 
#pragma omp simd 
#endif 
  for (i = 0; i < numZones; i++) {
    F11 = F_flat[iy_low[i] * numXVals + ix_low[i]];
    F22 = F_flat[(iy_low[i]+1) * numXVals + ix_low[i]+1];
    F21 = F_flat[iy_low[i] * numXVals + ix_low[i]+1];
    F12 = F_flat[(iy_low[i]+1) * numXVals + ix_low[i]];
    X1 = X[ix_low[i]];
    X2 = X[ix_low[i]+1];
    Y1 = Y[iy_low[i]];
    Y2 = Y[iy_low[i]+1];

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEXF
    {
      FILE *fp;
      fp = fopen (fn, mode);
      fprintf(fp,"%-5d %3d %3d %23.15e %23.15e %23.15e F11=%23.15e F12=%23.15e F21=%23.15e F22=%23.15e X1=%23.15e X2=%23.15e Y1=%23.15e Y2=%23.15e\n",
              i,ix_low[i],iy_low[i],X[ix_low[i]],Y[iy_low[i]],F[iy_low[i]][ix_low[i]],F11,F12,F21,F22,X1,X2,Y1,Y2);
      if (fp) fclose(fp);
    }
#endif

    /* exact inversion finite difference equation */
    resultY[i] =
      ((X1 - X2)*(Y1 - Y2)*searchF[i] + F12*Y1*(X2 - searchX[i]) -
       (-(F22*Y1) + F21*Y2)*(-X1 + searchX[i]) + F11*Y2*(-X2 + searchX[i])) /
      FLOOR(F12*(X2 - searchX[i]) - (F21 - F22)*(-X1 + searchX[i]) + F11*(-X2 + searchX[i]));
  }

    /* Now use resultY and searchX to determine dF/dx, dF/dy and then convert to dY/dx
       and dY/df later in eos_RecordType1.c interpolate function under
       CATEGORY2(all partial derivatives) */
  eos_BilinearDerivatives(X, Y, F, numXVals, numYVals, ix_low, nGhostData, iy_low, nGhostData, numZones,  searchX, resultY, searchDFx, searchDFy);


  EOS_FREE (ix_low);
  EOS_FREE (iy_low);
  EOS_FREE (ix_high);
  EOS_FREE (iy_high);

#ifdef _EOS_DUMP_INDEX_DATA_INVERSEBILINEARINTERPOLATEXF
  mode = appendMode; /* reset mode for indices output file */
#endif
}

/***************************************************************************
 *     eos_InverseRationalInterpolateF()
 *
 *     purpose   --  Given an array of f find values of x, the function is F(x).
 *
 *     arguments --
 *       EOS_BOOLEAN _create_ghostdata = input  boolean enables _create_ghostdata logic in eos_RationalInterpolate4
 *       EOS_BOOLEAN iterative = use an iterative algorithm if true
 *       nsrch    = input number of search F points
 *       searchF  = input search F values
 *       searchDFx= output array, the derivatives.
 *       resultX  = output of corresponding X values of size nserch (allocated by caller)
 *       nxtbl    = input  integer number of data table x values.
 *       xtbls    = in/out real array containing data table x values.
 *       ftbls    = in/out real array containing data table f values.
 *       ftbls_ht = input hashtable for ftbls (can be NULL)
 *       EOS_INTEGER *xyBounds     out, interpolation errors per xy-pair
 *       err      = output error code
 ****************************************************************************/
void eos_InverseRationalInterpolateF (EOS_BOOLEAN _create_ghostdata,
                                      EOS_INTEGER nsrch, EOS_REAL *searchF,
                                      EOS_REAL *searchDFx, EOS_REAL *resultX,
                                      EOS_INTEGER nxtbl, EOS_REAL *xtbls,
                                      EOS_REAL *ftbls, eos_HashTable1D* ftbls_ht, EOS_INTEGER *xyBounds,
                                      EOS_INTEGER *err)
{
  EOS_INTEGER err1, i, jv, *ix_low, inc;
  EOS_REAL *RIPointsX, *RIPointsF, F, ferr=0.0, upperbound, lowerbound, delta, deltaold;

  *err = EOS_OK;
  ix_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));

  // search F to locate indexes of X
  _eos_srchdf (nsrch, searchF, 1, nxtbl, ftbls, 1, ix_low, ftbls_ht, xyBounds, &err1);
  /* ignore error, it might be for one point only */

  /* take 4 x-points i and their corresponding F(x) for each search F value. 
     These points will stay constant with each iteration, 
     becacause X is limited by only 1 cell of X-grid (x_low, x_high) */

  RIPointsF = (EOS_REAL *) malloc (4*nsrch * sizeof (EOS_REAL));
  RIPointsX = (EOS_REAL *) malloc (4*nsrch * sizeof (EOS_REAL));

  for (jv = 0; jv < nsrch; jv++) {

    /* take 4 points starting at x_low-1, or x_low if x_low is 0 */
    inc = ix_low[jv] - 1;
    while (inc > nxtbl - 4)
      inc--;
    if (inc < 0)
      inc = 0;

    for (i = 0; i < 4; i++) {

      RIPointsX[jv*4+i] = xtbls[inc+i];
      RIPointsF[jv*4+i] = ftbls[inc+i];
    }
  }

    /* Calculate X and dX/dF */
  eos_RationalInterpolate4 (_create_ghostdata, &(searchF[0]), &(RIPointsF[0]), &(RIPointsX[0]), &(resultX[0]), &(searchDFx[0]), nsrch); 

  for (jv = 0; jv < nsrch; jv++) {
    searchDFx[jv] = 1.0 / FLOOR(searchDFx[jv]); /* convert dF/dx to dx/dF */
  }                             /* jv loop */

  if ((ferr > _eos_machinePrecisionData.maxErr) && (i >= _eos_machinePrecisionData.maxIter))
    *err = EOS_CONVERGENCE_FAILED;

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
    *err = err1;

  EOS_FREE (ix_low);
  EOS_FREE (RIPointsX);
  EOS_FREE (RIPointsF);
}

/***************************************************************************
 *     eos_InverseInterpolateF()
 *
 *     purpose   --  Given an array of f find values of x, the function is F(x)
 *                   using linear interpolation/extrapolation.
 *
 *     arguments --
 *       nsrch    = input number of search F points
 *       searchF  = input search F values
 *       searchDFx= output array, the derivatives.
 *       resultX  = output of corresponding X values of size nserch (allocated by caller)
 *       nxtbl    = input  integer number of data table x values.
 *       xtbls    = in/out real array containing data table x values.
 *       ftbls    = in/out real array containing data table f values.
 *                = out xyBounds     interpolation errors per xy-pair
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 ****************************************************************************/
void eos_InverseInterpolateF (EOS_INTEGER nsrch, EOS_REAL *searchF,
                              EOS_REAL *searchDFx, EOS_REAL *resultX,
                              EOS_INTEGER nxtbl, EOS_REAL *X, EOS_REAL *F,
                              eos_HashTable1D* F_ht,
                              EOS_INTEGER *xyBounds, EOS_INTEGER *err)
{
  EOS_INTEGER i, *ix_low;
  ix_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));


  *err = EOS_OK;
  // search F to locate indexes of X
  _eos_srchdf (nsrch, searchF, 1, nxtbl, F, 1, ix_low, F_ht, xyBounds, err);
  /* ignore error, it might be for only one point */

  for (i = 0; i < nsrch; i++) {
    if (xyBounds[i] == EOS_UNDEFINED) {
      resultX[i] = 0.0;
      continue;
    }
    searchDFx[i] =
      (F[ix_low[i] + 1] - F[ix_low[i]]) / (X[ix_low[i] + 1] - X[ix_low[i]]);
    resultX[i] =
      X[ix_low[i]] + (X[ix_low[i] + 1] - X[ix_low[i]]) * (searchF[i] -
                                                          F[ix_low[i] +
                                                            1]) /
      ((F[ix_low[i] + 1] - F[ix_low[i]]));
  }                             /* nsrch loop */

  EOS_FREE (ix_low);
}

/*! **************************************************************************************
 *     eos_RationalInterpolateXY()
 *
 *     purpose   --  Given arrays of x and y find values of f.
 *
 *     arguments --
 *       nsrch    = input number of search X,Y points
 *       searchX  = input search X values of size nsrch (allocated by caller)
 *       searchY  = input search Y values of size nsrch (allocated by caller)
 *       searchDFx, searchDFy = output, the partial derivatives of size nsrch (allocated by caller)
 *       resultF  = output of corresponding F values of size nsrch (allocated by caller)
 *       nxtbl = input  integer number of data table x values.
 *       nytbl = input  integer number of data table y values.
 *       xtbls = input  real array containing data table x values.
 *       ytbls = input  real array containing data table y values.
 *       ftbls = input  real array containing data table f values.
 *       ytbls_ht = input  hashtable for ytbls (can be NULL)
 *       ftbls_ht = input  hashtable for ftbls (can be NULL)
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 ******************************************************************************************/
#define ENABLE_EOS_RATIONALINTERPOLATEXY
#ifdef ENABLE_EOS_RATIONALINTERPOLATEXY

void eos_RationalInterpolateXY (EOS_INTEGER nsrch, EOS_REAL *searchX,
                                EOS_REAL *searchY, EOS_REAL *searchDFx,
                                EOS_REAL *searchDFy, EOS_REAL *resultF,
                                EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                                EOS_REAL *xtbls, EOS_REAL *ytbls,
                                EOS_REAL **ftbls, EOS_REAL *coldCurve, EOS_INTEGER nGhostData,
                                eos_HashTable1D* xtbls_ht, eos_HashTable1D* ytbls_ht,
                                EOS_INTEGER *xyBounds, EOS_INTEGER *err){

/* macros to enable debugging output in this function;
     used to make algorithm logic more easily read */
//#define _EOS_DUMP_INDEX_DATA_RATIONALINTERPOLATEXY

  EOS_INTEGER i, *ix_low, *iy_low, returnErr=EOS_OK;
#ifdef _EOS_DUMP_INDEX_DATA_RATIONALINTERPOLATEXY
  char *fn = "eos_RationalInterpolateXY.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  EOS_BOOLEAN useGpuData=EOS_FALSE;
#ifdef DO_OFFLOAD
  useGpuData = _EOS_GET_USEGPUDATA_EOSDATAMAP;
#endif /* DO_OFFLOAD */
  *err = EOS_OK;

#ifdef DO_OFFLOAD
  int t_, h_;

  t_ = omp_get_default_device();
  h_ = omp_get_initial_device();
#endif  

#ifdef DO_OFFLOAD
  if (!useGpuData)
#endif /* DO_OFFLOAD */
  {
    iy_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
    ix_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  }
#ifdef DO_OFFLOAD
  else
  {
    ix_low=omp_target_alloc(nsrch*sizeof(EOS_INTEGER),t_);
    iy_low=omp_target_alloc(nsrch*sizeof(EOS_INTEGER),t_);
  }
#endif /* DO_OFFLOAD */

  /* search table to locate indexes of X, Y */
  _eos_srchdf (nsrch, searchY, 1, nytbl, ytbls, 1, iy_low, ytbls_ht, xyBounds, err);
  _eos_srchdf (nsrch, searchX, 1, nxtbl, xtbls, 1, ix_low, xtbls_ht, xyBounds, err);

  /* ensure proper initial placement of birational stencil at table edges */

#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(ix_low, iy_low)
  {
#pragma omp teams distribute parallel for
#endif /* DO_OFFLOAD */
#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif

    for (i=0; i<nsrch; i++) {
      ix_low[i] = MAX(MIN(ix_low[i], nxtbl-3), 1);
      iy_low[i] = MAX(MIN(iy_low[i], nytbl-3), 1);
    }
#ifdef DO_OFFLOAD
  }
#endif

  i = 1; /* initialize iteration counter */

  eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low, searchX, searchY, resultF, searchDFx, searchDFy, xyBounds, err);

  /* set extrapolation error codes appropriately */
  if (!useGpuData && !skipExtrap) 
    _eos_CheckExtrapCategory0(nGhostData, nxtbl, nytbl, xtbls, ytbls,
			      nsrch, searchX, searchY, xyBounds, &returnErr);

#ifdef _EOS_DUMP_INDEX_DATA_RATIONALINTERPOLATEXY
  _eos_DumpIndicesToFile (fn, mode, nsrch, xtbls, ytbls, ftbls,
                          ix_low, iy_low, NULL, NULL);
#endif

#ifdef DUMP_WARNINGS
  printf("END OF WARNINGS\n");
#endif

  if (! *err &&  returnErr)
    *err = returnErr;

#ifdef DO_OFFLOAD
  if (!useGpuData)
#endif /* DO_OFFLOAD */
  {
    EOS_FREE (iy_low);
    EOS_FREE (ix_low);
  }
#ifdef DO_OFFLOAD
  else
  {
    omp_target_free(ix_low,t_);
    omp_target_free(iy_low,t_);
  }
#endif /* DO_OFFLOAD */

#ifdef _EOS_DUMP_INDEX_DATA_RATIONALINTERPOLATEXY
  mode = appendMode; /* reset mode for indices output file */
#endif
}
#endif /* ENABLE_EOS_RATIONALINTERPOLATEXY */

/*************************************************************************************
 *     eos_InverseRationalInterpolateFY()
 *
 *     purpose   --  Given arrays of f and y find values of x.
 *
 *     arguments --
 *       nsrch    = input number of search Y,F points
 *       searchY  = input search Y values
 *       searchF  = input search F values
 *       searchDFx, searchDFy = output, the partical derivatives.
 *       resultX  = output of corresponding X values of size nsrch (allocated by caller)
 *       nxtbl    = input  integer number of data table x values.
 *       nytbl    = input  integer number of data table y values.
 *       xtbls    = in/out real array containing data table x values.
 *       ytbls    = in/out real array containing data table y values.
 *       ftbls    = in/out real array containing data table f values.
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 *       coldCurve = optional rational input cold curve that was subtracted from F
 **************************************************************************************/
void eos_InverseRationalInterpolateFY (EOS_INTEGER nsrch, EOS_REAL *searchY,
                                       EOS_REAL *searchF, EOS_REAL *searchDFx,
                                       EOS_REAL *searchDFy, EOS_REAL *resultX,
                                       EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                                       EOS_REAL *xtbls, EOS_REAL *ytbls,
                                       EOS_REAL **ftbls, EOS_REAL *coldCurve,
                                       EOS_INTEGER nGhostData, eos_HashTable1D *xtbls_ht,
                                       eos_HashTable1D *ytbls_ht, eos_HashTable2D *ftbls_ht,
                                       EOS_INTEGER *xyBounds, EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  /* macros to enable debugging output in this function;
     used to make algorithm logic more easily read */
//#define _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEFY
//#define DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
# define _EOS_PRINT_ITERATION_MARK(macro_s1,macro_s2,macro_i,macro_s3)  \
  printf("%s %s iteration %d%s\n",macro_s1,macro_s2,macro_i,macro_s3);
# define _EOS_PRINT_ITERATION_DETAIL                                    \
  if (ferr_xy[j] > _eos_machinePrecisionData.maxErr)                    \
    printf("%s: i=%d j=%d lower=%21.15e upper=%21.15e delta=%21.15e ferr=%21.15e resultY=%21.15e searchDFy=%21.15e fvals=%21.15e searchX=%21.15e searchF=%21.15e\n", \
           tmp_str, i, j, lowerbound[j], upperbound[j], delta[j], ferr_xy[j], resultY[j], searchDFy[j], fvals[j], searchX[j], searchF[j]);
# define _EOS_PRINT_JLOOP_DATA                                          \
  printf("%2d. ", j);                                                   \
  k0 = MIN(iy_low[j], iy_high[j]);                                      \
  kn = MAX(iy_low[j]+1, iy_high[j]+1);                                  \
  for (k=k0; k<=kn; k++)                                                \
    printf("y[%d]=%12.5e ",k,ytbls[k]);                                 \
  printf("\n");                                                         \
  printf("    F[%d][%d]=%12.5e F[%d][%d]=%12.5e\n    F[%d][%d]=%12.5e F[%d][%d]=%12.5e : X=%12.5e F=%12.5e Y=%12.5e fvals=%12.5e delta=%12.5e L=%12.5e U=%12.5e err=%12.5e\n", \
         iy_low[j]   ,ix_low[j] ,ftbls[iy_low[j]   ][ix_low[j]],        \
         iy_low[j]+1 ,ix_low[j] ,ftbls[iy_low[j]+1 ][ix_low[j]],        \
         iy_high[j]  ,ix_high[j],ftbls[iy_high[j]  ][ix_high[j]],       \
         iy_high[j]+1,ix_high[j],ftbls[iy_high[j]+1][ix_high[j]],       \
         searchX[j],searchF[j],resultY[j],fvals[j],delta[j],            \
         lowerbound[j],upperbound[j],ferr_xy[j]                         \
         );
#else
# define _EOS_PRINT_ITERATION_MARK(macro_s1,macro_s2,macro_i,macro_s3)
# define _EOS_PRINT_ITERATION_DETAIL
# define _EOS_PRINT_JLOOP_DATA
#endif

  EOS_INTEGER i, j, *ix_low, *ix_high, *iy_low, *iy_high,
    *ix_low1, *ix_low2, returnErr=EOS_OK;
  EOS_REAL *fvals, *fvals1, *fvals2, ferr, *ferr_xy, *deltaold, *delta,
    *lowerbound, *upperbound, *mid_x;
  EOS_BOOLEAN foundRoot, *found_zero;
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
  EOS_CHAR tmp_str[50];
#endif

  EOS_REAL *ftbls_flat = &ftbls[0][0];

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEFY
  char *fn = "eos_InverseRationalInterpolateFY.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  *err = EOS_OK;

  iy_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_low1 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_low2 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  iy_high = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_high = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  found_zero = (EOS_BOOLEAN *) malloc (nsrch * sizeof (EOS_BOOLEAN));
  fvals = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals1 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals2 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  ferr_xy = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  delta = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  deltaold = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  lowerbound = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  upperbound = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  mid_x = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));

  /* search table to locate indexes of X, Y */
  eos_SearchIndices_YF (searchY, searchF, nsrch, xtbls, ytbls, ftbls, nxtbl,
                        nytbl, nGhostData, iy_low, ix_low, iy_high, ix_high,
                        coldCurve, ytbls_ht, ftbls_ht, 1, xyBounds, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_INTERP_EXTRAPOLATED ||
      eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
    returnErr = EOS_INTERP_EXTRAPOLATED;

  foundRoot = EOS_FALSE; /* set flag */
  for (i=0; i<nsrch; i++) {
    iy_low[i] = MAX(MIN(iy_low[i], nytbl-1-nGhostData*2), 1);
    ix_low[i] = MAX(MIN(ix_low[i], nxtbl-1-nGhostData*2), 1);
  }

  i = 1; /* initialize iteration counter */

  while (! foundRoot) { /* begin root finding loop */

    /* use a binary search to ensure that all searchF values are
       bounded for corresponding searchY values */

    /* reset binary search values */
    for (j=0; j<nsrch; j++) {
      if (i==1) { /* first time through loop, initialize indices to extreme bounds */
        ix_low[j] = 0;
        ix_low2[j] = nxtbl-3;
      }
      ix_low1[j] = (ix_low2[j] + ix_low[j])/2; /* what is the mid point ? */
      lowerbound[j] = xtbls[ix_low[j]];
      mid_x[j]      = xtbls[ix_low1[j]];
      upperbound[j] = xtbls[ix_low2[j]];
    } /* end for (j=0; j<nsrch; j++) */

    /*interpolate at upper, lower bounds and midpoint for whole array*/
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low1, iy_low,
                               mid_x, searchY, fvals1, searchDFx, searchDFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low,
                               lowerbound, searchY, fvals, searchDFx, searchDFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low2, iy_low,
                               upperbound, searchY, fvals2, searchDFx, searchDFy, xyBounds, err);

    for (j=0; j<nsrch; j++) {
      found_zero[j] = EOS_TRUE; /* reset flag */
      if (searchF[j] < fvals1[j]) /* change upperbound index */
        ix_low2[j] = MAX(AVERAGE (EOS_INTEGER, ix_low[j], ix_low2[j]), 1);
      else /* change lowerbound index */
        ix_low[j]  = MAX(AVERAGE (EOS_INTEGER, ix_low[j], ix_low2[j]), 1);

      ix_low1[j] = MAX(AVERAGE (EOS_INTEGER, ix_low[j], ix_low2[j]), 1); /* change middle index */

      if ((searchF[j] < fvals[j]) ||
          (searchF[j] > fvals2[j]) ||
          (ix_low2[j] - ix_low[j] > 1))
        found_zero[j] = foundRoot = EOS_FALSE; /* at least one root is not yet adequately bounded */

      if (ix_low2[j] - ix_low[j] <= 1)
        found_zero[j] = EOS_TRUE; /* override since extrapolation will occur */

      lowerbound[j] = xtbls[ix_low[j]];
      mid_x[j] = xtbls[ix_low1[j]];
      upperbound[j] = xtbls[ix_low2[j]];

    } /* end for (j=0; j<nsrch; j++) */

    foundRoot = EOS_TRUE;
    for (j=0; j<nsrch; j++)
      foundRoot = (foundRoot && found_zero[j])?EOS_TRUE:EOS_FALSE;

    i++; /* increment iteration counter */

  } /* end of root finding loop */

  /* Determine extrapolation error codes and assign them to the xyBounds array */
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;
  if (!skipExtrap){
    _eos_CheckExtrapCategory1(EOS_FALSE, nGhostData, iy_low, EOS_TRUE,
			      nxtbl, nytbl, xtbls, ytbls, ftbls,
			      nsrch, searchF, searchY, xtbls_ht, ytbls_ht, xyBounds, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_INTERP_EXTRAPOLATED ||
	eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
      returnErr = EOS_INTERP_EXTRAPOLATED;
  }

  /*if searchF > upperboundF, then we extend the search interval*/
  /*AMH: Should this just be an if (combine with above while; put if outside)?*/
  i = 1;
  do {

    foundRoot = EOS_TRUE;
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low2, iy_low,
                               upperbound, searchY, fvals2, searchDFx, searchDFy, xyBounds, err);
    for (j=0; j<nsrch; j++) {
      if (searchF[j] > fvals2[j]) { /* change upperbound value */
        upperbound[j] *= 2.0;
        foundRoot = EOS_FALSE; /* at least one root is not yet adequately bounded */
      }
    } /* end for (j=0; j<nsrch; j++) */

    i++; /* increment iteration counter */

  } while (! foundRoot && i <= 100);

  /* initial estimates of resultX[] values are averages of search interval's
     lower and upper bounds */
  for (j=0; j<nsrch; j++) {
    resultX[j] = (EOS_REAL) 0.5 * (upperbound[j] + lowerbound[j]);
    deltaold[j] = upperbound[j] - lowerbound[j];
    delta[j] = deltaold[j];
  }

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEFY
  _eos_DumpIndicesToFile (fn, mode, nsrch, xtbls, ytbls, ftbls,
                          ix_low, iy_low, lowerbound, upperbound);
#endif

#ifdef DUMP_WARNINGS
  printf("END OF WARNINGS\n");
#endif

  i = 1; /* initialize iteration counter */

  do { /* DO loop to perform Newton-Bisection  hybrid algorithm to
          solve for the inverted resultX[] values */

    /* calculate new fvals[] values based upon current resultX[] estimates */
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low,
                               resultX, searchY, fvals, searchDFx, searchDFy, xyBounds, err);

    for (j=0; j<nsrch; j++) {
      /* check for div-by-zero while using tabulated data */
      if (((EOS_REAL) 0) == (ftbls_flat[iy_low[j] * nxtbl +ix_low[j]+1] - ftbls_flat[iy_low[j] * nxtbl + ix_low[j]])) {
        *err = EOS_INTERP_EXTRAPOLATED;
        xyBounds[j] = EOS_CANT_INVERT_DATA;
        continue;
      }

      ferr_xy[j] = FABS((searchF[j] - fvals[j]) /
                        (ftbls_flat[iy_low[j] * nxtbl + ix_low[j]+1] - ftbls_flat[iy_low[j] * nxtbl + ix_low[j]]));
    }

    _EOS_PRINT_ITERATION_MARK("begin","Newton-Bisection",i,"");

    ferr = 0.0; /* initialize max error value */

    /* calculate f values error and new estimates for inverted resultX[] values */
    for (j=0; j<nsrch; j++) {

      deltaold[j] = delta[j];

      _EOS_PRINT_JLOOP_DATA;

      /* Narrow the interval */
      if ((fvals[j] - searchF[j]) < 0.0)
        lowerbound[j] = resultX[j];
      else
        upperbound[j] = resultX[j];

      if ((FABS (2.0 * (fvals[j] - searchF[j])) >
           FABS (deltaold[j] * searchDFx[j])) ||
          (((resultX[j] - upperbound[j]) * searchDFx[j] - (fvals[j] - searchF[j])) *
           ((resultX[j] - lowerbound[j]) * searchDFx[j] - (fvals[j] - searchF[j])) > 0.0)) {
        /* Perform Bisection -- Newton step is either out of range or not decreasing fast enough */
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
        sprintf(tmp_str,"bisect");
#endif
        delta[j] = (EOS_REAL) 0.5 *(upperbound[j] - lowerbound[j]);
        resultX[j] = lowerbound[j] + delta[j];
        ferr_xy[j] = FABS(delta[j] /
                          (xtbls[ix_low[j]+1] - xtbls[ix_low[j]]));
      }
      else {
        /* Perform Newton step */
        EOS_REAL d = ftbls_flat[iy_low[j] * nxtbl + ix_low[j]+1] - ftbls_flat[iy_low[j] * nxtbl + ix_low[j]];
        if (d == 0.0)
        {
          d = 1.0;
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
          printf("%s(%d):d = ftbls_flat[%d*%d+%d] - ftbls_flat[%d*%d+%d] = %g - %g = %g\n",
                 __FILE__, __LINE__,
                 iy_low[j]+1, nxtbl, ix_low[j], iy_low[j], nxtbl, ix_low[j],
                 ftbls_flat[(iy_low[j]+1)*nxtbl+ix_low[j]], ftbls_flat[iy_low[j]*nxtbl+ix_low[j]], d);
          assert(d != 0.0);
#endif
        }
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
        sprintf(tmp_str,"newton");
#endif
        ferr_xy[j] = FABS((searchF[j] - fvals[j]) / d);
        d = searchDFx[j];
        delta[j] = (fvals[j] - searchF[j]) / d;   /* estimate */
        resultX[j] = resultX[j] - delta[j];
      }

      _EOS_PRINT_ITERATION_DETAIL;

      /* determine current max estimate error */
      ferr = MAX (ferr, ferr_xy[j]);

    }

#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEFY
    sprintf(tmp_str," (max err=%12.5e)\n",ferr);
#endif
    _EOS_PRINT_ITERATION_MARK("end","Newton-Bisection",i,tmp_str);

    i++; /* increment iteration counter */

  } /* end of Newton-Bisection DO loop */
  while ((ferr > _eos_machinePrecisionData.maxErr) &&
         (i < _eos_machinePrecisionData.maxIter));

  if ((ferr > _eos_machinePrecisionData.maxErr) &&
      (i >= _eos_machinePrecisionData.maxIter)) {
    if (eos_GetStandardErrorCodeFromCustomErrorCode(returnErr) == EOS_INTERP_EXTRAPOLATED) {
      *err = EOS_INTERP_EXTRAPOLATED;
      /* adjust error code to account for extrapolation and failed convergence */
      for (j=0; j<nsrch; j++) {
        if (ferr_xy[j] > _eos_machinePrecisionData.maxErr && found_zero[j]) {
          returnErr = xyBounds[j] = EOS_CONVERGENCE_FAILED;
        }
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(returnErr) == EOS_CONVERGENCE_FAILED) {
        /* send user a modified error message */
        eos_SetCustomMsg_str (errMsg,
                              "EOS_INTERP_EXTRAPOLATED: Interpolation caused extrapolation beyond data table boundaries (Newton-Bisection method exceeded %i iterations given convergence criterion %e (rel.err=%g))",
                              _eos_machinePrecisionData.maxIter,
                              _eos_machinePrecisionData.maxErr, ferr);
      }
    }
    else {
      *err = EOS_CONVERGENCE_FAILED;
      /* send user a warning message */
      eos_SetCustomMsg_str (errMsg,
                            "EOSPAC WARNING: Newton-Bisection method exceeded %i iterations given convergence criterion %e (rel.err=%g)",
                            _eos_machinePrecisionData.maxIter,
                            _eos_machinePrecisionData.maxErr, ferr);
    }
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
    *err = returnErr;

  EOS_FREE (iy_low);
  EOS_FREE (ix_low1);
  EOS_FREE (ix_low2);
  EOS_FREE (iy_high);
  EOS_FREE (ix_low);
  EOS_FREE (ix_high);
  EOS_FREE (found_zero);
  EOS_FREE (fvals);
  EOS_FREE (fvals1);
  EOS_FREE (fvals2);
  EOS_FREE (ferr_xy);
  EOS_FREE (delta);
  EOS_FREE (deltaold);
  EOS_FREE (lowerbound);
  EOS_FREE (upperbound);
  EOS_FREE (mid_x);

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEFY
  mode = appendMode; /* reset mode for indices output file */
#endif
}

/*****************************************************************************************
 *     eos_InverseRationalInterpolateXF()
 *
 *     purpose   --  Given arrays of x and f find values of y.
 *
 *     arguments --
 *       nsrch    = input number of search X,F points
 *       searchX  = input search X values
 *       searchF  = input search F values
 *       searchDFx, searchDFy = output, the partial derivatives.
 *       resultY  = output of corresponding Y values of size nsrch (allocated by caller)
 *       nxtbl_in = input  integer number of data table x values.
 *       nytbl_in = input  integer number of data table y values.
 *       xtbls_in = input  real array containing data table x values.
 *       ytbls_in = input  real array containing data table y values.
 *       ftbls_in = input  real array containing data table f values.
 *       NOTE: coldCurve is already subtracted from searchF values -- unless
 *             the ENABLE_COLD_CURVE_REMOVAL macro is defined in
 *             eos_LoadRecordType1.
 *       X_ht = input  hashtable for xtbls (can be NULL)
 *       Y_ht = input  hashtable for ytbls (can be NULL)
 *       F_ht = input  hashtable for ftbls (can be NULL)
 *       EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *       err      = output error code
 ******************************************************************************************/
void eos_InverseRationalInterpolateXF (EOS_INTEGER nsrch, EOS_REAL *searchX,
                                       EOS_REAL *searchF, EOS_REAL *searchDFx,
                                       EOS_REAL *searchDFy, EOS_REAL *resultY,
                                       EOS_INTEGER nxtbl, EOS_INTEGER nytbl,
                                       EOS_REAL *xtbls, EOS_REAL *ytbls,
                                       EOS_REAL **ftbls, eos_HashTable1D* X_ht, eos_HashTable1D* Y_ht, eos_HashTable2D* F_ht,
                                       EOS_INTEGER nGhostData, EOS_INTEGER *xyBounds, EOS_INTEGER dataType,
                                       EOS_BOOLEAN invertAtSetup,
                                       EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  /* macros to enable debugging output in this function;
     used to make algorithm logic more easily read */
//#define _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEXF
//#define DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
# define _EOS_PRINT_ITERATION_MARK(macro_s1,macro_s2,macro_i,macro_s3)  \
  printf("%s %s iteration %d%s\n",macro_s1,macro_s2,macro_i,macro_s3);
# define _EOS_PRINT_ITERATION_DETAIL                                    \
  if (ferr_xy[j] > _eos_machinePrecisionData.maxErr)                    \
    printf("%s: i=%d j=%d lower=%21.15e upper=%21.15e delta=%21.15e ferr=%21.15e resultY=%21.15e searchDFy=%21.15e fvals=%21.15e searchX=%21.15e searchF=%21.15e\n", \
           tmp_str, i, j, lowerbound[j], upperbound[j], delta[j], ferr_xy[j], resultY[j], searchDFy[j], fvals[j], searchX[j], searchF[j]);
# define _EOS_PRINT_JLOOP_DATA                                          \
  printf("%2d. ", j);                                                   \
  k0 = MIN(iy_low[j], iy_high[j]);                                      \
  kn = MAX(iy_low[j]+1, iy_high[j]+1);                                  \
  for (k=k0; k<=kn; k++)                                                \
    printf("y[%d]=%12.5e ",k,ytbls[k]);                                 \
  printf("\n");                                                         \
  printf("    F[%d][%d]=%12.5e F[%d][%d]=%12.5e\n    F[%d][%d]=%12.5e F[%d][%d]=%12.5e : X=%12.5e F=%12.5e Y=%12.5e fvals=%12.5e delta=%12.5e L=%12.5e U=%12.5e err=%12.5e\n", \
         iy_low[j]   ,ix_low[j] ,ftbls[iy_low[j]   ][ix_low[j]],        \
         iy_low[j]+1 ,ix_low[j] ,ftbls[iy_low[j]+1 ][ix_low[j]],        \
         iy_high[j]  ,ix_high[j],ftbls[iy_high[j]  ][ix_high[j]],       \
         iy_high[j]+1,ix_high[j],ftbls[iy_high[j]+1][ix_high[j]],       \
         searchX[j],searchF[j],resultY[j],fvals[j],delta[j],            \
         lowerbound[j],upperbound[j],ferr_xy[j]                         \
         );
#else
# define _EOS_PRINT_ITERATION_MARK(macro_s1,macro_s2,macro_i,macro_s3)
# define _EOS_PRINT_ITERATION_DETAIL
# define _EOS_PRINT_JLOOP_DATA
#endif

  EOS_INTEGER i, j, *ix_low, *ix_high, *iy_low, *iy_high,
    *iy_low1, *iy_low2, returnErr=EOS_OK;
  EOS_REAL *fvals, *fvals1, *fvals2, ferr, *ferr_xy, *deltaold, *delta,
    *lowerbound, *upperbound, *mid_y;
  EOS_BOOLEAN foundRoot, *found_zero;
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
  EOS_CHAR tmp_str[50];
#endif

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEXF
  char *fn = "eos_InverseRationalInterpolateXF.indices";
  char *appendMode = "a";
  static char *mode = "w";
#endif

  *err = EOS_OK;

  iy_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  iy_low1 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  iy_low2 = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  iy_high = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_low = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  ix_high = (EOS_INTEGER *) malloc (nsrch * sizeof (EOS_INTEGER));
  found_zero = (EOS_BOOLEAN *) malloc (nsrch * sizeof (EOS_BOOLEAN));
  fvals = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals1 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  fvals2 = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  ferr_xy = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  delta = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  deltaold = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  lowerbound = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  upperbound = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  mid_y = (EOS_REAL *) malloc (nsrch * sizeof (EOS_REAL));
  EOS_REAL *ftbls_flat = &ftbls[0][0];

  /* search table to locate indexes of X, Y */
  eos_SearchIndices_XF (searchX, searchF, nsrch, xtbls, ytbls, ftbls, nxtbl,
                        nytbl, nGhostData, iy_low, ix_low, iy_high, ix_high, NULL, X_ht, F_ht, 0,
                        xyBounds, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_INTERP_EXTRAPOLATED ||
      eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
    returnErr = EOS_INTERP_EXTRAPOLATED;

  foundRoot = EOS_FALSE; /* set flag */
  for (i=0; i<nsrch; i++) {
    ix_low[i] = MAX(MIN(ix_low[i], nxtbl-1-nGhostData*2), 1);
    iy_low[i] = MAX(MIN(iy_low[i], nytbl-1-nGhostData*2), 1);
  }

  i = 1; /* initialize iteration counter */

  while (! foundRoot) { /* begin root finding loop */

    /* use a binary search to ensure that all searchF values are
       bounded for corresponding searchX values */

    /* reset binary search values */
    for (j=0; j<nsrch; j++) {
      if (i==1) { /* first time through loop, initialize indices to extreme bounds */
        iy_low[j] = 1;
        iy_low2[j] = nytbl-1-2*nGhostData;
      }
      iy_low1[j] = (iy_low2[j] + iy_low[j])/2; /* what is the mid point ? */
      lowerbound[j] = ytbls[iy_low[j]];
      mid_y[j]      = ytbls[iy_low1[j]];
      upperbound[j] = ytbls[iy_low2[j]];
    } /* end for (j=0; j<nsrch; j++) */

    /*interpolate at upper, lower bounds and midpoint for whole array*/
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low,
                               searchX, lowerbound, fvals, searchDFx, searchDFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low1,
                               searchX, mid_y, fvals1, searchDFx, searchDFy, xyBounds, err);
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low2,
                               searchX, upperbound, fvals2, searchDFx, searchDFy, xyBounds, err);

    for (j=0; j<nsrch; j++) {
      found_zero[j] = EOS_TRUE; /* reset flag */
      if (searchF[j] < fvals1[j]) /* change upperbound index */
        iy_low2[j] = MAX(AVERAGE (EOS_INTEGER, iy_low[j], iy_low2[j]), 1);
      else /* change lowerbound index */
        iy_low[j]  = MAX(AVERAGE (EOS_INTEGER, iy_low[j], iy_low2[j]), 1);

      iy_low1[j] = MAX(AVERAGE (EOS_INTEGER, iy_low[j], iy_low2[j]), 1); /* change middle index */

      if ((searchF[j] < fvals[j]) ||
          (searchF[j] > fvals2[j]) ||
          (iy_low2[j] - iy_low[j] > 1))
        found_zero[j] = EOS_FALSE; /* at least one root is not yet adequately bounded */

      if (iy_low2[j] - iy_low[j] <= 1)
        found_zero[j] = EOS_TRUE; /* override since extrapolation will occur */

      lowerbound[j] = ytbls[iy_low[j]];
      mid_y[j]      = ytbls[iy_low1[j]];
      upperbound[j] = ytbls[iy_low2[j]];

    } /* end for (j=0; j<nsrch; j++) */

    foundRoot = EOS_TRUE;
    for (j=0; j<nsrch; j++)
      foundRoot = (foundRoot && found_zero[j])?EOS_TRUE:EOS_FALSE;

    i++; /* increment iteration counter */

  } /* end of root finding loop */

  /* Determine extrapolation error codes and assign them to the xyBounds array */
  EOS_BOOLEAN skipExtrap = _EOS_GET_SKIPEXTRAPCHECK_EOSDATAMAP;;

  if (!skipExtrap){
    _eos_CheckExtrapCategory2(EOS_FALSE, nGhostData, ix_low, EOS_TRUE,
			      nxtbl, nytbl, xtbls, ytbls, ftbls,
			      nsrch, searchX, searchF, X_ht, Y_ht, xyBounds, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_INTERP_EXTRAPOLATED ||
	eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_UNDEFINED)
      returnErr = EOS_INTERP_EXTRAPOLATED;
  }
#ifndef __DISALLOW_POSITIVE_XF_EXTRAPOLATION_IF_INVERTATSETUP__
#  define __ALLOW_POSITIVE_EXTRAPOLATION_IF_INVERTATSETUP__
#endif
#ifdef __ALLOW_POSITIVE_EXTRAPOLATION_IF_INVERTATSETUP__
  i = 1;
  do {

    /* reset upperbound if necessary to allow for extrapolation */

    foundRoot = EOS_TRUE;
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low2,
                               searchX, upperbound, fvals2, searchDFx, searchDFy, xyBounds, err);
    for (j=0; j<nsrch; j++) {
      if (searchF[j] > fvals2[j]) { /* change upperbound value */
        upperbound[j] *= 2.0;
        foundRoot = EOS_FALSE; /* at least one root is not yet adequately bounded */
      }
    } /* end for (j=0; j<nsrch; j++) */

    i++; /* increment iteration counter */

  } while (! foundRoot && i <= 100);
#endif

#ifndef __DISALLOW_NEGATIVE_XF_EXTRAPOLATION_IF_INVERTATSETUP__
#  define __ALLOW_NEGATIVE_EXTRAPOLATION_IF_INVERTATSETUP__
#endif
#ifdef __ALLOW_NEGATIVE_EXTRAPOLATION_IF_INVERTATSETUP__
  if (invertAtSetup) {
    /* reset lowerbound if necessary to allow for extrapolation */

    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low,
                               searchX, lowerbound, fvals, searchDFx, searchDFy, xyBounds, err);
    for (j=0; j<nsrch; j++) {
      if (searchF[j] < fvals[j]) { /* change lowerbound value */
        lowerbound[j] = - ytbls[nytbl-1-2*nGhostData];
      }
    } /* end for (j=0; j<nsrch; j++) */

  }
#endif

  /* initial estimates of resultY[] values are averages of search interval's
     lower and upper bounds */
  for (j=0; j<nsrch; j++) {
    resultY[j] = (EOS_REAL) 0.5 * (upperbound[j] + lowerbound[j]);
    deltaold[j] = upperbound[j] - lowerbound[j];
    delta[j] = deltaold[j];
  }

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEXF
  _eos_DumpIndicesToFile (fn, mode, nsrch, xtbls, ytbls, ftbls,
                          ix_low, iy_low, lowerbound, upperbound);
#endif

#ifdef DUMP_WARNINGS
  printf("END OF WARNINGS\n");
#endif

  i = 1; /* initialize iteration counter */

  do { /* DO loop to perform Newton-Bisection  hybrid algorithm to
          solve for the inverted resultY[] values */

    /* calculate new fvals[] values based upon current resultY[] estimates */
    eos_BiRationalInterpolate (nsrch, nxtbl, nytbl, xtbls, ytbls, ftbls, ix_low, iy_low,
                               searchX, resultY, fvals, searchDFx, searchDFy, xyBounds, err);

    for (j=0; j<nsrch; j++) {
      /* check for div-by-zero while using tabulated data */
      if (((EOS_REAL) 0) == (ftbls_flat[(iy_low[j]+1)*nxtbl+ix_low[j]] - ftbls_flat[iy_low[j]*nxtbl+ix_low[j]])) {
        if (!skipExtrap) {
        *err = EOS_INTERP_EXTRAPOLATED;
        }
        xyBounds[j] = EOS_CANT_INVERT_DATA;
        continue;
      }

      ferr_xy[j] = FABS((searchF[j] - fvals[j]) /
                        (ftbls_flat[(iy_low[j]+1)*nxtbl+ix_low[j]] - ftbls_flat[iy_low[j]*nxtbl+ix_low[j]]));
    }
    _EOS_PRINT_ITERATION_MARK("begin","Newton-Bisection",i,"");

    ferr = 0.0; /* initialize max error value */

    /* calculate f values error and new estimates for inverted resultY[] values */
    for (j=0; j<nsrch; j++) {

      deltaold[j] = delta[j];

      _EOS_PRINT_JLOOP_DATA;

      /* Narrow the interval */
      if ((fvals[j] - searchF[j]) < 0.0)
        lowerbound[j] = resultY[j];
      else
        upperbound[j] = resultY[j];

      if ((FABS (2.0 * (fvals[j] - searchF[j])) >
           FABS (deltaold[j] * searchDFy[j])) ||
          (((resultY[j] - upperbound[j]) * searchDFy[j] - (fvals[j] - searchF[j])) *
           ((resultY[j] - lowerbound[j]) * searchDFy[j] - (fvals[j] - searchF[j])) > 0.0)) {
        /* Perform Bisection -- Newton step is either out of range or not decreasing fast enough */
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
        sprintf(tmp_str,"bisect");
#endif
        delta[j] = (EOS_REAL) 0.5 *(upperbound[j] - lowerbound[j]);
        resultY[j] = lowerbound[j] + delta[j];
        ferr_xy[j] = FABS(delta[j] /
                          (ytbls[iy_low[j]+1] - ytbls[iy_low[j]]));
      }
      else {
        /* Perform Newton step */
        EOS_REAL d = ftbls_flat[(iy_low[j]+1)*nxtbl+ix_low[j]] - ftbls_flat[iy_low[j]*nxtbl+ix_low[j]];
        EOS_REAL n = searchF[j] - fvals[j];
        if (d == 0.0)
        {
          d = 1.0;
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
          printf("%s(%d):d = ftbls_flat[%d*%d+%d] - ftbls_flat[%d*%d+%d] = %g - %g = %g\n",
                 __FILE__, __LINE__,
                 iy_low[j]+1, nxtbl, ix_low[j], iy_low[j], nxtbl, ix_low[j],
                 ftbls_flat[(iy_low[j]+1)*nxtbl+ix_low[j]], ftbls_flat[iy_low[j]*nxtbl+ix_low[j]], d);
          assert(d != 0.0);
#endif
        }
#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
        sprintf(tmp_str,"newton");
#endif
        ferr_xy[j] = FABS(n / d);
        d = searchDFy[j];
        delta[j] = (fvals[j] - searchF[j]) / d;   /* estimate */
        resultY[j] = resultY[j] - delta[j];
      }

      _EOS_PRINT_ITERATION_DETAIL;

      /* determine current max estimate error */
      ferr = MAX (ferr, ferr_xy[j]);

    }

#ifdef DEBUG_EOS_INVERSERATIONALINTERPOLATEXF
    sprintf(tmp_str," (max err=%12.5e)\n",ferr);
#endif
    _EOS_PRINT_ITERATION_MARK("end","Newton-Bisection",i,tmp_str);

    i++; /* increment iteration counter */

  } /* end of Newton-Bisection DO loop */
  while ((ferr > _eos_machinePrecisionData.maxErr) &&
         (i < _eos_machinePrecisionData.maxIter));

  if ((ferr > _eos_machinePrecisionData.maxErr) &&
      (i >= _eos_machinePrecisionData.maxIter)) {
    if (eos_GetStandardErrorCodeFromCustomErrorCode(returnErr) == EOS_INTERP_EXTRAPOLATED) {
      *err = EOS_INTERP_EXTRAPOLATED;
      /* adjust error code to account for extrapolation and failed convergence */
      for (j=0; j<nsrch; j++) {
        if (ferr_xy[j] > _eos_machinePrecisionData.maxErr && found_zero[j]) {
          returnErr = xyBounds[j] = EOS_CONVERGENCE_FAILED;
        }
      }
      if (eos_GetStandardErrorCodeFromCustomErrorCode(returnErr) == EOS_CONVERGENCE_FAILED) {
        /* send user a modified error message */
        eos_SetCustomMsg_str (errMsg,
                              "EOS_INTERP_EXTRAPOLATED: Interpolation caused extrapolation beyond data table boundaries (Newton-Bisection method exceeded %i iterations given convergence criterion %e (rel.err=%g))",
                              _eos_machinePrecisionData.maxIter,
                              _eos_machinePrecisionData.maxErr, ferr);
      }
    }
    else {
      *err = EOS_CONVERGENCE_FAILED;
      /* send user a warning message */
      eos_SetCustomMsg_str (errMsg,
                            "EOSPAC WARNING: Newton-Bisection method exceeded %i iterations given convergence criterion %e (rel.err=%g)",
                            _eos_machinePrecisionData.maxIter,
                            _eos_machinePrecisionData.maxErr, ferr);
    }
  }

  if (! *err &&  returnErr)
    *err = returnErr;

  EOS_FREE (iy_low);
  EOS_FREE (iy_low1);
  EOS_FREE (iy_low2);
  EOS_FREE (iy_high);
  EOS_FREE (ix_low);
  EOS_FREE (ix_high);
  EOS_FREE (fvals);
  EOS_FREE (fvals1);
  EOS_FREE (fvals2);
  EOS_FREE (ferr_xy);
  EOS_FREE (found_zero);
  EOS_FREE (delta);
  EOS_FREE (deltaold);
  EOS_FREE (lowerbound);
  EOS_FREE (upperbound);
  EOS_FREE (mid_y);

#ifdef _EOS_DUMP_INDEX_DATA_INVERSERATIONALINTERPOLATEXF
  mode = appendMode; /* reset mode for indices output file */
#endif
}

/*************************************************************************************
     eos_BiRationalInterpolate()
     purpose   -- performs 1 2-D rational interpolations based on Cranfill and Kerley
                  algorithm for search-interpolation on eos function f(x,y).
                  uses linear extrapolation.
     arguments --
       numZones    = input  integer number of zones being processed.
       numXVals    = input  integer number of data table x values.
       numYVals    = input  integer number of data table y values.
       XValues    = input  real(r8kind) array containing data table x values.
       YValues    = input  real(r8kind) array containing data table y values.
       FValues    = input  real(r8kind) array containing data table f values.
       ixv        = input  integer indices of nearest data table x values to searchXVals
       iyv        = input  integer indices of nearest data table y values to searchYVals
       searchXVals    = input  real(r8kind) search vector containing x values.
       searchYVals    = input  real(r8kind) search vector containing y values.
       searchFVals    = output real(r8kind) array containing interpolated values of eos function f(x,y) 
       searchDFx[numZones]           derivative wrt x
       searchDFy[numZones]           derivative wrt y
	   EOS_INTEGER *xyBounds     output interpolation errors per xy-pair

c     index map of gathered eos data table f values (ftg) --
c                   /
c       (f indices) /
c                   /  (y indices)
c       /////////////     iy-1 ->  iy+0 ->  iy+1 ->  iy+2 ->
c
c         (x indices)
c            ix-1 ->                 ,3,1     ,3,2
c
c
c            ix+0 ->        ,1,3     ,1,1     ,1,2     ,1,4
c                                         *
c
c            ix+1 ->        ,2,3     ,2,1     ,2,2     ,2,4
c
c
c            ix+2 ->                 ,4,1     ,4,2
c
**********************************************************************************/
void eos_BiRationalInterpolate (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                EOS_INTEGER numYVals, EOS_REAL *XValues,
                                EOS_REAL *YValues, EOS_REAL **FValues,
                                EOS_INTEGER *ixv, EOS_INTEGER *iyv,
                                EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                                EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                                EOS_REAL *searchDFy, EOS_INTEGER *xyBounds,
                                EOS_INTEGER *err)
{

  EOS_INTEGER i;
  *err = EOS_OK;

  /* if (2-point) search-interpolation is impossible, return. */
  if (numYVals <= 4 || numXVals <=4)  
    return;

  EOS_BOOLEAN get_df = ((searchDFx==NULL||searchDFy==NULL)?EOS_FALSE:EOS_TRUE);
#ifdef DO_OFFLOAD
  EOS_BOOLEAN useGpuData=_EOS_GET_USEGPUDATA_EOSDATAMAP;
#endif /* DO_OFFLOAD */


#ifndef DISABLE_OMP_SIMD_PRAGMA
#pragma omp simd
#endif
#ifdef DO_OFFLOAD
#pragma omp target if(useGpuData) is_device_ptr(FValues, searchXVals, searchYVals, ixv, iyv, XValues, YValues, searchFVals, searchDFx, searchDFy)
  {
    #pragma omp teams distribute parallel for
#endif
  for (i = 0; i < numZones; i++) {
#ifdef DO_OFFLOAD
    EOS_REAL *FValues_flat = useGpuData ? &FValues[0] : &FValues[0][0];
#else
    EOS_REAL *FValues_flat = &FValues[0][0];
#endif /* DO_OFFLOAD */
    EOS_INTEGER ix = MIN(numXVals - 3, MAX(1, ixv[i]));
    EOS_INTEGER iy = MIN(numYVals - 3, MAX(1, iyv[i]));
    
    EOS_REAL dxg1 = XValues[ix] - XValues[ix - 1];
    EOS_REAL dxg2 = XValues[ix + 1] - XValues[ix];
    EOS_REAL dxg3 = XValues[ix + 2] - XValues[ix + 1];
    EOS_REAL rxv = (searchXVals[i] - XValues[ix]) / dxg2;

    EOS_REAL dyg1 = YValues[iy] - YValues[iy - 1];
    EOS_REAL dyg2 = YValues[iy + 1] - YValues[iy];
    EOS_REAL dyg3 = YValues[iy + 2] - YValues[iy + 1];
    EOS_REAL ryv = (searchYVals[i] - YValues[iy]) / dyg2;
    EOS_REAL ftg31 = FValues_flat[(iy + 0) * numXVals + ( ix - 1 )];
    EOS_REAL ftg32 = FValues_flat[(iy + 1) * numXVals + ( ix - 1 )];
    EOS_REAL ftg13 = FValues_flat[(iy - 1) * numXVals + ( ix + 0 )];
    EOS_REAL ftg11 = FValues_flat[(iy + 0) * numXVals + ( ix + 0 )];
    EOS_REAL ftg12 = FValues_flat[(iy + 1) * numXVals + ( ix + 0 )];
    EOS_REAL ftg14 = FValues_flat[(iy + 2) * numXVals + ( ix + 0 )];
    EOS_REAL ftg23 = FValues_flat[(iy - 1) * numXVals + ( ix + 1 )];
    EOS_REAL ftg21 = FValues_flat[(iy + 0) * numXVals + ( ix + 1 )];
    EOS_REAL ftg22 = FValues_flat[(iy + 1) * numXVals + ( ix + 1 )];
    EOS_REAL ftg24 = FValues_flat[(iy + 2) * numXVals + ( ix + 1 )];
    EOS_REAL ftg41 = FValues_flat[(iy + 0) * numXVals + ( ix + 2 )];
    EOS_REAL ftg42 = FValues_flat[(iy + 1) * numXVals + ( ix + 2 )];

    EOS_REAL rx3 = -dxg1 / dxg2;
    EOS_REAL rx4 = dxg3 / dxg2 + (EOS_REAL) 1.0;
    EOS_REAL sxv = (rx3 > rxv) ? rx3 : rxv;
    sxv = (sxv < rx4) ? sxv : rx4;
     //        sxv  =  min (rx4, max (rx3, rxv))
    EOS_REAL txv = (rxv > (EOS_REAL) 0.0) ? rxv : (EOS_REAL) 0.0;
    txv = (txv < (EOS_REAL) 1.0) ? txv : (EOS_REAL) 1.0;
    //        txv  =  min ( one, max ( zero, rxv))

    EOS_REAL ry3 = -dyg1 / dyg2;
    EOS_REAL ry4 = dyg3 / dyg2 + (EOS_REAL) 1.0;
    EOS_REAL syv = (ry3 > ryv) ? ry3 : ryv;
    syv = (syv < ry4) ? syv : ry4;
    //        syv  =  min (ry4, max (ry3, ryv))
    EOS_REAL tyv = (ryv > (EOS_REAL) 0.0) ? ryv : (EOS_REAL) 0.0;
    tyv = (tyv < (EOS_REAL) 1.0) ? tyv : (EOS_REAL) 1.0;
    //        tyv  =  min ( one, max ( zero, ryv))

    EOS_REAL fx1 = ftg21 - ftg11;
    EOS_REAL fxx1 = (fx1 - (ftg31 - ftg11) / rx3) / ((EOS_REAL) 1.0 - rx3);
    EOS_REAL gxx1 = (fx1 - (ftg41 - ftg11) / rx4) / ((EOS_REAL) 1.0 - rx4) - fxx1;
    EOS_REAL fx2 = ftg22 - ftg12;
    EOS_REAL fxx2 = (fx2 - (ftg32 - ftg12) / rx3) / ((EOS_REAL) 1.0 - rx3);
    EOS_REAL gxx2 = (fx2 - (ftg42 - ftg12) / rx4) / ((EOS_REAL) 1.0 - rx4) - fxx2;

    EOS_REAL fy1 = ftg12 - ftg11;
    EOS_REAL fyy1 = (fy1 - (ftg13 - ftg11) / ry3) / ((EOS_REAL) 1.0 - ry3);
    EOS_REAL gyy1 = (fy1 - (ftg14 - ftg11) / ry4) / ((EOS_REAL) 1.0 - ry4) - fyy1;
    EOS_REAL fy2 = ftg22 - ftg21;
    EOS_REAL fyy2 = (fy2 - (ftg23 - ftg21) / ry3) / ((EOS_REAL) 1.0 - ry3);
    EOS_REAL gyy2 = (fy2 - (ftg24 - ftg21) / ry4) / ((EOS_REAL) 1.0 - ry4) - fyy2;
    
    EOS_REAL wx1 = txv * FABS (fxx1);
    wx1 = wx1 / (wx1 + ((EOS_REAL) 1.0 - txv) * FABS (fxx1 + gxx1) + TINY_D);
    EOS_REAL wx2 = txv * FABS (fxx2);
    wx2 = wx2 / (wx2 + ((EOS_REAL) 1.0 - txv) * FABS (fxx2 + gxx2) + TINY_D);
    EOS_REAL wy1 = tyv * FABS (fyy1);
    wy1 = wy1 / (wy1 + ((EOS_REAL) 1.0 - tyv) * FABS (fyy1 + gyy1) + TINY_D);
    EOS_REAL wy2 = tyv * FABS (fyy2);
    wy2 = wy2 / (wy2 + ((EOS_REAL) 1.0 - tyv) * FABS (fyy2 + gyy2) + TINY_D);
    
    fxx1 = fxx1 + wx1 * gxx1;
    EOS_REAL gxy1 = fxx2 + wx2 * gxx2 - fxx1;
    fyy1 = fyy1 + wy1 * gyy1;
    EOS_REAL gyx1 = fyy2 + wy2 * gyy2 - fyy1;
    EOS_REAL fxy1 =
      fx2 - fx1 - ((EOS_REAL) 1.0 - sxv - sxv) * gxy1 - ((EOS_REAL) 1.0 -
                                                         syv - syv) * gyx1;
    fx1 = fx1 - ((EOS_REAL) 1.0 - sxv - sxv) * fxx1 - syv * syv * gyx1;
    fy1 = fy1 - ((EOS_REAL) 1.0 - syv - syv) * fyy1 - sxv * sxv * gxy1;

    searchFVals[i] =
      ftg11 + rxv * fx1 - sxv * sxv * fxx1 + ryv * (fy1 + rxv * fxy1) -
      syv * syv * fyy1;
    
    if (get_df)
      searchDFx[i] =
        (fx1 + ryv * (fxy1 - wx2 * ((EOS_REAL) 1.0 - wx2) * gxx2) -
         ((EOS_REAL) 1.0 - ryv) * wx1 * ((EOS_REAL) 1.0 - wx1) * gxx1) / dxg2;
    if (get_df)
      searchDFy[i] =
        (fy1 + rxv * (fxy1 - wy2 * ((EOS_REAL) 1.0 - wy2) * gyy2) -
         ((EOS_REAL) 1.0 - rxv) * wy1 * ((EOS_REAL) 1.0 - wy1) * gyy1) / dyg2;
  }
#ifdef DO_OFFLOAD
  }
#endif

}

/**************************************************************************************************
     eos_ExpandGridInterpolate()
     purpose   -- performs grid expansion inserting nAdd points in Y-direction and nAdd points
	              in X direction increasing nY x nX grid to (nY + (nY-1)*nAdd) x (nX + (nX-1)*nAdd)
     arguments --
       nAdd        = number of points to insert.
       nX          = input  integer number of data table x values.
       nY          = input  integer number of data table y values.
       X           = input  real(r8kind) array containing data table x values.
       Y           = input  real(r8kind) array containing data table y values.
       F           = input  real(r8kind) array containing data table f values.size = nX x nY
       newX        = input  real(r8kind)array of new X values size = (nX + (nX-1)*nAdd)
       newY        = input  real(r8kind)array of new Y values size = (nY + (nY-1)*nAdd)
       newF        = input  real(r8kind)array of new F values size = (nX + (nX-1)*nAdd) x (nY + (nY-1)*nAdd)
       err         = output -- pointer to int error code
****************************************************************************************************/
void eos_ExpandGridInterpolate (EOS_INTEGER nAdd, EOS_INTEGER nX,
                                EOS_INTEGER nY, EOS_REAL *X, EOS_REAL *Y,
                                EOS_REAL **F, EOS_REAL *newX, EOS_REAL *newY,
                                EOS_REAL **newF, EOS_INTEGER *err)
{
  EOS_INTEGER i, j, k, l, xyBound = EOS_OK;
  EOS_REAL *F_flat = &F[0][0];

  * err = EOS_OK;

  for (i = 0; i < nY - 1; i++) {
    newY[i * (nAdd + 1)] = Y[i];
    for (j = 0; j < nX - 1; j++) {
      newX[j * (nAdd + 1)] = X[j];
      newF[i * (nAdd + 1)][j * (nAdd + 1)]  = F_flat[i * nX + j];
      for (k = 0; k <= nAdd; k++) {
        if (k > 0)
          newY[i * (nAdd + 1) + k] =
            Y[i] + k * (Y[i + 1] - Y[i]) / (float) (nAdd + 1);
        for (l = 0; l <= nAdd; l++) {
          /* fill in the interior and the top and left edge */
          if (l > 0)
            newX[j * (nAdd + 1) + l] =
              X[j] + l * (X[j + 1] - X[j]) / (float) (nAdd + 1);

          /* the window will be adjusted inside the call if i or j are not within 1, nY-3 range */
          if (l != 0 || k != 0)
            eos_BiRationalInterpolate (1, nX, nY, X, Y, F, &j, &i,
                                       &(newX[j * (nAdd + 1) + l]),
                                       &(newY[i * (nAdd + 1) + k]),
                                       &(newF[i * (nAdd + 1) + k]
                                         [j * (nAdd + 1) + l]), NULL, NULL,
                                       &xyBound, err);
        }                       /* l - loop */
      }                         /* k - loop */
    }                           /* j - loop */
  }                             /* i - loop */

  /* assign end points */
  newY[nY + (nY - 1) * nAdd - 1] = Y[nY - 1];
  newX[nX + (nX - 1) * nAdd - 1] = X[nX - 1];
  newF[nY + (nY - 1) * nAdd - 1][nX + (nX - 1) * nAdd - 1]=
    F_flat[(nY - 1) * nX + nX - 1];    
  /* fill in the edges */
  for (i = 0; i < nY - 1; i++) {
    newF[i * (nAdd + 1)][nX + (nX - 1) * nAdd - 1] = F_flat[i * nX + nX - 1];
    j = nX - 1;
    for (k = 1; k <= nAdd; k++)
      eos_BiRationalInterpolate (1, nX, nY, X, Y, F, &j, &i,
                                 &(newX[nX + (nX - 1) * nAdd - 1]),
                                 &(newY[i * (nAdd + 1) + k]),
                                 &(newF[i * (nAdd + 1) + k]
                                   [nX + (nX - 1) * nAdd - 1]), NULL, NULL,
                                 &xyBound, err);
  }

  for (j = 0; j < nX - 1; j++) {
    newF[nY + (nY - 1) * nAdd - 1][j * (nAdd + 1)]= F_flat[i * nX + nX - 1];    
    i = nY - 1;
    for (l = 0; l <= nAdd; l++)
      eos_BiRationalInterpolate (1, nX, nY, X, Y, F, &j, &i,
                                 &(newX[j * (nAdd + 1) + l]),
                                 &(newY[nY + (nY - 1) * nAdd - 1]),
                                 &(newF[nY + (nY - 1) * nAdd - 1]
                                   [j * (nAdd + 1) + l]), NULL, NULL,
                                 &xyBound, err);
  }
}

/***********************************************************************
 *
 * Integrate F(x) from xData[i0] to x using the trapezoid rule.
 * F(x) is represented by tablulated data:
 * fData[] and xData[]
 * xData is assumed to be sorted ascending
 * Reference LA-6903-MS for rational interpolation function.
 *
 * Returned Values:
 * EOS_INTEGER eos_TrapezoidIntegrate - output error code
 * EOS_REAL    *result                - integration result
 *
 * Input Values:
 * EOS_INTEGER ilower                 - starting index of xData[] interval of interest
 * EOS_REAL    x                      - integration range upper bound
 * EOS_INTEGER nData                  - number of data in fData[] and xData[]
 * EOS_REAL    *fData                 - array of F(x) data
 * EOS_REAL    *xData                 - array of x data
 * EOS_INTEGER nInsert                - number of evenly-spaced data to create between each
 *                                      neighboring xData[] value pair
 *
 *       f[i] f(x)  f[i+1]
 *  -------+----+------+-------
 *       x[i]   x   x[i+1]
 *      
 ***********************************************************************/
EOS_INTEGER eos_TrapezoidIntegrate (EOS_INTEGER ilower, EOS_INTEGER iupper, EOS_REAL x,
                                    EOS_INTEGER nData, EOS_REAL *fData, EOS_REAL *xData,
                                    EOS_INTEGER nInsert, EOS_REAL *result,
                                    EOS_BOOLEAN keepTempArays)
{
  int i, j, index;
  EOS_INTEGER ierr = EOS_OK;
  static const EOS_REAL zero = (EOS_REAL) 0;
  static const EOS_REAL two = (EOS_REAL) 2;

  static EOS_REAL    *xavg = NULL, *favg = NULL, *df = NULL;
  static EOS_INTEGER nVals=0, *xyBound = NULL;

  // DAP -- ignore any lacking xData[0]=0 until we decide upon a reasonable method of
  //        calculating Ut(T=0) in eos_Entropy() function.
  if ( /* xData[0] > zero || */ x > xData[nData - 1]) { // function not defined from 0 to x
    *result = zero;
    ierr = EOS_INTEGRATION_FAILED;
  }
  else if (nData <= 1) {        // a function doesn't appear to be defined
    *result = zero;
    ierr = EOS_UNDEFINED;
  }
  else if (x <= zero) {        // zero integration range is zero
    *result = zero;
  }
  else {

    if (! (xavg && favg && df && xyBound)
        && nVals < ((nInsert + 1) * (iupper - ilower) + 1)) {
      /* allocate temporary arrays */
      nVals = (nInsert + 1) * (iupper - ilower) + 1;
      xavg = (EOS_REAL *) malloc (nVals * sizeof (EOS_REAL));
      favg = (EOS_REAL *) malloc (nVals * sizeof (EOS_REAL));
      df = (EOS_REAL *) malloc (nVals * sizeof (EOS_REAL));
      xyBound = (EOS_INTEGER *) malloc (nVals * sizeof (EOS_INTEGER));
    }

    /* calculate all xavg values */
    index=0;
    for (i = ilower; i < iupper; i++) {
      xavg[index] = xData[i];
      index++;
      for (j = 1; j < nInsert + 1; j++) {
        xavg[index] = xavg[index - 1] + (xData[i + 1] - xData[i]) / (EOS_REAL)(nInsert+1);
        index++;
      }
    }
    xavg[index++] = xData[i];

    if (nData <= 3) {           // use a linear function to estimate average f(x)

      /* Note that argument 1 of eos_LineInterpolate is false, because the derivatives are
         irrelevant to this integration algorithm. */
      eos_LineInterpolate (EOS_FALSE, nVals, nData, 1, 0, xData, &fData, xavg, favg,
                           df, 'y', NULL, xyBound, &ierr);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_INTERP_EXTRAPOLATED)
        ierr = EOS_OK;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
        return (ierr);

    }
    else {                      // use a rational function to estimate average f(x)

      eos_RationalInterpolate (EOS_FALSE, nVals, nData, 1, 0, xData, fData, xavg, favg,
                               df, 'y', NULL, xyBound, &ierr);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) == EOS_INTERP_EXTRAPOLATED)
        ierr = EOS_OK;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
        return (ierr);

    }

    *result = zero;

    /* Trapezoid rule */
    for (i = 0; (i < nVals - 1) && (x >= xavg[i + 1]); i++)
      *result += (xavg[i + 1] - xavg[i]) * (favg[i] + favg[i + 1]) / two;

    if (! keepTempArays) {
      /* deallocate temporary arrays */
      EOS_FREE (xavg);
      EOS_FREE (favg);
      EOS_FREE (df);
      EOS_FREE (xyBound);
      nVals = 0;
    }

  }

  return (ierr);
}

/***********************************************************************
 * function: eos_MixInterpolate()
 * The mixed material interpolation uses established EOSPAC data tables 
 * and returns interpolated data of mixed materials requested by the host 
 * code. This routine is the typical way to generate mixed material data 
 * using the data tables' member data tables. The data tables to be mixed must 
 * be of the same table type. An error code is returned if the table type is 
 * not valid for mixing (EOS_NullTable, EOS_Info, etc.).
 * The eos_Mix routine will provide interpolated values corresponding to mixtures 
 * of materials in pressure (or pressure and temperature) equilibrium. Before calling 
 * this routine the host code may need to call eos_SetOption so the desired interpolation 
 * and/or mixing options can be changed from the documented defaults.
 *
 *
 * Input Values:
 * eos_Interpolation *me        - instance of interpolation class
 * EOS_INTEGER nTables          - the total number of data tables on which to operate
 * EOS_INTEGER eosHandles       - array table Handles to mix. 
 * EOS_INTEGER nXYPairs         - total number of pairs of independent variable values provided for 
 *                                interpolation for each table.
 * EOS_REAL *concInMix          - This is an EOS_REAL array containing the concentration corresponding to each 
 *                                independent variable value pair and to each tableHandle of the desired data to 
 *                                mix. There are nTables*nXYPairs elements in concInMix, and it is stored 
 *                                sequentially in memory.
 * EOS_REAL *xVals              - array of the primary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in xVals
 * EOS_REAL *yVals              - array of the secondary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in yVals
 *
 * Returned Values:
 * EOS_REAL *fVals              - array of the interpolated data corresponding to the given independent variable 
 *                                data (x and y). There are nXYPairs elements in fVals
 * EOS_REAL *dFx                - array of the interpolated partial derivatives of fVals with respect to x. There 
 *                                re nXYPairs elements in dFx.
 * EOS_REAL *dFy                - array of the interpolated partial derivatives of fVals with respect to y. There 
 *                                re nXYPairs elements in dFy.
 * EOS_INTEGER *errorCode       - error code.
 *      
 ***********************************************************************/
void eos_MixInterpolate (eos_Interpolation *me, EOS_INTEGER nTables,
                         EOS_INTEGER *tableHandles, EOS_INTEGER nXYPairs,
                         EOS_REAL *concInMix, EOS_REAL *xVals,
                         EOS_REAL *yVals, EOS_REAL *fVals, EOS_REAL *dFx,
                         EOS_REAL *dFy, EOS_INTEGER *errorCode)
{
  EOS_INTEGER firstVar, secondVar, depVar, i, j;
  EOS_REAL allowedErr, concSum = 0.0;
  * errorCode = EOS_OK;


  /* first make sure all table Handles are of the same dataType! */
  for (i = 1; i < nTables; i++) {
    if (gEosDataMap.tableTypes[tableHandles[i - 1]] !=
        gEosDataMap.tableTypes[tableHandles[i]]) {
      *errorCode = EOS_DATA_TYPE_NO_MATCH;
      return;
    }
  }

  /* make sure the sum of the concentrations makes up 1 approximately */
  if (!_eos_machinePrecisionData.gotMachinePrecision) {
    /* determine the current machine's floating point precision */
    _eos_machinePrecisionData.gotMachinePrecision = 1;
    eos_GetMachinePrecision (&_eos_machinePrecisionData.eps,
                             &_eos_machinePrecisionData.epsneg);
    _eos_machinePrecisionData.maxIter = 100;
    _eos_machinePrecisionData.maxErr =
      pow (MAX
           (_eos_machinePrecisionData.eps, _eos_machinePrecisionData.epsneg),
           0.75);
  }

  allowedErr = pow (10.0, (EOS_INTEGER)
                    log10 (MAX
                           (_eos_machinePrecisionData.eps,
                            _eos_machinePrecisionData.epsneg) * 1000.0));

  for (j = 0; j < nXYPairs; j++) {
    concSum = 0.0;
    for (i = 0; i < nTables; i++)
      concSum += concInMix[i * nXYPairs + j];

    if (FABS (concSum - (EOS_REAL) 1.0) > allowedErr) {
      *errorCode = EOS_INVALID_CONC_SUM;
      return;
    }
  }

  /* Conditionally allocate species-specific storage */
  for (i = 0; i < nTables; i++) {
    EOS_BOOLEAN optVal;
    EOS_INTEGER err = EOS_OK;

    /* Deallocate species-specific storage if necessary */
    EOS_FREE (me->interpolationDataList[tableHandles[i]]->xSpecies);
    EOS_FREE (me->interpolationDataList[tableHandles[i]]->ySpecies);
    EOS_FREE (me->interpolationDataList[tableHandles[i]]->FSpecies);
    EOS_FREE (me->interpolationDataList[tableHandles[i]]->dFxSpecies);
    EOS_FREE (me->interpolationDataList[tableHandles[i]]->dFySpecies);

    /* Check EOS_SAVE_SPECIES_DATA option for each table handle */
    eos_GetOptionEosInterpolation (me, tableHandles[i], EOS_SAVE_SPECIES_DATA, &optVal, &err);
    if (optVal && eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_OK) {
      me->interpolationDataList[tableHandles[i]]->nXYPairs = nXYPairs;
      me->interpolationDataList[tableHandles[i]]->xSpecies = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      me->interpolationDataList[tableHandles[i]]->ySpecies = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      me->interpolationDataList[tableHandles[i]]->FSpecies = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      me->interpolationDataList[tableHandles[i]]->dFxSpecies = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      me->interpolationDataList[tableHandles[i]]->dFySpecies = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
      /* Initialize output arrays to zero if possible */
      for (j = 0; j < nXYPairs; j++) {
        if (me->interpolationDataList[tableHandles[i]]->xSpecies)
          me->interpolationDataList[tableHandles[i]]->xSpecies[j]   = (EOS_REAL) 0.0;
        if (me->interpolationDataList[tableHandles[i]]->ySpecies)
          me->interpolationDataList[tableHandles[i]]->ySpecies[j]   = (EOS_REAL) 0.0;
        if (me->interpolationDataList[tableHandles[i]]->FSpecies)
          me->interpolationDataList[tableHandles[i]]->FSpecies[j]   = (EOS_REAL) 0.0;
        if (me->interpolationDataList[tableHandles[i]]->dFxSpecies)
          me->interpolationDataList[tableHandles[i]]->dFxSpecies[j] = (EOS_REAL) 0.0;
        if (me->interpolationDataList[tableHandles[i]]->dFySpecies)
          me->interpolationDataList[tableHandles[i]]->dFySpecies[j] = (EOS_REAL) 0.0;
      }
    }
  }

  /* Initialize output arrays to zero */
  for (i = 0; i < nXYPairs; i++) {
    fVals[i] = 0.0;
    dFx[i] = 0.0;
    dFy[i] = 0.0;
  }

  firstVar =
    EOS_TYPE_TO_INDEP_VAR1 (gEosDataMap.tableTypes[tableHandles[0]]);

  if (firstVar != EOS_D) { /* constrain compatible data types */
    *errorCode = EOS_BAD_DATA_TYPE;
    return;
  }

  secondVar =
    EOS_TYPE_TO_INDEP_VAR2 (gEosDataMap.tableTypes[tableHandles[0]]);
  depVar = EOS_TYPE_TO_DEP_VAR (gEosDataMap.tableTypes[tableHandles[0]]);

  switch (secondVar) {
  case EOS_T:
    {
      /* temperature-based function. */

      /* 300 - tables */
      if (depVar != EOS_Pt && depVar != EOS_Ut &&
          depVar != EOS_Pic && depVar != EOS_Uic &&
          depVar != EOS_Pe && depVar != EOS_Ue &&
          depVar != EOS_Pc && depVar != EOS_Uc &&
          /* 500- tables */
          depVar != EOS_Kr && depVar != EOS_Keo && depVar != EOS_Zfo
          && depVar != EOS_Kp &&
          /* 600- tables */
          depVar != EOS_Zfc && depVar != EOS_Kec && depVar != EOS_Ktc
          && depVar != EOS_B && depVar != EOS_Kc) {

        *errorCode = EOS_BAD_DATA_TYPE;
        return;
      }

      eos_MixTempInterpolation (me, nTables, tableHandles, nXYPairs,
                                concInMix, xVals, yVals, fVals, dFx, dFy,
                                errorCode);
    }
    break;
  case EOS_Ut:
  case EOS_Uic:
  case EOS_Ue:
    {
      /* energy-based function. */

      /* 300 - tables */
      if (depVar != EOS_Pt && depVar != EOS_Pic &&
          depVar != EOS_Pe && depVar != EOS_T) {
        *errorCode = EOS_BAD_DATA_TYPE;
        return;
      }

      eos_MixEnergyInterpolation (me, nTables, tableHandles, nXYPairs,
                                  concInMix, xVals, yVals, fVals, dFx, dFy,
                                  errorCode);
      break;
    }
  case EOS_Pt:
  case EOS_Pic:
  case EOS_Pe:
    {
      /* pressure-based function. */

      /* 300 - tables */
      if (depVar != EOS_Ut && depVar != EOS_Uic &&
          depVar != EOS_Ue && depVar != EOS_T) {
        *errorCode = EOS_BAD_DATA_TYPE;
        return;
      }

      eos_MixPressureInterpolation (me, nTables, tableHandles, nXYPairs,
                                    concInMix, xVals, yVals, fVals, dFx, dFy,
                                    errorCode);
      break;
    }
  case EOS_NullTable:          /* cold curve */
    {
      if (gEosDataMap.tableTypes[tableHandles[0]] != EOS_Pc_D &&
          gEosDataMap.tableTypes[tableHandles[0]] != EOS_Uc_D) {
        *errorCode = EOS_BAD_DATA_TYPE;
        return;
      }

      eos_MixTempInterpolation (me, nTables, tableHandles, nXYPairs,
                                concInMix, xVals, yVals, fVals, dFx, dFy,
                                errorCode);
      break;
    }
  default:
    {
      *errorCode = EOS_BAD_DATA_TYPE;
      return;
    }
    /*
      Not implemented for now. Olga
      case EOS_At? free energy 
      case EOS_St? entropy 
    */
  }
}

#define EXP10(arg) (exp ((EOS_REAL) 2.302585092994046 * MIN (HUGE_LOG_D, arg)))

/***********************************************************************
 * function: eos_MixTempInterpolate()
 * Mixing for the function with temperature being its second independent variable.
 * Iterates search-interpolation to compute temperature-based eos function f(x,y) and
 * derivatives wrt x and y for mixture of materials in pressure-balance.
 *
 * Input Values:
 * eos_Interpolation *me        - instance of interpolation class
 * EOS_INTEGER nTables          - the total number of data tables on which to operate
 * EOS_INTEGER *tableHandles    - array table Handles to mix. 
 * EOS_INTEGER nXYPairs         - total number of pairs of independent variable values provided for 
 *                                interpolation for each table.
 * EOS_REAL *concInMix          - This is an EOS_REAL array containing the concentration corresponding to each 
 *                                independent variable value pair and to each tableHandle of the desired data to 
 *                                mix. There are nTables*nXYPairs elements in concInMix, and it is stored 
 *                                sequentially in memory.
 * EOS_REAL *xVals              - array of the primary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in xVals
 * EOS_REAL *yVals              - array of the secondary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in yVals
 *
 * Returned Values:
 * EOS_REAL *fVals              - array of the interpolated data corresponding to the given independent variable 
 *                                data (x and y). There are nXYPairs elements in fVals
 * EOS_REAL *dFx                - array of the interpolated partial derivatives of fVals with respect to x. There 
 *                                re nXYPairs elements in dFx.
 * EOS_REAL *dFy                - array of the interpolated partial derivatives of fVals with respect to y. There 
 *                                re nXYPairs elements in dFy.
 * EOS_INTEGER *errorCode       - error code.
 *      
 ***********************************************************************/
void eos_MixTempInterpolation (eos_Interpolation *me, EOS_INTEGER nTables,
                               EOS_INTEGER *tableHandles,
                               EOS_INTEGER nXYPairs, EOS_REAL *concInMix,
                               EOS_REAL *xVals, EOS_REAL *yVals,
                               EOS_REAL *fVals, EOS_REAL *dFx, EOS_REAL *dFy,
                               EOS_INTEGER *errorCode)
{
  EOS_REAL *xmixr, *ymixr, *amixr, *abarv, *vmixr, *dmixr, *vbarv, *dvdpt,
    *pdvdp, *vdvdv, *tdvdt;
  EOS_REAL *pmixr1, *pmixr2, *pmixr3, *smixr1, *smixr2, *smixr3, *sbarv1,
    *sbarv2, *sbarv3, *pbarv1, *pbarv2, *pbarv3;
  EOS_INTEGER dataType, i, j, iter, ipmax, *zeroConc, returnErr = EOS_OK;
  EOS_REAL cngas, vfact, pmref, dpmax, dvmix, dsdpt, dpmix, vdpdv, denom,
    maxVal;

  static const EOS_REAL minCMIXR = (EOS_REAL) 1.0e-06;  // originally 1.0E-04
  static const EOS_REAL maxErr = (EOS_REAL) 1.0e-04;
  static const EOS_INTEGER maxIter = 20;

  EOS_REAL aveAtomicNumber, aveAtomicWgt, refDensity, xCnvrtFactor,
    yCnvrtFactor, fCnvrtFactor;

  * errorCode = EOS_OK;

  xmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  ymixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  vmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  dmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));

  amixr = (EOS_REAL *) malloc (nTables * sizeof (EOS_REAL));
  zeroConc = (EOS_INTEGER *) malloc (nTables * sizeof (EOS_INTEGER));

  abarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vbarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dvdpt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pdvdp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vdvdv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tdvdt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  /* macro used to free locally-allocated arrays */
#define FREE_EOS_MIXTEMPINTERPOLATION_ARRAYS                            \
  EOS_FREE(xmixr); EOS_FREE(ymixr); EOS_FREE(vmixr); EOS_FREE(dmixr); EOS_FREE(pmixr1); EOS_FREE(pmixr2); EOS_FREE(pmixr3); \
  EOS_FREE(smixr1); EOS_FREE(smixr2); EOS_FREE(smixr3); EOS_FREE(amixr); EOS_FREE(abarv); EOS_FREE(vbarv); EOS_FREE(sbarv1); \
  EOS_FREE(sbarv2); EOS_FREE(sbarv3); EOS_FREE(pbarv1); EOS_FREE(pbarv2); EOS_FREE(pbarv3); EOS_FREE(zeroConc); EOS_FREE(vdvdv); \
  EOS_FREE(tdvdt); EOS_FREE(pdvdp); EOS_FREE(dvdpt);

  * errorCode = EOS_OK;

  /* get atomic masses for all regions to be mixed */
  for (i = 0; i < nTables; i++) {
    /*call getTableData(itype-1_i_kind, jmixr-1_i_kind,
      &                     ftabs, mtabs, materialID, aveAtomicNumber,
      &                     aveAtomicWgt, refDensity, logAxes,
      &                     xCnvrtFactor, yCnvrtFactor, fCnvrtFactor,
      &                     fileId, nxTable, nyTable, xAxisIndex,
      &                     yAxisIndex, tableIndex)
    */
    eos_GetLoadedBulkDataEosDataMap (&gEosDataMap, tableHandles[i],
                                     &aveAtomicNumber, &aveAtomicWgt,
                                     &refDensity, errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      FREE_EOS_MIXTEMPINTERPOLATION_ARRAYS;     /* free all locally-allocated arrays */
      return;
    }
    amixr[i] = aveAtomicWgt;
  }
  /*
    c.... average atomic masses for all zones to be processed.
    c.... set indicators for regions mixed into the zones to be processed.
  */
  for (j = 0; j < nXYPairs; j++) {
    abarv[j] = (EOS_REAL) 0.0;
    //   fmltv(jv) = iregs(jv)/(iregs(jv)+TINY)
  }

  for (i = 0; i < nTables; i++) {
    zeroConc[i] = 1;
    for (j = 0; j < nXYPairs; j++) {
      abarv[j] += amixr[i] * concInMix[i * nXYPairs + j];
      sbarv1[j] = concInMix[i * nXYPairs + j] - minCMIXR;
      if (sbarv1[j] >= (EOS_REAL) 0.0)
        zeroConc[i] = 0;
    }
  }
  /*
    c.... initialize independent variables for all zones
    c.... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);
      ymixr[i * nXYPairs + j] = yVals[j];
      pmixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;
      pmixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;
      pmixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;
      smixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* F */
      smixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFx */
      smixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFy */
    }
  }
  /*
    c.... set pressure-balance function and derivatives
    c.... for all zones for each region to be mixed.
    c
    c     initialize average pressure-balance function.
  */
  for (j = 0; j < nXYPairs; j++)
    pbarv1[j] = (EOS_REAL) 0.0;

  /* cngas is computed later */

  /* beginning of iteration. */
  iter = 0;

  while (1) {
    iter = iter + 1;

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i]) {
        for (j = 0; j < nXYPairs; j++)
          xmixr[i * nXYPairs + j] =
            ymixr[i * nXYPairs + j] = (EOS_REAL) 0.0; /* zero value for a constituent with a zero concentration */
        continue;
      }

      /* find the type of the pressure balance function */
      dataType =
        EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);

      /* process conversion factors */
      eos_GetConversionFactorsFromTableHandle (tableHandles[i], &dataType,
                                               &xCnvrtFactor, &yCnvrtFactor,
                                               &fCnvrtFactor, errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        xCnvrtFactor = 1.0;
        yCnvrtFactor = 1.0;
        fCnvrtFactor = 1.0;
      }
      *errorCode = EOS_OK;

      /* compute gas constant for current table handle */
      cngas =
        ((EOS_REAL) 0.01 * UNIVERSAL_GAS_CONST) * fCnvrtFactor / yCnvrtFactor /
        xCnvrtFactor;

      /* load vectors containing proper x values, and */
      for (j = 0; j < nXYPairs; j++)
        xmixr[i * nXYPairs + j] =
          amixr[i] / MAX (TINY_D, vmixr[i * nXYPairs + j]);

      /* DAP -- xmixr exhibits rounding error that may cause the following call of
         eos_InterpolateEosInterpolation() to produce an extrapolation error code
         when it shouldn't. This implies that some sort of rounding needs to be
         imposed upon the values in xmixr prior to interpolation.
      */

      /* set pressure-balance function and derivatives. */
      dataType =
        EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);
      eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                       xmixr + i * nXYPairs,
                                       ymixr + i * nXYPairs,
                                       pmixr1 + i * nXYPairs,
                                       pmixr2 + i * nXYPairs,
                                       pmixr3 + i * nXYPairs, &dataType,
                                       errorCode);

      /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
        /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
        me->interpolationDataList[tableHandles[i]]->lastErrorCode =
          EOS_INTERP_EXTRAP_PBAL;
        returnErr = EOS_INTERP_EXTRAP_PBAL;
        *errorCode = EOS_OK;
      }

      /* if ((icate.eq.2_i_kind) .or. (icate.eq.3_i_kind)) then */
      if (EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]])
          == EOS_Zfo_DT /* 500-tables */  ||
          EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.
                                     tableTypes[tableHandles[i]]) ==
          EOS_Zfc_DT /* 600-tables */ ) {
        /* sesame category 2 or 3 function.
           pressure-balance function becomes electron number density. */
        for (j = 0; j < nXYPairs; j++) {
          pmixr1[i * nXYPairs + j] =
            EXP10 (pmixr1[i * nXYPairs + j]) / MAX (TINY_D,
                                                    vmixr[i * nXYPairs + j]);

          dmixr[i * nXYPairs + j] =
            (EOS_REAL) -0.01 / MAX (TINY_D, vmixr[i * nXYPairs + j]);
          /* transform derivative values of balance functions to be consistent with EOSPAC 5 for
             consistency in remaining mix algorithm here */
          pmixr2[i * nXYPairs + j] =
            pmixr1[i * nXYPairs] * ((EOS_REAL) 1.0 +
                                    pmixr2[i * nXYPairs + j]);
          pmixr3[i * nXYPairs + j] =
            pmixr1[i * nXYPairs] * pmixr3[i * nXYPairs + j];
        }

      }                         /* end sesame category 2 or 3 function */
      else {                    /* EOS_Pt_DT, EOS_Ut_DT, EOS_Pec_DT, EOS_Uec_DT, EOS_Pic_DT or EOS_Uic_DT */

        /* sesame category 1 function, non-logarithmic case. */
        for (j = 0; j < nXYPairs; j++) {
          dmixr[i * nXYPairs + j] =
            -cngas * yVals[j] / MAX (TINY_D, vmixr[i * nXYPairs + j]);
          ;
          /* transform derivative values of balance functions to be consistent with EOSPAC 5 for
             consistency in remaining mix algorithm here */
          pmixr2[i * nXYPairs + j] =
            xmixr[i * nXYPairs + j] * pmixr2[i * nXYPairs + j];
          pmixr3[i * nXYPairs + j] =
            ymixr[i * nXYPairs + j] * pmixr3[i * nXYPairs + j];
        }
      }                         /* end cat 1 functions */
    }                           /* nTables loop (170) */
    /* 
       average pressure-balance function and derivatives for all zones to be processed.
    */
    for (j = 0; j < nXYPairs; j++) {
      vbarv[j] = (EOS_REAL) 0.0;
      dvdpt[j] = (EOS_REAL) 0.0;
      pdvdp[j] = (EOS_REAL) 0.0;
      vdvdv[j] = (EOS_REAL) 0.0;
      tdvdt[j] = (EOS_REAL) 0.0;
    }

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i])
        continue;               // go to 200

      for (j = 0; j < nXYPairs; j++) {
        vbarv[j] =
          vbarv[j] + concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j];
        dpmix = pbarv1[j] - pmixr1[i * nXYPairs + j];
        vdpdv =
          -MAX (pmixr2[i * nXYPairs + j],
                MAX (-dmixr[i * nXYPairs + j], MAX (FABS (dpmix), TINY_D)));
        dmixr[i * nXYPairs + j] = vdpdv;
        vfact = concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j] / vdpdv;
        dvdpt[j] = dvdpt[j] + vfact;
        pdvdp[j] = pdvdp[j] + vfact * pmixr1[i * nXYPairs + j];
        vdvdv[j] = vdvdv[j] - vfact * pmixr2[i * nXYPairs + j];
        tdvdt[j] = tdvdt[j] - vfact * pmixr3[i * nXYPairs + j];
      }
    }

    for (j = 0; j < nXYPairs; j++) {
      sbarv1[j] = pbarv1[j];
      denom = -MAX (TINY_D, -dvdpt[j]);
      pbarv1[j] = pdvdp[j] / denom;
      pbarv2[j] = -vdvdv[j] / denom;
      pbarv3[j] = -tdvdt[j] / denom;
    }

    /* check for convergence of pressure-balance function. */

    for (j = 0; j < nXYPairs; j++) {
      pmref = (EOS_REAL) 0.5 *(FABS (pbarv1[j]) + FABS (sbarv1[j]) + TINY_D);
      sbarv1[j] = FABS (pbarv1[j] - sbarv1[j]) / pmref;
      dvdpt[j] = (EOS_REAL) 0.0;
    }

    /* ipmax = maximum array element in sbarv1 */

    maxVal = sbarv1[0];
    ipmax = 0;
    for (j = 1; j < nXYPairs; j++) {
      if (maxVal < sbarv1[j]) {
        maxVal = sbarv1[j];
        ipmax = i;
      }
    }

    dpmax = sbarv1[ipmax];
    if ((dpmax > maxErr) && (iter < maxIter)) {
      /* reset independent variables, and continue iteration.
         limit relative independent-variable changes to 50%. */
      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;             // go to 240
        for (j = 0; j < nXYPairs; j++) {
          dpmix = pbarv1[j] - pmixr1[i * nXYPairs + j];
          dvmix = dpmix / dmixr[i * nXYPairs + j];
          dvdpt[j] =
            MAX ((EOS_REAL) 1.0,
                 MAX (dvdpt[j], FABS ((EOS_REAL) 2.0 * dvmix)));
        }
      }

      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;
        for (j = 0; j < nXYPairs; j++) {
          dpmix = pbarv1[j] - pmixr1[i * nXYPairs + j];
          dvmix = dpmix / (dmixr[i * nXYPairs + j] * dvdpt[j]);
          vmixr[i * nXYPairs + j] =
            vmixr[i * nXYPairs + j] * ((EOS_REAL) 1.0 + dvmix);
        }
      }

      continue;                 /* iteration loop */
    }
    else if (iter == maxIter) {
      /* iteration did not converge; set error indicator and perform
         final iteration using initial guess for independent variables. */
      *errorCode = EOS_CONVERGENCE_FAILED;
      for (i = 0; i < nTables; i++)
        for (j = 0; j < nXYPairs; j++)
          vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);

      for (j = 0; j < nXYPairs; j++)
        pbarv1[j] = (EOS_REAL) 0.0;

      continue;                 /* iteration loop */
    }
    else
      break;
  }                             /* END do(1) iteration loop */

  /* Save species-specific X and Y if appropriate */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      if (me->interpolationDataList[tableHandles[i]]->xSpecies)
        me->interpolationDataList[tableHandles[i]]->xSpecies[j] = xmixr[i * nXYPairs + j];
      if (me->interpolationDataList[tableHandles[i]]->ySpecies)
        me->interpolationDataList[tableHandles[i]]->ySpecies[j] = ymixr[i * nXYPairs + j];
    }
  }

  /* end of iteration.
     ... if eos function is pressure or charge state,
     ... set values of eos function f(x,y) and derivatives
     ... wrt alog(x,y) for all zones to be processed, and return.
  */
  if (EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]]) ==
      gEosDataMap.tableTypes[tableHandles[0]])
    /* if data type same as pressure balance funct */
  {
    /* if eos function is pressure or charge state. */
    if (EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]]) ==
        EOS_Zfo_DT /* 500-tables */  ||
        EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]]) ==
        EOS_Zfc_DT /* 600-tables */ ) {
      /* sesame category 2 or 3 function. */
      /* pressure-balance function becomes charge state. */
      for (j = 0; j < nXYPairs; j++) {
        pbarv2[j] = vbarv[j] * pbarv2[j] - pbarv1[j];
        pbarv1[j] = vbarv[j] * pbarv1[j];
        pbarv3[j] = vbarv[j] * pbarv3[j];
      }
    }                           /* end cat 2 or 3 function */

    /* assign the output values */
    for (j = 0; j < nXYPairs; j++) {
      fVals[j] = pbarv1[j];
      dFx[j] = pbarv2[j] / MAX (TINY_D, xVals[j]);      /* calculate dF/d(rho) from dF/dln(rho) */
      dFy[j] = pbarv3[j] / MAX (TINY_D, yVals[j]);      /* calculate dF/d(T) from dF/dln(T) */
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = returnErr;
    FREE_EOS_MIXTEMPINTERPOLATION_ARRAYS;       /* free all locally-allocated arrays */
    return;                     /* we are done */
  }                             /* END if data type same as pressure balance funct */

  /*
    .... if eos function is not pressure or charge state,
    .... set state function and derivatives for all zones
    .... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 440

    eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                     xmixr + i * nXYPairs,
                                     ymixr + i * nXYPairs,
                                     smixr1 + i * nXYPairs,
                                     smixr2 + i * nXYPairs,
                                     smixr3 + i * nXYPairs, NULL, errorCode);

    /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
      /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
      me->interpolationDataList[tableHandles[i]]->lastErrorCode =
        EOS_INTERP_EXTRAPOLATED;
      returnErr = EOS_INTERP_EXTRAPOLATED;
      *errorCode = EOS_OK;
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      break;

    /* Olga note: we have to change all the equations for derivatives, because they used
       to be computed wrt ln x */
    if (EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]) ==
        EOS_Zfo_DT /* 500-tables */  ||
        EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]) ==
        EOS_Zfc_DT /* 600-tables */ ) {
      /* sesame category 2 or 3 function. */
      switch (gEosDataMap.tableTypes[tableHandles[i]]) {
      case EOS_Zfo_DT:
      case EOS_Zfc_DT:
        /* charge state function */
        for (j = 0; j < nXYPairs; j++) {
          smixr1[i * nXYPairs + j] = EXP10 (smixr1[i * nXYPairs + j]);
          smixr2[i * nXYPairs + j] = smixr1[j] * smixr2[i * nXYPairs + j];      /* calculate dF/dln(rho) from dF/d(rho) */
          smixr3[i * nXYPairs + j] = smixr1[j] * smixr3[i * nXYPairs + j];      /* calculate dF/dln(T) from dF/dT */
        }
        break;
      case EOS_Kr_DT:
      case EOS_Kp_DT:
        /* photon opacity function. */
        for (j = 0; j < nXYPairs; j++) {
          smixr1[i * nXYPairs + j] =
            EXP10 (smixr1[i * nXYPairs + j] * amixr[i]);
          smixr2[i * nXYPairs + j] = smixr1[j] * smixr2[i * nXYPairs + j];      /* calculate dF/dln(rho) from dF/d(rho) */
          smixr3[i * nXYPairs + j] = smixr1[j] * smixr3[i * nXYPairs + j];      /* calculate dF/dln(T) from dF/dT */
        }
        break;
      case EOS_Keo_DT:
      case EOS_Kc_DT:
        /* electron conductive opacity function. */
        for (j = 0; j < nXYPairs; j++) {
          smixr1[i * nXYPairs + j] =
            EXP10 (smixr1[i * nXYPairs + j]) * amixr[i] * pmixr1[i *
                                                                 nXYPairs +
                                                                 j];
          denom = MAX (TINY_D, pmixr1[i * nXYPairs + j]);
          smixr2[i * nXYPairs + j] = smixr1[j] * (smixr2[i * nXYPairs + j] + pmixr2[i * nXYPairs + j] / denom); /* calculate dF/dln(rho) from dF/d(rho) */
          smixr3[i * nXYPairs + j] = smixr1[j] * (smixr3[i * nXYPairs + j] + pmixr3[i * nXYPairs + j] / denom); /* calculate dF/dln(T) from dF/dT */
        }
        break;
      case EOS_Kec_DT:
      case EOS_Ktc_DT:
      case EOS_B_DT:
        /* electron conductivity function */
        for (j = 0; j < nXYPairs; j++) {
          smixr1[i * nXYPairs + j] =
            EXP10 (-smixr1[i * nXYPairs + j]) * vmixr[i * nXYPairs +
                                                      j] * pmixr1[i *
                                                                  nXYPairs +
                                                                  j];
          denom = MAX (TINY_D, pmixr1[i * nXYPairs + j]);
          smixr2[i * nXYPairs + j] = smixr1[j] * (-smixr2[i * nXYPairs + j] - (EOS_REAL) 1.0 + pmixr2[i * nXYPairs + j] / denom);       /* calculate dF/dln(rho) from dF/d(rho) */
          smixr3[i * nXYPairs + j] = smixr1[j] * (-smixr3[i * nXYPairs + j] - (EOS_REAL) 1.0 + pmixr3[i * nXYPairs + j] / denom);       /* calculate dF/dln(T) from dF/dT */
        }
        break;
      default:
        break;
      }                         /* END dataType switch */

    }                           /* END if data type same as pressure balance funct */
    else {                      /* 300 - tables */

      /* sesame category 1 function, non-logarithmic case */
      switch (gEosDataMap.tableTypes[tableHandles[i]]) {
      case EOS_Ut_DT:
      case EOS_Uic_DT:
      case EOS_Ue_DT:
      case EOS_Uc_D:
        /*internal energy function. */
        for (j = 0; j < nXYPairs; j++) {
          smixr1[i * nXYPairs + j] = smixr1[i * nXYPairs + j] * amixr[i];
          smixr2[i * nXYPairs + j] = xmixr[i * nXYPairs + j] * smixr2[i * nXYPairs + j] * amixr[i];     /* calculate dF/dln(rho) from dF/d(rho) */
          smixr3[i * nXYPairs + j] = ymixr[i * nXYPairs + j] * smixr3[i * nXYPairs + j] * amixr[i];     /* calculate dF/dln(T) from dF/dT */
        }
        break;
      default:
        break;
      }                         /* END dataType switch */
    }                           /* 300 - tables */
  }                             /* nTables loop */

  /* average state function and derivatives for all zones to be processed */

  for (j = 0; j < nXYPairs; j++) {
    sbarv1[j] = (EOS_REAL) 0.0;
    sbarv2[j] = (EOS_REAL) 0.0;
    sbarv3[j] = (EOS_REAL) 0.0;
  }

  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 470

    for (j = 0; j < nXYPairs; j++) {
      vdpdv = dmixr[i * nXYPairs + j];
      dsdpt =
        concInMix[i * nXYPairs + j] * (-smixr2[i * nXYPairs + j]) / vdpdv;
      sbarv1[j] =
        sbarv1[j] + concInMix[i * nXYPairs + j] * smixr1[i * nXYPairs + j];
      sbarv2[j] =
        sbarv2[j] + concInMix[i * nXYPairs + j] * smixr2[i * nXYPairs + j] +
        (pbarv2[j] - pmixr2[i * nXYPairs + j]) * dsdpt;
      sbarv3[j] =
        sbarv3[j] + concInMix[i * nXYPairs + j] * smixr3[i * nXYPairs + j] +
        (pbarv3[j] - pmixr3[i * nXYPairs + j]) * dsdpt;
    }
  }

  /* if appropriate, reset state function and derivatives for all zones to be processed. */
  switch (gEosDataMap.tableTypes[tableHandles[0]]) {
  case EOS_Kr_DT:
  case EOS_Kp_DT:

  case EOS_Ut_DT:
  case EOS_Uic_DT:
  case EOS_Ue_DT:
  case EOS_Uc_D:
    /* photon opacity or internal energy function. */
    for (j = 0; j < nXYPairs; j++) {
      denom = MAX (TINY_D, abarv[j]);
      sbarv1[j] = sbarv1[j] / denom;
      sbarv2[j] = sbarv2[j] / denom;
      sbarv3[j] = sbarv3[j] / denom;
    }
    break;

  case EOS_Keo_DT:
  case EOS_Kc_DT:
    /* electron conductive opacity function. */
    for (j = 0; j < nXYPairs; j++) {
      denom = MAX (TINY_D, abarv[j] * pbarv1[j]);
      sbarv1[j] = sbarv1[j] / denom;
      sbarv2[j] = (sbarv2[j] - sbarv1[j] * abarv[j] * pbarv2[j]) / denom;
      sbarv3[j] = (sbarv3[j] - sbarv1[j] * abarv[j] * pbarv3[j]) / denom;
    }
    break;

  case EOS_Kec_DT:
  case EOS_Ktc_DT:
  case EOS_B_DT:
    /* electron conductivity function */
    for (j = 0; j < nXYPairs; j++) {
      denom = MAX (TINY_D, sbarv1[j]);
      sbarv2[j] = sbarv2[j] / denom;
      sbarv3[j] = sbarv3[j] / denom;
      sbarv1[j] = vbarv[j] * pbarv1[j] / denom;
    }

    for (j = 0; j < nXYPairs; j++) {
      denom = MAX (TINY_D, pbarv1[j]);
      sbarv2[j] =
        sbarv1[j] * (-sbarv2[j] - (EOS_REAL) 1.0 + pbarv2[j] / denom);
      sbarv3[j] = sbarv1[j] * (-sbarv3[j] + pbarv3[j] / denom);
    }
    break;

  default:
    break;
  }                             /* END dataType switch */


  /* set values of eos function f(x,y) and derivatives
     wrt x and y for all zones to be processed, and return. */

  for (j = 0; j < nXYPairs; j++) {
    fVals[j] = sbarv1[j];
    dFx[j] = sbarv2[j] / MAX (TINY_D, xVals[j]);        /* calculate dF/d(rho) from dF/dln(rho) */
    dFy[j] = sbarv3[j] / MAX (TINY_D, yVals[j]);        /* calculate dF/d(T) from dF/dln(T) */
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
    *errorCode = returnErr;
  FREE_EOS_MIXTEMPINTERPOLATION_ARRAYS; /* free all locally-allocated arrays */
}

/**************************************************************************************************
     eos_GetOptionEosInterpolation()
     purpose   -- to get interpolation option
     arguments --
       EOS_INTEGER tableHandle  = handle of table to set the option to 
	   EOS_INTEGER optionFlag  = which option
	   EOS_BOOLEAN *optionVal   = EOS_TRUE/EOS_FALSE value of the option
	   EOS_INTEGER *err         = output -- pointer to int error code
****************************************************************************************************/
void eos_GetOptionEosInterpolation (eos_Interpolation *me,
                                    EOS_INTEGER tableHandle,
                                    EOS_INTEGER optionFlag,
                                    EOS_BOOLEAN *optionVal, EOS_INTEGER *err)
{
  *err = EOS_OK;

  if (me->numberOfHandles <= tableHandle) {
    *err = EOS_INVALID_TABLE_HANDLE;
    return;
  }

  switch (optionFlag) {

  case EOS_LINEAR:
  case EOS_RATIONAL:
  case EOS_USE_CUSTOM_INTERP:
    *optionVal =
      (me->interpolationDataList[tableHandle]->interpolationType ==
       optionFlag)?EOS_TRUE:EOS_FALSE;
    break;

  case EOS_DISABLE_GHOST_NODES:
    *optionVal = me->interpolationDataList[tableHandle]->disableGhostNodes;
    break;

  case EOS_SAVE_SPECIES_DATA:
    *optionVal = me->interpolationDataList[tableHandle]->saveSpeciesData;
    break;

  case EOS_DISCONTINUOUS_DERIVATIVES:
    *optionVal = me->interpolationDataList[tableHandle]->enableDiscontinuousDerivatives;
    break;

  case EOS_XY_PASSTHRU:
    *optionVal = me->interpolationDataList[tableHandle]->enableXYpassthru;
    break;

  case EOS_XY_MODIFY:
    *optionVal = me->interpolationDataList[tableHandle]->enableXYmodify;
    break;

  case EOS_USE_HOST_XY:
    *optionVal = me->interpolationDataList[tableHandle]->useHostXY;
    break;

  case EOS_SKIP_EXTRAP_CHECK:
    *optionVal = me->interpolationDataList[tableHandle]->skipExtrapCheck;
    break;

  default:
    *err = EOS_INVALID_OPTION_FLAG;
    break;

  }
}

/**************************************************************************************************
     eos_SetOptionEosInterpolation()
     purpose   -- to set interpolation option
     arguments --
       EOS_INTEGER tableHandle  = handle of table to set the option to 
	   EOS_INTEGER optionFlag   = which option to set
	   EOS_INTEGER *err         = output -- pointer to int error code
****************************************************************************************************/
void eos_SetOptionEosInterpolation (eos_Interpolation *me, EOS_INTEGER tableHandle,
                                    EOS_INTEGER optionFlag, EOS_BOOLEAN optionVal, EOS_INTEGER *err)
{
  EOS_BOOLEAN prerequisite = EOS_FALSE;
  EOS_INTEGER dataType;

  * err = EOS_OK;

  if (me->numberOfHandles <= tableHandle)
  {
    *err = EOS_INVALID_TABLE_HANDLE;
    return;
  }

  switch (optionFlag) {

  case EOS_LINEAR:
  case EOS_RATIONAL:
    me->interpolationDataList[tableHandle]->interpolationType = optionFlag;
    break;

  case EOS_DISABLE_GHOST_NODES:
    me->interpolationDataList[tableHandle]->disableGhostNodes = optionVal;
    break;

  case EOS_SAVE_SPECIES_DATA:
    me->interpolationDataList[tableHandle]->saveSpeciesData = optionVal;
    break;

  case EOS_DISCONTINUOUS_DERIVATIVES:
    prerequisite = eos_getBoolOptionFromTableHandle (tableHandle, EOS_LINEAR, err);
    if (!prerequisite) {
      *err = EOS_INVALID_OPTION_FLAG;
      *err = eos_SetCustomErrorMsg (tableHandle, *err,
                                    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because EOS_DISCONTINUOUS_DERIVATIVES can be used only in conjunction to EOS_LINEAR");
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, *err);
      return;
    }

    me->interpolationDataList[tableHandle]->enableDiscontinuousDerivatives = optionVal;
    break;

  case EOS_XY_PASSTHRU:
    me->interpolationDataList[tableHandle]->enableXYmodify = EOS_FALSE;
    me->interpolationDataList[tableHandle]->enableXYpassthru = optionVal;
    break;

  case EOS_XY_MODIFY:
    me->interpolationDataList[tableHandle]->enableXYpassthru = EOS_FALSE;
    me->interpolationDataList[tableHandle]->enableXYmodify = optionVal;
    break;

  case EOS_USE_HOST_XY:
    me->interpolationDataList[tableHandle]->enableXYpassthru = EOS_FALSE;
    me->interpolationDataList[tableHandle]->enableXYmodify = optionVal;
    me->interpolationDataList[tableHandle]->useHostXY = optionVal;
    break;

  case EOS_SKIP_EXTRAP_CHECK:
    me->interpolationDataList[tableHandle]->skipExtrapCheck = optionVal;
    break;

  case EOS_USE_CUSTOM_INTERP:
    me->interpolationDataList[tableHandle]->interpolationType = optionFlag;

    prerequisite = eos_getBoolOptionFromTableHandle (tableHandle, EOS_PT_SMOOTHING, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, *err);
      return;
    }
    if (!prerequisite) {
      *err = EOS_INVALID_OPTION_FLAG;
      *err = eos_SetCustomErrorMsg (tableHandle, *err,
                                    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because EOS_USE_CUSTOM_INTERP can be used only in conjunction to EOS_PT_SMOOTHING");
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, *err);
      return;
    }

    /* get eos Data pointer and the dataType from global data map */
    eos_GetEosDataEosDataMap (&gEosDataMap, tableHandle, &dataType, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK) {
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, *err);
      return;
    }
    if (!(dataType == EOS_V_PtT || dataType == EOS_Ut_PtT)) {
      *err = EOS_INVALID_OPTION_FLAG;
      *err = eos_SetCustomErrorMsg (tableHandle, *err,
                                    "EOS_INVALID_OPTION_FLAG: The option flag passed into eos_SetOption() is invalid because EOS_USE_CUSTOM_INTERP can be used only with EOS_V_PtT and EOS_Ut_PtT");
      ((eos_ErrorHandler *) me)->HandleError (me, tableHandle, *err);
      return;
    }
    break;

  default:
    *err = EOS_INVALID_OPTION_FLAG;
    break;

  }

  return;
}

/**************************************************************************************************
     eos_ResetOptionEosInterpolation()
     purpose   -- to reset interpolation option to default.
     arguments --
       EOS_INTEGER tableHandle  = handle of table to set the option to 
	   EOS_INTEGER optionFlag   = which option to set
	   EOS_INTEGER *err         = output -- pointer to int error code
****************************************************************************************************/
void eos_ResetOptionEosInterpolation (eos_Interpolation *me,
                                      EOS_INTEGER tableHandle,
                                      EOS_INTEGER optionFlag,
                                      EOS_INTEGER *err)
{
  *err = EOS_OK;

  if (me->numberOfHandles <= tableHandle) {
    *err = EOS_INVALID_TABLE_HANDLE;
    return;
  }

  switch (optionFlag) {

  case EOS_LINEAR:
  case EOS_RATIONAL:
  case EOS_USE_CUSTOM_INTERP:
    me->interpolationDataList[tableHandle]->interpolationType = EOS_RATIONAL;
    break;

  case EOS_DISABLE_GHOST_NODES:
    me->interpolationDataList[tableHandle]->disableGhostNodes = EOS_FALSE;
    break;

  case EOS_SAVE_SPECIES_DATA:
    me->interpolationDataList[tableHandle]->saveSpeciesData = EOS_FALSE;
    break;

  case EOS_DISCONTINUOUS_DERIVATIVES:
    me->interpolationDataList[tableHandle]->enableDiscontinuousDerivatives = EOS_FALSE;
    break;

  case EOS_XY_PASSTHRU:
    me->interpolationDataList[tableHandle]->enableXYpassthru = EOS_FALSE;
    break;

  case EOS_XY_MODIFY:
    me->interpolationDataList[tableHandle]->enableXYmodify = EOS_FALSE;
    break;

  case EOS_USE_HOST_XY:
    me->interpolationDataList[tableHandle]->useHostXY = EOS_FALSE;
    break;


  case EOS_SKIP_EXTRAP_CHECK:
    me->interpolationDataList[tableHandle]->skipExtrapCheck = EOS_FALSE;
    break;

  default:
    *err = EOS_INVALID_OPTION_FLAG;
    break;

  }

  return;
}


/**************************************************************************************************
     eos_SetNumberOfHandles()
     purpose   -- 
     arguments --
       nAdd        = number of points to insert.
	   err         = output -- pointer to int error code
****************************************************************************************************/
void eos_SetNumberOfHandles (eos_Interpolation *me, EOS_INTEGER nHandles, EOS_INTEGER *err)
{
  int i;
  * err = EOS_OK;

  if (me->numberOfHandles <= 0) {
    me->interpolationDataList =
      (eos_InterpolationData **) malloc (sizeof (eos_InterpolationData *) *
                                         (nHandles + me->numberOfHandles));
  }
  else {
    me->interpolationDataList =
      (eos_InterpolationData **) realloc (me->interpolationDataList,
                                          sizeof (eos_InterpolationData *) *
                                          (nHandles + me->numberOfHandles));
  }

  for (i = me->numberOfHandles; i < (nHandles + me->numberOfHandles); i++) {
    // Allocate and initialize the data struct
    me->interpolationDataList[i] =
      (eos_InterpolationData *) malloc (sizeof (eos_InterpolationData));
    me->interpolationDataList[i]->interpolationType = EOS_RATIONAL;
    me->interpolationDataList[i]->derivativeType = EOS_NORMAL_DERIVATIVES;
    me->interpolationDataList[i]->disableGhostNodes = EOS_FALSE;
    me->interpolationDataList[i]->saveSpeciesData = EOS_FALSE;
    me->interpolationDataList[i]->enableDiscontinuousDerivatives = EOS_FALSE;
    me->interpolationDataList[i]->enableXYpassthru = EOS_FALSE;
    me->interpolationDataList[i]->enableXYmodify = EOS_FALSE;
    me->interpolationDataList[i]->useHostXY = EOS_FALSE;
    me->interpolationDataList[i]->skipExtrapCheck = EOS_FALSE;
    me->interpolationDataList[i]->lastErrorCode = EOS_OK;
    me->interpolationDataList[i]->nXYPairs = 0;
    me->interpolationDataList[i]->nAlloc = 0;
    me->interpolationDataList[i]->xyBounds = NULL;
    me->interpolationDataList[i]->xSpecies = NULL;
    me->interpolationDataList[i]->ySpecies = NULL;
    me->interpolationDataList[i]->FSpecies = NULL;
    me->interpolationDataList[i]->dFxSpecies = NULL;
    me->interpolationDataList[i]->dFySpecies = NULL;
  }

  me->numberOfHandles += nHandles;
}

/***********************************************************************
 * function: eos_MixEnergyInterpolate()
 * Mixing for the function with internal energy being its second independent variable.
 * Iterates search-interpolation to compute energy-based eos function f(x,y) and
 * derivatives wrt x and y for mixture of materials in pressure- and temperature-balance.
 *
 * Input Values:
 * eos_Interpolation *me        - instance of interpolation class
 * EOS_INTEGER nTables          - the total number of data tables on which to operate
 * EOS_INTEGER *tableHandles    - array table Handles to mix. 
 * EOS_INTEGER nXYPairs         - total number of pairs of independent variable values provided for 
 *                                interpolation for each table.
 * EOS_REAL *concInMix          - This is an EOS_REAL array containing the concentration corresponding to each 
 *                                independent variable value pair and to each tableHandle of the desired data to 
 *                                mix. There are nTables*nXYPairs elements in concInMix, and it is stored 
 *                                sequentially in memory.
 * EOS_REAL *xVals              - array of the primary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in xVals
 * EOS_REAL *yVals              - array of the secondary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in yVals
 *
 * Returned Values:
 * EOS_REAL *fVals              - array of the interpolated data corresponding to the given independent variable 
 *                                data (x and y). There are nXYPairs elements in fVals
 * EOS_REAL *dFx                - array of the interpolated partial derivatives of fVals with respect to x. There 
 *                                re nXYPairs elements in dFx.
 * EOS_REAL *dFy                - array of the interpolated partial derivatives of fVals with respect to y. There 
 *                                re nXYPairs elements in dFy.
 * EOS_INTEGER *errorCode       - error code.
 *      
 ***********************************************************************/
void eos_MixEnergyInterpolation (eos_Interpolation *me, EOS_INTEGER nTables,
                                 EOS_INTEGER *tableHandles,
                                 EOS_INTEGER nXYPairs, EOS_REAL *concInMix,
                                 EOS_REAL *xVals, EOS_REAL *yVals,
                                 EOS_REAL *fVals, EOS_REAL *dFx,
                                 EOS_REAL *dFy, EOS_INTEGER *errorCode)
{
  EOS_REAL *xmixr, *ymixr, *amixr, *abarv, *vmixr, *dmixr, *vbarv, *dvdpt,
    *pdvdp, *vdvdv, *tdvdt, *tmixr1, *tmixr2, *tmixr3;
  EOS_REAL *emixr, *pmixr1, *pmixr2, *pmixr3, *smixr1, *smixr2, *smixr3,
    *sbarv1, *sbarv2, *sbarv3, *tbarv1, *tbarv2, *tbarv3, *pbarv1, *pbarv2,
    *pbarv3;
  EOS_INTEGER i, j, iter, ipmax, itmax, *zeroConc, dataType, returnErr =
    EOS_OK;
  EOS_REAL dtmax, dsdtp, efact, tmref, dvetp, *pdedp, demix, dtpmx, dtpmy,
    dtpve, cngas, vfact, pmref, dpmax, dvmix, dsdpt, maxVal, *ebarv, *dvdtp,
    *dedtp, *dedpt, *tdedt;

  static const EOS_REAL minCMIXR = (EOS_REAL) 1.0e-06;  // originally 1.0E-04
  static const EOS_REAL maxErr = (EOS_REAL) 1.0e-04;
  static const EOS_INTEGER maxIter = 20;

  EOS_REAL aveAtomicNumber, aveAtomicWgt, refDensity, xCnvrtFactor,
    yCnvrtFactor, fCnvrtFactor;

  * errorCode = EOS_OK;

  xmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  ymixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  vmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  dmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  emixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  pmixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));

  amixr = (EOS_REAL *) malloc (nTables * sizeof (EOS_REAL));
  zeroConc = (EOS_INTEGER *) malloc (nTables * sizeof (EOS_INTEGER));

  abarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vbarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  ebarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dvdpt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dedtp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dedpt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tdedt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dvdtp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pdvdp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pdedp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vdvdv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tdvdt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  /* macro used to free locally-allocated arrays */
#define FREE_EOS_MIXENERGYINTERPOLATION_ARRAYS                          \
  EOS_FREE(xmixr); EOS_FREE(ymixr); EOS_FREE(vmixr); EOS_FREE(dmixr); EOS_FREE(emixr); EOS_FREE(pmixr1); EOS_FREE(pmixr2); EOS_FREE(pmixr3); \
  EOS_FREE(tmixr1); EOS_FREE(tmixr2); EOS_FREE(tmixr3); EOS_FREE(smixr1); EOS_FREE(smixr2); EOS_FREE(smixr3); EOS_FREE(amixr); EOS_FREE(zeroConc); EOS_FREE(abarv); \
  EOS_FREE(ebarv); EOS_FREE(vbarv); EOS_FREE(dvdpt); EOS_FREE(dvdtp); EOS_FREE(pdvdp); EOS_FREE(pdedp); EOS_FREE(vdvdv); EOS_FREE(tdvdt); \
  EOS_FREE(sbarv1); EOS_FREE(sbarv2); EOS_FREE(sbarv3); EOS_FREE(pbarv1); EOS_FREE(pbarv2); EOS_FREE(pbarv3); EOS_FREE(tbarv1); \
  EOS_FREE(tbarv2); EOS_FREE(tbarv3); EOS_FREE(dedtp); EOS_FREE(dedpt); EOS_FREE(tdedt);

  * errorCode = EOS_OK;

  /* get atomic masses for all regions to be mixed */
  for (i = 0; i < nTables; i++) {
    eos_GetLoadedBulkDataEosDataMap (&gEosDataMap, tableHandles[i],
                                     &aveAtomicNumber, &aveAtomicWgt,
                                     &refDensity, errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      FREE_EOS_MIXENERGYINTERPOLATION_ARRAYS;   /* free all locally-allocated arrays */
      return;
    }
    amixr[i] = aveAtomicWgt;
  }
  /*
    c.... average atomic masses for all zones to be processed.
    c.... set indicators for regions mixed into the zones to be processed.
  */
  for (j = 0; j < nXYPairs; j++) {
    abarv[j] = (EOS_REAL) 0.0;
    //   fmltv(jv) = iregs(jv)/(iregs(jv)+TINY)
  }

  for (i = 0; i < nTables; i++) {
    zeroConc[i] = 1;
    for (j = 0; j < nXYPairs; j++) {
      abarv[j] += amixr[i] * concInMix[i * nXYPairs + j];
      sbarv1[j] = concInMix[i * nXYPairs + j] - minCMIXR;
      if (sbarv1[j] >= (EOS_REAL) 0.0)
        zeroConc[i] = 0;
    }
  }
  /*
    c.... initialize independent variables for all zones
    c.... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);
      emixr[i * nXYPairs + j] = abarv[j] * yVals[j];
      tmixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;
      tmixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;
      tmixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;
      pmixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;
      pmixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;
      pmixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;
      smixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* F */
      smixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFx */
      smixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFy */
    }
  }
  /*
    c.... set pressure- & temperature-balance function and derivatives
    c.... for all zones for each region to be mixed.
    c
    c     initialize average pressure-balance function.
  */
  for (j = 0; j < nXYPairs; j++) {
    pbarv1[j] = (EOS_REAL) 0.0;
    tbarv1[j] = (EOS_REAL) 0.0;
  }

  /* cngas is computed later */

  /* beginning of iteration. */
  iter = 0;

  while (1) {
    iter = iter + 1;

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i]) {
        for (j = 0; j < nXYPairs; j++)
          xmixr[i * nXYPairs + j] =
            ymixr[i * nXYPairs + j] = (EOS_REAL) 0.0; /* zero value for a constituent with a zero concentration */
        continue;
      }

      /* find the type of the pressure balance function */
      dataType =
        EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);

      /* process conversion factors */
      eos_GetConversionFactorsFromTableHandle (tableHandles[i], &dataType,
                                               &xCnvrtFactor, &yCnvrtFactor,
                                               &fCnvrtFactor, errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        xCnvrtFactor = 1.0;
        yCnvrtFactor = 1.0;
        fCnvrtFactor = 1.0;
      }

      /* compute gas constant for current table handle */
      cngas =
        ((EOS_REAL) 0.01 * (EOS_REAL) 4.0 / (EOS_REAL) 9.0 /
         UNIVERSAL_GAS_CONST) * fCnvrtFactor / yCnvrtFactor;

      /* load vectors containing proper x values */
      for (j = 0; j < nXYPairs; j++) {
        xmixr[i * nXYPairs + j] =
          amixr[i] / MAX (TINY_D, vmixr[i * nXYPairs + j]);
        ymixr[i * nXYPairs + j] = emixr[i * nXYPairs + j] / amixr[i];
        dmixr[i * nXYPairs + j] =
          cngas * emixr[i * nXYPairs + j] * emixr[i * nXYPairs +
                                                  j] / MAX (TINY_D,
                                                            vmixr[i *
                                                                  nXYPairs +
                                                                  j]);
        ymixr[i * nXYPairs + j] = MAX (TINY_D, ymixr[i * nXYPairs + j]);
      }

      /* temperature- & pressure-balance functions and derivatives. */
      dataType =
        EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);
      eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                       xmixr + i * nXYPairs,
                                       ymixr + i * nXYPairs,
                                       tmixr1 + i * nXYPairs,
                                       tmixr2 + i * nXYPairs,
                                       tmixr3 + i * nXYPairs, &dataType,
                                       errorCode);

      /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
        /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
        me->interpolationDataList[tableHandles[i]]->lastErrorCode =
          EOS_INTERP_EXTRAP_TBAL;
        returnErr = EOS_INTERP_EXTRAP_TBAL;
        *errorCode = EOS_OK;
      }

      dataType =
        EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);
      eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                       xmixr + i * nXYPairs,
                                       ymixr + i * nXYPairs,
                                       pmixr1 + i * nXYPairs,
                                       pmixr2 + i * nXYPairs,
                                       pmixr3 + i * nXYPairs, &dataType,
                                       errorCode);

      /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
        /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
        me->interpolationDataList[tableHandles[i]]->lastErrorCode =
          EOS_INTERP_EXTRAP_PBAL;
        returnErr = EOS_INTERP_EXTRAP_PBAL;
        *errorCode = EOS_OK;
      }

      /* Olga: THESE should NOT be input function, they should be BALANCE functions from table!!! */

      /* transform derivative values of balance functions to be consistent with EOSPAC 5 for
         consistency in remaining mix algorithm here */
      for (j = 0; j < nXYPairs; j++) {
        tmixr2[i * nXYPairs + j] = xmixr[i * nXYPairs + j] * tmixr2[i * nXYPairs + j];  /* calculate dF/dln(rho) from dF/d(rho) */
        tmixr3[i * nXYPairs + j] = ymixr[i * nXYPairs + j] * tmixr3[i * nXYPairs + j];  /* calculate dF/dln(U) from dF/dU */
        pmixr2[i * nXYPairs + j] = xmixr[i * nXYPairs + j] * pmixr2[i * nXYPairs + j];  /* calculate dF/dln(rho) from dF/d(rho) */
        pmixr3[i * nXYPairs + j] = ymixr[i * nXYPairs + j] * pmixr3[i * nXYPairs + j];  /* calculate dF/dln(T) from dF/dT */
      }
    }                           /* nTables loop (230) */
    /* 
       average temperature- & pressure-balance function and derivatives for all zones to be processed.
    */
    for (j = 0; j < nXYPairs; j++) {
      vbarv[j] = (EOS_REAL) 0.0;
      ebarv[j] = (EOS_REAL) 0.0;
      dvdtp[j] = (EOS_REAL) 0.0;
      dvdpt[j] = (EOS_REAL) 0.0;
      tdvdt[j] = (EOS_REAL) 0.0;
      pdvdp[j] = (EOS_REAL) 0.0;
      dedtp[j] = (EOS_REAL) 0.0;
      dedpt[j] = (EOS_REAL) 0.0;
      tdedt[j] = (EOS_REAL) 0.0;
      pdedp[j] = (EOS_REAL) 0.0;
    }

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i])
        continue;               // go to 260

      for (j = 0; j < nXYPairs; j++) {
        vbarv[j] =
          vbarv[j] + concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j];
        ebarv[j] =
          ebarv[j] + concInMix[i * nXYPairs + j] * emixr[i * nXYPairs + j];
        dtpmx =
          (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr3[i * nXYPairs + j]
          - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr3[i * nXYPairs + j];
        dtpmy =
          (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr2[i * nXYPairs + j]
          - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr2[i * nXYPairs + j];
        dtpve = MAX (-tmixr2[i * nXYPairs + j] * pmixr3[i * nXYPairs + j]
                     + pmixr2[i * nXYPairs + j] * tmixr3[i * nXYPairs + j],
                     MAX (dmixr[i * nXYPairs + j],
                          MAX (FABS (dtpmx), MAX (FABS (dtpmy), TINY_D)
                               )
                          )
                     );
        dmixr[i * nXYPairs + j] = dtpve;
        vfact = concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j] / dtpve;
        efact = concInMix[i * nXYPairs + j] * emixr[i * nXYPairs + j] / dtpve;
        dvdtp[j] = dvdtp[j] + vfact * pmixr3[i * nXYPairs + j];
        dvdpt[j] = dvdpt[j] - vfact * tmixr3[i * nXYPairs + j];
        tdvdt[j] =
          tdvdt[j] + vfact * pmixr3[i * nXYPairs + j] * tmixr1[i * nXYPairs +
                                                               j];
        pdvdp[j] =
          pdvdp[j] - vfact * tmixr3[i * nXYPairs + j] * pmixr1[i * nXYPairs +
                                                               j];
        dedtp[j] = dedtp[j] + efact * pmixr2[i * nXYPairs + j];
        dedpt[j] = dedpt[j] - efact * tmixr2[i * nXYPairs + j];
        tdedt[j] =
          tdedt[j] + efact * pmixr2[i * nXYPairs + j] * tmixr1[i * nXYPairs +
                                                               j];
        pdedp[j] =
          pdedp[j] - efact * tmixr2[i * nXYPairs + j] * pmixr1[i * nXYPairs +
                                                               j];
      }
    }

    for (j = 0; j < nXYPairs; j++) {
      dvetp = dvdtp[j] * dedpt[j] - dedtp[j] * dvdpt[j];
      dvetp =
        (dvetp !=
         0) ? (FABS (dvetp) + TINY_D) * (dvetp / FABS (dvetp)) : TINY_D;

      vfact = vbarv[j] / dvetp;
      efact = ebarv[j] / dvetp;
      sbarv1[j] = tbarv1[j];
      tbarv1[j] =
        (+dedpt[j] * (tdvdt[j] + pdvdp[j]) -
         dvdpt[j] * (tdedt[j] + pdedp[j])) / dvetp;
      tbarv2[j] = -vfact * dedpt[j];
      tbarv3[j] = -efact * dvdpt[j];
      sbarv2[j] = pbarv1[j];
      pbarv1[j] =
        (-dedtp[j] * (tdvdt[j] + pdvdp[j]) +
         dvdtp[j] * (tdedt[j] + pdedp[j])) / dvetp;
      pbarv2[j] = vfact * dedtp[j];
      pbarv3[j] = efact * dvdtp[j];
    }

    /* check for convergence of temp.- & pressure-balance function. */

    for (j = 0; j < nXYPairs; j++) {
      tmref = (EOS_REAL) 0.5 *(FABS (tbarv1[j]) + FABS (sbarv1[j]) + TINY_D);
      pmref = (EOS_REAL) 0.5 *(FABS (pbarv1[j]) + FABS (sbarv2[j]) + TINY_D);
      sbarv1[j] = FABS (tbarv1[j] - sbarv1[j]) / tmref;
      sbarv2[j] = FABS (pbarv1[j] - sbarv2[j]) / pmref;
      dvdpt[j] = (EOS_REAL) 0.0;
      dedtp[j] = (EOS_REAL) 0.0;
    }

    /* itmax, ipmax = maximum array elements in sbarv1, sbarv2 */
    maxVal = sbarv1[0];
    itmax = 0;
    for (j = 1; j < nXYPairs; j++) {
      if (maxVal < sbarv1[j]) {
        maxVal = sbarv1[j];
        itmax = i;
      }
    }

    maxVal = sbarv2[0];
    ipmax = 0;
    for (j = 1; j < nXYPairs; j++) {
      if (maxVal < sbarv2[j]) {
        maxVal = sbarv2[j];
        ipmax = i;
      }
    }

    dtmax = sbarv1[itmax];
    dpmax = sbarv2[ipmax];

    if (((dtmax > maxErr) || (dpmax > maxErr)) && (iter < maxIter)) {
      /* reset independent variables, and continue iteration.
         limit relative independent-variable changes to 50%. */
      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;             // go to 300
        for (j = 0; j < nXYPairs; j++) {
          dtpmx =
            (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr3[i * nXYPairs + j]
            - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr3[i * nXYPairs +
                                                              j];
          dtpmy =
            (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr2[i * nXYPairs + j]
            - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr2[i * nXYPairs +
                                                              j];
          dvmix = dtpmx / dmixr[i * nXYPairs + j];
          demix = dtpmy / dmixr[i * nXYPairs + j];
          dvdtp[j] =
            MAX ((EOS_REAL) 1.0,
                 MAX (dvdtp[j], FABS ((EOS_REAL) 2.0 * dvmix)));
          dedtp[j] =
            MAX ((EOS_REAL) 1.0,
                 MAX (dedtp[j], FABS ((EOS_REAL) 2.0 * demix)));
        }
      }

      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;
        for (j = 0; j < nXYPairs; j++) {
          dtpmx =
            (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr3[i * nXYPairs + j]
            - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr3[i * nXYPairs +
                                                              j];
          dtpmy =
            (tbarv1[j] - tmixr1[i * nXYPairs + j]) * pmixr2[i * nXYPairs + j]
            - (pbarv1[j] - pmixr1[i * nXYPairs + j]) * tmixr2[i * nXYPairs +
                                                              j];
          dvmix = dtpmx / (dmixr[i * nXYPairs + j] * dvdtp[j]);
          demix = dtpmy / (dmixr[i * nXYPairs + j] * dedtp[j]);
          vmixr[i * nXYPairs + j] =
            vmixr[i * nXYPairs + j] * ((EOS_REAL) 1.0 + dvmix);
          emixr[i * nXYPairs + j] =
            emixr[i * nXYPairs + j] * ((EOS_REAL) 1.0 + demix);
        }
      }

      continue;                 /* iteration loop */
    }
    else if (iter == maxIter) {
      /* iteration did not converge; set error indicator and perform
         final iteration using initial guess for independent variables. */

      *errorCode = EOS_CONVERGENCE_FAILED;
      for (i = 0; i < nTables; i++)
        for (j = 0; j < nXYPairs; j++) {
          vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);
          emixr[i * nXYPairs + j] = abarv[j] * yVals[j];
        }


      for (j = 0; j < nXYPairs; j++) {
        pbarv1[j] = (EOS_REAL) 0.0;
        tbarv1[j] = (EOS_REAL) 0.0;
      }

      continue;                 /* iteration loop */
    }
    else
      break;
  }                             /* END do(1) iteration loop */

  /* Save species-specific X and Y if appropriate */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      if (me->interpolationDataList[tableHandles[i]]->xSpecies)
        me->interpolationDataList[tableHandles[i]]->xSpecies[j] = xmixr[i * nXYPairs + j];
      if (me->interpolationDataList[tableHandles[i]]->ySpecies)
        me->interpolationDataList[tableHandles[i]]->ySpecies[j] = ymixr[i * nXYPairs + j];
    }
  }

  /* if eos function is temperature or pressure,
     set values of eos function f(x,y) and derivatives
     wrt x and y for all zones to be processed, and return.
  */


  if (EOS_TYPE_TO_PRES_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]]) ==
      gEosDataMap.tableTypes[tableHandles[0]])
    /* if data type same as pressure balance funct */
  {
    for (j = 0; j < nXYPairs; j++) {
      fVals[j] = pbarv1[j];
      dFx[j] = pbarv2[j] / MAX (TINY_D, xVals[j]);      /* calculate dF/d(rho) from dF/dln(rho) */
      dFy[j] = pbarv3[j] / MAX (TINY_D, yVals[j]);      /* calculate dF/dU from dF/dln(U) */
    }
    FREE_EOS_MIXENERGYINTERPOLATION_ARRAYS;     /* free all locally-allocated arrays */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = returnErr;
    return;                     /* we are done */
  }
  else if (EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]])
           == gEosDataMap.tableTypes[tableHandles[0]])
    /* if data type same as temperature balance funct */
  {
    for (j = 0; j < nXYPairs; j++) {
      fVals[j] = tbarv1[j];
      dFx[j] = tbarv2[j] / MAX (TINY_D, xVals[j]);      /* calculate dF/d(rho) from dF/dln(rho) */
      dFy[j] = tbarv3[j] / MAX (TINY_D, yVals[j]);      /* calculate dF/dU from dF/dln(U) */
    }
    FREE_EOS_MIXENERGYINTERPOLATION_ARRAYS;     /* free all locally-allocated arrays */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = returnErr;
    return;                     /* we are done */
  }                             /* END if data type same as pressure or temperature balance funct */


  /*
    .... if eos function is not pressure or temperature,
    .... set state function and derivatives for all zones
    .... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 440

    eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                     xmixr + i * nXYPairs,
                                     ymixr + i * nXYPairs,
                                     smixr1 + i * nXYPairs,
                                     smixr2 + i * nXYPairs,
                                     smixr3 + i * nXYPairs, NULL, errorCode);

    /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
      /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
      me->interpolationDataList[tableHandles[i]]->lastErrorCode =
        EOS_INTERP_EXTRAPOLATED;
      returnErr = EOS_INTERP_EXTRAPOLATED;
      *errorCode = EOS_OK;
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      break;


    /* sesame category 1 function, non-logarithmic case. */
    /* derivatives aren't with respect to ln */
    for (j = 0; j < nXYPairs; j++) {
      smixr2[i * nXYPairs + j] = xmixr[i * nXYPairs + j] * smixr2[i * nXYPairs + j];    /* calculate dF/dln(rho) from dF/d(rho) */
      smixr3[i * nXYPairs + j] = ymixr[i * nXYPairs + j] * smixr3[i * nXYPairs + j];    /* calculate dF/dln(U) from dF/dU */
    }
  }                             /* nTables loop */

  /* average state function and derivatives for all zones to be processed */

  for (j = 0; j < nXYPairs; j++) {
    sbarv1[j] = (EOS_REAL) 0.0;
    sbarv2[j] = (EOS_REAL) 0.0;
    sbarv3[j] = (EOS_REAL) 0.0;
  }

  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 470

    for (j = 0; j < nXYPairs; j++) {
      dtpve = dmixr[i * nXYPairs + j];
      dsdtp =
        concInMix[i * nXYPairs +
                  j] * (-smixr2[i * nXYPairs + j] * pmixr3[i * nXYPairs + j] +
                        pmixr2[i * nXYPairs + j] * smixr3[i * nXYPairs +
                                                          j]) / dtpve;
      dsdpt =
        concInMix[i * nXYPairs +
                  j] * (-tmixr2[i * nXYPairs + j] * smixr3[i * nXYPairs + j] +
                        smixr2[i * nXYPairs + j] * tmixr3[i * nXYPairs +
                                                          j]) / dtpve;
      sbarv1[j] =
        sbarv1[j] + concInMix[i * nXYPairs + j] * smixr1[i * nXYPairs + j];
      sbarv2[j] = sbarv2[j] + tbarv2[j] * dsdtp + pbarv2[j] * dsdpt;
      sbarv3[j] = sbarv3[j] + tbarv3[j] * dsdtp + pbarv3[j] * dsdpt;
    }
  }

  /* set values of eos function f(x,y) and derivatives
     wrt x and y for all zones to be processed, and return. */

  for (j = 0; j < nXYPairs; j++) {
    fVals[j] = sbarv1[j];
    dFx[j] = sbarv2[j];         /* calculate dF/d(rho) from dF/dln(rho) */
    dFy[j] = sbarv3[j];         /* calculate dF/d(U) from dF/dln(U) */
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
    *errorCode = returnErr;
  FREE_EOS_MIXENERGYINTERPOLATION_ARRAYS;       /* free all locally-allocated arrays */
}

/***********************************************************************
 * function: eos_MixPressureInterpolate()
 * Mixing for the function with pressure being its second independent variable.
 * Iterates search-interpolation to compute pressure-based eos function f(x,y) and
 * derivatives wrt x and y for mixture of materials in temperature-balance.
 *
 * Input Values:
 * eos_Interpolation *me        - instance of interpolation class
 * EOS_INTEGER nTables          - the total number of data tables on which to operate
 * EOS_INTEGER *tableHandles    - array table Handles to mix. 
 * EOS_INTEGER nXYPairs         - total number of pairs of independent variable values provided for 
 *                                interpolation for each table.
 * EOS_REAL *concInMix          - This is an EOS_REAL array containing the concentration corresponding to each 
 *                                independent variable value pair and to each tableHandle of the desired data to 
 *                                mix. There are nTables*nXYPairs elements in concInMix, and it is stored 
 *                                sequentially in memory.
 * EOS_REAL *xVals              - array of the primary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in xVals
 * EOS_REAL *yVals              - array of the secondary independent variable values to use during interpolation. 
 *                                There are nXYPairs elements in yVals
 *
 * Returned Values:
 * EOS_REAL *fVals              - array of the interpolated data corresponding to the given independent variable 
 *                                data (x and y). There are nXYPairs elements in fVals
 * EOS_REAL *dFx                - array of the interpolated partial derivatives of fVals with respect to x. There 
 *                                re nXYPairs elements in dFx.
 * EOS_REAL *dFy                - array of the interpolated partial derivatives of fVals with respect to y. There 
 *                                re nXYPairs elements in dFy.
 * EOS_INTEGER *errorCode       - error code.
 *      
 ***********************************************************************/
void eos_MixPressureInterpolation (eos_Interpolation *me, EOS_INTEGER nTables,
                                   EOS_INTEGER *tableHandles,
                                   EOS_INTEGER nXYPairs, EOS_REAL *concInMix,
                                   EOS_REAL *xVals, EOS_REAL *yVals,
                                   EOS_REAL *fVals, EOS_REAL *dFx,
                                   EOS_REAL *dFy, EOS_INTEGER *errorCode)
{
  EOS_REAL *xmixr, *ymixr, *amixr, *abarv, *vmixr, *dmixr, *vbarv, *dvdtp,
    *pdvdp, *vdvdv, *tdvdt;
  EOS_REAL *tmixr1, *tmixr2, *tmixr3, *smixr1, *smixr2, *smixr3, *sbarv1,
    *sbarv2, *sbarv3, *tbarv1, *tbarv2, *tbarv3;
  EOS_INTEGER dataType, i, j, iter, itmax, *zeroConc, returnErr = EOS_OK;
  EOS_REAL cngas, vfact, tmref, dtmax, dvmix, dsdtp, dtmix, vdtdv, denom,
    maxVal;

  static const EOS_REAL minCMIXR = (EOS_REAL) 1.0e-06;  // originally 1.0E-04
  static const EOS_REAL maxErr = (EOS_REAL) 1.0e-04;
  static const EOS_INTEGER maxIter = 20;

  EOS_REAL aveAtomicNumber, aveAtomicWgt, refDensity, xCnvrtFactor,
    yCnvrtFactor, fCnvrtFactor;
  * errorCode = EOS_OK;

  xmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  ymixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  vmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  dmixr = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  tmixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr1 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr2 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));
  smixr3 = (EOS_REAL *) malloc (nTables * nXYPairs * sizeof (EOS_REAL));

  amixr = (EOS_REAL *) malloc (nTables * sizeof (EOS_REAL));
  zeroConc = (EOS_INTEGER *) malloc (nTables * sizeof (EOS_INTEGER));

  abarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vbarv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dvdtp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  pdvdp = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  vdvdv = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tdvdt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  sbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv1 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv2 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  tbarv3 = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  /* macro used to free locally-allocated arrays */
#define FREE_EOS_MIXPRESSUREINTERPOLATION_ARRAYS                        \
  EOS_FREE(xmixr); EOS_FREE(ymixr); EOS_FREE(vmixr); EOS_FREE(dmixr); EOS_FREE(tmixr1); EOS_FREE(tmixr2); EOS_FREE(tmixr3); \
  EOS_FREE(smixr1); EOS_FREE(smixr2); EOS_FREE(smixr3); EOS_FREE(amixr); EOS_FREE(zeroConc); EOS_FREE(abarv); EOS_FREE(vbarv); \
  EOS_FREE(sbarv1); EOS_FREE(sbarv2); EOS_FREE(sbarv3); EOS_FREE(tbarv1); EOS_FREE(tbarv2); EOS_FREE(tbarv3); EOS_FREE(pdvdp); EOS_FREE(dvdtp); \
  EOS_FREE(vdvdv); EOS_FREE(tdvdt);

  * errorCode = EOS_OK;

  /* get atomic masses for all regions to be mixed */
  for (i = 0; i < nTables; i++) {
    eos_GetLoadedBulkDataEosDataMap (&gEosDataMap, tableHandles[i],
                                     &aveAtomicNumber, &aveAtomicWgt,
                                     &refDensity, errorCode);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
      FREE_EOS_MIXPRESSUREINTERPOLATION_ARRAYS; /* free all locally-allocated arrays */
      return;
    }
    amixr[i] = aveAtomicWgt;
  }
  /*
    c.... average atomic masses for all zones to be processed.
    c.... set indicators for regions mixed into the zones to be processed.
  */
  for (j = 0; j < nXYPairs; j++) {
    abarv[j] = (EOS_REAL) 0.0;
    //   fmltv(jv) = iregs(jv)/(iregs(jv)+TINY)
  }

  for (i = 0; i < nTables; i++) {
    zeroConc[i] = 1;
    for (j = 0; j < nXYPairs; j++) {
      abarv[j] += amixr[i] * concInMix[i * nXYPairs + j];
      sbarv1[j] = concInMix[i * nXYPairs + j] - minCMIXR;
      if (sbarv1[j] >= (EOS_REAL) 0.0)
        zeroConc[i] = 0;
    }
  }
  /*
    c.... initialize independent variables for all zones
    c.... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);
      tmixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;
      tmixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;
      tmixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;
      smixr1[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* F */
      smixr2[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFx */
      smixr3[i * nXYPairs + j] = (EOS_REAL) 0.0;        /* dFy */
    }
  }
  /*
    c.... set temperature-balance function and derivatives
    c.... for all zones for each region to be mixed.
    c
    c     initialize average pressure-balance function.
  */
  for (j = 0; j < nXYPairs; j++)
    tbarv1[j] = (EOS_REAL) 0.0;

  /* cngas is computed later */

  /* beginning of iteration. */
  iter = 0;

  while (1) {
    iter = iter + 1;

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i]) {
        for (j = 0; j < nXYPairs; j++)
          xmixr[i * nXYPairs + j] =
            ymixr[i * nXYPairs + j] = (EOS_REAL) 0.0; /* zero value for a constituent with a zero concentration */
        continue;
      }

      /* find the type of the temperature balance function */
      dataType =
        EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);

      /* process conversion factors */
      eos_GetConversionFactorsFromTableHandle (tableHandles[i], &dataType,
                                               &xCnvrtFactor, &yCnvrtFactor,
                                               &fCnvrtFactor, errorCode);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK) {
        xCnvrtFactor = 1.0;
        yCnvrtFactor = 1.0;
        fCnvrtFactor = 1.0;
      }

      /* compute gas constant for current table handle */
      cngas =
        ((EOS_REAL) 0.01 / UNIVERSAL_GAS_CONST) * fCnvrtFactor / yCnvrtFactor;

      /* load vectors containing proper x, y values */
      for (j = 0; j < nXYPairs; j++) {
        xmixr[i * nXYPairs + j] =
          amixr[i] / MAX (TINY_D, vmixr[i * nXYPairs + j]);
        ymixr[i * nXYPairs + j] = yVals[j];     // * vmixr[i*nXYPairs + j] / amixr[i] * xVals[j] + Fcold[j];
        dmixr[i * nXYPairs + j] = cngas * yVals[j] * vmixr[i * nXYPairs + j];
      }

      /* set temperature-balance function and derivatives. */
      dataType =
        EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[i]]);
      eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                       xmixr + i * nXYPairs,
                                       ymixr + i * nXYPairs,
                                       tmixr1 + i * nXYPairs,
                                       tmixr2 + i * nXYPairs,
                                       tmixr3 + i * nXYPairs, &dataType,
                                       errorCode);

      /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
        /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
        me->interpolationDataList[tableHandles[i]]->lastErrorCode =
          EOS_INTERP_EXTRAP_TBAL;
        returnErr = EOS_INTERP_EXTRAP_TBAL;
        *errorCode = EOS_OK;
      }

      /* transform derivative values of balance functions to be consistent with EOSPAC 5 for
         consistency in remaining mix algorithm here */
      for (j = 0; j < nXYPairs; j++) {
        tmixr2[i * nXYPairs + j] *= MAX (TINY_D, xmixr[i * nXYPairs + j]);      /* calculate dF/dln(rho) from dF/d(rho) */
        tmixr3[i * nXYPairs + j] *= MAX (TINY_D, ymixr[i * nXYPairs + j]);      /* calculate dF/dln(P) from dF/dP */
      }
    }                           /* nTables loop  */

    /* 
       average temperature-balance function and derivatives for all zones to be processed.
    */
    for (j = 0; j < nXYPairs; j++) {
      vbarv[j] = (EOS_REAL) 0.0;
      dvdtp[j] = (EOS_REAL) 0.0;
      tdvdt[j] = (EOS_REAL) 0.0;
      vdvdv[j] = (EOS_REAL) 0.0;
      pdvdp[j] = (EOS_REAL) 0.0;
    }

    for (i = 0; i < nTables; i++) {
      if (zeroConc[i])
        continue;

      for (j = 0; j < nXYPairs; j++) {
        vbarv[j] =
          vbarv[j] + concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j];
        dtmix = tbarv1[j] - tmixr1[i * nXYPairs + j];
        vdtdv =
          MAX (-tmixr2[i * nXYPairs + j],
               MAX (dmixr[i * nXYPairs + j], MAX (FABS (dtmix), TINY_D)));
        dmixr[i * nXYPairs + j] = vdtdv;
        vfact = concInMix[i * nXYPairs + j] * vmixr[i * nXYPairs + j] / vdtdv;
        dvdtp[j] = dvdtp[j] + vfact;
        tdvdt[j] = tdvdt[j] + vfact * tmixr1[i * nXYPairs + j];
        vdvdv[j] = vdvdv[j] - vfact * tmixr2[i * nXYPairs + j];
        pdvdp[j] = pdvdp[j] - vfact * tmixr3[i * nXYPairs + j];
      }
    }

    for (j = 0; j < nXYPairs; j++) {
      sbarv1[j] = tbarv1[j];
      denom = MAX (TINY_D, dvdtp[j]);
      tbarv1[j] = tdvdt[j] / denom;
      tbarv2[j] = -vdvdv[j] / denom;
      tbarv3[j] = -pdvdp[j] / denom;
    }

    /* check for convergence of temperature-balance function. */

    for (j = 0; j < nXYPairs; j++) {
      tmref = (EOS_REAL) 0.5 *(FABS (tbarv1[j]) + FABS (sbarv1[j]) + TINY_D);
      sbarv1[j] = FABS (tbarv1[j] - sbarv1[j]) / tmref;
      dvdtp[j] = (EOS_REAL) 0.0;
    }

    /* itmax = maximum array element in sbarv1 */
    maxVal = sbarv1[0];
    itmax = 0;
    for (j = 1; j < nXYPairs; j++) {
      if (maxVal < sbarv1[j]) {
        maxVal = sbarv1[j];
        itmax = i;
      }
    }

    dtmax = sbarv1[itmax];
    if ((dtmax > maxErr) && (iter < maxIter)) {
      /* reset independent variables, and continue iteration.
         limit relative independent-variable changes to 50%. */
      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;             // go to 240
        for (j = 0; j < nXYPairs; j++) {
          dtmix = tbarv1[j] - tmixr1[i * nXYPairs + j];
          dvmix = dtmix / dmixr[i * nXYPairs + j];
          dvdtp[j] =
            MAX ((EOS_REAL) 1.0,
                 MAX (dvdtp[j], FABS ((EOS_REAL) 2.0 * dvmix)));
        }
      }

      for (i = 0; i < nTables; i++) {
        if (zeroConc[i])
          continue;
        for (j = 0; j < nXYPairs; j++) {
          dtmix = tbarv1[j] - tmixr1[i * nXYPairs + j];
          dvmix = dtmix / (dmixr[i * nXYPairs + j] * dvdtp[j]);
          vmixr[i * nXYPairs + j] =
            vmixr[i * nXYPairs + j] * ((EOS_REAL) 1.0 + dvmix);
        }
      }

      continue;                 /* iteration loop */
    }
    else if (iter == maxIter) {
      /* iteration did not converge; set error indicator and perform
         final iteration using initial guess for independent variables. */
      *errorCode = EOS_CONVERGENCE_FAILED;
      for (i = 0; i < nTables; i++)
        for (j = 0; j < nXYPairs; j++)
          vmixr[i * nXYPairs + j] = abarv[j] / MAX (TINY_D, xVals[j]);

      for (j = 0; j < nXYPairs; j++)
        tbarv1[j] = (EOS_REAL) 0.0;

      continue;                 /* iteration loop */
    }
    else
      break;
  }                             /* do(1) iteration loop */

  /* Save species-specific X and Y if appropriate */
  for (i = 0; i < nTables; i++) {
    for (j = 0; j < nXYPairs; j++) {
      if (me->interpolationDataList[tableHandles[i]]->xSpecies)
        me->interpolationDataList[tableHandles[i]]->xSpecies[j] = xmixr[i * nXYPairs + j];
      if (me->interpolationDataList[tableHandles[i]]->ySpecies)
        me->interpolationDataList[tableHandles[i]]->ySpecies[j] = ymixr[i * nXYPairs + j];
    }
  }

  /* end of iteration.
     ... if eos function is temperature,
     ... set values of eos function f(x,y) and derivatives
     ... wrt x and y for all zones to be processed, and return.
  */
  if (EOS_TYPE_TO_TEMP_BAL_FUNC (gEosDataMap.tableTypes[tableHandles[0]]) ==
      gEosDataMap.tableTypes[tableHandles[0]])
    /* if data type same as temperature balance funct */
  {
    /* if eos function is pressure or charge state. */
    /* assign the output values */
    /* transform derivative values of fVals to be consistent with EOSPAC 6 output */
    for (j = 0; j < nXYPairs; j++) {
      fVals[j] = tbarv1[j];
      dFx[j] = tbarv2[j] / MAX (TINY_D, xVals[j]);      /* calculate dF/d(rho) from dF/dln(rho) */
      dFy[j] = tbarv3[j] / MAX (TINY_D, yVals[j]);      /* calculate dF/dP from dF/dln(P) */
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
      *errorCode = returnErr;
    FREE_EOS_MIXPRESSUREINTERPOLATION_ARRAYS;   /* free all locally-allocated arrays */
    return;                     /* we are done */
  }                             /* END if data type same as temperature balance funct */

  /*
    .... if eos function is not temperature,
    .... set state function and derivatives for all zones
    .... for each region to be mixed.
  */
  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 400

    eos_InterpolateEosInterpolation (me, tableHandles[i], nXYPairs,
                                     xmixr + i * nXYPairs,
                                     ymixr + i * nXYPairs,
                                     smixr1 + i * nXYPairs,
                                     smixr2 + i * nXYPairs,
                                     smixr3 + i * nXYPairs, NULL, errorCode);

    /*DAP: This is a kludge to ignore extrapolated balance function values as in MIXPAC 5 */
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_INTERP_EXTRAPOLATED) {
      /* store the extrapolation error, and results of eos_CheckExtrap() for this handle */
      me->interpolationDataList[tableHandles[i]]->lastErrorCode =
        EOS_INTERP_EXTRAPOLATED;
      returnErr = EOS_INTERP_EXTRAPOLATED;
      *errorCode = EOS_OK;
    }

    if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) != EOS_OK)
      break;

    /* sesame category 1 function, non-logarithmic case */
    switch (gEosDataMap.tableTypes[tableHandles[i]]) {
    case EOS_Ut_DPt:
    case EOS_Uic_DPic:
    case EOS_Ue_DPe:
      /*internal energy function. */
      for (j = 0; j < nXYPairs; j++) {
        smixr1[i * nXYPairs + j] = smixr1[i * nXYPairs + j] * amixr[i];
        smixr2[i * nXYPairs + j] = smixr2[i * nXYPairs + j] * amixr[i] * xmixr[i * nXYPairs + j];       /* calculate dF/dln(rho) from dF/d(rho) */
        smixr3[i * nXYPairs + j] = smixr3[i * nXYPairs + j] * amixr[i] * ymixr[i * nXYPairs + j];       /* calculate dF/dln(P) from dF/dP */
      }
      break;
    default:
      break;
    }                           /* END dataType switch */
  }                             /* nTables loop */

  /* average state function and derivatives for all zones to be processed */

  for (j = 0; j < nXYPairs; j++) {
    sbarv1[j] = (EOS_REAL) 0.0;
    sbarv2[j] = (EOS_REAL) 0.0;
    sbarv3[j] = (EOS_REAL) 0.0;
  }

  for (i = 0; i < nTables; i++) {
    if (zeroConc[i])
      continue;                 // go to 470

    for (j = 0; j < nXYPairs; j++) {
      vdtdv = dmixr[i * nXYPairs + j];
      dsdtp =
        concInMix[i * nXYPairs + j] * (-smixr2[i * nXYPairs + j]) / vdtdv;
      sbarv1[j] =
        sbarv1[j] + concInMix[i * nXYPairs + j] * smixr1[i * nXYPairs + j];
      sbarv2[j] =
        sbarv2[j] + concInMix[i * nXYPairs + j] * smixr2[i * nXYPairs + j] +
        (tbarv2[j] - tmixr2[i * nXYPairs + j]) * dsdtp;
      sbarv3[j] =
        sbarv3[j] + concInMix[i * nXYPairs + j] * smixr3[i * nXYPairs + j] +
        (tbarv3[j] - tmixr3[i * nXYPairs + j]) * dsdtp;
    }
  }

  /* if appropriate, reset state function and derivatives for all zones to be processed. */
  switch (gEosDataMap.tableTypes[tableHandles[0]]) {
  case EOS_Ut_DPt:
  case EOS_Uic_DPic:
  case EOS_Ue_DPe:
    /*internal energy function. */
    for (j = 0; j < nXYPairs; j++) {
      denom = MAX (TINY_D, abarv[j]);
      sbarv1[j] = sbarv1[j] / denom;
      sbarv2[j] = sbarv2[j] / denom;
      sbarv3[j] = sbarv3[j] / denom;
    }
    break;
  default:
    break;
  }                             /* switch od dataType */


  /* set values of eos function f(x,y) and derivatives
     wrt x and y for all zones to be processed, and return. */

  for (j = 0; j < nXYPairs; j++) {
    fVals[j] = sbarv1[j];
    dFx[j] = sbarv2[j] / MAX (TINY_D, xVals[j]);        /* calculate dF/d(rho) from dF/dln(rho) */
    dFy[j] = sbarv3[j] / MAX (TINY_D, yVals[j]);        /* calculate dF/d(P) from dF/dln(P) */
  }

  if (eos_GetStandardErrorCodeFromCustomErrorCode(*errorCode) == EOS_OK)
    *errorCode = returnErr;
  FREE_EOS_MIXPRESSUREINTERPOLATION_ARRAYS;     /* free all locally-allocated arrays */
}
