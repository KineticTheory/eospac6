/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests
 *  \brief Test category 1 inverse bilinear interpolation.
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

EOS_INTEGER eos_GetStandardErrorCodeFromCustomErrorCode (const EOS_INTEGER err);

int main ()
{
  enum
  { nTablesE = 2 };
  enum
  { nXYPairsE = 8 };

  EOS_INTEGER infoItem[1];
  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode, *xyBounds=NULL;
  EOS_INTEGER nTables, nXYPairs, NX, NY, NX0, one = 1;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen], Pminmax_str[100]="";
  EOS_REAL *X=NULL, *Y=NULL, *X0=NULL, *Y0=NULL, *F=NULL, *F0=NULL, *P0=NULL,
    *F2d=NULL, *dFx=NULL, *dFy=NULL;
  EOS_REAL val;
  EOS_INTEGER i, j, k, nAdd, i0, j0;


  i0 = 0; /* avoid the table edges if 1*/
  j0 = 0;
  nAdd = 3; /* number of data to insert in between existing data */

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_Pt_DT;     /* record type 1, subtable 1 , cat 0 */
  tableType[1] = EOS_D_PtT;     /* record type 1, subtable 1 , cat 1 */

  matID[0] = 2140;
  matID[1] = 2140;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {

    eos_SetOption (&tableHandle[i], &EOS_LINEAR, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }

    if (i>0) continue; /* DAP: delete this break once inversion is working */

    eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }

    eos_SetOption (&tableHandle[i], &EOS_MONOTONIC_IN_X, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* make many (x,y) pairs that span the table grid */

  infoItem[0] = EOS_NR;
  eos_GetTableInfo (&tableHandle[0], &one, infoItem, &val, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }
  NX = (EOS_INTEGER) val;
  infoItem[0] = EOS_NT;
  eos_GetTableInfo (&tableHandle[0], &one, infoItem, &val, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }
  NY = (EOS_INTEGER) val;
  nXYPairs = NX * NY;

  /* allocate memory continuously */
  if (X) free(X);
  if (Y) free(Y);
  if (X0) free(X0);
  if (Y0) free(Y0);
  if (F) free(F);
  if (F0) free(F0);
  if (dFx) free(dFx);
  if (dFy) free(dFy);
  if (xyBounds) free(xyBounds);
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  X0 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y0 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F0 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  xyBounds = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nXYPairs);

  /* initialize array(s) */
  for (i = 0; i < nXYPairs; i++)
    xyBounds[i] = EOS_OK;

  if (! (X && Y && F && dFx && dFy)) {
    printf ("Memory allocation error!\n");
    return 1;
  }

  infoItem[0] = EOS_R_Array;
  eos_GetTableInfo (&tableHandle[0], &NX, infoItem, X0, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  infoItem[0] = EOS_T_Array;
  eos_GetTableInfo (&tableHandle[0], &NY, infoItem, Y0, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  infoItem[0] = EOS_F_Array;
  eos_GetTableInfo (&tableHandle[0], &nXYPairs, infoItem, F0, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  NX0 = NX;

  /* expand X0 and Y0 arrays to include points between original grid */
  if (i0 > 0 || j0 > 0)
    F2d = (EOS_REAL *) malloc (sizeof (EOS_REAL) * ((NX-2*i0) * (NY-2*j0)));

  if (i0 > 0 || j0 > 0) { /* remove i0 row(s) from each end of X0[] and
                             remove j0 row(s) from each end of Y0[] and
                             remove the corresponding rows and columns from F0[] */
    for (j = 0; j < NY-2*j0; j++)
      Y0[j] = Y0[j+j0];
    for (i = 0; i < NX-2*i0; i++) {
      X0[i] = X0[i+i0];
      for (j = 0; j < NY-2*j0; j++)
        F2d[i + j*(NX-2*i0)] = F0[(i+i0) + (j+j0)*NX];
    }
    NX -= 2*i0;
    NY -= 2*j0;
  }
  if (i0 > 0 || j0 > 0) {
    free(F0); F0=NULL;
    F0 = F2d; F2d = NULL;
  }
  NX0 = NX;

  errorCode = __eos_GetExpandedGrid (nAdd, &NX, &X0);
  if (errorCode == -1) {
    printf ("ERROR (%d): __eos_GetExpandedGrid failed to allocate memory of X0[]\n", errorCode);
    return(errorCode);
  }
  errorCode = __eos_GetExpandedGrid (nAdd, &NY, &Y0);
  if (errorCode == -1) {
    printf ("ERROR (%d): __eos_GetExpandedGrid failed to allocate memory of Y0[]\n", errorCode);
    return(errorCode);
  }
  nXYPairs = NX * NY;

  /* allocate arrays */
  if (X) free(X);
  if (Y) free(Y);
  if (F) free(F);
  if (P0) free(P0);
  if (F2d) free(F2d);
  if (dFx) free(dFx);
  if (dFy) free(dFy);
  if (xyBounds) free(xyBounds);
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  P0 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F2d = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  xyBounds = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nXYPairs);

  /* initialize array(s) */
  for (i = 0; i < nXYPairs; i++)
    xyBounds[i] = EOS_OK;

  /* populate X and Y arrays for interpolation using tablehandle[0],
     EOS_Pt_DT */
  k=0;
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      X[k] = X0[i];
      Y[k] = Y0[j];
      k++;
    }
  }

  /* testing interpolation methods */
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");

  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, P0, dFx, dFy, &errorCode);        /* F(x,y) */
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(errorCode) != EOS_INTERP_EXTRAPOLATED)
      return errorCode;
  }

  printf("\n--- TEST eos_Interpolate using category 1 tableType: EOS_D_PtT ---\n");
  printf("nAdd %d\n",nAdd);
  printf("nIgnoredRows %d\n",i0);
  printf("nIgnoredColumns %d\n",j0);
  eos_Interpolate (&tableHandle[1], &nXYPairs, P0, Y, F, dFx, dFy, &errorCode);        /* F(x,y) */
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);

    if (eos_GetStandardErrorCodeFromCustomErrorCode(errorCode) == EOS_INTERP_EXTRAPOLATED) {
      eos_CheckExtrap (&tableHandle[1], &nXYPairs, P0, Y, xyBounds, &errorCode);
      errorCode = EOS_INTERP_EXTRAPOLATED;
    }
  }

  printf ("eos_Interpolate is complete\n");  

#define FABS(x) (((x)>0)? (x) : (EOS_REAL)-1.0*(x))
#define ERR(a,b) (100. * ((FABS(a)>10.e-15) ? FABS(a-b)/a : a-b ))

  k = 0;
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      if (!(k%50)) printf ("\n%7s %23s %23s %23s %23s %6s %23s %23s\n",
                           "i", "Pt", "T", "D", "D0", "err", "dD/dPt", "dD/dT");

      if (nAdd==0) sprintf(Pminmax_str," ,min = %23.15e, max = %23.15e",F0[j*NX0],F0[NX0-1+j*NX0]);
      printf ("%7i %23.15e %23.15e %23.15e %23.15e %6.0g%% %23.15e %23.15e%s %s\n",
              k, P0[k], Y[k], F[k], X[k], ERR(X[k],F[k]), dFx[k], dFy[k], Pminmax_str, ERROR_TO_TEXT(xyBounds[k]));
      k++;
    }
  }

  eos_DestroyAll (&errorCode);

  free(X);
  free(Y);
  free(F);
  free(X0);
  free(Y0);
  free(F0);
  free(P0);
  free(F2d);
  free(dFx);
  free(dFy);
  free(xyBounds);

  return 0;

}
