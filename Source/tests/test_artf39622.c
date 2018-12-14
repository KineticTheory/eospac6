/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests quick
 *  \brief Verify results of features associated with the following options:
 *    -# EOS_MONOTONIC_IN_X,
 *    -# EOS_MONOTONIC_IN_Y
 *
 * (see issue #artf39622, https://tf.lanl.gov/sf/go/artf39622)
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 7931
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  enum
  { nTablesE = 1 };             /* This is the maximum size of the modelTest_s.matID array */
  enum
  { nXYPairsE = 20 };

  EOS_INTEGER i, j, itest;
  EOS_REAL rho[nXYPairsE], T[nXYPairsE], P[nXYPairsE], U[nXYPairsE];
  EOS_REAL *X, *Y, F[nXYPairsE], dFx[nXYPairsE], dFy[nXYPairsE];
  EOS_INTEGER extrapCode[nXYPairsE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode, nXYPairs, nModelTests;

  typedef struct
  {
      EOS_INTEGER nTables;
      EOS_INTEGER matID[nTablesE];
      EOS_INTEGER tableType[nTablesE];
      EOS_CHAR *tableTypeLabel[nTablesE];
      const EOS_INTEGER modelFlag;
      EOS_CHAR *modelFlagLabel;
      EOS_REAL *x_ptr;
      EOS_REAL *y_ptr;
      EOS_REAL *f_ptr;
      EOS_INTEGER dump;
  } modelTest_s;

  EOS_INTEGER dump_cntr = 0;

  modelTest_s modelTests[] = {
    { nTablesE, {7931}, {EOS_Pt_DT}, {"EOS_Pt_DT"}, 0,                  "",                   rho, T, P, EOS_FALSE },
    { nTablesE, {7931}, {EOS_Pt_DT}, {"EOS_Pt_DT"}, EOS_MONOTONIC_IN_Y, "EOS_MONOTONIC_IN_Y", rho, T, F, EOS_FALSE },
    { nTablesE, {7931}, {EOS_Pt_DT}, {"EOS_Pt_DT"}, EOS_MONOTONIC_IN_X, "EOS_MONOTONIC_IN_X", rho, T, F, EOS_TRUE  },
    { nTablesE, {7931}, {EOS_Ut_DT}, {"EOS_Ut_DT"}, 0,                  "",                   rho, T, U, EOS_FALSE },
    { nTablesE, {7931}, {EOS_Ut_DT}, {"EOS_Ut_DT"}, EOS_MONOTONIC_IN_Y, "EOS_MONOTONIC_IN_Y", rho, T, F, EOS_FALSE },
    { nTablesE, {7931}, {EOS_Ut_DT}, {"EOS_Ut_DT"}, EOS_MONOTONIC_IN_X, "EOS_MONOTONIC_IN_X", rho, T, F, EOS_FALSE },
    { nTablesE, {7931}, {EOS_D_PtT}, {"EOS_D_PtT"}, 0,                  "",                   P,   T, F, EOS_FALSE },
    { nTablesE, {7931}, {EOS_T_DPt}, {"EOS_T_DPt"}, 0,                  "",                   rho, P, F, EOS_FALSE },
    { nTablesE, {7931}, {EOS_T_DUt}, {"EOS_T_DUt"}, 0,                  "",                   rho, U, F, EOS_FALSE }
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nXYPairs = nXYPairsE;
  nModelTests = sizeof(modelTests)/sizeof(modelTests[0]);

  errorCode = EOS_OK;

  rho[0]  = 8.587447703359544e-01;
  rho[1]  = 8.749210127307538e-01;
  rho[2]  = 8.997830465263674e-01;
  rho[3]  = 9.207589453331370e-01;
  rho[4]  = 9.427534500746709e-01;
  rho[5]  = 9.561372868386135e-01;
  rho[6]  = 9.783864163490695e-01;
  rho[7]  = 1.003709431198458e+00;
  rho[8]  = 1.019420950285979e+00;
  rho[9]  = 1.037346069062856e+00; // normal density for 7931
  rho[10] = 1.082279958421510e+00;
  rho[11] = 1.165399521095233e+00;
  rho[12] = 1.293149917376084e+00;
  rho[13] = 1.400931903634369e+00;
  rho[14] = 1.513947866842807e+00;
  rho[15] = 1.582719008592826e+00;
  rho[16] = 1.697043328768362e+00;
  rho[17] = 1.827162417521509e+00;
  rho[18] = 1.907894058629615e+00;
  rho[19] = 2.000000000000000e+00;

  for (i = 0; i < nXYPairs; i++)
    T[i] = 298.15;

  /* EOSPAC 6 tests */

  for (itest = 0; itest < nModelTests; itest++) {

    for (i = 0; i < modelTests[itest].nTables; i++)
      tableHandle[i] = 0;

    printf
      ("\n*********************************************************************\n");
    printf ("*** EOSPAC 6 TEST CASE %i ***\n", itest + 1);

    /* initialize table data objects */
    eos_CreateTables (&(modelTests[itest].nTables),
                      modelTests[itest].tableType, modelTests[itest].matID,
                      tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      for (i = 0; i < modelTests[itest].nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("%i: eos_CreateTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
      }
      continue;
    }

    if (modelTests[itest].modelFlag) {
      printf ("--- Set monotonicity options: type=%s (%i) ---\n",
	      modelTests[itest].modelFlagLabel, modelTests[itest].modelFlag);
      /* set monotonicity options */
      for (i = 0; i < modelTests[itest].nTables; i++) {
	eos_SetOption (&tableHandle[i], &modelTests[itest].modelFlag, EOS_NullPtr,
		       &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage (&errorCode, errorMessage);
	  printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
	}
      }
    }
    /* Enable data dump to file */
    for (i = 0; i < modelTests[itest].nTables; i++) {
      if (modelTests[itest].dump == EOS_FALSE) continue;
      dump_cntr++;
      if (dump_cntr == 0) {
        eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr,
                       &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
      else {
        eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                       &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
    }

    /* load data into table data objects */
    printf ("--- Load data ---\n");
    eos_LoadTables (&(modelTests[itest].nTables), tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      for (i = 0; i < modelTests[itest].nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("%i: eos_LoadTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
      }
      continue;
    }

    /* interpolate */
    for (i = 0; i < modelTests[itest].nTables; i++) {
      printf ("--- Interpolate using tableType %s for material %i ---\n",
              modelTests[itest].tableTypeLabel[i], modelTests[itest].matID[i]);
      eos_Interpolate (&tableHandle[i], &nXYPairs,
		       modelTests[itest].x_ptr, modelTests[itest].y_ptr, modelTests[itest].f_ptr, dFx, dFy,
                       &errorCode);
      if (errorCode != EOS_OK) {
	EOS_BOOLEAN equal;
	eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
        if (equal) {
          eos_CheckExtrap (&tableHandle[i], &nXYPairs, modelTests[itest].x_ptr, modelTests[itest].y_ptr, extrapCode,
                           &errorCode);
          if (errorCode != EOS_OK) {
            printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode, errorMessage);
          }
          else {
            for (j = 0; j < nXYPairs; j++) {
              if (extrapCode[j] != EOS_OK) {
                printf
                  ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e, extrap: %i\n",
                   j, modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
		   dFx[j], dFy[j], extrapCode[j]);
              }
              else {
                printf
                  ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n", j,
                   modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
		   dFx[j], dFy[j]);
              }
            }
          }
        }
      }
      else {
        for (j = 0; j < nXYPairs; j++) {
          printf ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n",
                  j, modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
		  dFx[j], dFy[j]);
        }
      }
    }
  }                             /* itest loop */

  /* Destroy all data objects */
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
  }

  return 0;

}
