/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests
 *  \brief Verify results of features associated with EOS_PT_SMOOTHING
 *  and EOS_USE_CUSTOM_INTERP options.
 *
 * Perform a comprehensive test of the functionality associated with the EOS_PT_SMOOTHING and
 * EOS_USE_CUSTOM_INTERP options. This provides testing of the data smoothing capabilities that
 * have been introduced in EOSPAC 6.1 for the SAGE code.
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_ResetOption
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 1540 2020 2022 2023 2030 2110 2140 2145 2160 2290 2291 2292 2293 2360 2441 2448 2460 2550 2551 2680 2700 2720 2740 2810 2860 2880 2961 2962 2980 2981 2983 2984 3050 3070 3100 3101 3120 3140 3180 3200 3210 3280 3332 3333 3334 3336 3510 3520 3541 3560 3660 3713 3715 3716 3717 3718 3719 3720 3730 3810 3811 3830 3950 4100 4270 4271 4272 5000 5001 5010 5011 5030 5171 5172 5180 5181 5190 5210 5250 5251 5263 5265 5271 5272 5280 5300 5410 5411 5500 5501 5502 5520 5530 5531 5540 5550 5560 5760 5761 5762 5770 5780 5781 7010 7020 7030 7081 7100 7102 7111 7120 7121 7122 7130 7150 7152 7153 7154 7155 7160 7171 7180 7190 7230 7244 7245 7252 7270 7281 7282 7283 7330 7331 7371 7380 7381 7383 7385 7386 7387 7390 7391 7410 7411 7432 7440 7450 7460 7470 7480 7510 7520 7521 7530 7541 7542 7550 7560 7561 7570 7580 7590 7591 7592 7593 7601 7602 7603 7610 7611 7612 7660 7740 7741 7750 7760 7761 7770 7771 7830 7831 7832 7833 7930 7931 8010 8020 8200
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <float.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#define SAFER_FREE(p) { assert(p != NULL); free(p); p=NULL; }
#define MIN(x,y) (((x)<(y))? (x) : (y))
#define MAX(x,y) (((x)>(y))? (x) : (y))
#define ABS(x)  (((x)>0)? (x) : -1*(x))
#define FABS(x) (((x)>0)? (x) : (EOS_REAL)-1.0*(x))

int main ()
{
  enum
  { nTablesE = 2 };
  enum
  { nXYPairsE = 16 };
  enum
  { nMaterialsE = 188 };

  EOS_INTEGER ii, i, j, k, one = 1;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE], vol[nXYPairsE], sie[nXYPairsE];
  EOS_INTEGER extrapCode[nXYPairsE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode, nTables, nXYPairs, nMaterials;
  EOS_BOOLEAN useCustomInterp, volArraySet, sieArraySet;

  typedef struct
  {
    EOS_INTEGER tableType;
    EOS_CHAR *tableTypeLabel;
    EOS_CHAR *description;
  } modelTest_s;
  /*
     Define all the tables to test
   */
  modelTest_s modelTests[nTablesE] = {
/*     {EOS_Pt_DT,  "EOS_Pt_DT",  "Pressure"}, */
/*     {EOS_Ut_DT,  "EOS_Ut_DT",  "Specific-Internal-Energy"}, */
    {EOS_Ut_PtT, "EOS_Ut_PtT", "Specific-Internal-Energy"},
    {EOS_V_PtT, "EOS_V_PtT", "Specific-Volume"}
  };

  /* materials to process */
  EOS_INTEGER materials[nMaterialsE] = {
    1540, 2020, 2022, 2023, 2030, 2110, 2140, 2145, 2160, 2290,
    2291, 2292, 2293, 2360, 2441, 2448, 2460, 2550, 2551, 2680,
    2700, 2720, 2740, 2810, 2860, 2880, 2961, 2962, 2980, 2981,
    2983, 2984, 3050, 3070, 3100, 3101, 3120, 3140, 3180,
    3200, 3210, 3280, 3332, 3333, 3334, 3336, 3510, 3520, 3541,
    3560, 3660, 3713, 3715, 3716, 3717, 3718, 3719, 3720, 3730,
    3810, 3811, 3830, 3950, 4100, 4270, 4271, 4272, 5000, 5001,
    5010, 5011, 5030, 5171, 5172, 5180, 5181, 5190, 5210, 5250,
    5251, 5263, 5265, 5271, 5272, 5280, 5300, 5410, 5411, 5500,
    5501, 5502, 5520, 5530, 5531, 5540, 5550, 5560, 5760, 5761,
    5762, 5770, 5780, 5781, 7010, 7020, 7030, 7081, 7100, 7102,
    7111, 7120, 7121, 7122, 7130, 7150, 7152, 7153, 7154,
    7155, 7160, 7171, 7180, 7190, 7230, 7244, 7245, 7252, 7270,
    7281, 7282, 7283, 7330, 7331, 7371, 7380, 7381, 7383, 7385,
    7386, 7387, 7390, 7391, 7410, 7411, 7432, 7440, 7450, 7460,
    7470, 7480, 7510, 7520, 7521, 7530, 7541, 7542, 7550, 7560,
    7561, 7570, 7580, 7590, 7591, 7592, 7593, 7601, 7602, 7603,
    7610, 7611, 7612, 7660, 7740, 7741, 7750, 7760, 7761, 7770,
    7771, 7830, 7831, 7832, 7833, 7930, 7931, 8010, 8020, 8200
  };

  /* Materials intentionally removed from testing:
     2982
   */

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  /* ..... START MAIN CODE ..... */

  nTables = nTablesE;
  nXYPairs = nXYPairsE;
  nMaterials = nMaterialsE;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

/*   X[0] = X[4] = X[8] = X[12] = 300.; */
/*   X[1] = X[5] = X[9] = X[13] = 600.; */
/*   X[2] = X[6] = X[10] = X[14] = 8200.; */
/*   X[3] = X[7] = X[11] = X[15] = 830.; */

/*   Y[0] = Y[1] = Y[2] = Y[3] = 200.; */
/*   Y[4] = Y[5] = Y[6] = Y[7] = 62000.; */
/*   Y[8] = Y[9] = Y[10] = Y[11] = 400.; */
/*   Y[12] = Y[13] = Y[14] = Y[15] = 2000.; */

  X[0] = X[4] = X[8] = X[12] = 1.000000000000001E-16;
  X[1] = X[5] = X[9] = X[13] = 5.263157894736736E+04;
  X[2] = X[6] = X[10] = X[14] = 1.052631578947347E+05;
  X[3] = X[7] = X[11] = X[15] = 1.578947368421021E+05;

  Y[0] = Y[1] = Y[2] = Y[3] = 1.160450500000000E+02;
  Y[4] = Y[5] = Y[6] = Y[7] = 6.107635309900391E+08;
  Y[8] = Y[9] = Y[10] = Y[11] = 1.221526945935028E+09;
  Y[12] = Y[13] = Y[14] = Y[15] = 1.832290360880018E+09;

  //nMaterials=2;

  for (j = 0; j < nMaterials; j++) {

    //if (j >0) break;
    //if (materials[j] != 2550 ) continue;

    for (i = 0; i < nTables; i++) {

      /* initialize table data objects */
      /*       printf("\n*********************************************************************\n"); */
      /*       printf("*** CREATING TEST CASE %i: %s for material %i ***\n%s\n", */
      /*             i+1,modelTests[i].tableTypeLabel,materials[j], */
      /*             modelTests[i].description); */
      eos_CreateTables (&one, &(modelTests[i].tableType), &(materials[j]),
                        &(tableHandle[i]), &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("%i: eos_CreateTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
        return errorCode;
      }

      eos_SetOption (&(tableHandle[i]), &EOS_PT_SMOOTHING, EOS_NullPtr,
                     &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        return errorCode;
      }

      //eos_SetOption ( &(tableHandle[i]), &EOS_DEBUG_PRINT , EOS_NullPtr, &errorCode );
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        return errorCode;
      }

#if 0
      /* Enable data dump to file */
      if ((i == 0) && (j == 0)) {
        eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr,
                       &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          return errorCode;
        }
      }
      else {
        eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                       &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          return errorCode;
        }
      }
#endif
    }                           /* nTables loop */

    /* load data into table data objects */
    printf
      ("\n*********************************************************************\n");
    for (i = 0; i < nTables; i++) {
      printf ("*** LOADING TABLE %i: %s (%s) for material %i ***\n",
              i + 1, modelTests[i].tableTypeLabel, modelTests[i].description,
              materials[j]);
      eos_LoadTables (&one, &tableHandle[i], &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("%i: eos_LoadTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
        return errorCode;
      }
    }                           /* nTables loop */
    printf
      ("*********************************************************************\n");

    volArraySet = EOS_FALSE;
    sieArraySet = EOS_FALSE;

    /* interpolate */
    for (i = 0; i < nTables; i++) {

      eos_ResetOption (&tableHandle[i], &EOS_DEBUG_PRINT, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_ResetOption ERROR %i: %s\n", errorCode, errorMessage);
        return errorCode;
      }

      useCustomInterp = EOS_FALSE;
      for (ii = 0; ii < 2; ii++) {

        if (ii == 0) {          /* skip default interpolation method */
          useCustomInterp = EOS_TRUE;
          continue;
        }

        printf ("*** INTERPOLATING DATA: %s (%s) for material %i %s***\n",
                modelTests[i].tableTypeLabel, modelTests[i].description,
                materials[j],
                ((useCustomInterp) ?
                 "(EOS_USE_CUSTOM_INTERP) see combined results below " : ""));

        if (useCustomInterp) {
          eos_SetOption (&tableHandle[i], &EOS_USE_CUSTOM_INTERP, EOS_NullPtr,
                         &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
            continue;
          }
        }
        else {
          eos_ResetOption (&tableHandle[i], &EOS_USE_CUSTOM_INTERP,
                           &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_ResetOption ERROR %i: %s\n", errorCode,
                    errorMessage);
            continue;
          }
        }

        eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                         &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          if (errorCode != EOS_INTERP_EXTRAPOLATED)
            printf ("eos_Interpolate ERROR %i: %s\n", errorCode,
                    errorMessage);
          if (errorCode == EOS_INTERP_EXTRAPOLATED) {
            eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, extrapCode,
                             &errorCode);
            if (errorCode != EOS_OK) {
              printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode,
                      errorMessage);
            }
            else {
              for (k = 0; k < nXYPairs; k++) {
                if (extrapCode[k] != EOS_OK) {
                  printf
                    ("\ti=%i\tX = %.15e, Y = %.15e, F = %.15e, dFx = %.15e, dFy = %.15e, extrap: %i\n",
                     k, X[k], Y[k], F[k], dFx[k], dFy[k], extrapCode[k]);
                }
                else {
                  printf
                    ("\ti=%i\tX = %.15e, Y = %.15e, F = %.15e, dFx = %.15e, dFy = %.15e\n",
                     k, X[k], Y[k], F[k], dFx[k], dFy[k]);
                }
              }
            }
          }
        }
        else {
          for (k = 0; k < nXYPairs; k++) {
            if (useCustomInterp) {
              /* printf("p = %.15e t = %.15e F = %.15e\n", X[k], Y[k], F[k]); */
              if (modelTests[i].tableType == EOS_Ut_PtT) {
                sie[k] = F[k];
                sieArraySet = EOS_TRUE;
              }
              if (modelTests[i].tableType == EOS_V_PtT) {
                vol[k] = F[k];
                volArraySet = EOS_TRUE;
              }
            }
            else
              printf
                ("\ti=%i\tX = %.15e, Y = %.15e, F = %.15e, dFx = %.15e, dFy = %.15e\n",
                 k, X[k], Y[k], F[k], dFx[k], dFy[k]);
          }
        }
        useCustomInterp = EOS_TRUE;
        printf ("\n");
      }
      useCustomInterp = EOS_FALSE;
      if (i < nTables - 1)
        printf ("----------------\n");
    }                           /* nTables loop */

    if (volArraySet && sieArraySet) {
      printf ("*** CUSTOM INTERPOLATION RESULTS: for material %i ***\n",
              materials[j]);

      for (k = 0; k < nXYPairs; k++)
        printf ("p = %.15e t = %.15e v = %.15e e = %.15e\n", X[k], Y[k],
                vol[k], sie[k]);
    }

    /* Destroy all data objects */
    eos_DestroyAll (&errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
    }

  }                             /* nMaterials loop */

  return 0;

}
