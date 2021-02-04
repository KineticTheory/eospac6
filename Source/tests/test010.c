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
 *  \brief Verify results of features associated with the following
 *  options:
 *    -# EOS_SPLIT_FORCED,
 *    -# EOS_SPLIT_IDEAL_GAS,
 *    -# EOS_SPLIT_COWAN,
 *    -# and EOS_SPLIT_NUM_PROP.
 *
 *  This is a more extensive test than test011 in that is performs
 *  category 0, 2 and 4 interpolations in conjunction with the
 *  splitting options.
 *
 * Check for the recurrence of an unexpected error code (EOS_DATA_TYPE_NOT_FOUND) from eos_LoadTables
 * if EOS_SPLIT_NUM_PROP is set using eos_SetOptions (see issue #artf1962, https://tf.lanl.gov/sf/go/artf1962).
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
 * MATIDS TO TEST: 3716 7931 31540
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  enum
  { nTablesE = 6 };             /* This is the maximum size of the modelTest_s.matID array */
  enum
  { nXYPairsE = 20 };

  EOS_INTEGER i, j, itest;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
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
    EOS_BOOLEAN forced;
  } modelTest_s;

  modelTest_s modelTests[] = {
    {
     6,
     {7931, 7931, 7931, 7931, 7931, 7931},
     {EOS_Pt_DT, EOS_Ut_DT, EOS_T_DUt, EOS_Uc_D, EOS_Pt_DUt, EOS_Pc_D},
     {"EOS_Pt_DT", "EOS_Ut_DT", "EOS_T_DUt", "EOS_Uc_D", "EOS_Pt_DUt",
      "EOS_Pc_D"},
     0, EOS_FALSE,
     },
    {
     6,
     {7931, 7931, 7931, 7931, 7931, 7931},
     {EOS_Pt_DT, EOS_Ut_DT, EOS_T_DUt, EOS_Uc_D, EOS_Pt_DUt, EOS_Pc_D},
     {"EOS_Pt_DT", "EOS_Ut_DT", "EOS_T_DUt", "EOS_Uc_D", "EOS_Pt_DUt",
      "EOS_Pc_D"},
     EOS_SPLIT_NUM_PROP, EOS_FALSE,
     },
    {
     6,
     {7931, 7931, 7931, 7931, 7931, 7931},
     {EOS_Pt_DT, EOS_Ut_DT, EOS_T_DUt, EOS_Uc_D, EOS_Pt_DUt, EOS_Pc_D},
     {"EOS_Pt_DT", "EOS_Ut_DT", "EOS_T_DUt", "EOS_Uc_D", "EOS_Pt_DUt",
      "EOS_Pc_D"},
     0, EOS_FALSE,
     },
    {
     6,
     {7931, 7931, 7931, 7931, 7931, 7931},
     {EOS_Pt_DT, EOS_Ut_DT, EOS_T_DUt, EOS_Uc_D, EOS_Pt_DUt, EOS_Pc_D},
     {"EOS_Pt_DT", "EOS_Ut_DT", "EOS_T_DUt", "EOS_Uc_D", "EOS_Pt_DUt",
      "EOS_Pc_D"},
     EOS_SPLIT_NUM_PROP, EOS_TRUE,
     },
    {
     1,
     {3716, -1, -1, -1, -1, -1},
     {EOS_Pt_DT, EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,
      EOS_NullTable},
     {"EOS_Pt_DT", "EOS_NullTable", "EOS_NullTable", "EOS_NullTable",
      "EOS_NullTable", "EOS_NullTable"},
     0, EOS_FALSE,
     },
    {
     1,
     {31540, -1, -1, -1, -1, -1},
     {EOS_Tm_D, EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,
      EOS_NullTable},
     {"EOS_Tm_D", "EOS_NullTable", "EOS_NullTable", "EOS_NullTable",
      "EOS_NullTable", "EOS_NullTable"},
     0, EOS_FALSE,
     },
    {
     1,
     {31540, -1, -1, -1, -1, -1},
     {EOS_Tm_D, EOS_NullTable, EOS_NullTable, EOS_NullTable, EOS_NullTable,
      EOS_NullTable},
     {"EOS_Tm_D", "EOS_NullTable", "EOS_NullTable", "EOS_NullTable",
      "EOS_NullTable", "EOS_NullTable"},
     EOS_SPLIT_NUM_PROP, EOS_FALSE,
     }
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nXYPairs = nXYPairsE;
  nModelTests = sizeof(modelTests)/sizeof(modelTests[0]);

  errorCode = EOS_OK;

  X[0] = 0.85;
  X[1] = 2.0317881088625;
  X[2] = 3.213576217725;
  X[3] = 4.3953643265875;
  X[4] = 5.57715243545;
  X[5] = 6.75894054431249;
  X[6] = 7.94072865317499;
  X[7] = 9.12251676203749;
  X[8] = 10.3043048709;
  X[9] = 11.4860929797625;
  X[10] = 12.667881088625;
  X[11] = 13.8496691974875;
  X[12] = 15.03145730635;
  X[13] = 16.2132454152125;
  X[14] = 17.395033524075;
  X[15] = 18.5768216329375;
  X[16] = 19.0529;
  X[17] = 20.9403978506625;
  X[18] = 22.122185959525;
  X[19] = 23.3039740683875;

  for (i = 0; i < nXYPairs; i++)
    Y[i] = 200.;

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

    printf ("--- Set splitting options: type=%i, forced=%i ---\n",
            modelTests[itest].modelFlag, modelTests[itest].forced);
    if (modelTests[itest].modelFlag) {
      /* set splitting options */
      for (i = 0; i < modelTests[itest].nTables; i++) {
        if (modelTests[itest].forced) {
          eos_SetOption (&tableHandle[i], &EOS_SPLIT_FORCED, EOS_NullPtr,
                         &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          }
        }
        eos_SetOption (&tableHandle[i], &(modelTests[itest].modelFlag),
                       EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
    }
    /* Enable data dump to file */
    for (i = 0; i < modelTests[itest].nTables; i++) {
      if (itest == 0 && i == 0) {
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
              modelTests[itest].tableTypeLabel[i],
              modelTests[itest].matID[i]);
      eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                       &errorCode);
      if (errorCode != EOS_OK) {
	EOS_BOOLEAN equal;
	eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
        if (equal) {
          eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, extrapCode,
                           &errorCode);
          if (errorCode != EOS_OK) {
            printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode,
                    errorMessage);
          }
          else {
            for (j = 0; j < nXYPairs; j++) {
              if (extrapCode[j] != EOS_OK) {
                printf
                  ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, extrap: %i\n",
                   j, X[j], Y[j], F[j], dFx[j], dFy[j], extrapCode[j]);
              }
              else {
                printf
                  ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n", j,
                   X[j], Y[j], F[j], dFx[j], dFy[j]);
              }
            }
          }
        }
      }
      else {
        for (j = 0; j < nXYPairs; j++) {
          printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n",
                  j, X[j], Y[j], F[j], dFx[j], dFy[j]);
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
