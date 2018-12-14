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
 *  \brief Verify results of features associated with the following
 *  options:
 *    -# EOS_SPLIT_FORCED,
 *    -# EOS_SPLIT_IDEAL_GAS,
 *    -# EOS_SPLIT_COWAN,
 *    -# and EOS_SPLIT_NUM_PROP.
 *
 * The general purpose of test002 is to exercise the data splitting options (EOS_SPLIT_COWAN,
 * EOS_SPLIT_IDEAL_GAS, EOS_SPLIT_NUM_PROP, and EOS_SPLIT_FORCED) for various SESAME subtables
 * of record type 1, category 0 data.
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
 * MATIDS TO TEST: 2140 3716
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

int main ()
{
  enum
  { nTablesE = 1 };             /* This must be 1 unless modelTest_s.matID is changed to an array */
  enum
  { nXYPairsE = 4 };

  EOS_INTEGER i, j, itest;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER extrapCode[nXYPairsE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode, nTables, nXYPairs, nModelTests;
  typedef struct
  {
    EOS_INTEGER matID;
    EOS_INTEGER tableType;
    EOS_CHAR *tableTypeLabel;
    const EOS_INTEGER modelFlag;
    EOS_BOOLEAN forced;
    EOS_CHAR *description;
  } modelTest_s;
  /*
   * EOS_Pt_DT, material 2140 works for Sesame table 301 (record type 1)
   * EOS_Pic_DT, material 2140 works for Sesame table 303 (record type 1)
   * EOS_Pe_DT, material 2140 works for Sesame table 304 (record type 1)
   */
  modelTest_s modelTests[] = {
    {3716, EOS_Pt_DT, "EOS_Pt_DT", 0, EOS_FALSE,
     "Total Pressure"},
    {3716, EOS_Ut_DT, "EOS_Ut_DT", 0, EOS_FALSE,
     "Total Internal Energy"},
    {3716, EOS_At_DT, "EOS_At_DT", 0, EOS_FALSE,
     "Total Free Energy"},
    {2140, EOS_Pt_DT, "EOS_Pt_DT", 0, EOS_FALSE,
     "Total Pressure"},
    {2140, EOS_Ut_DT, "EOS_Ut_DT", 0, EOS_FALSE,
     "Total Internal Energy"},
    {2140, EOS_At_DT, "EOS_At_DT", 0, EOS_FALSE,
     "Total Free Energy"},
    {3716, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_COWAN, EOS_FALSE,
     "Cowan Model,  not forced,  data missing"},
    {3716, EOS_Pe_DT, "EOS_Pe_DT", EOS_SPLIT_COWAN, EOS_TRUE,
     "Cowan Model,  forced,  data missing"},
    {2140, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_COWAN, EOS_FALSE,
     "Cowan Model,  not forced,  data exists"},
    {2140, EOS_Pe_DT, "EOS_Pe_DT", EOS_SPLIT_COWAN, EOS_TRUE,
     "Cowan Model,  forced,  data exists"},
    {3716, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_IDEAL_GAS, EOS_FALSE,
     "Ideal Gas Model,  not forced,  data missing"},
    {3716, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_IDEAL_GAS, EOS_TRUE,
     "Ideal Gas Model,  forced,  data missing"},
    {2140, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_IDEAL_GAS, EOS_FALSE,
     "Ideal Gas Model,  not forced,  data exists"},
    {2140, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_IDEAL_GAS, EOS_TRUE,
     "Ideal Gas Model,  forced,  data exists"},
    {3716, EOS_Pe_DT, "EOS_Pe_DT", EOS_SPLIT_NUM_PROP, EOS_FALSE,
     "Number Proportional Model,  not forced,  data missing"},
    {3716, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_NUM_PROP, EOS_TRUE,
     "Number Proportional Model,  forced,  data missing"},
    {2140, EOS_Pe_DT, "EOS_Pe_DT", EOS_SPLIT_NUM_PROP, EOS_FALSE,
     "Number Proportional Model,  not forced,  data exists"},
    {2140, EOS_Pic_DT, "EOS_Pic_DT", EOS_SPLIT_NUM_PROP, EOS_TRUE,
     "Number Proportional Model,  forced,  data exists"}
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;
  nModelTests = sizeof(modelTests)/sizeof(modelTests[0]);

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  X[0] = 300.;
  X[1] = 600.;
  X[2] = 8200.;
  X[3] = 830.;

  Y[0] = 200.;
  Y[1] = 62000.;
  Y[2] = 400.;
  Y[3] = 2000.;

  for (itest = 0; itest < nModelTests; itest++) {

    printf
      ("\n*********************************************************************\n");
    printf ("*** TEST CASE %i: %s for material %i ***\n%s\n", itest + 1,
            modelTests[itest].tableTypeLabel, modelTests[itest].matID,
            modelTests[itest].description);

    /* initialize table data objects */
    eos_CreateTables (&nTables, &(modelTests[itest].tableType),
                      &(modelTests[itest].matID), tableHandle, &errorCode);
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
      continue;
    }

    printf ("--- Set splitting options: type=%i, forced=%i ---\n",
            modelTests[itest].modelFlag, modelTests[itest].forced);
    if (modelTests[itest].modelFlag) {
      /* set splitting options */
      for (i = 0; i < nTables; i++) {
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
    for (i = 0; i < nTables; i++) {
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

      /* Destroy all data objects */
      eos_DestroyAll (&errorCode);
      if (errorCode != EOS_OK) {
	tableHandleErrorCode = EOS_OK;
	eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
	eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
		errorMessage);
      }

      continue;
    }

    /* interpolate */
    for (i = 0; i < nTables; i++) {
      printf ("--- Interpolate using tableType %s for material %i ---\n",
              modelTests[itest].tableTypeLabel, modelTests[itest].matID);
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
                  ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, extrap: %i (%s)\n",
                   j, X[j], Y[j], F[j], dFx[j], dFy[j], extrapCode[j], ERROR_TO_TEXT(extrapCode[j]));
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

    /* Destroy all data objects */
    eos_DestroyAll (&errorCode);
    if (errorCode != EOS_OK) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
              errorMessage);
    }
  }

  return 0;

}
