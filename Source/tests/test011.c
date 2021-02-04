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
 * Uses the following routines:
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  enum
  { nTablesE = 2 };
  enum
  { nXYPairsE = 4 };

  EOS_INTEGER i, j;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs;
  EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_Ue_DT",
    "EOS_Ue_DT",
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER itest,
    splitOptionList[3] =
    { EOS_SPLIT_IDEAL_GAS, EOS_SPLIT_COWAN, EOS_SPLIT_NUM_PROP };
  EOS_CHAR *splitOption_str[3] =
    { "EOS_SPLIT_IDEAL_GAS", "EOS_SPLIT_COWAN", "EOS_SPLIT_NUM_PROP" };

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  /*
   * EOS_Ut_DT, material 2140 works for Sesame table 301 (record type 1)
   * EOS_Ue_DT, material 2140 works for Sesame table 304 (record type 1)
   */
  tableType[0] = EOS_Ue_DT;
  tableType[1] = EOS_Ue_DT;

  numIndVars[0] = 2;
  numIndVars[1] = 2;

  matID[0] = 2140;
  matID[1] = 2140;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  for (itest = 0; itest < 3; itest++) {

    /*
     * initialize table data objects
     */

    eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      for (i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("eos_CreateTables ERROR %i: %s\n", tableHandleErrorCode,
                errorMessage);
      }
    }

    /*
     * set some options
     */

    for (i = 0; i < nTables; i++) {
      /* enable data dump */
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                     &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      }
      /* enable smoothing */
      //eos_SetOption ( &tableHandle[i], &EOS_SMOOTH, EOS_NullPtr, &errorCode );
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      }
      /* enable monotonicity with respect to X */
      //eos_SetOption ( &tableHandle[i], &EOS_MONOTONIC_IN_X, EOS_NullPtr, &errorCode );
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      }
      /* enable num-prop ion/electron splitting */
      eos_SetOption (&tableHandle[i], &(splitOptionList[itest]), EOS_NullPtr,
                     &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      }
    }

    if (itest == 0)             /* enable data dump */
      eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr,
                     &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
    /* force num-prop ion/electron splitting */
    eos_SetOption (&tableHandle[1], &EOS_SPLIT_FORCED, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }

    /*
     * load data into table data objects
     */

    eos_LoadTables (&nTables, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      for (i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("eos_LoadTables ERROR %i: %s\n", tableHandleErrorCode,
                errorMessage);
      }
    }

    /*
     * interpolate -- errors codes are intentionally produced
     */
    X[0] = 3000.;
    X[1] = 6000.;
    X[2] = 8200.;
    X[3] = 8300.;

    Y[0] = 20000.0;
    Y[1] = 620000.0;
    Y[2] = 4000000.0;
    Y[3] = 200000000.0;

    for (i = 0; i < nTables; i++) {
      printf ("\n--- Interpolate using tableType %s ---\n",
              tableTypeLabel[i]);
      eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                       &errorCode);
      if (i == 0)
        printf ("%s: NOT FORCED\n", splitOption_str[itest]);
      if (i == 1)
        printf ("%s: FORCED\n", splitOption_str[itest]);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
      }
      else {
        for (j = 0; j < nXYPairs; j++) {
          if (numIndVars[i] == 1)
            printf ("\ti=%i\tX = %e, F = %e, dFx = %e, errorCode: %d\n",
                    j, X[j], F[j], dFx[j], errorCode);
          if (numIndVars[i] == 2)
            printf
              ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
               j, X[j], Y[j], F[j], dFx[j], dFy[j], errorCode);
        }
      }
    }

    /*
     * Destroy all data objects
     */

    eos_DestroyAll (&errorCode);
    if (errorCode != EOS_OK) {
      for (i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
                errorMessage);
      }
    }
  }
  return 0;

}
