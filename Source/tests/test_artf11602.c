/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup tests
 * \brief Verify elimination of _eos_CompressArray platform-dependency
 *        problems. See SourceForge© Issue #artf10445 for more details:
 *        https://tf.lanl.gov/sf/go/artf10445
 *
 * \note
 * MATIDS TO TEST: 9981 9991
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "eos_Interface.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

static const EOS_INTEGER EOS_ALLOW_ALL_INFO_ITEMS = 11002;  /* Override category restrictions related to selected table information parameters */

int main ()
{
  enum
  { nTablesE = 3 };
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
    "EOS_T_DPic",
    "EOS_T_DPic",
    "EOS_T_DPic"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER one = 1;

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_T_DPic;
  tableType[1] = EOS_T_DPic;
  tableType[2] = EOS_T_DPic;

  numIndVars[0] = 2;
  numIndVars[1] = 2;
  numIndVars[2] = 2;

  matID[0] = 9981;
  matID[1] = 9991;
  matID[2] = 9991;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

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
  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
  }
  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
  }
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_ALLOW_ALL_INFO_ITEMS, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
  }

  /*
   * load data into table data objects
   */

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i (TH=%i): %s\n", tableHandleErrorCode,
              tableHandle[i], errorMessage);
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
    printf ("\n--- Interpolate using tableType %s ---\n", tableTypeLabel[i]);
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                     &errorCode);
    printf ("%s Interpolation Results:\n", tableTypeLabel[i]);
    if (errorCode != EOS_OK) {
      char* p = NULL;
      eos_GetErrorMessage (&errorCode, errorMessage);
      p = strstr(errorMessage, "(err=");
      if (p)
	errorMessage[p-errorMessage] = '#';
      printf ("eos_Interpolate ERROR %i (TH=%i): %s\n", errorCode,
              tableHandle[i], errorMessage);
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

  /* retrieve table info */
  for (i = 0; i < nTables; i++) {
    enum
      { nInfoItemsE = 5 };
    EOS_INTEGER nInfoItems = nInfoItemsE;
    EOS_INTEGER infoItems[nInfoItemsE] = {
      EOS_NR,
      EOS_NT,
      EOS_R_Array,
      EOS_T_Array,
      EOS_F_Array
    };
    EOS_INTEGER NR, NT;
    EOS_REAL *R=NULL, *T=NULL, *F=NULL;
    EOS_REAL infoVal;
    NR = NT = 0;
    printf ("\n\"--- Table information for tableType %s , tableHandle=%i, material=%i ---\"\n",
            tableTypeLabel[i], tableHandle[i], matID[i]);
    for (j = 0; j < nInfoItems; j++) {
      if (infoItems[j] == EOS_NR) {
	eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
	NR = (EOS_INTEGER) infoVal;
	if (! errorCode) assert (NR > 0);
      }
      else if (infoItems[j] == EOS_NT) {
	eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
	NT = (EOS_INTEGER) infoVal;
	if (! errorCode) assert (NT > 0);
      }
      else if (infoItems[j] == EOS_R_Array) {
	EOS_FREE(R);
	assert (NR > 0 && NT > 0);
	R = (EOS_REAL*) malloc (sizeof (EOS_REAL) * NR);
	assert (R != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &NR, &(infoItems[j]), R, &errorCode);
      }
      else if (infoItems[j] == EOS_T_Array) {
	EOS_FREE(T);
	assert (NR > 0 && NT > 0);
	T = (EOS_REAL*) malloc (sizeof (EOS_REAL) * NT);
	assert (T != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &NT, &(infoItems[j]), T, &errorCode);
      }
      else if (infoItems[j] == EOS_F_Array) {
	nXYPairs = NR * NT;
	assert (NR > 0 && NT > 0 && nXYPairs > 0);
	F = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nXYPairs);
	assert (F != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &nXYPairs, &(infoItems[j]), F, &errorCode);
      }
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
	return errorCode;
      }
    }

    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++)
      printf("%23.15e", T[j]);
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++)
	printf("%23.15e", F[j+k*NR]);
      printf("\n");
    }

    EOS_FREE(R);
    EOS_FREE(T);
    EOS_FREE(F);
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

  return 0;

}
