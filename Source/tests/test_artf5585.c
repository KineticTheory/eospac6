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
 *  \brief Verify results of EOS_EXPAND_DATA setup option.
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf5585">artf5585</a> for details.
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{
  int i, j;
  EOS_REAL *X = NULL, *Y = NULL, *F = NULL;
  enum
  { nTablesE = 1 };
  enum
  { nInfoItemsE = 5 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, nX, nY, nF, one_i = 1;
  EOS_REAL num_to_insert = 4.0;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_REAL infoVal;
  EOS_INTEGER nInfoItems;
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_NR,
    EOS_NT,
    EOS_R_Array,
    EOS_T_Array,
    EOS_F_Array
  };

  nInfoItems = nInfoItemsE;
  nTables = nTablesE;

  tableType[0] = EOS_Pt_DT;

  matID[0] = 2140;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s (fcmp_ignore)\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      return 0;
    }
  }
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_INSERT_DATA, &num_to_insert, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      return 0;
    }
  }

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s (fcmp_ignore)\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* fetch loaded data */
  for (i = 0; i < nTables; i++) {
    nX = nY = 0;
    printf ("\n\"--- Table information for tableType %s , tableHandle=%i, material=%i ---\"\n",
            get_tableType_str(tableType[i]), tableHandle[i], matID[i]);
    for (j = 0; j < nInfoItems; j++) {
      if (infoItems[j] == EOS_NR) {
	eos_GetTableInfo (&(tableHandle[i]), &one_i, &(infoItems[j]), &infoVal, &errorCode);
	nX = (EOS_INTEGER) infoVal;
	assert (nX > 0);
      }
      else if (infoItems[j] == EOS_NT) {
	eos_GetTableInfo (&(tableHandle[i]), &one_i, &(infoItems[j]), &infoVal, &errorCode);
	nY = (EOS_INTEGER) infoVal;
	assert (nY > 0);
      }
      else if (infoItems[j] == EOS_R_Array) {
	EOS_FREE(X);
	assert (nX > 0 && nY > 0);
	X = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nX);
	assert (X != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &nX, &(infoItems[j]), X, &errorCode);
      }
      else if (infoItems[j] == EOS_T_Array) {
	EOS_FREE(Y);
	assert (nX > 0 && nY > 0);
	Y = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nY);
	assert (Y != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &nY, &(infoItems[j]), Y, &errorCode);
      }
      else if (infoItems[j] == EOS_F_Array) {
	nF = nX * nY;
	assert (nX > 0 && nY > 0 && nF > 0);
	F = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nF);
	assert (F != NULL);
	eos_GetTableInfo (&(tableHandle[i]), &nF, &(infoItems[j]), F, &errorCode);
      }
      if (errorCode != EOS_OK && errorCode != EOS_INVALID_INFO_FLAG) {
        /* Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
           applicable to a specific tableHandle. */
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
      }
    }

    continue; /* data is dumped to TablesLoaded.dat, so don't dump it here */

    printf("%23s", " \"X       |       Y ->\"");
    for (j = 0; j < nY; j++)
      printf("%23.15e", Y[j]);
    printf("\n");
    for (j = 0; j < nX; j++) {
      int k;
      printf("%23.15e", X[j]);
      for (k = 0; k < nY; k++)
	printf("%23.15e", F[j+k*nX]);
      printf("\n");
    }
  }

  eos_DestroyAll (&errorCode);

  EOS_FREE(X);
  EOS_FREE(Y);
  EOS_FREE(F);

  return 0;

}
