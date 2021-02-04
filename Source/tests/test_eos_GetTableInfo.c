/*********************************************************************
 * Example Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup C tests
 * \brief Test the data loaded for 301 tables to verify the following:
 *           -# free energy and entropy tables are loaded properly
 *           -# eos_GetTableInfo works as expected
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "eos_Interface.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{
  enum
  { nTablesE = 1 };
  enum
  { nInfoItemsE = 5 };

  EOS_INTEGER i, j;
  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs;
  EOS_REAL infoVal;
  EOS_INTEGER NR, NT;
  EOS_REAL *R=NULL, *T=NULL, *F=NULL;
  EOS_INTEGER nInfoItems;
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_NR,
    EOS_NT,
    EOS_R_Array,
    EOS_T_Array,
    EOS_F_Array
  };
  EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_At_DT"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER one = 1;

  typedef struct
  {
      EOS_INTEGER matID;
      EOS_INTEGER tableType;
  } modelTest_s;
  modelTest_s tests[] = {
    {3720, EOS_At_DT }
  };

  nTables = nTablesE;
  nInfoItems = nInfoItemsE;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
    matID[i]     = tests[i].matID;
    tableType[i] = tests[i].tableType;
  }

  /* initialize table data objects */
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

  /* load data into table data objects */
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("\tTH=%i: %s\n", tableHandle[i], errorMessage);
    }
  }

  /* retrieve table info */
  R = T = F = NULL;
  for (i = 0; i < nTables; i++) {
    NR = NT = 0;
    printf ("\n\"--- Table information for tableType %s , tableHandle=%i, material=%i ---\"\n",
            tableTypeLabel[i], tableHandle[i], matID[i]);
    for (j = 0; j < nInfoItems; j++) {
      if (infoItems[j] == EOS_NR)
      {
        eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
        NR = (EOS_INTEGER) infoVal;
        assert (NR > 0);
      }
      else if (infoItems[j] == EOS_NT)
      {
        eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
        NT = (EOS_INTEGER) infoVal;
        assert (NT > 0);
      }
      else if (infoItems[j] == EOS_R_Array)
      {
        EOS_FREE(R);
        assert (NR > 0 && NT > 0);
        R = (EOS_REAL*) malloc (sizeof (EOS_REAL) * NR);
        assert (R != NULL);
        eos_GetTableInfo (&(tableHandle[i]), &NR, &(infoItems[j]), R, &errorCode);
      }
      else if (infoItems[j] == EOS_T_Array)
      {
        EOS_FREE(T);
        assert (NR > 0 && NT > 0);
        T = (EOS_REAL*) malloc (sizeof (EOS_REAL) * NT);
        assert (T != NULL);
        eos_GetTableInfo (&(tableHandle[i]), &NT, &(infoItems[j]), T, &errorCode);
      }
      else if (infoItems[j] == EOS_F_Array)
      {
        nXYPairs = NR * NT;
        assert (NR > 0 && NT > 0 && nXYPairs > 0);
        F = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nXYPairs);
        assert (F != NULL);
        eos_GetTableInfo (&(tableHandle[i]), &nXYPairs, &(infoItems[j]), F, &errorCode);
      }
      else
      {
        errorCode = EOS_OK;
      }

      if (errorCode != EOS_OK && errorCode != EOS_INVALID_INFO_FLAG) {
        /* Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
           applicable to a specific tableHandle. */
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
      }
    }

    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++) {
      printf("%23.15e", T[j]);
    }
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++)
        printf("%23.15e", F[j+k*NR]);
      printf("\n");
    }
  }

  /* Destroy all data objects */
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

  /* deallocate temporary memory */
  EOS_FREE(R);
  EOS_FREE(T);
  EOS_FREE(F);

  return 0;

}
