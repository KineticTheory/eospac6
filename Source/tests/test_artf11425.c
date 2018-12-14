/*********************************************************************
 * Example Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup tests
 * \brief Test the data loaded for 301 tables to verify the following:
 *           -# pressure and internal energy tables are loaded properly
 *           -# free energy and entropy tables are loaded properly
 *           -# the calculated free energy and entropy tables are
 *              thermodynamically consistent the with the internal energy table
 *        See SourceForgeÅ© Issues #artf10445 and #artf11434 for more details:
 *        https://tf.lanl.gov/sf/go/artf10445
 *        https://tf.lanl.gov/sf/go/artf11434
 *
 * \note
 * MATIDS TO TEST: 2140 2141
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
    { nTablesE = 4 };
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
  EOS_REAL infoVal, *P=NULL, *U=NULL, *A=NULL, *S=NULL;
  EOS_REAL *A2=NULL;
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
    "EOS_Pt_DT",
    "EOS_Ut_DT",
    "EOS_At_DT",
    "EOS_St_DT"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER one = 1;

  nTables = nTablesE;
  nInfoItems = nInfoItemsE;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_Ut_DT;
  tableType[2] = EOS_At_DT;
  tableType[3] = EOS_St_DT;

  matID[0] = 2140;
  matID[1] = 2140;
  matID[2] = 2140;
  matID[3] = 2140;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
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
      if (infoItems[j] == EOS_NR) {
	eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
	NR = (EOS_INTEGER) infoVal;
	assert (NR > 0);
      }
      else if (infoItems[j] == EOS_NT) {
	eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]), &infoVal, &errorCode);
	NT = (EOS_INTEGER) infoVal;
	assert (NT > 0);
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
	if      (tableType[i] == EOS_Pt_DT) P = F;
	else if (tableType[i] == EOS_Ut_DT) U = F;
	else if (tableType[i] == EOS_At_DT) A = F;
	else if (tableType[i] == EOS_St_DT) S = F;
	eos_GetTableInfo (&(tableHandle[i]), &nXYPairs, &(infoItems[j]), F, &errorCode);
      }
      if (errorCode != EOS_OK && errorCode != EOS_INVALID_INFO_FLAG) {
        /* Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
           applicable to a specific tableHandle. */
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
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
  }

  /* calculate A=U-TS to verify EOS_At_DT data */
  if (U && A && S) {
    printf ("\n\"--- Calculated Free Energy A=U-TS ---\"\n");
    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++)
      printf("%23.15e", T[j]);
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++) {
	EOS_REAL Acalc;
	Acalc = U[j+k*NR] - T[k] * S[j+k*NR];
	printf("%23.15e", Acalc);
      }
      printf("\n");
    }
  }

  /* Compare (relative difference) A=U-TS with EOS_At_DT data for material 2140 */
  if (U && A && S) {
    EOS_REAL max_reldiff = 0.;
    printf ("\n\"--- Calculated Free Energy (A=U-TS) versus Tabulated EOS_At_DT data (relative difference) for material=%i---\"\n", matID[0]);
    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++)
      printf("%23.15e", T[j]);
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++) {
	EOS_REAL Acalc, rel_diff;
	Acalc = U[j+k*NR] - T[k] * S[j+k*NR];
	rel_diff = (A[j+k*NR]-Acalc)/((A[j+k*NR] != 0.0) ? A[j+k*NR] : 1.0);
	if (fabs(max_reldiff) < fabs(rel_diff))
	  max_reldiff = rel_diff;
	printf("%23.15e", rel_diff);
      }
      printf(" fcmp_ignore\n");
    }
    printf("max_reldiff = %23.15e fcmp_ignore\n", max_reldiff);
  }

  /* Compare calculated free energy of material 2140 with tabulated
     free energy of material 2141. */
  nTables = 1;
  tableType[0] = EOS_At_DT;
  matID[0] = 2141;

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
  printf ("\n\"--- Table information for tableType EOS_At_DT , tableHandle=%i, material %i ---\"\n",
	  tableHandle[0], matID[0]);
  F = (EOS_REAL*) malloc (sizeof (EOS_REAL) * nXYPairs);
  assert (F != NULL);
  A2 = F;
  eos_GetTableInfo (&(tableHandle[0]), &nXYPairs, &(infoItems[4]), F, &errorCode);
  if (errorCode != EOS_OK && errorCode != EOS_INVALID_INFO_FLAG) {
    /* Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
       applicable to a specific tableHandle. */
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
  }

  /* tabulated EOS_At_DT data for 2141 */
  if (A && A2) {
    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++)
      printf("%23.15e", T[j]);
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++)
	printf("%23.15e", A2[j+k*NR]);
      printf("\n");
    }
  }

  /* Relative difference of tabulated EOS_At_DT data for 2141 and 2140 */
  if (A && A2) {
    EOS_REAL max_reldiff = 0.;
    printf ("\n\"--- Tabulated Free Energy (EOS_At_DT) data comparison (relative difference) for materials %i and %i ---\"\n", matID[0], matID[1]);
    printf("%23s", " \"R       |       T ->\"");
    for (j = 0; j < NT; j++)
      printf("%23.15e", T[j]);
    printf("\n");
    for (j = 0; j < NR; j++) {
      int k;
      printf("%23.15e", R[j]);
      for (k = 0; k < NT; k++) {
	EOS_REAL rel_diff;
	rel_diff = (A[j+k*NR]-A2[j+k*NR])/((A[j+k*NR] != 0.0) ? A[j+k*NR] : 1.0);
	if (fabs(max_reldiff) < fabs(rel_diff))
	  max_reldiff = rel_diff;
	printf("%23.15e", rel_diff);
      }
      printf(" fcmp_ignore\n");
    }
    printf("max_reldiff = %23.15e fcmp_ignore\n", max_reldiff);
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

  /* deallocate up temporary memory */
  EOS_FREE(R);
  EOS_FREE(T);
  EOS_FREE(P);
  EOS_FREE(U);
  EOS_FREE(A);
  EOS_FREE(A2);
  EOS_FREE(S);

  return 0;

}
