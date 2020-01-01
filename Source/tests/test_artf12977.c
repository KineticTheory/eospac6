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
 *  \brief Test packing and unpacking of data to verify issue #artf12977 is resolved.
 *         See <a href="https://tf.lanl.gov/sf/go/artf12977">artf12977</a> for details.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_DestroyAll
 * eos_DestroyTables
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_GetPackedTables
 * eos_GetPackedTablesSize
 * eos_LoadTables
 * eos_SetOption
 * eos_SetPackedTables
 *
 * \note
 * MATIDS TO TEST: 2023 2023 32023 32023
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <assert.h>

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

#ifdef _DEBUG_PACKING_FUNCTIONS
  extern EOS_INTEGER packedBytesResetOverride;
#endif

extern EOS_BOOLEAN disable_SetPackedTablesPrint;

void writeMessage(EOS_CHAR* m, EOS_CHAR *mode) {
  FILE *tableFile = NULL;
  EOS_CHAR *fname = "TablesLoaded.dat";

  printf("%s", m);
  tableFile = fopen(fname, mode);
  assert(tableFile != NULL);
  fprintf(tableFile, "%s", m);
  fclose(tableFile);
  tableFile = NULL;
}

int main ()
{
  int i, j;
  EOS_REAL *X, *Y, *FXY, *dFx, *dFy, *Y2, *X2;
  enum
  { nTablesE = 4 };

  EOS_INTEGER tableType[] = { EOS_Pt_DT, EOS_Ut_DT, EOS_Tm_D, EOS_Tf_D };
  EOS_INTEGER matID[]     = { 2023,      2023,      32023,    32023    };

  EOS_INTEGER *tableHandle;

  EOS_INTEGER errorCode;

  EOS_INTEGER nTables, nXYPairs, packedTablesSize[nTablesE], one = 1;
  EOS_INTEGER nTables0, nHandles0, nAlloc0;

  EOS_CHAR *packedData[nTablesE];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = sizeof(tableType)/sizeof(tableType[0]);
  nXYPairs = 8;

  tableHandle = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nTables);

  writeMessage("\n====== LOAD AND PACK ALL TABLES SIMULTANEOUSLY ======\n", "w");

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return errorCode;
    }
  }

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  nTables0  = get_no_tables();
  nHandles0 = get_no_handles();
  nAlloc0 = get_no_alloc();

  eos_GetPackedTablesSize (&nHandles0, tableHandle, &packedTablesSize[0], &errorCode);

  printf("*** main::BEFORE PACKING: nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	 nTables0, nHandles0, nAlloc0);

  packedData[0] = (EOS_CHAR *) malloc (packedTablesSize[0]);

  eos_GetPackedTables (&nHandles0, tableHandle, packedData[0], &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_GetPackedTables ERROR %i: %s\n", errorCode, errorMessage);
    EOS_FREE (packedData[0]);
    return errorCode;
  }

  nTables0  = get_no_tables();
  nHandles0 = get_no_handles();
  nAlloc0 = get_no_alloc();

  printf("*** main::AFTER PACKING: nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	 nTables0, nHandles0, nAlloc0);

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
    EOS_FREE (packedData[0]);
    return errorCode;
  }

  printf("*** main::BEFORE UNPACKING: nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	 nTables0, nHandles0, nAlloc0);

  eos_SetPackedTables (&nHandles0, &packedTablesSize[0], packedData[0], tableHandle,
		       &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetPackedTables ERROR %i: %s\n", errorCode, errorMessage);
    EOS_FREE (packedData[0]);
    return errorCode;
  }

  nTables0  = get_no_tables();
  nHandles0 = get_no_handles();
  nAlloc0 = get_no_alloc();

  printf("*** main::AFTER UNPACKING: nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	 nTables0, nHandles0, nAlloc0);

  EOS_FREE (packedData[0]);

  /* allocate memory for interpolation */
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y2 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  X2 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  FXY = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  X[0] = .93;
  X[1] = 2;
  X[2] = 2;
  X[3] = 2;
  X[4] = 1.15;
  X[5] = .2;
  X[6] = .2;
  X[7] = .2;

  Y[0] = 2000;
  Y[1] = 2000;
  Y[2] = 5000;
  Y[3] = 6000;
  Y[4] = 6000;
  Y[5] = 6000;
  Y[6] = 4975;
  Y[7] = 1050;

  /* testing interpolation methods */

  for (j = 0; j < nTables; j++) {
    printf ("\n--- TEST eos_Interpolate using category %i tableType: %s ---\n",
	    get_dataTypeCategory(tableType[j]), get_tableType_str(tableType[j]));
    eos_Interpolate (&tableHandle[j], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
      continue;
    }
    printf ("%s Interpolation Results:\n", get_tableType_str(tableType[j]));
    for (i = 0; i < nXYPairs; i++) {
      printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
	      i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }
  }

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
    EOS_FREE (packedData[0]);
    return errorCode;
  }



  writeMessage("\n====== LOAD AND PACK ALL TABLES SEQUENTIALLY (use eos_DestroyAll in loop) ======\n", "a");

  for (i = 0; i < nTables; i++) {

    printf("\n--- i=%d ------------------------------------------\n", i);

    eos_CreateTables (&one, &tableType[i], &matID[i], &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    /* Enable data dump to file */
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    eos_LoadTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    eos_GetPackedTablesSize (&one, &tableHandle[i], &packedTablesSize[i], &errorCode);

    printf("*** main::BEFORE PACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    packedData[i] = (EOS_CHAR *) malloc (packedTablesSize[i]);

    eos_GetPackedTables (&one, &tableHandle[i], packedData[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_GetPackedTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      EOS_FREE (packedData[i]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::AFTER PACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    printf("*** main::DESTROY ALL\n");
    eos_DestroyAll (&errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      EOS_FREE (packedData[i]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::BEFORE UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    for (j = 0; j <= i; j++) {
      eos_SetPackedTables (&one, &packedTablesSize[j], packedData[j], &tableHandle[j],
                           &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetPackedTables ERROR %i: %s for tableHandle=%i\n",
                errorCode, errorMessage, tableHandle[j]);
        return errorCode;
      }
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::AFTER UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

  }
  for (i = 0; i < nTables; i++) {
    EOS_FREE (packedData[i]);
  }

  /* testing interpolation methods */

  for (j = 0; j < nTables; j++) {
    printf ("\n--- TEST eos_Interpolate using category %i tableType: %s ---\n",
	    get_dataTypeCategory(tableType[j]), get_tableType_str(tableType[j]));
    eos_Interpolate (&tableHandle[j], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
      continue;
    }
    printf ("%s Interpolation Results:\n", get_tableType_str(tableType[j]));
    for (i = 0; i < nXYPairs; i++) {
      printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
	      i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }
  }

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s for tableHandle=%i\n",
	    errorCode, errorMessage, tableHandle[i]);
    return errorCode;
  }



  writeMessage("\n====== LOAD AND PACK ALL TABLES INDIVIDUALLY (store in packedData[i]) ======\n", "a");

  for (i = 0; i < nTables; i++) {

#ifdef _DEBUG_PACKING_FUNCTIONS
    if (i > 0)
      packedBytesResetOverride = EOS_TRUE;
    else
      packedBytesResetOverride = EOS_FALSE;
    printf("\n\tpackedBytesResetOverride = %i\n", packedBytesResetOverride);
#endif

    printf("\n--- i=%d ------------------------------------------\n", i);

    eos_CreateTables (&one, &tableType[i], &matID[i], &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    /* Enable data dump to file */
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    eos_LoadTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    eos_GetPackedTablesSize (&one, &tableHandle[i], &packedTablesSize[i], &errorCode);

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::BEFORE PACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    packedData[i] = (EOS_CHAR *) malloc (packedTablesSize[i]);

    eos_GetPackedTables (&one, &tableHandle[i], packedData[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_GetPackedTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      for (j = 0; j < nTables; j++)
	EOS_FREE (packedData[j]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::AFTER PACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);
  }

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s for tableHandle=%i\n",
	    errorCode, errorMessage, tableHandle[i]);
    return errorCode;
  }



  writeMessage("\n====== UNPACK PACK ALL TABLES INDIVIDUALLY (use stored packedData[i]) ======\n", "a");
  writeMessage("====== This emulates a child processor receiving an MPI broadcast.    ======\n", "a");

  disable_SetPackedTablesPrint = EOS_FALSE;

  for (i = 0; i < nTables; i++) {

    printf("\n--- i=%d ------------------------------------------\n", i);

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::BEFORE UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    eos_SetPackedTables (&one, &packedTablesSize[i], packedData[i], &tableHandle[i],
			 &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetPackedTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      for (j = 0; j < nTables; j++)
	EOS_FREE (packedData[j]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::AFTER UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    EOS_FREE (packedData[i]);

  }

  disable_SetPackedTablesPrint = EOS_TRUE;

  /* testing interpolation methods */

  for (j = 0; j < nTables; j++) {
    printf ("\n--- TEST eos_Interpolate using category %i tableType: %s ---\n",
	    get_dataTypeCategory(tableType[j]), get_tableType_str(tableType[j]));
    eos_Interpolate (&tableHandle[j], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
      continue;
    }
    printf ("%s Interpolation Results:\n", get_tableType_str(tableType[j]));
    for (i = 0; i < nXYPairs; i++) {
      printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
	      i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }
  }

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s for tableHandle=%i\n",
	    errorCode, errorMessage, tableHandle[i]);
    return errorCode;
  }



#if 1
#ifdef _DEBUG_PACKING_FUNCTIONS
  packedBytesResetOverride = EOS_FALSE;
#endif

  writeMessage("\n====== LOAD AND PACK ALL TABLES INDIVIDUALLY (use eos_DestroyTables in loop) ======\n", "a");

  for (i = 0; i < nTables; i++) {

    printf("\n--- i=%d ------------------------------------------\n", i);

    eos_CreateTables (&one, &tableType[i], &matID[i], &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    /* Enable data dump to file */
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    eos_LoadTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    eos_GetPackedTablesSize (&one, &tableHandle[i], &packedTablesSize[i], &errorCode);

    printf("*** main::BEFORE PACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    packedData[i] = (EOS_CHAR *) malloc (packedTablesSize[i]);

    eos_GetPackedTables (&one, &tableHandle[i], packedData[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_GetPackedTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      for (j = 0; j < nTables; j++)
	EOS_FREE (packedData[j]);
      return errorCode;
    }

    eos_DestroyTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_DestroyTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      for (j = 0; j < nTables; j++)
	EOS_FREE (packedData[j]);
      return errorCode;
    }

  }

  writeMessage("\n====== UNPACK ALL TABLES INDIVIDUALLY ======\n", "a");

  for (i = 0; i < nTables; i++) {

    printf("\n--- i=%d ------------------------------------------\n", i);

    printf("*** main::BEFORE UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    eos_SetPackedTables (&one, &packedTablesSize[i], packedData[i], &tableHandle[i],
			 &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetPackedTables ERROR %i: %s for tableHandle=%i\n",
	      errorCode, errorMessage, tableHandle[i]);
      for (j = 0; j < nTables; j++)
	EOS_FREE (packedData[j]);
      return errorCode;
    }

    nTables0  = get_no_tables();
    nHandles0 = get_no_handles();
    nAlloc0 = get_no_alloc();

    printf("*** main::AFTER UNPACKING: i=%d, nTables0=%d, nHandles0=%d, nAlloc0=%d\n",
	   i, nTables0, nHandles0, nAlloc0);

    EOS_FREE (packedData[i]);

  }


  /* testing interpolation methods */

  for (j = 0; j < nTables; j++) {
    printf ("\n--- TEST eos_Interpolate using category %i tableType: %s ---\n",
	    get_dataTypeCategory(tableType[j]), get_tableType_str(tableType[j]));
    eos_Interpolate (&tableHandle[j], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
      continue;
    }
    printf ("%s Interpolation Results:\n", get_tableType_str(tableType[j]));
    for (i = 0; i < nXYPairs; i++) {
      printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
	      i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }
  }

  printf("*** main::DESTROY ALL\n");
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s for tableHandle=%i\n",
	    errorCode, errorMessage, tableHandle[i]);
    return errorCode;
  }

#endif

  EOS_FREE (X);
  EOS_FREE (Y);
  EOS_FREE (Y2);
  EOS_FREE (X2);
  EOS_FREE (FXY);
  EOS_FREE (dFx);
  EOS_FREE (dFy);
  EOS_FREE (tableHandle);

  return 0;

}
