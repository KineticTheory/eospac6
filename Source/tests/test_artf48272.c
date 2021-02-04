/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests quick
 *  \brief Ensure the new eos_SetDataFileName and eos_GetMaxDataFileNameLength
 *         work as expected for ASCII data file.
 *         See SourceForge© Issue #artf48272 for more details:
 *         https://tf.lanl.gov/sf/go/artf48272
 *
 * \note
 * MATIDS TO TEST: none
* 99999
 * REQUIRED FILE: data/sesame_small.ascii
 * REQUIRED FILE: data/sesame_medium.ascii
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <assert.h>
#include <math.h>
#include <string.h>

#define EOS_FREE(p) { assert(p != NULL); free(p); p=NULL; }

EOS_BOOLEAN _eos_fileExistsAndValid(EOS_CHAR *filename); /* function in eos_Utils.c */


int main ()
{
  int i, j;
  enum {nTablesE = 3};
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE], tableType[nTablesE];
  EOS_INTEGER errorCode, max_length;
  EOS_INTEGER nTables = nTablesE;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR *fileName = NULL, *tmp = NULL;
  EOS_REAL *X=NULL, *Y=NULL, *F=NULL;

  EOS_CHAR *files[] = {
    "./tests/data/sesame_small.ascii",
    "./tests/data/sesame_medium.ascii",
    "./tests/data/sesame_big.ascii" /* DAP:2020-05-09 do not use this file, since it's unsupported in ses_io, which has insufficient error handling for ASCII */
  };
  EOS_INTEGER materialIDinFile[] = {
    99999,
    99999,
    99999
  };
  int whichFileIndexToUse = 0;
  int filesL;

  errorCode = EOS_OK;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_Ut_DT;
  tableType[2] = EOS_At_DT;

  filesL = sizeof(materialIDinFile) / sizeof(materialIDinFile[0]);

  for (whichFileIndexToUse = 0; whichFileIndexToUse < filesL; whichFileIndexToUse++) {

    if (whichFileIndexToUse == filesL-1)
      break; /* DAP:2020-05-09 do not use this file, since it's unsupported in ses_io, which has insufficient error handling for ASCII */

    for (i = 0; i < nTables; i++) matID[i] = materialIDinFile[whichFileIndexToUse];

    /* define custom file name */
    eos_GetMaxDataFileNameLength (&max_length);
    fileName = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
    tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
    strcpy(fileName, files[whichFileIndexToUse]);
    for (i = 0; i < 10; i++) {
      if (_eos_fileExistsAndValid(fileName)) break;
      fileName = (EOS_CHAR *) realloc (fileName, (strlen(fileName) + 4) * sizeof (EOS_CHAR));
      sprintf(tmp, "./.%s", fileName);
      strcpy(fileName, tmp);
    }
    EOS_FREE(tmp);

    /* initialize table handle */
    eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %d: %s\n", errorCode, errorMessage);
      return(errorCode);
    }

    for (i = 0; i < nTables; i++) {

      /* set data dump options */
      if (whichFileIndexToUse == 0 && i == 0)
        eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
      else
        eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        return 0;
      }

      /* set custom matID-to-file association */
      eos_SetDataFileName (&tableHandle[i], &matID[i], &tableType[i], fileName, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_SetDataFileName ERROR %d: %s\n", errorCode, errorMessage);
      }

    }

    /* load data */
    eos_LoadTables (&nTables, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %d: %s\n", errorCode, errorMessage);
      if (whichFileIndexToUse == filesL-1) errorCode = EOS_OK; /* reset after expected failure */
      goto CLEANUP;
    }

    /* fetch data associated with each table handle */
    for (i = 0; i < nTables; i++) {
      EOS_INTEGER infoItem, one = 1, NR, NT, NRNT;
      EOS_REAL val;

      infoItem = EOS_NR;
      eos_GetTableInfo (&tableHandle[i], &one, &infoItem, &val, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
                tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
        return errorCode;
      }
      NR = (EOS_INTEGER) val;

      infoItem = EOS_NT;
      eos_GetTableInfo (&tableHandle[i], &one, &infoItem, &val, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
                tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
        return errorCode;
      }
      NT = (EOS_INTEGER) val;

      NRNT = NR*NT;

      X = (EOS_REAL *) safe_malloc (NR,    sizeof (EOS_REAL));
      Y = (EOS_REAL *) safe_malloc (NT,    sizeof (EOS_REAL));
      F = (EOS_REAL *) safe_malloc (NRNT, sizeof (EOS_REAL));

      if (! (X && Y && F)) {
        printf ("Memory allocation error!\n");
        return 2;
      }

      infoItem = EOS_R_Array;
      eos_GetTableInfo (&tableHandle[i], &NR, &infoItem, X, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
                tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
        return errorCode;
      }

      infoItem = EOS_T_Array;
      eos_GetTableInfo (&tableHandle[i], &NT, &infoItem, Y, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
                tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
        return errorCode;
      }

      infoItem = EOS_F_Array;
      eos_GetTableInfo (&tableHandle[i], &NRNT, &infoItem, F, &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
                tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
        return errorCode;
      }

      if (i > 0)
        for (j = 0; j < 29*5; j++)
          printf("-");
      printf("\nTH %d: %s for material %d from file %s\n",
             tableHandle[i], get_tableType_str(tableType[i]), matID[i],
             get_tableHandleFileName (tableHandle[i]));

      printf("X = [\n");
      for (j = 0; j < NR; j++) {
        printf("     %24.15e", X[j]);
        if ((j + 1) % 5 == 0)
          printf("\n");
      }
      printf("]\n\n");

      printf("Y = [\n");
      for (j = 0; j < NT; j++) {
        printf("     %24.15e", Y[j]);
        if ((j + 1) % 5 == 0)
          printf("\n");
      }
      printf("]\n\n");

      printf("F = [\n");
      for (j = 0; j < NRNT; j++) {
        printf("     %24.15e", F[j]);
        if ((j + 1) % 5 == 0)
          printf("\n");
      }
      printf("]\n\n");

      /* deallocate memory */
      if (X) EOS_FREE(X);
      if (Y) EOS_FREE(Y);
      if (F) EOS_FREE(F);
    }

  CLEANUP:
    /* deallocate memory */
    EOS_FREE(fileName);

    /* destroy objects associated with all table handles */
    eos_DestroyAll (&errorCode);

    if (whichFileIndexToUse < filesL - 1) {
      for (j = 0; j < 29*5; j++)
        printf("=");
      printf("\n");
    }

  }

  return errorCode;

}
