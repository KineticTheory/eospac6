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
 *         work as expected. See SourceForge© Issue #artf16562 for more details:
 *         https://tf.lanl.gov/sf/go/artf16562
 *
 * \note
 * MATIDS TO TEST: 93720
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
  EOS_INTEGER matID[2];
  EOS_INTEGER tableHandle[2], tableType[2];
  EOS_INTEGER errorCode, err, max_length;
  EOS_INTEGER nTables = 2, nXYPairs = 4;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR *fileName = NULL, *tmp = NULL;
  EOS_REAL *X=NULL, *Y=NULL, *F=NULL, *dFx=NULL, *dFy=NULL;

  char *indexFileName = "sesameFilesDir.txt";
  FILE *fp = NULL;
  EOS_CHAR *file_str = NULL;

  errorCode = EOS_OK;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_Pt_DT;
  matID[0] = 93720;
  matID[1] = 93720;

  /*  Read sesameFilesDir.txt into memory */
  errorCode = get_fileContent(indexFileName, &file_str);
  if(errorCode)
    goto CLEANUP;

  /* Modify sesameFilesDir.txt to include only one valid file name and the END token */
  eos_GetMaxDataFileNameLength (&max_length);
  fileName = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  strcpy(fileName, "./tests/data/sesame3");
  tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(fileName)) break;
    sprintf(tmp, "./.%s", fileName);
    strcpy(fileName, tmp);
  }
  EOS_FREE(tmp);
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s\n\n    END    \n", fileName); /* append END token with leading and trailing white space */
    fclose (fp);              /* close indexFileName */
  }
  else {
    errorCode = -2;
    goto CLEANUP;
  }

  /* initialize table handle */
  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %d: %s\n", errorCode, errorMessage);
    goto CLEANUP;
  }
  /* load data results in an expected error */
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    for (i = 0; i < nTables; i++) {
      EOS_INTEGER th_err = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &th_err);
      eos_GetErrorMessage (&th_err, errorMessage);
      printf ("eos_LoadTables ERROR %d: %s\n", th_err, errorMessage);
    }
  }

  /* define custom matID-to-file association */
  strcpy(fileName, "./tests/data/93270littlebin");
  tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(fileName)) break;
    sprintf(tmp, "./.%s", fileName);
    strcpy(fileName, tmp);
  }
  EOS_FREE(tmp);
  eos_SetDataFileName (&tableHandle[0], &matID[0], &tableType[0], fileName, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetDataFileName ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* define custom matID-to-file association */
  strcpy(fileName, "./tests/data/testbin");
  tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(fileName)) break;
    sprintf(tmp, "./.%s", fileName);
    strcpy(fileName, tmp);
  }
  EOS_FREE(tmp);
  eos_SetDataFileName (&tableHandle[1], &matID[1], &tableType[1], fileName, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetDataFileName ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* set data dump options */
  i = 0;
  eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    errorCode = 0;
    goto CLEANUP;
  }
  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      errorCode = 0;
      goto CLEANUP;
    }
  }

  /* load data */
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* attempt to interpolate data associated with each table handle */
  for (i = 0; i < nTables; i++) {

    if (i > 0)
      for (j = 0; j < 163; j++)
	printf("-");
    printf("\nTH %d: %s for material %d from file %s\n",
	   tableHandle[i], get_tableType_str(tableType[i]), matID[i],
	   get_tableHandleFileName (tableHandle[i]));

    X = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    Y = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    F = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    dFx = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    dFy = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));

    if (! (Y && F && dFx && dFy)) {
      printf ("Memory allocation error!\n");
      errorCode = 2;
      goto CLEANUP;
    }

    X[0] = 1000.;
    Y[0] = 500.;
    for (j = 1; j < nXYPairs; j++) {
      X[j] = X[j-1] + 1500.;
      Y[j] = Y[j-1] + 150.;
    }
    
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy, &errorCode);

    {
      EOS_INTEGER extrapCode = EOS_INTERP_EXTRAPOLATED;
      EOS_BOOLEAN extrapolated;
      eos_ErrorCodesEqual(&errorCode, &extrapCode, &extrapolated);

      if (errorCode != EOS_OK && ! extrapolated) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_Interpolate ERROR %d: %s for type %d, %s\n",
		errorCode, errorMessage, tableType[i], get_tableType_str(tableType[i]));
      }
      else {
	for (j = 1; j < nXYPairs; j++) {
	  printf ("\ti=%i\tX = %23.15e, Y = %23.15e, F = %23.15e, dFx = %23.15e, dFy = %23.15e\n",
		  j, X[j], Y[j], F[j], dFx[j], dFy[j]);
	}
      }
    }

    /* deallocate memory */
    EOS_FREE(X);
    EOS_FREE(Y);
    EOS_FREE(F);
    EOS_FREE(dFx);
    EOS_FREE(dFy);
  }

  /* test some error handling */
  printf("\nThe following error message is expected:\n");
  eos_SetDataFileName (&tableHandle[1], &matID[1], &tableType[1], fileName, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetDataFileName ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* reset errorCode since everything has been successfully handled so far */
  errorCode = 0;

 CLEANUP:
  /* destroy objects associated with all table handles */
  eos_DestroyAll (&err);

  /*  Write file_str to sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s", file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    errorCode = -3;
  }

  EOS_FREE(fileName);
  EOS_FREE(file_str);

  return errorCode;

}
