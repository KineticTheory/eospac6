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
 *  \brief Ensure the inverse interpolation problem does not recur.
 *         See SourceForge© Issue #artf38112 for more details:
 *         https://tf.lanl.gov/sf/go/artf38112
 *
 * \note
 * Material ID 2 tested exists in tests/data/zr_bin.2.mod
 * REQUIRED FILE:  data/zr_bin.2.mod
 * MATIDS TO TEST: 2
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
  EOS_INTEGER errorCode, max_length;
  EOS_INTEGER nTables = 2, nXYPairs = 1;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR *fileName = NULL, *tmp = NULL;
  EOS_REAL *X=NULL, *Y=NULL, *F=NULL, *dFx=NULL, *dFy=NULL;

  char *indexFileName = "sesameFilesDir.txt";
  FILE *fp = NULL;

  errorCode = EOS_OK;

  tableType[0] = EOS_Ut_DT;
  tableType[1] = EOS_T_DUt;
  matID[0] = 2;
  matID[1] = 2;

  /* Modify sesameFilesDir.txt to include only one valid file name and the END token */
  eos_GetMaxDataFileNameLength (&max_length);
  fileName = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  strcpy(fileName, "./tests/data/zr_bin.2.mod");
  for (i = 0; i < 10; i++) {
    tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
    if (_eos_fileExistsAndValid(fileName)) break;
    fileName = (EOS_CHAR *) realloc (fileName, (strlen(fileName) + 4) * sizeof (EOS_CHAR));
    sprintf(tmp, "./.%s", fileName);
    strcpy(fileName, tmp);
    EOS_FREE (tmp);
  }
  EOS_FREE (tmp);
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s\n\n    END    \n", fileName); /* append END token with leading and trailing white space */
    fclose (fp);              /* close indexFileName */
    EOS_FREE (fileName);
  }
  else {
    errorCode = -2;
    EOS_FREE (fileName);
    return(errorCode);
  }

  /* initialize table handle */
  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %d: %s\n", errorCode, errorMessage);
    return(errorCode);
  }
  /* load data results in an expected error */
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* set data dump options */
  i = 0;
  eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    return 0;
  }
  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  /* load data */
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %d: %s\n", errorCode, errorMessage);
  }

  /* attempt to interpolate data associated with each table handle */
  X = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
  Y = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
  F = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
  dFx = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
  dFy = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));

  if (! (Y && F && dFx && dFy)) {
    printf ("Memory allocation error!\n");
    return 2;
  }

  for (i = 0; i < nTables; i++) {

    if (i > 0)
      for (j = 0; j < 163; j++)
	printf("-");
    printf("\nTH %d: %s for material %d from file %s\n",
	   tableHandle[i], get_tableType_str(tableType[i]), matID[i],
	   get_tableHandleFileName (tableHandle[i]));

    /* Interpolate TH 0 */
    if (i == 0) {
      X[0] = 6.;
      Y[0] = 298.15;
    }
    else {
      X[0] = 6.;
      Y[0] = F[0];
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
	for (j = 0; j < nXYPairs; j++) {
	  printf ("\ti=%i\tX = %23.15e, Y = %23.15e, F = %23.15e, dFx = %23.15e, dFy = %23.15e\n",
		  j, X[j], Y[j], F[j], dFx[j], dFy[j]);
	}
      }
    }

  }

  /* deallocate memory */
  if (X) EOS_FREE(X);
  if (Y) EOS_FREE(Y);
  if (F) EOS_FREE(F);
  if (dFx) EOS_FREE(dFx);
  if (dFy) EOS_FREE(dFy);

  /* destroy objects associated with all table handles */
  eos_DestroyAll (&errorCode);

  return 0;

}
