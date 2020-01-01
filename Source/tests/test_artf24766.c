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
 *  \brief Test code to exercise the Taylor Polynomial class methods and attributes
 *         via the public interface's EOS_USE_TAYLOR_FIT option.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_SetOption
 * eos_GetErrorMessage
 * eos_GetErrorCode
 * eos_LoadTables
 *
 * \note
 * REQUIRED FILE:  data/ses3720_taylor
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <assert.h>
#include <string.h>

#define EOS_FREE(p) { assert(p != NULL); free(p); p=NULL; }

/* prototypes */
EOS_BOOLEAN _eos_fileExistsAndValid(EOS_CHAR *filename);

EOS_INTEGER main() {

  enum { nTablesE = 8 };

  int i, j;

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, max_length;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_CHAR *req_filename = NULL, *tmp = NULL;

  nTables = nTablesE;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_Ut_DT;
  tableType[2] = EOS_At_DT;
  tableType[3] = EOS_St_DT;
  tableType[4] = EOS_Pt_DT;
  tableType[5] = EOS_Ut_DT;
  tableType[6] = EOS_At_DT;
  tableType[7] = EOS_St_DT;

  matID[0] = 3720;
  matID[1] = 3720;
  matID[2] = 3720;
  matID[3] = 3720;
  matID[4] = 3720;
  matID[5] = 3720;
  matID[6] = 3720;
  matID[7] = 3720;

  /* find REQUIRED FILE */
  eos_GetMaxDataFileNameLength (&max_length);
  req_filename = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  strcpy(req_filename, "./tests/data/ses3720_taylor");
  tmp = (EOS_CHAR *) safe_malloc(max_length, sizeof(EOS_CHAR));
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(req_filename)) break;
    req_filename = (EOS_CHAR *) realloc (req_filename, (strlen(req_filename) + 4) * sizeof (EOS_CHAR));
    sprintf(tmp, "./.%s", req_filename);
    strcpy(req_filename, tmp);
  }
  EOS_FREE(tmp);

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
    goto CLEANUP;
  }

  for (i = 0; i < nTables/2; i++) {
    if (i == 0)
      eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    else
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      goto CLEANUP;
    }

    /* set the required SESAME file name for all table handles */
    eos_SetDataFileName (&tableHandle[i], &matID[i], &tableType[i], req_filename, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetDataFilename ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      goto CLEANUP;
    }

    eos_SetOption (&tableHandle[i], &EOS_USE_TAYLOR_FIT, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      goto CLEANUP;
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
    goto CLEANUP;
  }


  {
    enum { nXYPairsE = 1000 };
    EOS_INTEGER nXYPairs = nXYPairsE;
    EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE], dFy[nXYPairsE];
    EOS_REAL xMin = 2.7e-06;
    EOS_REAL xMax = 5.4e04;
    EOS_REAL yMin = 0;
    EOS_REAL yMax = 1.160485e09;
    EOS_INTEGER xyBounds[nXYPairsE];

    generate_Log10DistributedPoints(X, Y, nXYPairs, xMin, xMax, yMin, yMax);

    for (i = 0; i < nTables; i++) {

      for (j = 0; j < nXYPairs; j++)
	xyBounds[j] = EOS_OK;

      if (i < nTables/2)
	printf ("\n--- Evaluate Taylor Surface Fit using tableType %s for material %d ---\n", get_tableType_str(tableType[i]), matID[i]);
      else
	printf ("\n--- Interpolate using tableType %s for material %d ---\n", get_tableType_str(tableType[i]), matID[i]);

      eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy, &errorCode);

      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	if (errorCode != EOS_INTERP_EXTRAPOLATED) {
	  printf ("%d: %s\n", errorCode, errorMessage);
	  continue;
	}
	eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, xyBounds, &errorCode);
      }

      for (j = 0; j < nXYPairs; j++) {
	printf
	  ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e",
	   j, X[j], Y[j], F[j], dFx[j], dFy[j]);
	printf (" %s\n", ((xyBounds[j]!=EOS_OK) ? ERROR_TO_TEXT(xyBounds[j]) : ""));
      }
    }
  }

 CLEANUP:
  eos_DestroyAll (&errorCode);
  EOS_FREE(req_filename);

  return 0;

}
