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
 *  \brief Perform the following tests:
 *    -# Ensure tables can be created, loaded and interpolated with the EOS_CALC_FREE_ENERGY
 *       option both disabled and enabled. See SourceForge<A9> Issue #artf8688 for more details:
 *       https://tf.lanl.gov/sf/go/artf8688
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i, j;
  EOS_INTEGER one = 1;
#define N_TABLES 2
  enum { nTablesE = N_TABLES };

  EOS_INTEGER tableType[nTablesE] = {
    EOS_At_DT,
    EOS_At_DT
  };
  EOS_CHAR*tableTypeStr[nTablesE] = {
    "EOS_At_DT",
    "EOS_At_DT"
  };

  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER nTables;

  EOS_INTEGER nXYPairs, *xyBounds;
  EOS_REAL *xVals, *yVals, *fVals, *dFx, *dFy;

  nTables = nTablesE;

  for (i = 0; i < nTables; i++)
    matID[i] = 3720;

  errorCode = EOS_OK;
  printf ("Results for %d\n", matID[0]);
  printf ("TH\n--\n");
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
    printf ("%2i. creating table %s (%i)\n", i, tableTypeStr[i],
	    tableType[i]);
    eos_CreateTables (&one, &tableType[i], &matID[i], &tableHandle[i],
		      &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      return -1;
    }
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    if (i == 0) {
      eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr,
		     &errorCode);
    }
    else {
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
		     &errorCode);
      eos_SetOption (&tableHandle[i], &EOS_CALC_FREE_ENERGY, EOS_NullPtr,
		     &errorCode);
    }
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return -1;
    }
  }

  printf ("\nTH\n--\n");
  for (i = 0; i < nTables; i++) {
    printf ("%2i. loading table %s (%i)\n", tableHandle[i], tableTypeStr[i],
	    tableType[i]);
    eos_LoadTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
	eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	printf ("    eos_LoadTables ERROR %i: %s\n", tableHandleErrorCode,
		errorMessage);
      }
    }
  }

  /* Interpolate some data */
  nXYPairs = 4;
  xyBounds = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
  xVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  yVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  fVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFx = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFy = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  for (j = 0; j < nXYPairs; j++) {
    xVals[j] = (EOS_REAL) (j + 1) * 10.0;
    yVals[j] = (EOS_REAL) (j + 1) * 100.0;
  }

  for (i = 0; i < nTables; i++) {
    printf ("\nInterpolating %s data\n", tableTypeStr[i]);
    eos_Interpolate (&tableHandle[i], &nXYPairs, xVals, yVals, fVals, dFx, dFy,
		     &errorCode);

    if (errorCode != EOS_OK) {
      if (errorCode == EOS_INTERP_EXTRAPOLATED) {
	eos_CheckExtrap (&tableHandle[i], &nXYPairs, xVals, yVals, xyBounds,
			 &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage (&errorCode, errorMessage);
	  printf ("eos_CheckExtrap ERROR: %s\n", errorMessage);
	  return 1;
	}
	for (j = 0; j < nXYPairs; j++)
	  printf ("\tX = %.15e, Y = %.15e, F = %.15e, extrapCode = %d\n",
		  xVals[j], yVals[j], fVals[j], xyBounds[j]);
      }
      else {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_Interpolate ERROR: %s\n", errorMessage);
      }
    }
    else {
      for (j = 0; j < nXYPairs; j++)
	printf ("\tX = %.15e, Y = %.15e, F = %.15e\n", xVals[j], yVals[j],
		fVals[j]);

    }
  }

  eos_DestroyAll (&errorCode);

  printf ("\n");

  free(xyBounds);
  free(xVals);
  free(yVals);
  free(fVals);
  free(dFx);
  free(dFy);

  return 0;

}
