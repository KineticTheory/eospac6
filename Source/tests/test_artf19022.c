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
 *  \brief Determine the limited list of dat types allowed by eos_Mix.
 *
 * \note
 * MATIDS TO TEST: 2161 13718 23713
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <assert.h>
#include <math.h>

#define EOS_FREE(p) { assert(p != NULL); free(p); p=NULL; }


int main ()
{
  extern EOS_INTEGER MAX_TYPES;

  int i, j, k;
  EOS_INTEGER matID;
  EOS_INTEGER tableHandle, tableType, tableNum;
  EOS_INTEGER errorCode;
  EOS_INTEGER cat, subCategory;
  EOS_INTEGER nTables = 1, nXYPairs = 4;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_REAL *X=NULL, *Y=NULL, *F=NULL, *dFx=NULL, *dFy=NULL, *concInMix=NULL;

  tableHandle = 0;

  for (k = 0; k < MAX_TYPES; k++) {

    errorCode = EOS_OK;

    tableType = get_eosDataType(k);
    tableNum = get_eosTableNum(k);
    cat = get_dataTypeCategory(tableType);
    subCategory = get_dataTypeSubCategory(tableType);

    if (cat != 0 && cat != 1 && cat != 2 && cat != 3 && cat != 4)
      continue;

    if (subCategory == 0)
      continue;

    switch (tableNum) {
    case 301:
    case 303:
    case 304:
    case 305:
    case 306:
    case 311:
    case 321:
    case 401:
    case 411:
    case 412:
    case 431:
      matID = 2161; /* tin stored locally */
      break;
    case 501:
    case 502:
    case 503:
    case 504:
    case 505:
      matID = 13718; /* aluminum stored on server */
      break;
    case 601:
    case 602:
    case 603:
    case 604:
    case 605:
      matID = 23713; /* aluminum stored on server */
      break;
    default:
      continue;
    }

    /* initialize table handle */
    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %d: %s for type %d, %s (fcmp_ignore)\n",
	      errorCode, errorMessage, tableType, get_tableType_str(tableType));
      return errorCode;
    }

    /* load data */
    eos_LoadTables (&nTables, &tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %d: %s for type %d, %s (fcmp_ignore)\n",
	      errorCode, errorMessage, tableType, get_tableType_str(tableType));
    }

    /* attempt to mix data associated with each table handle */
    for (i = 0; i < nTables; i++) {

      X = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
      Y = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
      F = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
      dFx = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
      dFy = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
      concInMix = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));

      for (j = 0; j < nXYPairs; j++)
	concInMix[j] = 1.0;

      if (! (Y && F && dFx && dFy)) {
	printf ("Memory allocation error!\n");
	return 2;
      }

      eos_Mix (&nTables, &tableHandle, &nXYPairs, concInMix, X, Y, F, dFx, dFy, &errorCode);

      if (errorCode == EOS_BAD_DATA_TYPE) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_Mix ERROR %d: %s for type %d, %s\n",
		errorCode, errorMessage, tableType, get_tableType_str(tableType));
      }
      else
	printf ("valid data type %d, %s\n", tableType, get_tableType_str(tableType));

      /* deallocate memory */
      if (X) EOS_FREE(X);
      if (Y) EOS_FREE(Y);
      if (F) EOS_FREE(F);
      if (dFx) EOS_FREE(dFx);
      if (dFy) EOS_FREE(dFy);
      if (concInMix) EOS_FREE(concInMix);
    }

    /* destroy objects associated with all table handles */
    eos_DestroyAll (&errorCode);

  }

  return 0;

}
