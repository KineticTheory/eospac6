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
 *  \brief Ensure Comments and Info can be packed and unpacked.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf26182"></a>
 *  for details.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_DestroyAll
 * eos_DestroyTables
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_GetPackedTables
 * eos_GetPackedTablesSize
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 * eos_SetPackedTables
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

int main ()
{
  int i;
  EOS_REAL *X, *Y, *FXY, *dFx, *dFy, *Y2, *X2;
  enum
  { nTablesE = 4 };
  enum
  { nXYPairsE = 8 };
  enum
  { nInfoItemsE = 5 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_INTEGER xyBounds[nXYPairsE];

  EOS_INTEGER nTables, nXYPairs, packedTablesSize;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_REAL infoVals[nInfoItemsE];
  EOS_INTEGER nInfoItems = nInfoItemsE, one_i = 1, itmp;
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_Exchange_Coeff,
    EOS_Mean_Atomic_Mass,
    EOS_Mean_Atomic_Num,
    EOS_Modulus,
    EOS_Normal_Density
  };
  EOS_CHAR* infoItems_str[nInfoItemsE] = {
    "EOS_Exchange_Coeff",
    "EOS_Mean_Atomic_Mass",
    "EOS_Mean_Atomic_Num",
    "EOS_Modulus",
    "EOS_Normal_Density"
  };
  EOS_CHAR *comment_str = NULL;

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_T_DPt;
  tableType[2] = EOS_Comment;
  tableType[3] = EOS_Info;

  matID[0] = 3720;
  matID[1] = 3720;
  matID[2] = 3720;
  matID[3] = 3720;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* Enable data dump to file */
  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
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

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }

  /* allocate memory */
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

  /*
   * testing interpolation, get comments and get info methods
   */

  printf ("\n\nBEFORE UNPACKING: \n");
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        //F(x,y)
  for (i = 0; i < nXYPairs; i++)
    xyBounds[i] = EOS_OK;
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    for (i = 0; i < nXYPairs; i++)
      xyBounds[i] = EOS_OK;
    if (errorCode == EOS_INTERP_EXTRAPOLATED)
      eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);
  }
  printf ("EOS_Pt_DT Interpolation Results:\n");
  for (i = 0; i < nXYPairs; i++) {
    printf
      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d %s\n",
       i, X[i], Y[i], FXY[i], dFx[i], dFy[i], xyBounds[i], ERROR_TO_TEXT(xyBounds[i]));
  }

  for (i = 0; i < nTables; i++) if (tableType[i] == EOS_Comment) break;
  assert(i < nTables);
  itmp = EOS_Cmnt_Len;
  eos_GetTableInfo (&tableHandle[i], &one_i, &itmp, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  comment_str = (EOS_CHAR *) malloc (sizeof (EOS_CHAR) * (EOS_INTEGER)infoVals[0]);
  assert(comment_str != NULL);
  eos_GetTableCmnts (&tableHandle[2], comment_str, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableCmnts ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }

  printf ("\nCOMMENTS:\n%s\n\n", comment_str);

  for (i = 0; i < nTables; i++) if (tableType[i] == EOS_Info) break;
  assert(i < nTables);
  eos_GetTableInfo (&tableHandle[i], &nInfoItems, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  for (i = 0; i < nInfoItems; i++)
    printf ("%s: %g\n", infoItems_str[i], infoVals[i]);
  printf("\n");


  /*
   * packed tables: delete the tables and then set them again with packed data
   */
  eos_GetPackedTablesSize (&nTables, tableHandle, &packedTablesSize, &errorCode);

  assert(errorCode == EOS_OK);

  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&nTables, tableHandle, packedData, &errorCode);

  assert(errorCode == EOS_OK);

  eos_DestroyAll (&errorCode);

  assert(errorCode == EOS_OK);

  eos_SetPackedTables (&nTables, &packedTablesSize, packedData, tableHandle, &errorCode);

  assert(errorCode == EOS_OK);

  free (packedData);


  /*
   * testing interpolation, get comments and get info methods
   */

  printf ("\n\nAFTER UNPACKING: \n");
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        //F(x,y)
  for (i = 0; i < nXYPairs; i++)
    xyBounds[i] = EOS_OK;
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    if (errorCode == EOS_INTERP_EXTRAPOLATED)
      eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);
  }
  printf ("EOS_Pt_DT Interpolation Results:\n");
  for (i = 0; i < nXYPairs; i++) {
    printf
      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d %s\n",
       i, X[i], Y[i], FXY[i], dFx[i], dFy[i], xyBounds[i], ERROR_TO_TEXT(xyBounds[i]));
  }

  for (i = 0; i < nTables; i++) if (tableType[i] == EOS_Comment) break;
  assert(i < nTables);
  itmp = EOS_Cmnt_Len;
  eos_GetTableInfo (&tableHandle[i], &one_i, &itmp, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  comment_str = (EOS_CHAR *) malloc (sizeof (EOS_CHAR) * (EOS_INTEGER)infoVals[0]);
  assert(comment_str != NULL);
  eos_GetTableCmnts (&tableHandle[2], comment_str, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableCmnts ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }

  printf ("\nCOMMENTS:\n%s\n\n", comment_str);

  for (i = 0; i < nTables; i++) if (tableType[i] == EOS_Info) break;
  assert(i < nTables);
  eos_GetTableInfo (&tableHandle[i], &nInfoItems, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  for (i = 0; i < nInfoItems; i++)
    printf ("%s: %g\n", infoItems_str[i], infoVals[i]);
  printf("\n");


  eos_DestroyAll (&errorCode);

  free(X);
  free(Y);
  free(Y2);
  free(X2);
  free(FXY);
  free(dFx);
  free(dFy);

  return 0;

}
