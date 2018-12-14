/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests quick
 *  \brief Ensure incorrect error codes not returnd.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf11292">artf11292</a>
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
 * MATIDS TO TEST: 2140 9003
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
  //    enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_INTEGER xyBounds[nXYPairsE];

  EOS_INTEGER nTables, nXYPairs, packedTablesSize, one = 1;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = 1;
  nXYPairs = nXYPairsE;

  /* EOS_Pt_DT, material 2140 works for table 301 (record type 1) */
  /* EOS_Uls_T, material 2030 works for table 401 (record type 2) */
  /* EOS_Ogb, material 13718 works for table 501 (record type 3) */
  /* EOS_Comment, material 4100 works for tables 101-199 (record type 4) */
  /* EOS_Info, material 2140 works for tables 201 (record type 5) */
  //    tableType[0] = EOS_Pt_DT;  /*record type 1, subtable 1 */
  //    tableType[1] = EOS_Uls_T; /*record type 2, subtable 5 */
  //    tableType[2] = EOS_Ogb;
  //    tableType[3] = EOS_Comment;
  //    tableType[4] = EOS_Info;
  tableType[0] = EOS_Pt_DT;     /* record type 1, substable 1 , cat 0 */
  tableType[1] = EOS_T_DPt;     /* record type 1, substable 1 , cat 2 */
  tableType[2] = EOS_D_PtT;     /* record type 1, substable 1 , cat 1 */
  tableType[3] = EOS_Pt_DUt;    /* record type 1, category 3: EOS_Pt_DT, EOS_T_DUt (where EOS_T_DUt is subtable 1, category 2) */

  //    tableType[5] = EOS_Pt_DT;
  //    tableType[6] = EOS_Ut_DT;  record type 1, subtable 2
  //    tableType[7] = EOS_Uls_T;  record type 2, subtable 5
  //    tableType[8] = EOS_Uv_T;   record type 2, subtable 4

  matID[0] = 9003;
  matID[1] = 2140;
  matID[2] = 2140;
  matID[3] = 2140;
  //    matID[1] = 2030;
  //    matID[2] = 13718;
  //    matID[3] = 4100;
  //    matID[4] = 2140;

  //    matID[5] = 2140;
  //    matID[6] = 2140;
  //    matID[7] = 2030;
  //    matID[8] = 2030;

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
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
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

  /* testing interpolation methods */

  printf ("\n\nBEFORE UNPACKING: \n");
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        //F(x,y)
  if (errorCode != EOS_OK) {
    EOS_BOOLEAN equal;
    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    for (i = 0; i < nXYPairs; i++)
      xyBounds[i] = EOS_OK;
    if (equal)
      eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);
  }
  printf ("EOS_Pt_DT Interpolation Results:\n");
  for (i = 0; i < nXYPairs; i++) {
    printf
      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d %s\n",
       i, X[i], Y[i], FXY[i], dFx[i], dFy[i], xyBounds[i], ERROR_TO_TEXT(xyBounds[i]));
  }

  /* packed tables: delete the first table and then set it again with packed data */

  eos_GetPackedTablesSize (&one, tableHandle, &packedTablesSize, &errorCode);

  assert(errorCode == EOS_OK);

  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&one, tableHandle, packedData, &errorCode);

  assert(errorCode == EOS_OK);

  eos_DestroyAll (&errorCode);

  assert(errorCode == EOS_OK);

  eos_SetPackedTables (&one, &packedTablesSize, packedData, tableHandle,
                       &errorCode);

  assert(errorCode == EOS_OK);

  free (packedData);

  /* testing interpolation methods */

  printf ("\n\nAFTER UNPACKING: \n");
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        //F(x,y)
  if (errorCode != EOS_OK) {
    EOS_BOOLEAN equal;
    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    for (i = 0; i < nXYPairs; i++)
      xyBounds[i] = EOS_OK;
    if (equal)
      eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);
  }
  printf ("EOS_Pt_DT Interpolation Results:\n");
  for (i = 0; i < nXYPairs; i++) {
    printf
      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d %s\n",
       i, X[i], Y[i], FXY[i], dFx[i], dFy[i], xyBounds[i], ERROR_TO_TEXT(xyBounds[i]));
  }

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
