/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests
 *  \brief Test for invalid memory access in eos_SetPackedTables.
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf27244">artf27244</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

/* local function prototype(s)  */
EOS_INTEGER _printDataForHandle (EOS_INTEGER th);

int main ()
{
  int i;
  EOS_REAL *data = NULL;
  enum
  { nTablesE = 6 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, packedTablesSize;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;

  i = 0;
  if (i < nTables) tableType[i++] = EOS_Comment;
  if (i < nTables) tableType[i++] = EOS_Pt_DT;
  if (i < nTables) tableType[i++] = EOS_Ut_DT;
  if (i < nTables) tableType[i++] = EOS_Pm_D;
  if (i < nTables) tableType[i++] = EOS_T_DPt;
  if (i < nTables) tableType[i++] = EOS_T_DUt;

  assert(i == nTables);

  for (i = 0; i < nTables; i++)
    matID[i] = 2140;

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
    /* report errors, but do not exit */
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
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
    /* report errors, but do not exit */
  }

  /* print some data */
  printf ("\n\nBEFORE UNPACKING:\n\n");

  printf ("tableHandle[%d..%d] = (",0,nTables-1);
  for (i = 0; i < nTables; i++) printf(" %d",tableHandle[i]);
  printf (" )\n\n");

  for (i = 0; i < nTables; i++) {
    errorCode = _printDataForHandle (tableHandle[i]);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }
    if (i+1 < nTables) printf("\n"); /* insert line if another table pending */
  }

  /* get tables */
  eos_GetPackedTablesSize (&nTables, tableHandle, &packedTablesSize, &errorCode);
  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&nTables, tableHandle, packedData, &errorCode);

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  /* initialize tableHandle[] array values with huge integers to verify eos_SetPackedTables
   * will not fail due to using them before resetting them */
  if (sizeof((int)i) < sizeof(tableHandle[0]))
    for (i = 0; i < nTables; i++) tableHandle[i] = (EOS_INTEGER) LONG_MAX;
  else
    for (i = 0; i < nTables; i++) tableHandle[i] = (EOS_INTEGER) INT_MAX;

  /* pack tables */
  eos_SetPackedTables (&nTables, &packedTablesSize, packedData, tableHandle,
                       &errorCode);
  free (packedData);

  /* print some data */
  printf ("\n\nAFTER UNPACKING:\n\n");

  printf ("tableHandle[%d..%d] = (",0,nTables-1);
  for (i = 0; i < nTables; i++) printf(" %d",tableHandle[i]);
  printf (" )\n\n");

  for (i = 0; i < nTables; i++) {
    errorCode = _printDataForHandle (tableHandle[i]);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }
    if (i+1 < nTables) printf("\n"); /* insert line if another table pending */
  }

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  if (data) free(data);

  return 0;

}

EOS_INTEGER _printDataForHandle (EOS_INTEGER th)
{
  int j, k, l;
  EOS_INTEGER matid, type;
  EOS_INTEGER infoItems[2];
  EOS_REAL infoVals[2];
  EOS_INTEGER errorCode = EOS_OK, one =1, two = 2;

  /* get the matid and data type for this tableHandle */
  infoItems[0] = EOS_Material_ID;
  infoItems[1] = EOS_Table_Type;
  eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) return errorCode;
  matid = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
  type  = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */

  if (type == EOS_Comment) {

    EOS_CHAR *comments = NULL;

    infoItems[0] = EOS_Cmnt_Len;
    eos_GetTableInfo(&th, &one, infoItems, infoVals, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    l = (EOS_INTEGER) (infoVals[0] + 0.5) + 1; /* round to nearest integer */

    /* allocate memory */
    comments = (EOS_CHAR *) malloc (sizeof (EOS_INTEGER) * l);
    eos_GetTableCmnts(&th, comments, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    printf("TableHandle: %d\tDataType: %s\tMaterialID: %d\nDescription: %s\n\n%s\n",
	   th, get_tableType_str(type), matid, get_tableType_description(type), comments);
    free (comments);

  }
  else {

    EOS_REAL *X=NULL, *Y=NULL, *F=NULL;
    EOS_INTEGER num_phases, num_dens, num_temp, num_items;
  
    infoItems[0] = EOS_NR;
    infoItems[1] = EOS_NT;

    /* for this tableHandle, how many density values, and temperature values */
    eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    num_phases = 1;
    num_dens   = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
    num_temp   = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */

    /* allocate memory */
    X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_dens);
    Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_temp);
    num_items = num_phases * num_dens * num_temp;
    F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);

    /* fetch loaded density data */
    *infoItems = EOS_R_Array;
    eos_GetTableInfo(&th, &num_dens, infoItems, X, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    /* fetch loaded temperature data */
    *infoItems = EOS_T_Array;
    eos_GetTableInfo(&th, &num_temp, infoItems, Y, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    /* fetch loaded function data */
    *infoItems = EOS_F_Array;
    eos_GetTableInfo(&th, &num_items, infoItems, F, &errorCode);
    if (errorCode != EOS_OK) return errorCode;

    printf("TableHandle: %d\tDataType: %s\tMaterialID: %d\nDescription: %s\n\n",
	   th, get_tableType_str(type), matid, get_tableType_description(type));

    /* print data arrays to stdout */
    printf ("%23s %23s", "density", "temperature");
    printf (" %23s", get_tableType_str(type));
    printf ("\n");
    for (k = 0; k < num_temp; k++) {
      for (l = 0; l < num_dens; l++) {
	printf ("%23.15e %23.15e", X[l], Y[k]);
	for (j = 0; j < num_phases; j++)
	  printf (" %23.15e", F[num_temp*num_dens*j + num_dens*k + l]);
	printf ("\n");
      }
      printf ("\n");
    }

    /* deallocate memory */
    if (X) free(X);
    if (Y) free(Y);
    if (F) free(F);

  }

  return errorCode;
}
