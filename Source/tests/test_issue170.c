/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup tests quick
 * \brief Ensure graceful handling of negative table handles.
 *        See eos/eospac-dev/eospac6#170 for more details:
 *        https://git.lanl.gov/eos/eospac-dev/eospac6/issues/170
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{
  enum
  { nTablesE = 4 };
  enum
  { nXYPairsE = 20 };

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables;
  EOS_INTEGER table_type[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER  table_handles[nTablesE];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER one = 1;
  int i, j;
  EOS_INTEGER nXYPairs;
  EOS_REAL rho[nXYPairsE], T[nXYPairsE];
  EOS_REAL F[nXYPairsE], dFx[nXYPairsE], dFy[nXYPairsE];

  no_tables = nTablesE;
  nXYPairs = nXYPairsE;

  table_type[0] = EOS_Pt_DT;
  table_type[1] = EOS_Ut_DT;
  table_type[2] = EOS_At_DT;
  table_type[3] = EOS_St_DT;

  matID[0] = 3720;
  matID[1] = 3720;
  matID[2] = 3720;
  matID[3] = 3720;

  rho[0]  =  2.690420093858480e+00;
  rho[1]  =  2.691197191463364e+00;
  rho[2]  =  2.692391549611788e+00;
  rho[3]  =  2.693399220016645e+00;
  rho[4]  =  2.694455823678862e+00;
  rho[5]  =  2.695098775684624e+00;
  rho[6]  =  2.696167611377819e+00;
  rho[7]  =  2.697384114797317e+00;
  rho[8]  =  2.698138887355369e+00;
  rho[9]  =  2.699276984977833e+00;
  rho[10] =  2.700238698525876e+00;
  rho[11] =  2.701314435462308e+00;
  rho[12] =  2.702182392236353e+00;
  rho[13] =  2.703256700455038e+00;
  rho[14] =  2.704476114862477e+00;
  rho[15] =  2.705458097533895e+00;
  rho[16] =  2.706317855863906e+00;
  rho[17] =  2.707358648464633e+00;
  rho[18] =  2.708070801277661e+00;
  rho[19] =  2.709303484438058e+00;

  for (i = 0; i < nXYPairs; i++)
    T[i] = 298.15;

  printf("\n--- TEST eos_CreateTables ---\n");

  /* create tables */
  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error);

  /* check for errors */
  if(error == EOS_OK) printf("eos_CreateTables EOS_OK\n");
  else if(error != EOS_OK) {
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", error, errorMessage);
    /* check all table handles */
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_CreateTables ERROR %i: %s\n", table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else{
	printf("Table handle %i: eos_CreateTables error code EOS_OK\n", table_handles[i]);
      }
    }
  }

  /* negate a handle */
  table_handles[2] *= -1;

  printf("\n--- TEST eos_ResetOption and eos_SetOption with a negative handle ---\n");

  for (i = 0; i < no_tables; i++) {
    eos_ResetOption (&table_handles[i], &EOS_DUMP_DATA, &error);
    if (error != EOS_OK) {
      eos_GetErrorMessage (&error, errorMessage);
      printf ("eos_ResetOption ERROR %i: %s\n", error, errorMessage);
    }

    if (i == 0) {
      eos_SetOption (&table_handles[0], &EOS_DUMP_DATA, EOS_NullPtr, &error);
      if (error != EOS_OK) {
	eos_GetErrorMessage (&error, errorMessage);
	printf ("eos_SetOption ERROR %i: %s\n", error, errorMessage);
      }
    }
    else {
      eos_SetOption (&table_handles[i], &EOS_APPEND_DATA, EOS_NullPtr, &error);
      if (error != EOS_OK) {
	eos_GetErrorMessage (&error, errorMessage);
	printf ("eos_SetOption ERROR %i: %s\n", error, errorMessage);
      }
    }
  }

  printf("\n--- TEST eos_LoadTables with a negative handle ---\n");

  /* load data */
  eos_LoadTables (&no_tables, table_handles, &error);

  /* check for errors */
  if(error == EOS_OK)
    printf("eos_LoadTables EOS_OK\n");
  else if(error != EOS_OK) {
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", error, errorMessage);
    /* check all table handles */
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_LoadTables ERROR %i: %s\n", table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else {
	printf("Table handle %i: eos_LoadTables error code EOS_OK\n", table_handles[i]);
      }
    }
  }

  /* test eos_GetErrorMessage with a Standard Error Code */
  printf("\n--- TEST eos_GetErrorMessage with a Standard Error Code ---\n");

  error = EOS_INVALID_TABLE_HANDLE;
  eos_GetErrorMessage (&error, errorMessage);
  printf("\teos with Standard Error Code, EOS_INVALID_TABLE_HANDLE (%d):\n\t'%s'\n",
	 EOS_INVALID_TABLE_HANDLE, errorMessage);

  printf("\n--- TEST eos_GetTableMetaData with a negative handle ---\n");

  for (i = 0; i < no_tables; i++) {
    EOS_CHAR infoString[EOS_META_DATA_STRLEN];
    EOS_INTEGER infoType = EOS_File_Name;

    eos_GetTableMetaData (&table_handles[i], &infoType, infoString, &error);
    if (error != EOS_OK) {
      eos_GetErrorMessage (&error, errorMessage);
      printf ("eos_GetTableMetaData ERROR %i: %s\n", error, errorMessage);
    }
    else {
      printf ("Table Handle %i: EOS_File_Name = %s\n", table_handles[i], infoString);
    }
  }

  printf("\n--- TEST eos_DestroyTables with a negative handle ---\n");

  for (i = 0; i < no_tables; i++) {
    printf ("--- Interpolate handle %i ---\n", table_handles[i]);
    eos_Interpolate(&table_handles[i], &nXYPairs, rho, T, F, dFx, dFy, &error);
    if (error != EOS_OK) {
      EOS_INTEGER extrapCode[nXYPairsE];
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &error, &equal);
      eos_GetErrorMessage (&error, errorMessage);
      printf ("eos_Interpolate ERROR %i: %s\n", error, errorMessage);
      if (equal) {
	eos_CheckExtrap (&table_handles[i], &nXYPairs, rho, T, extrapCode, &error);
	if (error != EOS_OK) {
	  printf ("eos_CheckExtrap ERROR %i: %s\n", error, errorMessage);
	}
	else {
	  for (j = 0; j < nXYPairs; j++) {
	    if (extrapCode[j] != EOS_OK) {
	      printf ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e, extrap: %i\n",
		      j, rho[j], T[j], F[j], dFx[j], dFy[j], extrapCode[j]);
	    }
	    else {
	      printf ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n",
		      j, rho[j], T[j], F[j], dFx[j], dFy[j]);
	    }
	  }
	}
      }
    }
    else {
      for (j = 0; j < nXYPairs; j++) {
	printf ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n",
		j, rho[j], T[j], F[j], dFx[j], dFy[j]);
      }
    }

  }

  printf("\n--- TEST eos_DestroyTables with a negative handle ---\n");

  eos_DestroyTables (&one, &table_handles[2], &error);
  if(error != EOS_OK) {
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_DestroyTables ERROR %i: %s\n", error, errorMessage);
  }

  printf("\n--- TEST eos_DestroyAll ---\n");

  eos_DestroyAll (&error);
  if(error != EOS_OK) {
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s\n", error, errorMessage);
  }

  printf ("\n");
  return(error);

}
