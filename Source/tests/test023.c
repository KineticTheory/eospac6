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
 *  \brief Check that eos_GetTableInfo returns the correct information.
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

int main(){

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables = 1;
  EOS_INTEGER table_type[1];
  EOS_INTEGER matID[1];
  EOS_INTEGER  table_handles[1];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER numTableInfoItems = 1;
  EOS_INTEGER infoItems[numTableInfoItems];
  EOS_REAL infoVals[1];
  EOS_INTEGER errorCode;
  EOS_CHAR* cmntStr = NULL;
  int i;

  table_type[0] = EOS_Comment;
  matID[0] = 2140;
  infoItems[0] = EOS_Cmnt_Len;

  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 
  /*check for errors*/
  if(error == EOS_OK)
    printf("Create Tables error message is:  EOS_OK\n");
  else if(error != EOS_OK){
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", error, errorMessage);
    /*now go through all table handles*/
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_CreateTables ERROR %i: %s\n",
                table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else{
	printf("Table handle %i: eos_CreateTables error code EOS_OK\n", table_handles[i]);
      }
    }
 }

  eos_LoadTables (&no_tables, table_handles, &errorCode);

  /*check for errors*/
  if(error == EOS_OK)
    printf("Load Tables error message is:  EOS_OK\n");
  else if(error != EOS_OK){
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", error, errorMessage);
    /*now go through all table handles*/
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_CreateTables ERROR %i: %s\n",
                table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else{
	printf("Table handle %i: eos_CreateTables error code EOS_OK\n", table_handles[i]);
      }
    }
 }

  eos_GetTableInfo(&table_handles[0], &numTableInfoItems, infoItems, infoVals, &errorCode);

  cmntStr = (EOS_CHAR *) malloc ( (int) infoVals[0] * sizeof (EOS_CHAR));
  eos_GetTableCmnts(&table_handles[0], cmntStr, &errorCode);
  printf("Comment is: %s\n", cmntStr);

  free(cmntStr);
  eos_DestroyAll (&errorCode);
  return 0;
}







