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
 *  \brief Ensure that eos_GetErrorCode returns the correct error code
 *  for an incorrect material number and the resulting incorrect
 *  table handle.
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
  EOS_INTEGER no_tables = 2;
  EOS_INTEGER table_type[2];
  EOS_INTEGER matID[2];
  EOS_INTEGER  table_handles[2];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  int i;
  
  table_type[0] = EOS_Pt_DT;
  table_type[1] = EOS_Uls_T;

  matID[0] = 2140;
  matID[1] = 0;


  /* create tables */
  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 

  /* check for errors */
  if(error == EOS_OK)
    printf("eos_CreateTables EOS_OK\n");
  else if(error != EOS_OK){
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", error, errorMessage); 
    /* check all table handles */
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

  /* load data */
  eos_LoadTables (&no_tables, table_handles, &error);

  /* check for errors */
  if(error == EOS_OK)
    printf("eos_LoadTables EOS_OK\n");
  else if(error != EOS_OK){
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", error, errorMessage); 
    /* check all table handles */
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_LoadTables ERROR %i: %s\n",
                table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else{
	printf("Table handle %i: eos_LoadTables error code EOS_OK\n", table_handles[i]);
      }
    }
  }

  eos_DestroyAll (&error);

  return 0;
}
