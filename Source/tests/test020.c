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
 *  \brief Ensure eos_CreateTables returns EOS_OK when the number of
 *  tables requested is zero.
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../src/TEST_FUNCTIONS.h"
#include "../src/eos_Interface.h"

int main(){

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables = 0;
  EOS_INTEGER table_type[1];
  EOS_INTEGER matID[1];
  EOS_INTEGER  table_handles[1];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  int i;
  

  table_type[0] = EOS_Pc_D;
  matID[0] = 2120;
  printf("calling create table\n");
  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 
  printf("done calling create table\n");
  /*check for errors*/
  if(error == EOS_OK)
    printf("test successful\n");
  else if(error != EOS_OK){
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", error, errorMessage); 
    /*now go through all table handles*/
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                table_handles[i], tableHandleErrorCode, errorMessage);
      }
    }
  }

  eos_DestroyAll (&error);

  return 0;
}
