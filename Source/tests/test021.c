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
 *  \brief Ensure that the data passed in to eos_CreateTables does not
 *  become corrupted in eos_CreateTables.
 *
 * \note
 * MATIDS TO TEST: 2030 2140 13718
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

int main(){

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables = 3;
  EOS_INTEGER table_type[3];
  EOS_INTEGER matID[3];
  EOS_INTEGER  table_handles[3];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  int i;
  
  table_type[0] = EOS_Pt_DT;
  table_type[1] = EOS_Uls_T;
  table_type[2] = EOS_Ogb;
  matID[0] = 2140;
  matID[1] = 2030;
  matID[2] = 13718;

  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 

  printf("Table handles are %i %i %i\n",  table_handles[0], table_handles[1], table_handles[2]);

  /*check for errors*/
  if(error == EOS_OK)
    printf("EOS_OK\n");
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

  /*now check that nothing has been changed by CreateTables*/

  if(no_tables==get_no_tables())
    printf("no tables correct\n");
  else 
    printf("no tables incorrect\n");

  if(get_table_type(0) == EOS_Pt_DT && get_table_type(1) == EOS_Uls_T  && get_table_type(2) == EOS_Ogb) 
    printf("table type correct\n");
  else 
    printf("table type incorrect\n");

  if(get_matID(0) == 2140 && get_matID(1) == 2030 &&  get_matID(2) == 13718)
    printf("matID correct\n");
  else
    printf("matID incorrect\n");

  eos_DestroyAll (&error);

  return 0;
}
