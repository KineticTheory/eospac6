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
 *  \brief Check that the correct tables are being loaded for EOS_Aic_DT
 *         and material 5250 with a forced EOS_SPLIT_COWAN option.
 *
 * \note
 * MATIDS TO TEST: 5250
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../src/TEST_FUNCTIONS.h"
#include "../src/eos_Interface.h"

int main(){
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables = 1;
  EOS_INTEGER table_type[1];
  EOS_INTEGER matID[1];
  EOS_INTEGER table_handles[1];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;

  EOS_INTEGER errorCode;
  EOS_INTEGER tableOption;
  int i;

  table_type[0] = EOS_Aic_DT;
  matID[0] = 5250;
  printf("material ID: %i\n-------------------\n", matID[0]);

  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 


  /*check for errors*/
  if(error == EOS_OK) {
    printf("eos_CreateTables ... EOS_OK\n");
    print_nr_and_nt(table_handles[0]);
  }
  else {
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
	printf("Table handle %i: eos_CreateTables ... EOS_OK\n", table_handles[i]);
      }
      print_nr_and_nt(table_handles[i]);
    }
  }

  /*set options to dump data to file*/
  tableOption = EOS_DUMP_DATA;
  eos_SetOption(&table_handles[0], &tableOption, EOS_NullPtr, &error);
  /*check for errors*/
  if(error == EOS_OK)
    printf("eos_SetOption ... EOS_OK (EOS_DUMP_DATA)\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (EOS_DUMP_DATA)\n", errorCode, errorMessage);
  }
  print_nr_and_nt(table_handles[0]);

  tableOption = EOS_SPLIT_FORCED;
  eos_SetOption(&table_handles[0], &tableOption, EOS_NullPtr, &error);
  /*check for errors*/
  if(error == EOS_OK)
    printf("eos_SetOption ... EOS_OK (EOS_SPLIT_FORCED)\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (EOS_SPLIT_FORCED)\n", errorCode, errorMessage);
  }
  print_nr_and_nt(table_handles[0]);

  tableOption = EOS_SPLIT_COWAN;
  eos_SetOption(&table_handles[0], &tableOption, EOS_NullPtr, &error);
  /*check for errors*/
  if(error == EOS_OK)
    printf("eos_SetOption ... EOS_OK (EOS_SPLIT_COWAN)\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (EOS_SPLIT_COWAN)\n", errorCode, errorMessage);
  }
  print_nr_and_nt(table_handles[0]);

  /*load table*/
  eos_LoadTables(&no_tables, table_handles, &error); 

  /*error check*/
  if(error == EOS_OK) {
    printf("eos_LoadTables ... EOS_OK\n");
    print_nr_and_nt(table_handles[0]);
  }
  else {
    eos_GetErrorMessage (&error, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", error, errorMessage); 
    /*now go through all table handles*/
    for (i = 0; i < no_tables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&table_handles[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("Table handle %i: eos_LoadTables ERROR %i: %s\n",
                table_handles[i], tableHandleErrorCode, errorMessage);
      }
      else{
	printf("Table handle %i: eos_LoadTables ... EOS_OK\n", table_handles[i]);
      }
      print_nr_and_nt(table_handles[i]);
    }
  }

  eos_DestroyAll (&error);

  return 0;
}
