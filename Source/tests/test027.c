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
 *  \brief Ensure the inverse bilinear interpolator works for
 *  category 1, record type 1
 *
 *  (case where given y, and f(x, y) to get x).
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../src/TEST_FUNCTIONS.h"
#include "../src/eos_Interface.h"

//go back and change nomenclature for xvals,fvals

int main(){
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER no_tables = 1;
  EOS_INTEGER table_type[1];
  EOS_INTEGER matID[1];
  EOS_INTEGER  table_handles[1];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER errorCode;
  EOS_INTEGER tableOption = EOS_DUMP_DATA;
  EOS_INTEGER interpOption =  EOS_LINEAR;
  EOS_INTEGER one = 1;
  EOS_REAL xVals[1], yVals[1], fVals[1], dFx[1], dFy[1];
  int i;
  table_type[0] = EOS_D_PtT;
  matID[0] = 2140;
  xVals[0] = 27.; /*total pressure*/
  yVals[0] = 300.; /*temperature*/

  eos_CreateTables(&no_tables, table_type, matID, table_handles, &error); 



  /*check for errors*/
  if(error == EOS_OK)
    printf("eos_CreateTables error EOS_OK\n");
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
	printf("Table handle %i: eos_CreateTables error code EOS_OK\n", table_handles[i]);
      }
    }
  }

  /*set options to dump data to file*/
  eos_SetOption(&table_handles[0], &tableOption, EOS_NullPtr, &error);
  /*check for errors*/
  if(error == EOS_OK)
    printf("eos_SetOption error EOS_OK\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
  }

  /*load table*/
  eos_LoadTables(&no_tables, table_handles, &error); 

  /*error check*/
  if(error == EOS_OK)
    printf("eos_LoadTables error EOS_OK\n");
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
	printf("Table handle %i: eos_LoadTables error code EOS_OK\n", table_handles[i]);
      }
    }
  }

  /*set interpolation method to bilinear interpolation*/

  eos_SetOption(&table_handles[0], &interpOption, EOS_NullPtr, &error);

  if(error == EOS_OK)
    printf("eos_SetOption error EOS_OK\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
  }

  eos_Interpolate(table_handles, &one, xVals, yVals, fVals, dFx, dFy, &error);
  printf("after interp dfx, dfy = %g, %g\n", dFx[0],dFy[0]);

  if(error == EOS_OK)
    printf("eos_Interpolate error EOS_OK\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
  }

  printf("interpolated value is %f\n", fVals[0]); 

  eos_DestroyAll (&error);

  return 0;
}

