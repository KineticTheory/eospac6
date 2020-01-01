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
 *  category 2, record type 1.
 *
 *  (case where given x, and f(x, y) to get y).
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
  EOS_INTEGER errorCode;
  EOS_INTEGER tableOption = EOS_DUMP_DATA;
  EOS_INTEGER interpOption =  EOS_LINEAR;
  EOS_INTEGER one = 1;
  EOS_REAL xVals[1], yVals[1], fVals[1], dFx[1], dFy[1];
  int i;
  table_type[0] = EOS_T_DPt;
  matID[0] = 2140;
  xVals[0] = 9.; /*density*/
  yVals[0] = 27.; /*total pressure*/

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
  if(error == EOS_OK)
    printf("eos_Interpolate error EOS_OK\n");
  else {
    char* p = NULL;
    eos_GetErrorMessage (&error, errorMessage);
    p = strstr(errorMessage, "(err=");
    if (p)
      errorMessage[p-errorMessage] = '#';
    printf ("eos_Interpolate ERROR %i: %s\n", error, errorMessage);
  }

  printf("input density value is %f\n", xVals[0]); 
  printf("input pressure value is %f\n", yVals[0]); 
  printf("interpolated value is %f\n", fVals[0]); 

  eos_DestroyAll (&error);

  return 0;
}

