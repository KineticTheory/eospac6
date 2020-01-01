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
 *  \brief Ensure the bilinear interpolator works.
 *
 * \note
 * MATIDS TO TEST: 2140
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
  EOS_INTEGER  table_handles[1];
  EOS_INTEGER error = EOS_OK;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER errorCode;
  EOS_INTEGER tableOption = EOS_DUMP_DATA;
  EOS_INTEGER interpOption =  EOS_LINEAR;
  EOS_INTEGER no_vals =7+4;
  EOS_REAL xVals[no_vals], yVals[no_vals], fVals[no_vals], dFx[no_vals], dFy[no_vals];
  int i;
  table_type[0] = EOS_Pt_DT;
  matID[0] = 2140;
  xVals[0] = 1.; /*density*/
  yVals[0] = 300.; /*temperature--reset to 100 when done*/
  xVals[1] = .03; /*density*/
  yVals[1] = 100.; /*temperature*/
  xVals[2] = .03; /*density*/
  yVals[2] = 3e8; /*temperature*/
  xVals[3] = 10e4; /*density*/
  yVals[3] = 100.; /*temperature*/
  xVals[4] = 10e4; /*density*/
  yVals[4] = 3.e8; /*temperature*/
  xVals[5] = 5.4513885; /*density*/
  yVals[5] = 1.2e8; /*temperature*/
#define eps 1e-6
  xVals[6] = 9.4011992*(1-eps); /*density*/
  yVals[6] = 5.8024249e3*(1-eps); /*temperature*/
  xVals[7] = 9.4011992*(1+eps); /*density*/
  yVals[7] = 5.8024249e3*(1-eps); /*temperature*/
  xVals[8] = 9.4011992*(1-eps); /*density*/
  yVals[8] = 5.8024249e3*(1+eps); /*temperature*/
  xVals[9] = 9.4011992*(1+eps); /*density*/
  yVals[9] = 5.8024249e3*(1+eps); /*temperature*/
  xVals[10] = .825; /*density*/
  yVals[10] = 300; /*temperature*/

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

  eos_Interpolate(table_handles, &no_vals, xVals, yVals, fVals, dFx, dFy, &error);
  if(error == EOS_OK)
    printf("eos_Interpolate error EOS_OK\n");
  else {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
  }

  //output statement:

  for(i=0;i<no_vals;i++){
    printf("interpolated value is %g, derivative x is %g, derivative y is %g \n", fVals[i], dFx[i], dFy[i]); 
  }

  eos_DestroyAll (&error);

  return 0;
}

