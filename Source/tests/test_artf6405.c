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
 *  \brief Perform the following tests:
 *    -# Verify fatal error while using EOS_PT_SMOOTHING, EOS_V_PtT and
 *       EOS_DUMP_DATA was fixed.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf6405">artf6405</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#if defined(__GNUC__) && defined(__linux__)
#define _GNU_SOURCE
#include <fenv.h>

#ifdef FE_DIVBYZERO
#define _ENABLE_DIVBYZERO_EXCEPTIONS_  // only use this on Linux systems with gcc and FE_DIVBYZERO
#else
#undef _GNU_SOURCE
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <float.h>

#ifdef _ENABLE_DIVBYZERO_EXCEPTIONS_
//#include <fenv.h>
#endif

#include "eos_Interface.h"


int main(int argc, char **argv){

#ifdef _ENABLE_DIVBYZERO_EXCEPTIONS_
  feenableexcept(FE_DIVBYZERO);
#endif

  enum 
    {nTablesE=1};
  enum
    {nXYPairsE=16};

  EOS_INTEGER i, j, nTables, nXYPairs;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_CHAR *tableTypeLabel[nTablesE] = {"EOS_V_PtT"};
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  /*EOS_Pt_DT, material 2140 works for Sesame table 301 (record type 1)*/
  /*EOS_Dv_T, material 2140 works for Sesame table 401 (record type 2)*/
  /*EOS_Ogb, material 12140 works for Sesame table 501 (record type 3)*/
  /*EOS_Comment, material 2140 works for Sesame tables 101-199 (record type 4)*/
  /*EOS_Info, material 2140 works for Sesame table 201 (record type 5)*/

#ifdef _ENABLE_DIVBYZERO_EXCEPTIONS_
  printf("DIVBYZERO EXCEPTIONS ARE ENABLED\n");
#endif

  tableType[0] = EOS_V_PtT;
  numIndVars[0] = 2;
  matID[0] = 2140;
  errorCode = EOS_OK;
  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  for(i=0; i<nTables; i++)
    tableHandle[i] = 0;
  printf("1\n");
  /*Initialize data table objects.*/
  for(i=0;i<nTables;i++)
    eos_CreateTables(&nTables, &tableType[i], &matID[i], &tableHandle[i], &errorCode);
  printf("2\n");
  if (errorCode != EOS_OK) {
    for(i=0;i<nTables;i++){
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("%i: eos_CreateTables ERROR %i: %s\n",
	      tableHandle[i], tableHandleErrorCode, errorMessage);    
    }
  }
  printf("3\n");
  /*set some options*/
  printf("4\n");
  for(i=0; i<nTables;i++){
    eos_SetOption(&(tableHandle[i]), &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
   
    eos_SetOption(&(tableHandle[i]), &EOS_PT_SMOOTHING, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }

    eos_SetOption(&(tableHandle[i]), &EOS_USE_CUSTOM_INTERP, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
  }
  printf("5\n");
  /*load data*/
  printf("*******************************************************\n");
  printf("*******************************************************\n");
  printf("*******************************************************\n");
    eos_LoadTables(&nTables, tableHandle, &errorCode);
  printf("*******************************************************\n");
  printf("*******************************************************\n");
  printf("*******************************************************\n");
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      for(i=0;i<nTables;i++){
	tableHandleErrorCode = EOS_OK;
	eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
	if (tableHandleErrorCode != EOS_OK) {
	  eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	  printf ("%i: eos_LoadTables ERROR %i: %s\n",
		  tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
  }

    /*interpolate*/

  X[0] = 1.000000000000001e-16;
  X[1] = 5.263157894736736e+04;
  X[2] = 1.052631578947347e+05;
  X[3] = 1.578947368421021e+05;
  X[4] = 1.000000000000001e-16;
  X[5] = 5.263157894736736e+04;
  X[6] = 1.052631578947347e+05;
  X[7] = 1.578947368421021e+05;
  X[8] = 1.000000000000001e-16;
  X[9] = 5.263157894736736e+04;
  X[10] = 1.052631578947347e+05;
  X[11] = 1.578947368421021e+05;
  X[12] = 1.000000000000001e-16;
  X[13] = 5.263157894736736e+04;
  X[14] = 1.052631578947347e+05;
  X[15] = 1.578947368421021e+05;

  Y[0] = 1.160450500000000e+02;
  Y[1] = 1.160450500000000e+02;
  Y[2] = 1.160450500000000e+02;
  Y[3] = 1.160450500000000e+02;
  Y[4] = 6.107635309900391e+08;
  Y[5] = 6.107635309900391e+08;
  Y[6] = 6.107635309900391e+08;
  Y[7] = 6.107635309900391e+08;
  Y[8] = 1.221526945935028e+09;
  Y[9] = 1.221526945935028e+09;
  Y[10] = 1.221526945935028e+09;
  Y[11] = 1.221526945935028e+09;
  Y[12] = 1.832290360880018e+09;
  Y[13] = 1.832290360880018e+09;
  Y[14] = 1.832290360880018e+09;
  Y[15] = 1.832290360880018e+09;

  for(i=0;i<nTables;i++){
    eos_SetOption(&(tableHandle[i]), &EOS_USE_CUSTOM_INTERP, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }

    printf("\n---Interpolate using tableType %s ---", tableTypeLabel[i]);
    eos_Interpolate(&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy, &errorCode);
    if(errorCode != EOS_OK){
	eos_GetErrorMessage(&errorCode, errorMessage);
	printf("eos_Interpolate ERROR %i: TH= %i \n ERROR MESSAGE: %s", errorCode, tableHandle[i], errorMessage);
    }    
    else {
      for(j=0; j<nXYPairs;j++){
	if( numIndVars[i]==1){
	  printf("i=%i, X=%g, F=%g, dFx=%g, errorCode = %i\n", j,X[j], F[j], dFx[j], errorCode);
	}
	if( numIndVars[i]==2){
	  printf("i=%i, X=%g, Y= %g, F=%g, errorCode = %i\n", j,X[j], Y[j], F[j], errorCode);
	}
      }
    }
  }

    /* Destroy all data objects */

  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    for(i=0;i<nTables; i++){
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode(&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage(&tableHandleErrorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
    }
  }

  return 0;
}
