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
 *    -# Ensure extrapolation is allowed down to rho=0,
 *    -# ensure eos_LoadTables does not return EOS_INTERP_EXTRAPOLATED
 *       (specific to artf5716),
 *    -# and test self-consistency of various interpolated data.
 *
 *  See SourceForge issues
 *  <a href="https://tf.lanl.gov/sf/go/artf5435">artf5435</a>
 *  and
 *  <a href="https://tf.lanl.gov/sf/go/artf5716">artf5716</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 5030
 */

#include <iostream>
#include <stdio.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <math.h>

using namespace std;

#define SIZE(x) 6-(int)log10(fabs(x))

void _printf_r (const char *s, const EOS_REAL val, const char *s2="")
{
  if (val > 1.0e-1 && val < 1.0e4)
    printf("%s%.*f%s", s, SIZE(val), val, s2);
  else
    printf("%s%.2e%s", s, val, s2);
}

int main ()
{
  // Set the number of tables.
  const EOS_INTEGER nTablesE = 3;
  EOS_INTEGER nTables = nTablesE;

  // Setup the table types.
  EOS_INTEGER tableType[nTablesE];

  tableType[0] = EOS_Pt_DT;     // P(RHO, T)
  tableType[1] = EOS_Ut_DT;     // U(RHO, T)
  tableType[2] = EOS_Ut_PtT;    // U(P, T)

  // Setup the material id for each of the tables.
  EOS_INTEGER matID[nTablesE];
  int sesid = 5030;
  matID[0] = sesid;
  matID[1] = sesid;
  matID[2] = sesid;

  // Initialize the error code.
  EOS_INTEGER errorCode = EOS_OK, xyBounds[nTablesE];
  EOS_INTEGER errorCode0 = EOS_OK;
  EOS_INTEGER errorCode1 = EOS_OK;
  EOS_INTEGER errorCode2 = EOS_OK;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen]=" ";
  EOS_CHAR errorMessage0[EOS_MaxErrMsgLen]=" ";
  EOS_CHAR errorMessage1[EOS_MaxErrMsgLen]=" ";
  EOS_CHAR errorMessage2[EOS_MaxErrMsgLen]=" ";
  EOS_INTEGER tableHandleErrorCode = EOS_OK;

  // Setup and default the table handles.
  EOS_INTEGER tableHandle[nTablesE];

  // Initialize interpolator input
  EOS_INTEGER npairs = 1;
  EOS_REAL RHO_EP = 1.0e-4;
  EOS_REAL T_EP = 2.0e3;
  EOS_REAL org_rho;
  EOS_REAL PtT0,UtT0,UtP;
  EOS_REAL dPdRHO_T;
  EOS_REAL dPdT_RHO;
  EOS_CHAR *version;
  EOS_INTEGER vlen;
  int i, j;

  eos_GetVersionLength(&vlen);
  version = (EOS_CHAR*) malloc(vlen * sizeof(EOS_CHAR));
  eos_GetVersion(version);

  cout << endl << "fcmp_ignore: EOSPAC version " << version << endl;;

  free(version);

  for (i = 0; i < nTables; i++)
    tableHandle[i] = 0;

  // Create the tables.
  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("           TH %i ERROR %i: %s\n",
	      tableHandle[i], tableHandleErrorCode, errorMessage);
    }
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    if (i == 0)
      eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr,
                     &errorCode);
    else
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                     &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }

//#define TEST_BILINEAR
#ifdef TEST_BILINEAR
    /* enable EOS_LINEAR option for all table handles */
    eos_SetOption (&tableHandle[i], &EOS_LINEAR, EOS_NullPtr,
		   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
#endif
  }

  // load data into table data objects
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("         TH %i ERROR %i: %s\n",
	      tableHandle[i], tableHandleErrorCode, errorMessage);
    }
  }

  cout << endl;
  _printf_r(" eos_Interpolate:\n  rho=", RHO_EP);
  _printf_r(", T=", T_EP,"\n\n");

#define p 3.0
  org_rho = RHO_EP;
  RHO_EP *= -1.0; // use negative temperature for first interpolation set

  for (i = -1; i < 10; i++) {

    if (i==0) {
      RHO_EP = org_rho;
      RHO_EP /= pow(1.5, p);
    }

    eos_Interpolate (&tableHandle[1], &npairs, &RHO_EP, &T_EP,
		     &UtT0, &dPdRHO_T, &dPdT_RHO, &errorCode0);
    eos_Interpolate (&tableHandle[0], &npairs, &RHO_EP, &T_EP,
		     &PtT0, &dPdRHO_T, &dPdT_RHO, &errorCode1);
    eos_Interpolate (&tableHandle[2], &npairs, &PtT0, &T_EP,
		     &UtP, &dPdRHO_T, &dPdT_RHO, &errorCode2);

    for (j = 0; j < nTablesE; j++)
      xyBounds[j] = EOS_OK;

    EOS_BOOLEAN equal;
    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode0, &equal);
    if (equal)
      eos_CheckExtrap(&tableHandle[1], &npairs, &RHO_EP, &T_EP,
		      &xyBounds[0], &errorCode);
    else if (errorCode0 != EOS_OK)
      eos_GetErrorMessage (&errorCode0, errorMessage0);

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode1, &equal);
    if (equal)
      eos_CheckExtrap(&tableHandle[0], &npairs, &RHO_EP, &T_EP,
		     &xyBounds[1], &errorCode);
    else if (errorCode1 != EOS_OK)
      eos_GetErrorMessage (&errorCode1, errorMessage1);

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode2, &equal);
    if (equal)
      eos_CheckExtrap(&tableHandle[2], &npairs, &PtT0, &T_EP,
		     &xyBounds[2], &errorCode);
    else if (errorCode2 != EOS_OK)
      eos_GetErrorMessage (&errorCode2, errorMessage2);

    if ((int)p != i) printf("    ");
    else printf(" >>>");

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INVALID_TABLE_HANDLE, &errorCode0, &equal);
    if (equal) {
      printf("%2s(%2s, %2s) failed because TH %i is invalid.\n",
	     "Ut", "rho", "T", tableHandle[1]);
    }
    else {
              printf("Ut(rho=%22.15e, T=%22.15e)", RHO_EP, T_EP);
      printf("=%22.15e  %s%s\n", UtT0, ((xyBounds[0]!=EOS_OK) ? ERROR_TO_TEXT(xyBounds[0]) : ""),
	     ((errorCode0 != EOS_OK) ? errorMessage0 : ""));
    }

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INVALID_TABLE_HANDLE, &errorCode1, &equal);
    if (equal) {
      printf("    %2s(%2s, %2s) failed because TH %i is invalid.\n",
	     "Pt", "rho", "T", tableHandle[0]);
    }
    else {
      printf("    Pt(rho=%22.15e, T=%22.15e)", RHO_EP, T_EP);
      printf("=%22.15e  %s%s\n", PtT0, ((xyBounds[1]!=EOS_OK) ? ERROR_TO_TEXT(xyBounds[1]) : ""),
	     ((errorCode1 != EOS_OK) ? errorMessage1 : ""));
    }

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INVALID_TABLE_HANDLE, &errorCode2, &equal);
    if (equal) {
      printf("    %2s(%2s, %2s) failed because TH %i is invalid.\n",
	     "Ut", "Pt", "T", tableHandle[2]);
    }
    else {
      printf("    Ut(Pt =%22.15e, T=%22.15e)", PtT0, T_EP);
      printf("=%22.15e  %s%s", UtP, ((xyBounds[2]!=EOS_OK) ? ERROR_TO_TEXT(xyBounds[2]) : ""),
	   ((errorCode2 != EOS_OK) ? errorMessage2 : ""));
    }

    if ((int)p == i) printf("<<<");
    printf("\n\n");

    RHO_EP *= 1.5;
  }

  eos_DestroyAll (&errorCode);

  return 0;

}
