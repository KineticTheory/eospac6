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
 *  \brief Verify eos_LoadTables functionality on qsc.lanl.gov, and
 *  attempt to produce strange extrapolation behavior in eos_Interpolate.
 *
 *  See SourceForge issues
 *  <a href="https://tf.lanl.gov/sf/go/artf1931">artf1931</a>
 *  and
 *  <a href="https://tf.lanl.gov/sf/go/artf1936">artf1936</a>
 *  for details.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 5251
 */

#include <iostream>
#include <stdio.h>
#include "eos_Interface.h"

using namespace std;

int main ()
{
  // Set the number of tables.
  const EOS_INTEGER nTablesE = 4;
  EOS_INTEGER nTables = nTablesE;

  // Setup the table types.
  EOS_INTEGER tableType[nTablesE];
  tableType[0] = EOS_Pt_DT;     // P(RHO, T)
  tableType[1] = EOS_Pt_DUt;    // P(RHO, I)
  tableType[2] = EOS_Ut_DT;     // I(RHO, T)
  tableType[3] = EOS_T_DUt;     // T(RHO, I)
  //tableType[4] = EOS_Uic_DT;   // Ion Energy(RHO, T)
  //tableType[5] = EOS_Ue_DT;    // Electron Energy(RHO, T)

  // Setup the material id for each of the tables.
  EOS_INTEGER matID[nTablesE];
  int sesid = 5251;
  matID[0] = sesid;
  matID[1] = sesid;
  matID[2] = sesid;
  matID[3] = sesid;
  //matID[4] = sesid;
  //matID[5] = sesid;

  // Initialize the error code.
  EOS_INTEGER errorCode = EOS_OK;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER tableHandleErrorCode = EOS_OK;

  // Setup and default the table handles.
  EOS_INTEGER tableHandle[nTablesE];

  // Initialize interpolator input
  EOS_INTEGER npairs = 1;
  EOS_REAL RHO_EP = 2.e3;
  EOS_REAL T_EP = 4.e8;
  EOS_REAL Pout;
  EOS_REAL dPdRHO_T;
  EOS_REAL dPdT_RHO;
  int i;

  for (i = 0; i < nTables; i++)
    tableHandle[i] = 0;

  // Create the tables.
  cout << "&&&&& Before eos_CreateTables" << endl;
  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    tableHandleErrorCode = EOS_OK;
    eos_GetErrorCode (&tableHandle[0], &tableHandleErrorCode);
    eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
    //cout << "eos_CreateTables ERROR " << tableHandleErrorCode << ": " << errorMessage << endl;
    printf ("eos_CreateTables ERROR %i: %s\n", tableHandleErrorCode,
            errorMessage);
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
  }

  // load data into table data objects
  cout << "&&&&& Before eos_LoadTables" << endl;
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    tableHandleErrorCode = EOS_OK;
    eos_GetErrorCode (&tableHandle[0], &tableHandleErrorCode);
    eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
    //cout << "eos_LoadTables ERROR " << tableHandleErrorCode << ": " << errorMessage << endl;
    printf ("eos_LoadTables ERROR %i: %s\n", tableHandleErrorCode,
            errorMessage);
  }

  cout << "&&&&& Interpolation 1:" << endl;
  Pout = 1.e8;
  cout << "      before eos_Interpolate, Pout=" << Pout
    << ", RHO_EP=" << RHO_EP << ", T_EP=" << T_EP << endl;
  eos_Interpolate (&tableHandle[0], &npairs, &RHO_EP, &T_EP,
                   &Pout, &dPdRHO_T, &dPdT_RHO, &errorCode);
  cout << "      after eos_Interpolate, Pout=" << Pout << endl;

  cout << "&&&&& Interpolation 2:" << endl;
  Pout = 2.e8;
  cout << "      before eos_Interpolate, Pout=" << Pout
    << ", RHO_EP=" << RHO_EP << ", T_EP=" << T_EP << endl;
  eos_Interpolate (&tableHandle[0], &npairs, &RHO_EP, &T_EP,
                   &Pout, &dPdRHO_T, &dPdT_RHO, &errorCode);
  cout << "      after eos_Interpolate, Pout=" << Pout << endl;

  // Destroy all data objects
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    cout << "eos_DestroyAll ERROR " << errorCode
	 << ": " << errorMessage << endl;
  }

  return 0;

}
