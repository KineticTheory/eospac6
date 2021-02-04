/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup CPP tests quick
 *  \brief Ensure resolution of unexpected extrapolation output and
 *  core dump on qsc.lanl.gov.
 *
 *  See SourceForge issues
 *  <a href="https://tf.lanl.gov/sf/go/artf1930">artf1930</a>
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
 * MATIDS TO TEST: 9003
 */

#include <iostream>
#include <stdio.h>
#include <iomanip>
#include <string>
#include "eos_Interface.h"

using namespace std;

void print_table_errors (int *handles, int nh, string label);
void print_error (int ecode, string label);

int main ()
{
  // Set the number of tables.
  const int ntablesE = 6;
  int ntables = ntablesE;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  // Setup the table types.
  int tableType[ntablesE];
  tableType[0] = EOS_Pt_DT;     // P(RHO, T)
  tableType[1] = EOS_Pt_DUt;    // P(RHO, I)
  tableType[2] = EOS_Ut_DT;     // I(RHO, T)
  tableType[3] = EOS_T_DUt;     // T(RHO, I)
  tableType[4] = EOS_Uic_DT;    // Ion Energy(RHO, T)
  tableType[5] = EOS_Ue_DT;     // Electron Energy(RHO, T)

  // Setup the material id for each of the tables.
  // RHO = 0.8 0.9 1.0 1.1 1.2
  // T = 4800 4900 5000 5100 5200
  // Mat 9001: P=3 everywhere, I=4 everywhere
  // Mat 9002: P = 1. + 10.*(RHO - .8), I=P
  // Mat 9003: P = 1. + .01*(T - 4800), I=P
  // Mat 9004: P = .92*RHO**2 + 1.e-11*T**3, I=P
  // Mat 9005: P = ideal gas, gamma=1.4, A=1.00797, I=P
  // Mat 9006: P = ideal gas, gamma=5/3, A=131.3,   I=P
  int matid[ntablesE];
  int i;

  for (i = 0; i < ntables; i++)
    matid[i] = 9003;

  // Initialize the error code.
  int error_code = EOS_OK;

  // Setup and default the table handles.
  int table_handles[ntablesE];
  for (i = 0; i < ntables; i++) {
    table_handles[i] = 0;
  }

  // Create the tables.
  cout << endl;
  cout << "eos_CreateTables - Create the tables." << endl;
  eos_CreateTables (&ntables, tableType, matid, table_handles, &error_code);

  // Check and print errors from the create tables step.
  if (error_code != EOS_OK) {
    print_table_errors (table_handles, ntables, "eos_CreateTables");
    return 0;
  }

  /* Enable data dump to file */
  for (i = 0; i < ntables; i++) {
    eos_SetOption (&table_handles[i], &EOS_DUMP_DATA, EOS_NullPtr,
                   &error_code);
    if (error_code != EOS_OK) {
      eos_GetErrorMessage (&error_code, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", error_code, errorMessage);
    }
  }

  // Load data into table data objects
  cout << "eos_LoadTables - Load the tables." << endl;
  eos_LoadTables (&ntables, table_handles, &error_code);

  // Check and print errors from the load tables step.
  if (error_code != EOS_OK) {
    print_table_errors (table_handles, ntables, "eos_LoadTables");
    return 0;
  }

  // Setup data for the interpolation.
  const int NP = 8;
  int npairs = NP;
  double X[NP], Y[NP], F[NP], dFx[NP], dFy[NP], Fexp[NP];
  for (i = 0; i < NP; i++) {
    F[i] = 0.;
    dFx[i] = 0.;
    dFy[i] = 0.;
  }

  // There are 8 regions around a table where extrapolation is done. Each
  // of the following points corresponds to those regions. For example,
  // the first point has RHO within the table boundaries, but T is below
  // the table boundaries.
  X[0] = 0.93;
  Y[0] = 2000.;                 // Bottom
  X[1] = 2.;
  Y[1] = 2000.;                 // Bottom right
  X[2] = 2.;
  Y[2] = 5000.;                 // Right
  X[3] = 2.;
  Y[3] = 6000.;                 // Top right
  X[4] = 1.15;
  Y[4] = 6000.;                 // Top
  X[5] = 0.2;
  Y[5] = 6000.;                 // Top left
  X[6] = 0.2;
  Y[6] = 4975.;                 // Left
  X[7] = 0.2;
  Y[7] = 1050.;                 // Bottom left

  // These are the expected results for the above 8 points for how
  // I think eospac is doing the extrapolation.
  Fexp[0] = -27.;
  Fexp[1] = -27.;
  Fexp[2] = 3.;
  Fexp[3] = 13.;
  Fexp[4] = 13.;
  Fexp[5] = 13.;
  Fexp[6] = 2.75;
  Fexp[7] = -36.5;

  // Do the interpolation.
  eos_Interpolate (&table_handles[0], &npairs, X, Y, F, dFx, dFy,
                   &error_code);

  // Check and print errors from the interpolation.
  // Note that we allow extrapolation.
  EOS_BOOLEAN equal;
  eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &error_code, &equal);
  if (error_code != EOS_OK && ! equal) {
    print_error (error_code, "eos_Interpolate");
    return 1;
  }

  // Print results.
  cout << endl;
  cout << "        X         Y         F        Fexp" << endl;
  for (i = 0; i < npairs; i++) {

    cout << setprecision (5) << setw (10) << X[i];
    cout << setprecision (5) << setw (10) << Y[i];
    cout << setprecision (5) << setw (10) << F[i];
    cout << setprecision (5) << setw (10) << Fexp[i];
    cout << endl;
  }
  cout << endl;

  // Destroy all data objects
  eos_DestroyAll (&error_code);
  if (error_code != EOS_OK) {
    eos_GetErrorMessage (&error_code, errorMessage);
    cout << "eos_DestroyAll ERROR " << error_code
	 << ": " << errorMessage << endl;
  }

  return 0;
}


// ===========================================================================
// Print a single error.
// ===========================================================================
void print_error (int ecode, string label)
{
  // Translate the error code into an error message.
  char emessage[EOS_MaxErrMsgLen];
  eos_GetErrorMessage (&ecode, emessage);

  // Print the error message.
  cout << label << " ERROR " << ecode << ": " << emessage << endl;
  cout << endl;
}


// ===========================================================================
// Print error messages for all the tables.
// ===========================================================================
void print_table_errors (int *handles, int ntables, string label)
{
  int i;
  for (i = 0; i < ntables; i++) {
    // Get the error code from the table.
    int ecode = EOS_OK;
    eos_GetErrorCode (&handles[i], &ecode);

    // Process the error if it is an error.
    if (ecode != EOS_OK) {
      // Translate the error code into an error message.
      char emessage[EOS_MaxErrMsgLen];
      eos_GetErrorMessage (&ecode, emessage);

      // Print the error message.
      cout << "Error in table " << i << endl;
      cout << "    " << label << " ERROR " << ecode << ": " <<
        emessage << endl;
      cout << endl;
    }
  }

  cout << endl;
}
