/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup CPP tests
 *  \brief Ensure data is correctly monotonic per data type requirements.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf5435">artf5837</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <iostream>
#include <stdio.h>
#include "../src/eos_Interface.h"
#include <math.h>
#include <cstdlib>
#include "TEST_FUNCTIONS.h"
using namespace std;

#define SIZE(x) 6-(int)log10(fabs(x))

void _printf_r (const char *s, const EOS_REAL val, const char *s2="")
{
  if (val > 1.0e-1 && val < 1.0e4)
    printf("%s%.*f%s", s, SIZE(val), val, s2);
  else
    printf("%s%.2e%s", s, val, s2);
}

// main
int main ()
{
  // Set the number of tables.
  const EOS_INTEGER nTablesE = 4;
  EOS_INTEGER nTables = nTablesE;
  int sesid = 3720;

  // Setup the table types.
  EOS_INTEGER *tableType;
  const EOS_INTEGER nTestIterE = 1;
  EOS_INTEGER tableType_Iter[nTestIterE][nTablesE] = {
    {EOS_Info, EOS_D_PtT, EOS_Ut_DT, EOS_Pt_DUt}
  };

  // Setup the material id for each of the tables.
  EOS_INTEGER matID[nTablesE];
  matID[0] = sesid;
  matID[1] = sesid;
  matID[2] = sesid;
  matID[3] = sesid;

  // Initialize the error code.
  EOS_INTEGER errorCode = EOS_OK;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen]=" ";
  EOS_INTEGER tableHandleErrorCode = EOS_OK;

  // Setup and default the table handles.
  EOS_INTEGER tableHandle[nTablesE];
 
  // Initialize interpolator input

  int i, j, k;

  for (j = 0; j < nTestIterE; j++) {

    tableType = tableType_Iter[j];

    // print test iteration header
    if (j>0) cout << endl;
    cout << "----- TEST #" << j << " using ";
    for (k = 0; k < nTables; k++)
      cout << ((k > 0 && k < nTables-1)
               ? ", "
               : ((k == 0)
                  ? ""
                  : ", and "
                  )
               )
           << get_tableType_str(tableType[k]);
    cout << " -----" << endl << endl;

    for (i = 0; i < nTables; i++)
      tableHandle[i] = 0;

    // Create the tables.
    eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      for (int i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("           TH %i ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }

    /* Enable data dump to file */
    for (i = 0; i < nTables; i++) {
      if (i == 0 && j == 0)
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

    for(i = 1; i<nTables;i++) {
      eos_SetOption(&tableHandle[i], &EOS_MONOTONIC_IN_Y, EOS_NullPtr, &errorCode);
      if(errorCode != EOS_OK)
        printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }

    // Display gEosDataMap.tableHandlesMap details before calling eos_LoadTables
    //     cout << "BEFORE EOS_LOADTABLES" << endl;
    //     display_gEosDataMap_tableHandlesMap_details();

    // load data into table data objects
    eos_LoadTables (&nTables, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      for (int i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("         TH %i ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }

    cout << endl;

    // Display gEosDataMap.tableHandlesMap details after calling eos_LoadTables
    cout << "AFTER EOS_LOADTABLES" << endl;
    display_gEosDataMap_tableHandlesMap_details();

    // Destroy all data
    eos_DestroyAll(&errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
      for (int i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("         TH %i ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }

  } // end  j-loop

  return 0;

}
