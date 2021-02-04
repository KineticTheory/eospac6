/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests
 *  \brief Ensure tables can be created, loaded and destroyed.
 *
 * Test the loading and interpolation of cold curve data for several SESAME materials.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 3333 3717 5251 5410
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i;
  enum
  { nTablesE = 4 };
  //    enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, one = 1;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;

  tableType[0] = EOS_Pc_D;      /* record type 1, substable 1 , cat 0 */
  tableType[1] = EOS_Pc_D;      /* record type 1, substable 1 , cat 2 */
  tableType[2] = EOS_Pc_D;      /* record type 1, substable 1 , cat 1 */
  tableType[3] = EOS_Pc_D;      /* record type 1, category 3: EOS_Pt_DT, EOS_T_DUt (where EOS_T_DUt is subtable 1, category 2) */

  matID[0] = 5410;
  matID[1] = 3333;
  matID[2] = 5251;
  matID[3] = 3717;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }
  else
    for (i = 0; i < nTables; i++)
      printf ("%i: successfuly created table type=%d, matId=%d\n", i,
              tableType[i], matID[i]);

  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  for (i = 0; i < nTables; i++) {
    eos_LoadTables (&one, &(tableHandle[i]), &errorCode);
    if (errorCode != EOS_OK) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    else
      printf ("%i: successfuly loaded table type=%d, matId=%d\n", i,
              tableType[i], matID[i]);
  }

  eos_DestroyAll (&errorCode);

  return 0;

}
