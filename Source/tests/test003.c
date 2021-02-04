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
 *  \brief Test eos_GetTableInfo and eos_GetTableCmnts public functions.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_GetTableCmnts
 * eos_GetTableInfo
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 2030 2140 4100 13718
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i;
  enum
  { nTablesE = 5 };
//      enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER nTables, six = 6, four = 4, eight = 8;
  EOS_CHAR *cmntStr = NULL;

  EOS_REAL infoVals[10] = { 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. };
  EOS_INTEGER infoItems[10];

  nTables = nTablesE;

  /* EOS_Pt_DT, material 2140 works for table 301 (record type 1) */
  /* EOS_Uls_T, material 2030 works for table 401 (record type 2) */
  /* EOS_Ogb, material 13718 works for table 501 (record type 3) */
  /* EOS_Comment, material 4100 works for tables 101-199 (record type 4) */
  /* EOS_Info, material 2140 works for tables 201 (record type 5) */
  tableType[0] = EOS_Pt_DT;     /*record type 1, subtable 1 */
  tableType[1] = EOS_Uls_T;     /*record type 2, subtable 5 */
  tableType[2] = EOS_Ogb;
  tableType[3] = EOS_Comment;
  tableType[4] = EOS_Info;

  matID[0] = 2140;
  matID[1] = 2030;
  matID[2] = 13718;
  matID[3] = 4100;
  matID[4] = 2140;

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

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    }
  }

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    return 0;
  }


  /* packed tables */

  infoItems[0] = EOS_X_Convert_Factor;
  infoItems[1] = EOS_Y_Convert_Factor;
  infoItems[2] = EOS_F_Convert_Factor;
  infoItems[3] = EOS_Log_Val;
  infoItems[4] = EOS_Material_ID;
  infoItems[5] = EOS_Table_Type;

  eos_GetTableInfo (&tableHandle[0], &six, infoItems, infoVals, &errorCode);
  eos_GetTableInfo (&tableHandle[1], &six, infoItems, infoVals, &errorCode);
  eos_GetTableInfo (&tableHandle[2], &six, infoItems, infoVals, &errorCode);

  infoItems[0] = EOS_Cmnt_Len;
  infoItems[1] = EOS_Log_Val;
  infoItems[2] = EOS_Material_ID;
  infoItems[3] = EOS_Table_Type;

  eos_GetTableInfo (&tableHandle[3], &four, infoItems, infoVals, &errorCode);
  cmntStr = (EOS_CHAR *) malloc ((int) infoVals[0] * sizeof (EOS_CHAR));
  eos_GetTableCmnts (&tableHandle[3], cmntStr, &errorCode);
  printf ("%s\n", cmntStr);
  free (cmntStr);

  infoItems[0] = EOS_Mean_Atomic_Num;
  infoItems[1] = EOS_Mean_Atomic_Mass;
  infoItems[2] = EOS_Modulus;
  infoItems[3] = EOS_Log_Val;
  infoItems[4] = EOS_Material_ID;
  infoItems[5] = EOS_Table_Type;
  infoItems[6] = EOS_Normal_Density;
  infoItems[7] = EOS_Exchange_Coeff;

  eos_GetTableInfo (&tableHandle[4], &eight, infoItems, infoVals, &errorCode);
  eos_DestroyAll (&errorCode);
  return 0;

}
