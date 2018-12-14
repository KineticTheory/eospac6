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
 *  \brief Test eos_GetTableInfo public function to ensure it returns
 *         the mean atomic number, the mean atomic mass, the normal
 *         solid density, the exchange coefficient, and the solid bulk
 *         modulus for eos_RecordType1, eos_RecordType2,
 *         eos_RecordType3, and eos_RecordType6 data objects.
 *
 * Uses the following routines:
 * eos_CreateTables
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_GetTableInfo
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 2030 2140 2161 13718
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i, j;
  enum
  { nTablesE = 8 };
  enum
  { nInfoItemsE = 8 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER nTables, nInfoItems;

  EOS_REAL infoVals[nInfoItemsE] =
    { 0., 0., 0., 0., 0., 0., 0., 0. };
  EOS_INTEGER infoItems[nInfoItemsE] =
    { EOS_Material_ID,
      EOS_Mean_Atomic_Num,
      EOS_Mean_Atomic_Mass,
      EOS_Normal_Density,
      EOS_Log_Val,
      EOS_Table_Type,
      EOS_Exchange_Coeff,
      EOS_Modulus
    };
  EOS_CHAR *infoItems_str[nInfoItemsE] =
    { "EOS_Material_ID",
      "EOS_Mean_Atomic_Num",
      "EOS_Mean_Atomic_Mass",
      "EOS_Normal_Density",
      "EOS_Log_Val",
      "EOS_Table_Type",
      "EOS_Exchange_Coeff",
      "EOS_Modulus"
    };
  EOS_BOOLEAN infoItems_int[nInfoItemsE] =
    { EOS_TRUE,
      EOS_FALSE,
      EOS_FALSE,
      EOS_FALSE,
      EOS_TRUE,
      EOS_TRUE,
      EOS_FALSE,
      EOS_FALSE
    };
  EOS_CHAR *tableDecription_str[nTablesE] =
    {
      "EOS_Pt_DT: record type 1, subtable 1",
      "EOS_Uls_T: record type 2, subtable 5",
      "EOS_Ogb: record type 3",
      "EOS_M_DT: record type 6",
      "EOS_Info: record type 5",
      "EOS_Info: record type 5",
      "EOS_Info: record type 5",
      "EOS_Info: record type 5"
    };

  nTables = nTablesE;

  /* EOS_Pt_DT, material 2140 works for table 301 (record type 1) */
  /* EOS_Uls_T, material 2030 works for table 401 (record type 2) */
  /* EOS_Ogb, material 13718 works for table 501 (record type 3) */
  /* EOS_Info, material 2140 works for tables 201 (record type 5) */
  /* EOS_Info, material 2030 works for tables 201 (record type 5) */
  /* EOS_Info, material 13718 works for tables 201 (record type 5) */
  /* EOS_Info, material 2161 works for tables 201 (record type 5) */
  tableType[0] = EOS_Pt_DT;     /*record type 1, subtable 1 */
  tableType[1] = EOS_Uls_T;     /*record type 2, subtable 5 */
  tableType[2] = EOS_Ogb;       /*record type 3             */
  tableType[3] = EOS_M_DT;      /*record type 6             */
  tableType[4] = EOS_Info;      /*record type 5             */
  tableType[5] = EOS_Info;      /*record type 5             */
  tableType[6] = EOS_Info;      /*record type 5             */
  tableType[7] = EOS_Info;      /*record type 5             */

  matID[0] = 2140;
  matID[1] = 2030;
  matID[2] = 13718;
  matID[3] = 2161;
  matID[4] = 2140;
  matID[5] = 2030;
  matID[6] = 13718;
  matID[7] = 2161;

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
    if (i == 0)
      eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    else
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
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

  for (i = 0; i < nTables; i++) {
    nInfoItems = nInfoItemsE;

    eos_GetTableInfo (&tableHandle[i], &nInfoItems, infoItems, infoVals, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_GetTableInfo ERROR %i: %s\n", errorCode, errorMessage);
    }

    if (i > 0) printf ("\n");
    printf ("%s\n", tableDecription_str[i]);
    for (j = 0; j < nInfoItems; j++) {
      if (infoItems_int[j])
	printf ("%20s: %d\n", infoItems_str[j], (EOS_INTEGER)infoVals[j]);
      else
	printf ("%20s: %22.15e\n", infoItems_str[j], infoVals[j]);
    }
  }
  return 0;

}
