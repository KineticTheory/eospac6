/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup C tests quick
 *  \brief This is a unit test of the eos_Utils::eos_SetCustomErrorMsg
 *  function.
 *
 * Uses the following routines:
 * eos_SetCustomErrorMsg
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>

#include "eos_Interface.h"

/* prototypes from eos_Utils.h */
EOS_CHAR* eos_GetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err);
void eos_SetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err, const EOS_CHAR *fmt, ...);


void eos_CreateTables_work(EOS_INTEGER tableType, EOS_INTEGER matID, EOS_INTEGER *tableHandle)
{
  EOS_INTEGER i;
  EOS_INTEGER nTables = 1;
  EOS_INTEGER tableHandleErrorCode = EOS_OK;
  EOS_INTEGER errorCode = EOS_OK;
  EOS_CHAR    errorMessage[EOS_MaxErrMsgLen];

  eos_CreateTables (&nTables, &tableType, &matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (tableHandle, &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("TH=%i eos_CreateTables ERROR %i: %s (fcmp_ignore)\n",
		*tableHandle, tableHandleErrorCode, errorMessage);
      }
    }
  }
}

int main ()
{
  EOS_INTEGER err;
  FILE *tableFile;
  static EOS_CHAR *fname = "TablesLoaded.dat";
  EOS_INTEGER tableType = EOS_Pt_DT;
  EOS_INTEGER matID     = 3720;
  EOS_INTEGER tableHandle;

  /* initialize an arbitrary table */
  eos_CreateTables_work(tableType, matID, &tableHandle);

  /* Create dummy TablesLoaded.dat file */
  tableFile = fopen (fname, "w");
  if (!tableFile)
    return -1;
  fprintf (tableFile, "dummy data 1 2 3 4 5 6 7 8 9 0\n");
  fclose (tableFile);

  err = EOS_CONVERGENCE_FAILED;
  eos_SetCustomErrorMsg (tableHandle, err,
                         "int %i, sci: %e, float: %f\n", 100, 100., 100.);
  printf ("%s", eos_GetCustomErrorMsg (tableHandle, err));

  eos_DestroyAll (&err);
  
  return 0;
}
