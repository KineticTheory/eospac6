/*********************************************************************
 * Utility Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 * \ingroup C tests quick
 * \brief Ensure graceful handling of missing table dependencies in eos_SetExtrapolationBoundsRecordType1.
 *        See eos/eospac-dev/eospac6#554 for more details:
 *        https://git.lanl.gov/eos/eospac-dev/eospac6/issues/554
 *
 * \note
 * MATIDS TO TEST: 5030
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include "ses_defines.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}
#define MIN(x,y) (((x)<(y))? (x) : (y))
#define MAX(x,y) (((x)>(y))? (x) : (y))

EOS_BOOLEAN parseLong(const EOS_CHAR *str, EOS_INTEGER *rval)
{
  char *temp;
  EOS_BOOLEAN rc = EOS_TRUE;
  long val;
  errno = 0;
  val = strtol(str, &temp, 10);

  if (temp == str || *temp != '\0' ||
      ((val == LONG_MIN || val == LONG_MAX) && errno == ERANGE)) {
    rc = EOS_FALSE;
  }
  else {
    *rval = val;
  }
  return rc;
}

int main (int argc, char *argv[])
{
  enum { nTablesE = 5000 }; /* maximum number of SESAME tables allowed */

  EOS_INTEGER i, j;
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER tableTypes[nTablesE];
  EOS_INTEGER tableList[nTablesE];
  EOS_INTEGER matIDs[nTablesE];
  EOS_INTEGER err, errorCode, tableHandleErrorCode, nTables;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER *tableList_tmp, *tableSize;
  EOS_INTEGER nInsert = 0;

  if ((tableList_tmp = (EOS_INTEGER *) calloc (nTablesE, sizeof (EOS_INTEGER))) == NULL )
  {
    err = -10;
    goto CLEANUP;
  }
  if ((tableSize = (EOS_INTEGER *) calloc (nTablesE, sizeof (EOS_INTEGER))) == NULL )
  {
    err = -10;
    goto CLEANUP;
  }  
  /* set matID */
  matIDs[0] = 5030;

  {

    /* get the table list and associated sizes;
     * tableList[] array exent defined by first index containing -9999999;
     * tableSize[] contain no. 64-bit words in each table listed in tableList[] */
    err = get_matidTableInfo (matIDs[0], &tableList_tmp, &tableSize);
    if (err < 0) {
      fprintf(stderr,"\n%s ERROR %d: %s\n\n", argv[0], err,  "fatal get_matidTableInfo()");
      goto CLEANUP;
    }
    else if (err >= EOS_MIN_ERROR_CODE_VALUE) {
      eos_GetErrorMessage (&err, errorMessage);
      fprintf (stderr, "get_matidTableInfo ERROR %d: %s\n\n", err,  errorMessage);
      goto CLEANUP;
    }

    nTables = 0;
    for (i=0; i<nTablesE; i++) {
      EOS_INTEGER k=0;
      if (tableList_tmp[i] < 0) break;
      k = get_tableTypesFromSesameTableNumber (tableList_tmp[i], &tableTypes[nTables]);
      for (j=nTables; j<nTables+k; j++) {
        tableList[j] = tableList_tmp[i];
      }
      nTables += k;
    }

    {
      EOS_INTEGER k=0;
      for (j=0; j<nTables; j++) { /* remove all but category 0 types from tableTypes[] */
        EOS_INTEGER  c = get_dataTypeCategory(tableTypes[j]);
        if (c) continue;
        tableTypes[k] = tableTypes[j];
        tableList[k] = tableList[j];
        k++;
      }
      nTables = k;
    }
 
    if (nTables >= nTablesE)
    {
      err = -12;
      goto CLEANUP;
    }

    for (i=0; i<nTables; i++) {
      matIDs[i] = matIDs[0];
    }

  }

  /* set nInsert */
  nInsert = 1;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  {

    /* Initialize table data objects */
    printf ("--- Initialize ---\n");
    eos_CreateTables (&nTables, tableTypes, matIDs, tableHandle, &errorCode);
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
    }

    /* Enable EOS_INSERT_DATA option */
    {
      EOS_REAL num = (EOS_REAL)nInsert;
      EOS_INTEGER opt2 = EOS_INSERT_DATA;
      /* setup options */
      printf ("--- Set %s option: type=%i, nInsert=%f ---\n",
              get_optionStr_FromFlag(opt2), opt2, num);
      for (i = 0; i < nTables; i++) {
        eos_SetOption (&tableHandle[i], &opt2, &num, &errorCode);
        if (errorCode == EOS_OK) {
          EOS_CHAR str[100];
          sprintf(str, " with %s=%d option", get_optionStr_FromFlag(opt2), nInsert);
          printf ("    %2d. %s (table #%d) for material %i%s ***\n        %s\n",
                  i+1, get_tableType_str(tableTypes[i]), tableList[i], matIDs[i], str,
                  get_DataTypeDescription(tableTypes[i]));
        }
        else {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
    }
    /* Enable data EOS_DUMP_DATA or EOS_APPEND_DATA option */
    for (i = 0; i < nTables; i++) {
      if (i == 0) {
        eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
      else {
        eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
    }

    /* Load data into table data objects */
    printf ("--- Load data ---\n");
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
    }
    for (i = 0; i < nTables; i++) {
      EOS_INTEGER opt2 = EOS_INSERT_DATA;
      eos_GetErrorCode (&tableHandle[i], &errorCode);
      if (errorCode == EOS_OK) {
        EOS_CHAR str[100];
        sprintf(str, " with %s=%d option", get_optionStr_FromFlag(opt2), nInsert);
        printf ("    %2d. %s (table #%d) for material %i%s ***\n",
                i+1, get_tableType_str(tableTypes[i]), tableList[i], matIDs[i], str);
      }
    }
  }

 CLEANUP:
  /* Destroy all data objects */
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    tableHandleErrorCode = EOS_OK;
    eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
    eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
    printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode, errorMessage);
  }

  EOS_FREE(tableList_tmp);
  EOS_FREE(tableSize);

  return((int)err);

}
