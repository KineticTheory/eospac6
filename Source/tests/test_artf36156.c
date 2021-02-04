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
 *  \brief Verify EOS_Fmax option functions as advertised for eos_RecordType2
 *         objects.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf36156">artf36156</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2030 2140 2160 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "eos_Interface.h"

enum {
  nTablesE = 1
};

int main (int argc, char **argv)
{
  EOS_INTEGER i;

  EOS_INTEGER tableType = EOS_Pv_T;
  EOS_INTEGER matID;
  EOS_INTEGER matIDlist[] = { 2030, 2140, 2160, 3720 };
  EOS_INTEGER N;
  EOS_INTEGER tableHandle;
  EOS_INTEGER errorCode;
  EOS_INTEGER nTables;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  N = sizeof(matIDlist) / sizeof(matIDlist[0]);

  if (argc > 1) { 
    matIDlist[0] = (EOS_INTEGER) atoi(argv[1]);
    N = 1;
  }

  nTables = nTablesE;

  for(i=0; i<N; i++) {

    errorCode = EOS_OK;

    matID = matIDlist[i];

    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }

    eos_LoadTables (&nTables, &tableHandle, &errorCode);
    if (errorCode != EOS_OK) {

      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */

    }
    else { /* print data */

      EOS_INTEGER infoItem, one=1;
      EOS_REAL Tmax, Fmax, y, dF[2];
      EOS_CHAR s[50];

      /* SESAME currently lists the Fmax at Tmax in the 401 vapor pressure table;
	 this may be a poor assumption for future tabulations
      */
      infoItem = EOS_Tmax;
      eos_GetTableInfo(&tableHandle, &one, &infoItem, &Tmax, &errorCode);

      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("%d: %s\n", errorCode, errorMessage);
      }

      sprintf(s, "%s", "");
      infoItem = EOS_Fmax;
      eos_GetTableInfo(&tableHandle, &one, &infoItem, &Fmax, &errorCode);

      if (errorCode != EOS_OK) {
	eos_Interpolate(&tableHandle, &one, &Tmax, &y, &Fmax, &dF[0], &dF[1], &errorCode);
	sprintf(s, "%s", " (Interpolated)");
      }

      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("%d: %s\n", errorCode, errorMessage);
      }
      else {
	printf ("%d Critical Point: %.15e K , %.15e GPa%s\n", matID, Tmax, Fmax, s);
      }

    }

    /* destroy all tables */
    eos_DestroyAll (&errorCode);

  }

  return 0;

}

