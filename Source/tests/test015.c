/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup tests quick
 *  \brief Perform the following tests:
 *    -# Demonstrate that eos_GetVersionLength function works,
 *    -# and demonstrate expected error messages from various public
 *       functions.
 *
 * Verify that when either eos_Mix or eos_Interpolate receives nXYPairs=0 from the host code, that it
 * immediately sets an appropriate error code and exits (see issue #artf1976, https://tf.lanl.gov/sf/go/artf1976).
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_GetErrorMessage
 * eos_GetVersion
 * eos_GetVersionLength
 * eos_Interpolate
 * eos_Mix
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  EOS_INTEGER versionInfoLength;
  EOS_CHAR *versionInfo;
  EOS_INTEGER errorCode, tableHandle = 0, nXYPairs = 0, nMatIDsToMix =
    0, *xyBounds = NULL;
  EOS_REAL *xVals = NULL, *yVals = NULL, *fVals = NULL, *dFx = NULL, *dFy =
    NULL, *C = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  FILE *tableFile;
  static EOS_CHAR *fname = "TablesLoaded.dat";

  /* Create dummy TablesLoaded.dat file */
  tableFile = fopen (fname, "w");
  if (!tableFile)
    return 1;
  fprintf (tableFile, "dummy data 1 2 3 4 5 6 7 8 9 0\n");
  fclose (tableFile);

  eos_GetVersionLength (&versionInfoLength);
  versionInfo = malloc (sizeof (EOS_CHAR) * versionInfoLength);
  eos_GetVersion (versionInfo);

  printf ("versionInfoLength = '%d'\n", versionInfoLength);
  printf ("versionInfo = '%s'\n", versionInfo);

  printf ("\nForced interpolation error\n");
  eos_Interpolate (&tableHandle, &nXYPairs, xVals, yVals, fVals, dFx, dFy,
                   &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_Interpolate ERROR: %s\n\n", errorMessage);
  }

  printf ("\nForced checkExtrap error\n");
  eos_CheckExtrap (&tableHandle, &nXYPairs, xVals, yVals, xyBounds,
                   &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CheckExtrap ERROR: %s\n\n", errorMessage);
  }

  printf ("\nForced mixing error\n");
  eos_Mix (&nMatIDsToMix, &tableHandle, &nXYPairs, C, xVals, yVals, fVals,
           dFx, dFy, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_Mix ERROR: %s\n\n", errorMessage);
  }

  free(versionInfo);

  return 0;
}
