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
 *  \brief This is a unit test of the eos_GetVersionLength and
 *  eos_GetVersion public functions.
 *
 * Uses the following routines:
 * eos_GetVersion
 * eos_GetVersionLength
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"

int main ()
{
  EOS_INTEGER versionInfoLength;
  EOS_CHAR *versionInfo;
  static EOS_CHAR *versionInfoDefault =
    "No version information has been defined for this build of EOSPAC 6.";
  static EOS_CHAR *versionInfoRelease = "EOSPAC ";
  FILE *tableFile;
  static EOS_CHAR *fname = "TablesLoaded.dat";

  /* Create dummy TablesLoaded.dat file */
  tableFile = fopen (fname, "w");
  if (!tableFile)
    return -1;
  fprintf (tableFile, "dummy data 1 2 3 4 5 6 7 8 9 0\n");
  fclose (tableFile);

  eos_GetVersionLength (&versionInfoLength);
  versionInfo = malloc (sizeof (EOS_CHAR) * versionInfoLength);
  eos_GetVersion (versionInfo);

  printf ("versionInfoLength = '%d'\n", versionInfoLength);
  printf ("versionInfo = '%s'\n", versionInfo);

  if (strncmp (versionInfo, versionInfoRelease, strlen (versionInfoRelease))
      && strcmp (versionInfo, versionInfoDefault)) {
    printf ("versionInfo test failed %d\n", !EOS_OK);
  }
  else {
    printf ("versionInfo test passed %d\n", EOS_OK);
  }

  free(versionInfo);

  return 0;
}
