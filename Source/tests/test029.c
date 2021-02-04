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
 *  \brief This is a unit test of the eos_Utils::_eos_CreateGhostData
 *  and eos_Utils::_eos_DestroyGhostData functions.
 *
 *  Multiple tests are performed:
 *    -# expansion of a 2-D table
 *    -# expansion of a 1-D table
 *    -# failed expansion of a 2-D table
 *    -# failed expansion of a 1-D table
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdlib.h>
#include <stdio.h>
#include "../src/eos_types_internal.h"
#include "../src/eos_Utils.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{
  enum
  { NX_enum = 6 };
  enum
  { NY_enum = 5 };

  EOS_INTEGER err, i, j;
  FILE *tableFile;
  static EOS_CHAR *fname = "TablesLoaded.dat";

  EOS_INTEGER nGhostData=1, nxtbl_in=NX_enum, nytbl_in=NY_enum;
  EOS_REAL xtbls_in[NX_enum], ytbls_in[NY_enum], *ftbls_in[NY_enum],
    coldCurve_in[NX_enum];
  EOS_INTEGER nxtbl=0, nytbl=0;
  EOS_REAL *xtbls=NULL, *ytbls=NULL, **ftbls=NULL, *coldCurve=NULL;
  EOS_CHAR *errorMsg = NULL;

  /* Create dummy TablesLoaded.dat file */
  tableFile = fopen (fname, "w");
  if (!tableFile)
    return 1;
  fprintf (tableFile, "dummy data 1 2 3 4 5 6 7 8 9 0\n");
  fclose (tableFile);

  for (j = 0; j < nytbl_in; j++)
    ftbls_in[j] = (EOS_REAL*) malloc(nxtbl_in * sizeof(EOS_REAL));

  /**************************
   * expansion of a 2-D table
   **************************/
  err = EOS_OK;
  printf ("**** BEFORE 2-D EXPANSION ****\n");
  /* dump data in columnar format */
  printf("columnar format --------------\n");
  for (j = 0; j < nytbl_in; j++) {
    ytbls_in[j] = 2.0 * (EOS_REAL)(j);
    for (i = 0; i < nxtbl_in; i++) {
      xtbls_in[i] = 2.0 * (EOS_REAL)((i+2)*(i+2));
      ftbls_in[j][i] = 4.0 * xtbls_in[i] * ytbls_in[j] - 4.0;
      coldCurve_in[i] = ftbls_in[0][i];
      printf ("x[%i] = %23.15e   y[%i] = %23.15e   f[%i][%i] = %23.15e   CC[%i] = %23.15e\n",
	      i, xtbls_in[i], j, ytbls_in[j], j, i, ftbls_in[j][i], i, coldCurve_in[i]);
    }
  }

  /* dump data in tabular format */
  printf("\ntabular format --------------\n");
  printf("%23s "," ");
  for (j = 0; j < nytbl_in; j++)
    printf ("%23.15e ", ytbls_in[j]);
  printf("\n");
  for (i = 0; i < nxtbl_in; i++) {
    printf ("%23.15e ", xtbls_in[i]);
    for (j = 0; j < nytbl_in; j++)
      printf ("%23.15e ", ftbls_in[j][i]);
    printf("\n");
  }

  _eos_CreateGhostData (EOS_FALSE, nGhostData, nxtbl_in, nytbl_in, xtbls_in, ytbls_in, ftbls_in, coldCurve_in,
			&nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, &coldCurve, &err, &errorMsg);

  printf ("\n**** AFTER 2-D EXPANSION ****\n");

  if (err != EOS_OK) {
    printf("_eos_CreateGhostData ERROR %i %s\n", err, errorMsg);
    return err;
  }
  EOS_FREE(errorMsg);

  if (! (xtbls && ytbls && ftbls && coldCurve)) {
    printf("memory allocation failed in _eos_CreateGhostData\n");
    return 1;
  }

  /* dump new data in columnar format */
  printf("columnar format --------------\n");
  for (j = 0; j < nytbl; j++) {
    for (i = 0; i < nxtbl; i++) {
      printf ("x[%i] = %23.15e   y[%i] = %23.15e   f[%i][%i] = %23.15e   CC[%i] = %23.15e\n",
	      i, xtbls[i], j, ytbls[j], j, i, ftbls[j][i], i, coldCurve[i]);
    }
  }

  /* dump new data in tabular format */
  printf("\ntabular format --------------\n");
  printf("%23s "," ");
  for (j = 0; j < nytbl; j++)
    printf ("%23.15e ", ytbls[j]);
  printf("\n");
  for (i = 0; i < nxtbl; i++) {
    printf ("%23.15e ", xtbls[i]);
    for (j = 0; j < nytbl; j++)
      printf ("%23.15e ", ftbls[j][i]);
    printf("\n");
  }

  /* free memory used to store expanded table */
  _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

  /**************************
   * expansion of a 1-D table
   **************************/
  err = EOS_OK;
  printf ("\n**** BEFORE 1-D EXPANSION ****\n");
  /* dump data in columnar format */
  printf("columnar format --------------\n");
  nytbl_in = 1;
  for (j = 0; j < nytbl_in; j++) {
    ytbls_in[j] = 2.0 * (EOS_REAL)(j);
    for (i = 0; i < nxtbl_in; i++) {
      xtbls_in[i] = 2.0 * (EOS_REAL)((i+2)*(i+2));
      ftbls_in[j][i] = 4.0 * xtbls_in[i] * xtbls_in[i] * xtbls_in[i] - 4.0;
      printf ("x[%i] = %23.15e   y[%i] = %23.15e   f[%i][%i] = %23.15e\n",
	      i, xtbls_in[i], j, ytbls_in[j], j, i, ftbls_in[j][i]);
    }
  }

  _eos_CreateGhostData (EOS_FALSE, nGhostData, nxtbl_in, nytbl_in, xtbls_in, ytbls_in, ftbls_in, NULL,
			&nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errorMsg);

  printf ("\n**** AFTER 1-D EXPANSION ****\n");

  if (err != EOS_OK) {
    printf("_eos_CreateGhostData ERROR %i %s\n", err, errorMsg);
    return err;
  }
  EOS_FREE(errorMsg);

  if (! (xtbls && ytbls && ftbls)) {
    printf("memory allocation failed in _eos_CreateGhostData\n");
    return 1;
  }

  /* dump new data in columnar format */
  printf("columnar format --------------\n");
  for (j = 0; j < nytbl; j++) {
    for (i = 0; i < nxtbl; i++) {
      printf ("x[%i] = %23.15e   y[%i] = %23.15e   f[%i][%i] = %23.15e\n",
	      i, xtbls[i], j, ytbls[j], j, i, ftbls[j][i]);
    }
  }

  /* free memory used to store expanded table */
  _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

  /******************************************
   * expansion of a 1-D table with ytbls=NULL
   ******************************************/
  err = EOS_OK;
  printf ("\n**** BEFORE 1-D EXPANSION WITH YTBLS=NULL ****\n");
  /* dump data in columnar format */
  printf("columnar format --------------\n");
  nytbl_in = 999;
  j = 0;
  for (i = 0; i < nxtbl_in; i++) {
    xtbls_in[i] = 5.0 * (EOS_REAL)((i+2)*(i+2));
    ftbls_in[j][i] = 2.0 * xtbls_in[i] * xtbls_in[i] * xtbls_in[i] - 4.0;
    printf ("x[%i] = %23.15e   f[%i][%i] = %23.15e\n",
	    i, xtbls_in[i], j, i, ftbls_in[j][i]);
  }

  _eos_CreateGhostData (EOS_FALSE, nGhostData, nxtbl_in, nytbl_in, xtbls_in, NULL, ftbls_in, NULL,
			&nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errorMsg);

  printf ("\n**** AFTER 1-D EXPANSION WITH YTBLS=NULL ****\n");

  if (err != EOS_OK) {
    printf("_eos_CreateGhostData ERROR %i %s\n", err, errorMsg);
    return err;
  }
  EOS_FREE(errorMsg);

  if (! (xtbls && ytbls && ftbls)) {
    printf("memory allocation failed in _eos_CreateGhostData\n");
    return 1;
  }

  /* dump new data in columnar format */
  printf("columnar format --------------\n");
  j = 0;
  for (i = 0; i < nxtbl; i++) {
    printf ("x[%i] = %23.15e   f[%i][%i] = %23.15e\n",
	    i, xtbls[i], j, i, ftbls[j][i]);
  }

  /* free memory used to store expanded table */
  _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

  printf ("\n**** FORCED ERRORS ****\n");

  nytbl_in = 0;
  _eos_CreateGhostData (EOS_FALSE, nGhostData, nxtbl_in, nytbl_in, xtbls_in, ytbls_in, ftbls_in, coldCurve_in,
			&nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errorMsg);

  if (err != EOS_OK) {
    printf("_eos_CreateGhostData ERROR %i %s\n", err, errorMsg);
  }
  EOS_FREE(errorMsg);

  /* free memory used to store expanded table */
  _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

  nxtbl_in = 0;
  nytbl_in = 5;
  _eos_CreateGhostData (EOS_FALSE, nGhostData, nxtbl_in, nytbl_in, xtbls_in, NULL, ftbls_in, coldCurve_in,
			&nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, &errorMsg);

  if (err != EOS_OK) {
    printf("_eos_CreateGhostData ERROR %i %s\n", err, errorMsg);
  }
  EOS_FREE(errorMsg);

  /* free memory used to store expanded table */
  _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);

  /* deallocate memory */
  for (j = 0; j < nytbl_in; j++)
    free(ftbls_in[j]);

  return 0;
}
