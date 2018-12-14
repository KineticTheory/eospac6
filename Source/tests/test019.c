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
 *  \brief Ensure that the mass fraction data associated with EOS_M_DT
 *    -# loads properly,
 *    -# can be correctly fetched using the public interface,
 *    -# and can be interpolated.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf4007">artf4007</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2161
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  enum
    { nTablesE = 1 };
  enum
    { nXYPairsE = 4 };

  int i, j, k, l, m;
  EOS_REAL *X=NULL, *Y=NULL, *Y_mod=NULL, *FXY=NULL, *dFx=NULL, *dFy=NULL, infoVals[3];
  EOS_INTEGER *xyBounds=NULL, infoItems[3], infoItem[1],
    num_phases, num_dens, num_temp, num_items;

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, nXYPairs, three = 3;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen], c[23];

#ifdef TEST_TABLESLOADED_DAT
  FILE *oFile;
#endif

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  tableType[0] = EOS_M_DT;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
    matID[i] = 2161;
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
    return errorCode;
  }

  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return errorCode;
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
    return errorCode;
  }


  /* test data retrieval options */

  infoItems[0] = EOS_NUM_PHASES;
  infoItems[1] = EOS_NR;
  infoItems[2] = EOS_NT;
  for (i = 0; i < nTables; i++) {

    /* for this tableHandle, how many
       phases (or subtables loaded),
       density values, and
       temperature values
    */
    eos_GetTableInfo(&tableHandle[i], &three, infoItems, infoVals, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    num_phases = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
    num_dens   = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */
    num_temp   = (EOS_INTEGER) (infoVals[2] + 0.5); /* round to nearest integer */

//#define DUMP_GETTABLEINFO
#ifdef DUMP_GETTABLEINFO

    /* allocate memory */
    X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_dens);
    Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_temp);
    num_items = num_phases * num_dens * num_temp;
    FXY = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);

#else /* ! DUMP_GETTABLEINFO */

    /* allocate memory */
    nXYPairs = num_dens * num_temp;
    X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
    Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_temp);
    Y_mod = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
    num_items = num_phases * num_dens * num_temp;
    FXY = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);
/*     dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items); */
/*     dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items); */
    xyBounds = (EOS_INTEGER *) malloc (sizeof (EOS_REAL) * num_items);

#endif /* DUMP_GETTABLEINFO */

    /* fetch loaded density data */
    *infoItem = EOS_R_Array;
    eos_GetTableInfo(&tableHandle[i], &num_dens, infoItem, X, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    /* fetch loaded temperature data */
    *infoItem = EOS_T_Array;
    eos_GetTableInfo(&tableHandle[i], &num_temp, infoItem, Y, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    /* comment the DUMP_GETTABLEINFO define below to fetch the loaded
       data using eos_GetTableInfo instead of using eos_Interpolate */
#ifdef DUMP_GETTABLEINFO

    /* fetch loaded function data */
    *infoItem = EOS_F_Array;
    eos_GetTableInfo(&tableHandle[i], &num_items, infoItem, FXY, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

#else /* ! DUMP_GETTABLEINFO */

    /* reset X and Y array values */
    m = 0;
    for (j = 0; j < num_temp; j++) {
      for (k = 0; k < num_dens; k++) {
	X[m] = X[k];
	Y_mod[m] = Y[j];
	m++;
      }
    }

    /* interpolate loaded function data to determine if grid is retained */
    printf ("--- TEST eos_Interpolate using category 0 tableType: EOS_M_DT --- (fcmp_ignore)\n");
    errorCode = EOS_OK;
    eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y_mod, FXY, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_Interpolate ERROR %d: %s (fcmp_ignore)\n", errorCode, errorMessage);
    }
    eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y_mod, xyBounds, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CheckExtrap ERROR %d: %s (fcmp_ignore)\n", errorCode, errorMessage);
    }

#endif /* DUMP_GETTABLEINFO */

    /* uncomment the TEST_TABLESLOADED_DAT define below to generate a file
       named TablesLoaded.txt, which is formatted like TablesLoaded.dat */
//#define TEST_TABLESLOADED_DAT
#ifdef TEST_TABLESLOADED_DAT
  oFile = fopen ("TablesLoaded.txt", "w");
#endif /* TEST_TABLESLOADED_DAT */

//#define ORIGINAL_STDOUT
#ifdef ORIGINAL_STDOUT
    /* print data arrays to stdout */
    for (j = 0; j < num_phases; j++) {
      printf ("Material %d, Table 321, Subtable %d\n", matID[i], j+1);
      for (k = 0; k < num_temp; k++) {
        for (l = 0; l < num_dens; l++) {
          printf ("%23.15e %23.15e %23.15e\n",
                  X[l], FXY[num_temp*num_dens*j + num_dens*k + l], Y[k]);
        }
	printf ("\n");
      }

#ifdef TEST_TABLESLOADED_DAT
      fprintf (oFile,"Material %d, Table 321, Subtable %d\n", matID[i], j+1);
      for (k = 0; k < num_temp; k += 10) {
	fprintf (oFile,"             y =");
	for (m = 0; m < 10; m++)
	  fprintf (oFile,"%16.7e", Y[k+m]);
	fprintf (oFile,"\n x =\n");
        for (l = 0; l < num_dens; l++) {
	  fprintf (oFile,"%16.7e", X[l]);
	  for (m = 0; m < 10; m++)
	    fprintf (oFile,"%16.7e",
		     FXY[num_temp*num_dens*j + num_dens*(k+m) + l]);
	  fprintf (oFile,"\n");
        }
	fprintf (oFile,"\n");
      }
#endif /* TEST_TABLESLOADED_DAT */
    }

#ifdef TEST_TABLESLOADED_DAT
    fclose(oFile);
#endif /* TEST_TABLESLOADED_DAT */

#else /* ! ORIGINAL_STDOUT */

    /* print data arrays to stdout */
    printf ("Material %d, Table 321\n", matID[i]);
    printf ("%23s %23s", "density", "temperature");
    for (j = 0; j < num_phases; j++) {
      sprintf (c, "Mass Fraction %d", j+1);
      printf (" %23s", c);
    }
    printf ("\n");
    for (k = 0; k < num_temp; k++) {
      for (l = 0; l < num_dens; l++) {
	printf ("%23.15e %23.15e", X[l], Y[k]);
	for (j = 0; j < num_phases; j++)
          printf (" %23.15e", FXY[num_temp*num_dens*j + num_dens*k + l]);
	printf ("\n");
      }
      printf ("\n");
    }

#endif /* ORIGINAL_STDOUT */

    /* deallocate memory */
    if (X) free(X);
    if (Y) free(Y);
    if (Y_mod) free(Y_mod);
    if (FXY) free(FXY);
    if (dFx) free(dFx);
    if (dFy) free(dFy);
    if (xyBounds) free(xyBounds);

    if (i+1 < nTables) printf("\n"); /* insert line if another table pending */
  }

  eos_DestroyAll (&errorCode);

  return 0;

}
