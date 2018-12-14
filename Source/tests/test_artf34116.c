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
 *  \brief Test for EOS_XY_MODIFY and EOS_XY_PASSTHRU option.
 *         In addition to interpolation, ensure the following functions
 *         work as expected:
 *         eos_GetPackedTablesSize, eos_GetPackedTables, eos_SetPackedTables
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf34116">artf34116</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

#define MAX(x,y) (((x)>(y))? (x) : (y))
#define MIN(x,y) (((x)<(y))? (x) : (y))

/* Define the number of interpolated values to display for each table handle. */
#define __SHORT__ 5

/* Define the number of intervals to interpolate between each tabulated data point. */
#define _N_INTERVAL_ 50

/* local function prototype(s)  */
EOS_INTEGER populateXYArrays (EOS_INTEGER th, EOS_INTEGER n_interval, EOS_INTEGER*N, EOS_REAL **X, EOS_REAL **Y);
EOS_INTEGER _calculateDataForHandle (EOS_INTEGER th, EOS_INTEGER N, EOS_REAL *X, EOS_REAL *Y, EOS_REAL **F, EOS_REAL **dFx, EOS_REAL **dFy);

/*
 * EOS_REAL    wctime    - wall clock time (seconds) elapsed since time state was reset
 * EOS_REAL    cputime   - cpu clock time (seconds) elapsed since time state was reset
 * EOS_REAL    cpucycles - cpu clock time (seconds) elapsed since time state was reset
 * EOS_INTEGER err       - error flag
 * EOS_INTEGER num_X     - number of x values use to calculate N
 * EOS_INTEGER num_Y     - number of y values use to calculate N
 * EOS_INTEGER N         - num_X * num_Y
 */
typedef struct {
    EOS_REAL wctime;
    EOS_REAL cputime;
    EOS_REAL cpucycles;
    EOS_INTEGER err;
    EOS_INTEGER num_X;
    EOS_INTEGER num_Y;
    EOS_INTEGER N;
} TIMER_RESULTS_t;

enum {
  nTablesE = 3
};

TIMER_RESULTS_t TIMER_RESULTS[nTablesE];
TIMER_RESULTS_t TIMER_RESULTS_TMP;

int main (int argc, char **argv)
{
  EOS_INTEGER i;

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER selectedtableType = EOS_Pt_DT;
  EOS_CHAR    selectedtableType_str[50];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_INTEGER nInterval = _N_INTERVAL_;
  EOS_INTEGER nTables, packedTablesSize;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  if (argc > 1)
    nInterval = (EOS_INTEGER) atoi(argv[1]);

  if (argc > 2) {
    strcpy(selectedtableType_str, argv[2]);
    selectedtableType = get_tableType_FromStr(selectedtableType_str);
    if (selectedtableType == EOS_NullTable) {
      printf("Invalid table type, %s\n", selectedtableType_str);
      return 1;
    }
  }

  nTables = nTablesE;
  if (argc > 3)
    nTables = MIN(nTablesE, (EOS_INTEGER) atoi(argv[3]));

  i = 0;
  if (i < nTables) tableType[i++] = selectedtableType;
  if (i < nTables) tableType[i++] = selectedtableType;
  if (i < nTables) tableType[i++] = selectedtableType;

  assert(i == nTables);

  for (i = 0; i < nTables; i++)
    matID[i] = 3720;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i: %s\n",
                tableHandle[i], tableHandleErrorCode, errorMessage);
      }
    }
    /* report errors, but do not exit */
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    EOS_REAL xconvert = 3.0;
    EOS_REAL yconvert = 4.0;
    EOS_REAL fconvert = 1.0;

    if (argc == 1) { /* assume regression testing */
      eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
	/* report errors, but do not exit */
      }
    }

    eos_SetOption (&tableHandle[i], &EOS_X_CONVERT, &xconvert, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }

    eos_SetOption (&tableHandle[i], &EOS_Y_CONVERT, &yconvert, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }

    eos_SetOption (&tableHandle[i], &EOS_F_CONVERT, &fconvert, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }

    if ( i < 1 ) continue;
    eos_SetOption (&tableHandle[i], &EOS_XY_MODIFY, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }

    if ( i < 2 ) continue;
    eos_SetOption (&tableHandle[i], &EOS_XY_PASSTHRU, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
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
    /* report errors, but do not exit */
  }

  /* print some data */
  printf ("#\n# PACKING tableHandle[%d..%d] = (",0,nTables-1);
  for (i = 0; i < nTables; i++) printf(" %d",tableHandle[i]);
  printf (" )\n#\n");

  /* get tables */
  eos_GetPackedTablesSize (&nTables, tableHandle, &packedTablesSize, &errorCode);
  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&nTables, tableHandle, packedData, &errorCode);

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  /* initialize tableHandle[] array values with huge integers to verify eos_SetPackedTables
   * will not fail due to using them before resetting them */
  if (sizeof((int)i) < sizeof(tableHandle[0]))
    for (i = 0; i < nTables; i++) tableHandle[i] = (EOS_INTEGER) LONG_MAX;
  else
    for (i = 0; i < nTables; i++) tableHandle[i] = (EOS_INTEGER) INT_MAX;

  /* pack tables */
  eos_SetPackedTables (&nTables, &packedTablesSize, packedData, tableHandle,
                       &errorCode);
  free (packedData);

  /* print some data */

  {
    EOS_INTEGER N;
    EOS_REAL *X = NULL, *Y = NULL;
    EOS_REAL *F[nTablesE], *dFx[nTablesE], *dFy[nTablesE];

    errorCode = populateXYArrays (tableHandle[0], nInterval, &N, &X, &Y);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    printf ("#\n"
	    "# AFTER UNPACKING\n"
	    "#    The following is a subset of %d of the actual %d interpolated values for each table handle:\n"
	    "#\n",
	    __SHORT__*2, N);

    for (i = 0; i < nTables; i++) {
      errorCode = _calculateDataForHandle (tableHandle[i], N, X, Y, &F[i], &dFx[i], &dFy[i]);
      if ( i < nTables-1) printf ("\n\n");
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("%d: %s\n", errorCode, errorMessage);
      }
    }

  }

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  /* print timer results */
  printf ( "\n\n#\n# TIMER_RESULTS\n" );
  printf ( "# %3s %10s %10s %10s %10s %10s %10s %10s %10s\n#\n",
	   "TH", "WCTIME", "CPUTIME", "CPUCYCLES", "ERR", "nInterval", "num_X", "num_Y", "N" );
  for (i = 0; i < nTables; i++) {
    printf ( "# %3d %10g %10g %10g %10d %10d %10d %10d %10d\n",
	     tableHandle[i],
	     TIMER_RESULTS[tableHandle[i]].wctime,
	     TIMER_RESULTS[tableHandle[i]].cputime,
	     TIMER_RESULTS[tableHandle[i]].cpucycles,
	     TIMER_RESULTS[tableHandle[i]].err,
	     nInterval,
	     TIMER_RESULTS[tableHandle[i]].num_X,
	     TIMER_RESULTS[tableHandle[i]].num_Y,
	     TIMER_RESULTS[tableHandle[i]].N);
  }
  printf ( "\n\n" );

  return 0;

}

EOS_INTEGER __populateXYArrays__ (EOS_INTEGER th, EOS_INTEGER n_interval, EOS_INTEGER *N, EOS_REAL **X, EOS_REAL **Y)
{
  EOS_INTEGER infoItems[2], i, j;
  EOS_REAL infoVals[2], xconv, yconv, *X_tmp=NULL, *Y_tmp=NULL;
  EOS_INTEGER errorCode = EOS_OK, two = 2;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER num_X, num_Y;

  /* for this tableHandle, how many density values, and temperature values? */
  infoItems[0] = EOS_NR;
  infoItems[1] = EOS_NT;
  eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) return errorCode;

  num_X   = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
  num_Y   = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */

  /* for this tableHandle, what are the defined conversion factors? */
  infoItems[0] = EOS_X_Convert_Factor;
  infoItems[1] = EOS_Y_Convert_Factor;
  eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) return errorCode;

  xconv = infoVals[0];
  yconv = infoVals[1];

  /* allocate memory */
  X_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_X);
  Y_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_Y);

  /* fetch loaded density data */
  *infoItems = EOS_R_Array;
  eos_GetTableInfo(&th, &num_X, infoItems, X_tmp, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  /* fetch loaded temperature data */
  *infoItems = EOS_T_Array;
  eos_GetTableInfo(&th, &num_Y, infoItems, Y_tmp, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  /* create intervals in X */
  assert( __eos_GetExpandedGrid (n_interval, &num_X, &X_tmp) == EOS_OK );

  /* create intervals in Y */
  assert( __eos_GetExpandedGrid (n_interval, &num_Y, &Y_tmp) == EOS_OK );

  /* allocate memory */
  *N = num_X * num_Y;
  *X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *N);
  *Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *N);

  TIMER_RESULTS_TMP.num_X = num_X;
  TIMER_RESULTS_TMP.num_Y = num_Y;
  TIMER_RESULTS_TMP.N        = *N;

  /* remove conversion factors */
  for (i = 0; i < num_X; i++) X_tmp[i] /= xconv;
  for (i = 0; i < num_Y; i++) Y_tmp[i] /= yconv;

  for (j=num_Y-1; j>=0; j--) { /* spread samples in arrays */
    for (i=0; i<num_X; i++) {
      (*X)[i+j*num_X] = X_tmp[i];
      (*Y)[i+j*num_X] = Y_tmp[j];
    }
  }

  if (X_tmp) free(X_tmp);
  if (Y_tmp) free(Y_tmp);

  return errorCode;
}

EOS_INTEGER populateXYArrays (EOS_INTEGER th_in, EOS_INTEGER n_interval, EOS_INTEGER *N, EOS_REAL **X, EOS_REAL **Y)
{
  EOS_REAL *Y_tmp=NULL, *dFx=NULL, *dFy=NULL;
  EOS_INTEGER errorCode = EOS_OK;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER cat, type, type0, matid, one = 1,indepVar2, th;

  matid = get_matID(th_in);
  type = get_table_type(th_in);
  cat = get_dataTypeCategory(type);
  indepVar2 = get_dataTypeIndepVar2(type);

  switch (cat) {
  case 0: /* data already loaded for non-inverted type */
    th = th_in;

    errorCode = __populateXYArrays__ (th, n_interval, N, X, Y);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    break;

  case 2: /* load data for inverted type */

    if      (type == EOS_T_DPt) {
      type0 = EOS_Pt_DT;
    }
    else if (type == EOS_T_DUt) {
      type0 = EOS_Ut_DT;
    }
    else if (type == EOS_T_DAt) {
      type0 = EOS_At_DT;
    }
    else if (type == EOS_T_DSt) {
      type0 = EOS_St_DT;
    }
    else {
      fprintf(stderr, "table type, %s, is unsupported\n", get_tableType_str(type));
      assert(0);
    }

    eos_CreateTables (&one, &type0, &matid,  &th, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf(stderr, "eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      assert(0);
    }

    eos_LoadTables (&one, &th, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf(stderr, "eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      assert(0);
    }

    errorCode = __populateXYArrays__ (th, n_interval, N, X, &Y_tmp);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    *Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *N);
    assert(*Y);
    dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *N);
    assert(dFx);
    dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * *N);
    assert(dFy);

    eos_Interpolate (&th, N, *X, Y_tmp, *Y, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }
    
    if (dFx) free(dFx);
    if (dFy) free(dFy);
    if (Y_tmp) free(Y_tmp);

    break;

  default:
    fprintf(stderr, "table type, %s, is of an unacceptable category, %d\n",
	    get_tableType_str(type), cat);
    assert(0);
    break;
  }

  TIMER_RESULTS[th_in].num_X = TIMER_RESULTS_TMP.num_X;
  TIMER_RESULTS[th_in].num_Y = TIMER_RESULTS_TMP.num_Y;
  TIMER_RESULTS[th_in].N        = TIMER_RESULTS_TMP.N;

  return errorCode;
}

EOS_INTEGER _calculateDataForHandle (EOS_INTEGER th, EOS_INTEGER N, EOS_REAL *X, EOS_REAL *Y,
				     EOS_REAL **F, EOS_REAL **dFx, EOS_REAL **dFy)
{
  int j;
  EOS_INTEGER errorCode = EOS_OK;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_BOOLEAN equal = EOS_TRUE;
  EOS_BOOLEAN true = EOS_TRUE, false = EOS_FALSE;

  /* allocate memory */
  *F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * N);
  *dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * N);
  *dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * N);

  printf ("--------------------\nTH = %d\n--------------------\n\n", th);

  eos_Time (&true, &TIMER_RESULTS[th].wctime, &TIMER_RESULTS[th].cputime,
	    &TIMER_RESULTS[th].cpucycles, &TIMER_RESULTS[th].err);

  eos_Interpolate (&th, &N, X, Y, *F, *dFx, *dFy, &errorCode);

  eos_Time (&false, &TIMER_RESULTS[th].wctime, &TIMER_RESULTS[th].cputime,
	    &TIMER_RESULTS[th].cpucycles, &TIMER_RESULTS[th].err);

  TIMER_RESULTS[th].num_X = TIMER_RESULTS[0].num_X;
  TIMER_RESULTS[th].num_Y = TIMER_RESULTS[0].num_Y;
  TIMER_RESULTS[th].N = TIMER_RESULTS[0].N;

  eos_ErrorCodesEqual (&errorCode, (EOS_INTEGER *)&EOS_INTERP_EXTRAPOLATED, &equal);

  if (errorCode != EOS_OK && ! equal) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
    return errorCode;
  }

  for (j = 0; j < N; j++)
#ifdef __SHORT__
    if ( j <  __SHORT__ || j > N-1-__SHORT__)
#endif
    printf ("%23.15e %23.15e %23.15e %23.15e %23.15e\n", X[j], Y[j], (*F)[j], (*dFx)[j], (*dFy)[j]);

  return errorCode;
}
