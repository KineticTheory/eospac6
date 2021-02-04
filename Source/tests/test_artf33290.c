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
 *  \brief Test for EOS_DISCONTINUOUS_DERIVATIVES option. Compare to default
 *         continuous derivatives in conjuncion with EOS_LINEAR option.
 *         In addition to interpolation, ensure the following functions
 *         work as expected:
 *         eos_GetPackedTablesSize, eos_GetPackedTables, eos_SetPackedTables
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf33290">artf33290</a>
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

/* local function prototype(s)  */
#define _N_INTERVAL_ 20
EOS_INTEGER _printDataForHandle (EOS_INTEGER th, EOS_INTEGER n_interval);

int main ()
{
  int i;
  EOS_REAL *data = NULL;
  enum
  { nTablesE = 2 };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, packedTablesSize;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;

  i = 0;
  if (i < nTables) tableType[i++] = EOS_Pt_DT;
  if (i < nTables) tableType[i++] = EOS_Pt_DT;

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
    /* report errors, but do not exit */
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }
    eos_SetOption (&tableHandle[i], &EOS_LINEAR, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      /* report errors, but do not exit */
    }
  }
  eos_SetOption (&tableHandle[nTables-1], &EOS_DISCONTINUOUS_DERIVATIVES, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    /* report errors, but do not exit */
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

#if 0
  /* print some data */
  printf ("#\n# BEFORE PACKING:\n#\n");

  errorCode = _printDataForHandle (tableHandle[0], 0);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }

  printf("\n\n"); /* insert two blank lines */

  for (i = 0; i < nTables; i++) {
    errorCode = _printDataForHandle (tableHandle[i], _N_INTERVAL_);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }
    if (i+1 < nTables) printf("\n"); /* insert line if another table pending */
  }
#endif

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
  printf ("#\n# AFTER UNPACKING:\n#\n");

  printf ("# tableHandle[%d..%d] = (",0,nTables-1);
  for (i = 0; i < nTables; i++) printf(" %d",tableHandle[i]);
  printf (" )\n#\n");

  errorCode = _printDataForHandle (tableHandle[0], 0);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }

  printf("\n\n"); /* insert two blank lines */

  for (i = 0; i < nTables; i++) {
    errorCode = _printDataForHandle (tableHandle[i], _N_INTERVAL_);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }
    if (i+1 < nTables) printf("\n"); /* insert line if another table pending */
  }

  /* destroy all tables */
  eos_DestroyAll (&errorCode);

  if (data) free(data);

  return 0;

}

// **********************************************************************
//      Sort an array a[] into ascending numerical order using a recursive
//      Quicksort algorithm.
// 
// The arguments are:
// 	EOS_INTEGER n   input: number of elements in a[].
// 	EOS_REAL*   a   in/out: replaced on output by its sorted rearrangement.
// 	EOS_INTEGER err output: error code.
// 
// Returned Values:
//      EOS_INTEGER _QuickSort    Return maximum number levels of recursion
//                                reached during sorting.
// 
// **********************************************************************/
#define QUICKSORT_RECURSION_LIMIT 500
static EOS_INTEGER max_recursion_level = 0;
enum { OK = 0,
       exceededQuicksortRecursionLimit = 13
};
EOS_INTEGER _QuickSort (EOS_INTEGER N, EOS_REAL a[], EOS_INTEGER lvl, EOS_INTEGER *err)
{
  EOS_INTEGER i = 0, j = N - 1;
  EOS_REAL x = a[N / 2], h;

  *err = EOS_OK;

  if (lvl == 0)
    max_recursion_level = 0;

  if (lvl > max_recursion_level)
    max_recursion_level = lvl;

  //  partition
  do {
    while (a[i] < x)
      i++;
    while (a[j] > x)
      j--;
    if (i <= j) {
      h = a[i];
      a[i] = a[j];
      a[j] = h;
      i++;
      j--;
    }
  } while (i <= j);

  if (lvl > QUICKSORT_RECURSION_LIMIT) {
    *err = exceededQuicksortRecursionLimit;
    return max_recursion_level;
  }

  //  recursion
  if (0 < j)
    _QuickSort (j + 1, &(a[0]), lvl + 1, err);
  if (*err != EOS_OK)
    return max_recursion_level;

  if (i < N - 1)
    _QuickSort (N - i, &(a[i]), lvl + 1, err);

  return max_recursion_level;
}

/*!
 * \brief This function is
 *
 * \par STDERR
 *       All information and error messages are returned
 *
 * \param[in] *argc       - int : number of variable arguments;
 *
 * \param[in] **argv      - char** : pointer to array of cmdline arguments;
 *
 */
int getSamples(EOS_BOOLEAN random, int N, EOS_REAL v_lower, EOS_REAL v_upper, EOS_REAL *v)
{
  int i, nparam=1;
  double *vdata, a=0, b=1;
  EOS_INTEGER err = EOS_OK;
  vdata = (double*)malloc(N*nparam*sizeof(EOS_REAL));
  if (random) latinCube(vdata, &a, &b, nparam, N);
  for (i=0;i<N;i++) {
    EOS_REAL fact = ((EOS_REAL)i)/((EOS_REAL)N);
    if (random) fact = (EOS_REAL)vdata[i];
    v[i] = v_lower + (v_upper - v_lower) * fact;
  }
  if (vdata) {
    free(vdata);
    vdata = NULL;
  }
  max_recursion_level = _QuickSort (N, v, 0, &err);

  return(err);
}

EOS_INTEGER _printDataForHandle (EOS_INTEGER th, EOS_INTEGER n_interval)
{
  int i, j, k, l;
  EOS_INTEGER matid, type;
  EOS_INTEGER infoItems[2];
  EOS_REAL infoVals[2];
  EOS_INTEGER errorCode = EOS_OK, two = 2;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_BOOLEAN equal = EOS_TRUE;

  EOS_REAL *X=NULL, *Y=NULL, *F=NULL, *X_tmp=NULL, *Y_tmp=NULL, *F_tmp=NULL, *dFx_tmp=NULL, *dFy_tmp=NULL;
  EOS_INTEGER num_dens, num_temp, num_items;

  /* get the matid and data type for this tableHandle */
  infoItems[0] = EOS_Material_ID;
  infoItems[1] = EOS_Table_Type;
  eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) return errorCode;
  matid = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
  type  = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */

  infoItems[0] = EOS_NR;
  infoItems[1] = EOS_NT;

  /* for this tableHandle, how many density values, and temperature values */
  eos_GetTableInfo(&th, &two, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) return errorCode;

  num_dens   = (EOS_INTEGER) (infoVals[0] + 0.5); /* round to nearest integer */
  num_temp   = (EOS_INTEGER) (infoVals[1] + 0.5); /* round to nearest integer */

  /* allocate memory */
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_dens);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_temp);

  num_items = num_dens * num_temp;
  F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);

  /* fetch loaded density data */
  *infoItems = EOS_R_Array;
  eos_GetTableInfo(&th, &num_dens, infoItems, X, &errorCode);
  if (errorCode != EOS_OK) return errorCode;

  /* fetch loaded temperature data */
  *infoItems = EOS_T_Array;
  eos_GetTableInfo(&th, &num_temp, infoItems, Y, &errorCode);
  if (errorCode != EOS_OK) return errorCode;

  printf("# TableHandle: %d\tDataType: %s\tMaterialID: %d\n# Description: %s\n",
         th, get_tableType_str(type), matid, get_tableType_description(type));
  printf("# EOS_LINEAR: %s\tEOS_DISCONTINUOUS_DERIVATIVES: %s\n#\n",
         (get_interpolationOptions_bval(th, EOS_LINEAR)?"true":"false"),
         (get_interpolationOptions_bval(th, EOS_DISCONTINUOUS_DERIVATIVES)?"true":"false"));

  /* print interpolated interval data to stdout */
  printf ("# %21s %23s", "density", "temperature");
  printf (" %23s", get_tableType_str(type));
  printf (" %23s", "dFx");
  printf (" %23s", "dFy");
  printf ("\n");

  if (n_interval > 0) {

    X_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * n_interval);
    Y_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * n_interval);
    F_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * n_interval);
    dFx_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * n_interval);
    dFy_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * n_interval);

    for (k = 0; k < 3 /* num_temp */; k++) {

      for (j = 0; j < n_interval; j++)
        Y_tmp[j] = Y[k];

      for (l = 1; l < num_dens; l++) {

        getSamples(EOS_FALSE, n_interval-1, X[l-1], X[l], X_tmp+1);
        X_tmp[0] = X[l-1];

        eos_Interpolate (&th, &n_interval, X_tmp, Y_tmp, F_tmp, dFx_tmp, dFy_tmp, &errorCode);
        eos_ErrorCodesEqual (&errorCode, (EOS_INTEGER *)&EOS_INTERP_EXTRAPOLATED, &equal);

        if (errorCode != EOS_OK && ! equal) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("%d: %s\n", errorCode, errorMessage);
        }

        for (j = 0; j < n_interval; j++)
          printf ("%23.15e %23.15e %23.15e %23.15e %23.15e\n",
                  X_tmp[j], Y_tmp[j], F_tmp[j], dFx_tmp[j], dFy_tmp[j]);

      }

      printf ("\n");

    }

  }
  else {

    X_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);
    Y_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);
    F_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);
    dFx_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);
    dFy_tmp = (EOS_REAL *) malloc (sizeof (EOS_REAL) * num_items);

    for (j=num_temp-1; j>=0; j--) { /* spread samples in arrays */
      for (i=0; i<num_dens; i++) {
        X_tmp[i+j*num_dens] = X[i];
        Y_tmp[i+j*num_dens] = Y[j];
      }
    }

    eos_Interpolate (&th, &num_items, X_tmp, Y_tmp, F_tmp, dFx_tmp, dFy_tmp, &errorCode);
    eos_ErrorCodesEqual (&errorCode, (EOS_INTEGER *)&EOS_INTERP_EXTRAPOLATED, &equal);

    if (errorCode != EOS_OK && ! equal) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%d: %s\n", errorCode, errorMessage);
    }

    j = 0;
    for (k = 0; k < 3 /* num_temp */; k++) {

      for (l = 0; l < num_dens; l++) {

        printf ("%23.15e %23.15e %23.15e %23.15e %23.15e\n",
                X_tmp[j], Y_tmp[j], F_tmp[j], dFx_tmp[j], dFy_tmp[j]);
        j++;

      }

      printf ("\n");

    }

  }

  /* deallocate memory */
  if (X) free(X);
  if (Y) free(Y);
  if (F) free(F);
  if (X_tmp) free(X_tmp);
  if (Y_tmp) free(Y_tmp);
  if (F_tmp) free(F_tmp);
  if (dFx_tmp) free(dFx_tmp);
  if (dFy_tmp) free(dFy_tmp);

  return errorCode;
}
