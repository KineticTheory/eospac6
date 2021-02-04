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
 *  \brief Ensure selected tables, which are associated with Sesame 411 and 412 tables
 *         can be created, loaded, fetched, interpolated and destroyed.
 *
 * \note
 * MATIDS TO TEST: 33336
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include <assert.h>
#include <math.h>

#define BAK_GET_CAT0_ARRAY_INDEX(t) ((t == EOS_Tm_D) ? 0 :	\
				  (t == EOS_Pm_D) ? 1 :	\
				  (t == EOS_Um_D) ? 2 :	\
				  (t == EOS_Am_D) ? 3 :	\
				  (t == EOS_Tf_D) ? 4 :	\
				  (t == EOS_Pf_D) ? 5 :	\
				  (t == EOS_Uf_D) ? 6 :	\
				  (t == EOS_Af_D) ? 7 : 8)
EOS_INTEGER _GET_CAT0_ARRAY_INDEX(EOS_INTEGER t, EOS_INTEGER *tableType, EOS_INTEGER nTables) {
  EOS_INTEGER i;

  assert ((t == EOS_Tm_D) ||
	  (t == EOS_Pm_D) ||
	  (t == EOS_Um_D) ||
	  (t == EOS_Am_D) ||
	  (t == EOS_Tf_D) ||
	  (t == EOS_Pf_D) ||
	  (t == EOS_Uf_D) ||
	  (t == EOS_Af_D));
  for (i=0; i<nTables; i++)
    if (t == tableType[i]) break;

  assert (i < nTables);

  return i;
}

EOS_INTEGER _GET_CAT1_CAT0XREF(EOS_INTEGER t, EOS_INTEGER *tableType, EOS_INTEGER nTables) {
  EOS_INTEGER t_ref=-1;

  if      (t == EOS_D_Tm) t_ref = EOS_Tm_D;
  else if (t == EOS_D_Pm) t_ref = EOS_Pm_D;
  else if (t == EOS_D_Um) t_ref = EOS_Um_D;
  else if (t == EOS_D_Am) t_ref = EOS_Am_D;
  else if (t == EOS_D_Tf) t_ref = EOS_Tf_D;
  else if (t == EOS_D_Pf) t_ref = EOS_Pf_D;
  else if (t == EOS_D_Uf) t_ref = EOS_Uf_D;
  else if (t == EOS_D_Af) t_ref = EOS_Af_D;
  else printf("No t_ref found for %s\n", get_tableType_str(t));
  assert(t_ref != -1);

  return t_ref;
}

#define EOS_FREE(p) { assert(p != NULL); free(p); p=NULL; }


int main ()
{
  int i, j, k;
  EOS_REAL e1 = 1.0e-10; /* relative difference tolerance */
  EOS_REAL e2 = 1.0e-20; /* absolute difference tolerance */
  enum
  { nTablesE = 24 };
  enum
  { nTables_nonMonoE = 2 };
  enum
  { nXYPairsE = 4 };
  //    enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE] =
    { /* cat 0 */
      EOS_Tm_D, EOS_Pm_D, EOS_Um_D, EOS_Tf_D, EOS_Pf_D, EOS_Uf_D,
      /* cat 1 */
      EOS_D_Tm, EOS_D_Pm, EOS_D_Um, EOS_D_Tf, EOS_D_Pf, EOS_D_Uf,
      /* cat 3 */
      EOS_Tm_Pm, EOS_Tm_Um, EOS_Pm_Tm, EOS_Pm_Um, EOS_Um_Tm, EOS_Um_Pm,
      EOS_Tf_Pf, EOS_Tf_Uf, EOS_Pf_Tf, EOS_Pf_Uf, EOS_Uf_Tf, EOS_Uf_Pf
    };
  EOS_INTEGER tableType_nonMono[nTables_nonMonoE] =
    { /* cat 0 */
      EOS_Pm_D, EOS_Pf_D
    };
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE], tableHandle_nonMono[nTables_nonMonoE];
  EOS_INTEGER *TH, *TT;
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_INTEGER cat, ref1, ref2;
  EOS_INTEGER nTables, nTables_nonMono, nXYPairs, one = 1;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER infoItem[1], NX[nTablesE+nTables_nonMonoE], NY[nTablesE+nTables_nonMonoE], *xyBounds=NULL, index, index2, nx, ny, t_ref;
  EOS_REAL val, *X=NULL, *Y=NULL, *F=NULL, *dFx=NULL, *dFy=NULL, *F_ref;
  EOS_REAL *X0[nTablesE+nTables_nonMonoE], *Y0[nTablesE+nTables_nonMonoE], *F0[nTablesE+nTables_nonMonoE];

  nTables = nTablesE;
  nTables_nonMono = nTables_nonMonoE;

  for (i = 0; i < nTables; i++) {
    X0[i] = NULL;
    Y0[i] = NULL;
    F0[i] = NULL;
  }

  for (i = 0; i < nTables; i++)
    matID[i] = 33336;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  /* initialize table handles */
  printf("\n--- INITIALIZE TABLE HANDLES ---\n");
  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i for %s: %s\n",
                tableHandle[i], tableHandleErrorCode, get_tableType_str(tableType[i]), errorMessage);
      }
    }
    return 0;
  }
  else
    for (i = 0; i < nTables; i++)
      printf ("%i: successfully created forced-monotonic table type=%s (%d), matId=%d\n", tableHandle[i],
              get_tableType_str(tableType[i]), tableType[i], matID[i]);

  eos_CreateTables (&nTables_nonMono, tableType_nonMono, matID, tableHandle_nonMono, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables_nonMono; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle_nonMono[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_CreateTables ERROR %i for %s: %s\n",
                tableHandle_nonMono[i], tableHandleErrorCode, get_tableType_str(tableType_nonMono[i]), errorMessage);
      }
    }
    return 0;
  }
  else
    for (i = 0; i < nTables_nonMono; i++)
      printf ("%i: successfully created table type=%s (%d), matId=%d\n", tableHandle_nonMono[i],
              get_tableType_str(tableType_nonMono[i]), tableType_nonMono[i], matID[i]);

  /* initialize table handles' options */
  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  for (i = 0; i < nTables; i++) { /* enforce monotonicity of cat 0 data for future
				     interpolations and comparisons */
    cat = get_dataTypeCategory(tableType[i]);
    if (cat != 0)
      continue;
    if (tableType[i] != EOS_Pm_D && tableType[i] != EOS_Pf_D)
      continue;
    eos_SetOption (&tableHandle[i], &EOS_MONOTONIC_IN_X, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return 0;
    }
  }

  /* load data */
  printf("\n--- LOAD DATA ---\n");
  for (i = 0; i < nTables; i++) {
    eos_LoadTables (&one, &(tableHandle[i]), &errorCode);
    if (errorCode != EOS_OK) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i for %s: %s\n",
                tableHandle[i], tableHandleErrorCode, get_tableType_str(tableType[i]), errorMessage);
      }
    }
    else
      printf ("%i: successfully loaded forced-monotonic table type=%s (%d), matId=%d\n", tableHandle[i],
              get_tableType_str(tableType[i]), tableType[i], matID[i]);
  }

  for (i = 0; i < nTables_nonMono; i++) {
    eos_LoadTables (&one, &(tableHandle_nonMono[i]), &errorCode);
    if (errorCode != EOS_OK) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle_nonMono[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("%i: eos_LoadTables ERROR %i for %s: %s\n",
                tableHandle_nonMono[i], tableHandleErrorCode, get_tableType_str(tableType_nonMono[i]), errorMessage);
      }
    }
    else
      printf ("%i: successfully loaded table type=%s (%d), matId=%d\n", tableHandle_nonMono[i],
              get_tableType_str(tableType_nonMono[i]), tableType[i], matID[i]);
  }

  /* fetch category 0 data for later comparisons */
  printf("\n--- FETCH DATA ---\n\n");
  for (k = 0; k < nTables + nTables_nonMono; k++) {

    if (k < nTables) {
      i = k;
      TH = tableHandle;
      TT = tableType;
    }
    else {
      i = k - nTables;
      TH = tableHandle_nonMono;
      TT = tableType_nonMono;
    }

    cat = get_dataTypeCategory(TT[i]);

    if (cat != 0)
      continue;

    ref1 = get_dataTypeReference1(TT[i]);
    ref2 = get_dataTypeReference2(TT[i]);

    printf ("%-10s => cat %i, ref1=%-13s and ref2=%-13s    (TH: %i)%s\n",
	    get_tableType_str(TT[i]), cat, get_tableType_str(ref1),
	    get_tableType_str(ref2), TH[i],
	    ((TH == tableHandle) ? " forced-monotonic" : ""));

    index = k;

    infoItem[0] = EOS_NR;
    eos_GetTableInfo (&TH[i], &one, infoItem, &val, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	      TH[i], errorCode, get_tableType_str(TT[i]), errorMessage);
      return errorCode;
    }
    NX[index] = (EOS_INTEGER) val;

    infoItem[0] = EOS_NT;
    eos_GetTableInfo (&TH[i], &one, infoItem, &val, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	      TH[i], errorCode, get_tableType_str(TT[i]), errorMessage);
      return errorCode;
    }
    NY[index] = (EOS_INTEGER) val;

    nXYPairs = NX[index] * NY[index];

    /* allocate memory */
    X0[index] = (EOS_REAL *) safe_malloc (NX[index], sizeof (EOS_REAL));
    Y0[index] = (EOS_REAL *) safe_malloc (NY[index], sizeof (EOS_REAL));
    F0[index] = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));

    if (! (X0[index] && Y0[index] && F0[index])) {
      printf ("Memory allocation error!\n");
      return 1;
    }

    /* initialize array(s) */
    infoItem[0] = EOS_R_Array;
    eos_GetTableInfo (&TH[i], &NX[index], infoItem, X0[index], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	      TH[i], errorCode, get_tableType_str(TT[i]), errorMessage);
      return errorCode;
    }

    infoItem[0] = EOS_T_Array;
    eos_GetTableInfo (&TH[i], &NY[index], infoItem, Y0[index], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	      TH[i], errorCode, get_tableType_str(TT[i]), errorMessage);
      return errorCode;
    }

    infoItem[0] = EOS_F_Array;
    eos_GetTableInfo (&TH[i], &nXYPairs, infoItem, F0[index], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	      TH[i], errorCode, get_tableType_str(TT[i]), errorMessage);
      return errorCode;
    }

#if 1
    for (j = 0; j < NX[index]; j++)
      printf("\tX0 = %23.15e  F0 = %23.15e\n", X0[index][j], F0[index][j]);
    printf("\n");
#endif

  }

  /* interpolate data associated with each table handle */
  printf("\n--- INTERPOLATE DATA ---\n");
  for (i = 0; i < nTables; i++) {
    cat = get_dataTypeCategory(tableType[i]);
    ref1 = get_dataTypeReference1(tableType[i]);
    ref2 = get_dataTypeReference2(tableType[i]);

    printf ("\n%-10s => cat %i, ref1=%-13s and ref2=%-13s    (TH: %i)\n",
	    get_tableType_str(tableType[i]), cat, get_tableType_str(ref1),
	    get_tableType_str(ref2), tableHandle[i]);

    F_ref = NULL; /* initialize cross-referenced table values */

    switch (cat) {
    case 0:
      index = _GET_CAT0_ARRAY_INDEX(tableType[i], tableType, nTables);
      X     = X0[index];
      F_ref = F0[index];
      break;
    case 1:
      t_ref = _GET_CAT1_CAT0XREF (tableType[i], tableType, nTables);
      index = _GET_CAT0_ARRAY_INDEX(t_ref, tableType, nTables);
      X     = F0[index];
      F_ref = X0[index];
      break;
    case 3:
      if ((ref2 == EOS_D_Tm || ref2 == EOS_D_Tf) &&
	  (ref1 == EOS_Pm_D || ref1 == EOS_Pf_D))
	index = nTables + _GET_CAT0_ARRAY_INDEX(ref1, tableType_nonMono, nTables_nonMono);
      else
	index = _GET_CAT0_ARRAY_INDEX(ref1, tableType, nTables);
      t_ref = _GET_CAT1_CAT0XREF(ref2, tableType, nTables);
      index2 = _GET_CAT0_ARRAY_INDEX(t_ref, tableType, nTables);
      X     = F0[index2];
      F_ref = F0[index];
      break;
    default:
      assert(cat == 0 || cat == 1 || cat == 3);
    }

    assert(index >= 0 && index < nTables + nTables_nonMono);

    nx = NX[index];
    ny = NY[index];

    nXYPairs = nx * ny;
    if (nXYPairs <= 0) {
      printf("NX[%i]=%i NY[%i]=%i nXYPairs=%i\n", index, nx, index, ny, nXYPairs);
      assert(nXYPairs>0);
    }

    Y = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    F = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    dFx = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    dFy = (EOS_REAL *) safe_malloc (nXYPairs, sizeof (EOS_REAL));
    xyBounds = (EOS_INTEGER *) safe_malloc (nXYPairs, sizeof (EOS_INTEGER));

    if (! (Y && F && dFx && dFy && xyBounds)) {
      printf ("Memory allocation error!\n");
      return 2;
    }

    /* initialize array(s) */
    for (j = 0; j < nXYPairs; j++) {
      xyBounds[j] = EOS_OK;
      F[j] = 0.0;
      Y[j] = 0.0;
    }

    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy, &errorCode);
    if (errorCode != EOS_OK && errorCode != EOS_INTERP_EXTRAPOLATED) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_Interpolate ERROR %d: %s\n", errorCode, errorMessage);
    }
    else {
      eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, xyBounds, &errorCode);

      for (j = 0; j < nx; j++) {
	printf("\tX = %23.15e  F = %23.15e", X[j], F[j]);
	if (F_ref) printf("  F_ref = %23.15e", F_ref[j]);
	if (xyBounds[j]) printf("  (%s)", ERROR_TO_TEXT(xyBounds[j]));
	if (F_ref && fcmp(F[j], F_ref[j], e1, e2)) printf("  DIFFER %i", fcmp(F[j], F_ref[j],e1, e2));
	printf("\n");
      }
    }

    /* deallocate memory */
    printf("\n--- CLEAN UP ---\n");
    if (Y) EOS_FREE(Y);
    if (F) EOS_FREE(F);
    if (dFx) EOS_FREE(dFx);
    if (dFy) EOS_FREE(dFy);
    if (xyBounds) EOS_FREE(xyBounds);
  }

  /* destroy objects associated with all table handles */
  eos_DestroyAll (&errorCode);

  /* deallocate memory */
  for (i = 0; i < nTables+nTables_nonMonoE; i++) {
    if (X0[i]) EOS_FREE(X0[i]);
    if (Y0[i]) EOS_FREE(Y0[i]);
    if (F0[i]) EOS_FREE(F0[i]);
  }

  return 0;

}
