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
 *  \brief Compare inverse interpolation results both with and without EOS_INVERT_AT_SETUP;
 *         artf47921 test 1 of 32. See SourceForge Issue #artf47921 for more details.
 *
 *  The following environment variables allow for varied testing and analysis:
 *     TEST_ARTF47921_INSERT_DATA- override default list of EOS_INSERT_DATA values (1, 2, 3) with whitespace- and/or comma-delimited list
 *     TEST_ARTF47921_NINSERT    - override nInsert, which is defined in test_artf47921_timer.h
 *     TEST_ARTF47921_EOS_MATID  - override matID, which is defined below as 3720
 *     TEST_ARTF47921_MELT_MATID - define an associated melt material id
 *     TEST_ARTF47921_SMOOTH     - enable EOS_SMOOTH option
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"
#include "test_artf47921_timer.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}
#define MIN(x,y) (((x)<(y))? (x) : (y))
#define MAX(x,y) (((x)>(y))? (x) : (y))

typedef struct
{
    EOS_INTEGER matID;
    EOS_INTEGER melt_matID;
    EOS_INTEGER tableType;
    EOS_INTEGER tableTypeCat0;
    EOS_INTEGER modelFlag;
} modelTest_s;

void* make_test_entry (EOS_INTEGER matID, EOS_INTEGER melt_matID, EOS_INTEGER tableType,
                       EOS_INTEGER tableTypeCat0, const EOS_INTEGER modelFlag)
{
  modelTest_s *p = NULL;
  
  p = (modelTest_s*) safe_malloc(1, sizeof(modelTest_s));
  p->matID          = matID;
  p->melt_matID     = melt_matID;
  p->tableType      = tableType;
  p->tableTypeCat0  = tableTypeCat0;
  p->modelFlag      = modelFlag;

  return(p);
}

void destroy_test_entries (EOS_INTEGER *n, modelTest_s ***t)
{
  int i;
  for (i = 0; i < *n; i++) {
    modelTest_s **tref = *t;
    modelTest_s *p = tref[i];
    EOS_FREE(p);
  }
  EOS_FREE(*t);
  *n = 0;
}

int main ()
{
  enum { nTablesE = 2 };

  EOS_INTEGER FOUR = 4;
  EOS_INTEGER i, j, itest;
  EOS_REAL *X, *Y, *F, *dFx, *dFy;
  EOS_REAL **X_history, **Y_history, **F_history, **dFx_history, **dFy_history;
  EOS_INTEGER *extrapCode;
  EOS_INTEGER **extrapCode_history;
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode, nTables, nXYPairs, nModelTests;
  EOS_INTEGER NR, NT;
  EOS_REAL *X0=NULL, *Y0=NULL;
  modelTest_s **modelTests = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_BOOLEAN hideExtrapolatedResults = EOS_TRUE;

  nModelTests = 2;
  modelTests = (modelTest_s**) safe_malloc(nModelTests, sizeof(modelTest_s*));
  modelTests[0] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, 0                   );
  modelTests[1] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, EOS_INVERT_AT_SETUP );

  { /* redefine modelTests[itest]->matID if required */
    char *myenvar = getenv("TEST_ARTF47921_INSERT_DATA");
    if (myenvar != NULL) {      /* parse the whitespace- and/or comma-delimited list of values */
      EOS_CHAR *token, *state;
      i = 0;
      for (token = strtok_r(myenvar, ", 	", &state); token != NULL; token = strtok_r(NULL, ", 	", &state)) {
        i = (EOS_INTEGER)atol(token);
        nModelTests++;
        modelTests = (modelTest_s**) safe_realloc(modelTests, nModelTests, sizeof(modelTest_s*));
        modelTests[nModelTests-1] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, EOS_INVERT_AT_SETUP+EOS_INSERT_DATA+i );
      }
    }
    else {
      nModelTests += 3;
      modelTests = (modelTest_s**) safe_realloc(modelTests, nModelTests, sizeof(modelTest_s*));
      modelTests[2] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, EOS_INVERT_AT_SETUP+EOS_INSERT_DATA+1 );
      modelTests[3] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, EOS_INVERT_AT_SETUP+EOS_INSERT_DATA+2 );
      modelTests[4] = make_test_entry(3720, 33720, EOS_T_DPt, EOS_Pt_DT, EOS_INVERT_AT_SETUP+EOS_INSERT_DATA+3 );
    }
  }
  { /* redefine nInsert if environment variable is set */
    char *myenvar = getenv("TEST_ARTF47921_NINSERT");
    if (myenvar != NULL)
      nInsert = (EOS_INTEGER)atol(myenvar);
  }
  { /* redefine modelTests[itest]->matID if required */
    char *myenvar = getenv("TEST_ARTF47921_EOS_MATID");
    if (myenvar != NULL) {
      for (itest = 0; itest < nModelTests; itest++)
	modelTests[itest]->matID = (EOS_INTEGER)atol(myenvar);
    }
  }
  { /* redefine modelTests[itest]->melt_matID if required */
    char *myenvar = getenv("TEST_ARTF47921_MELT_MATID");
    if (myenvar != NULL) {
      for (itest = 0; itest < nModelTests; itest++)
	modelTests[itest]->melt_matID = (EOS_INTEGER)atol(myenvar);
    }
  }

  nTables = nTablesE;

  TIMER_RESULTS_SETUP = (TIMER_RESULTS_t*)malloc(nModelTests*sizeof(TIMER_RESULTS_t));
  TIMER_RESULTS_INTRP = (TIMER_RESULTS_t*)malloc(nModelTests*sizeof(TIMER_RESULTS_t));

  X_history   = (EOS_REAL**)malloc(nModelTests * sizeof(EOS_REAL*));
  Y_history   = (EOS_REAL**)malloc(nModelTests * sizeof(EOS_REAL*));
  F_history   = (EOS_REAL**)malloc(nModelTests * sizeof(EOS_REAL*));
  dFx_history = (EOS_REAL**)malloc(nModelTests * sizeof(EOS_REAL*));
  dFy_history = (EOS_REAL**)malloc(nModelTests * sizeof(EOS_REAL*));
  extrapCode_history = (EOS_INTEGER**)malloc(nModelTests * sizeof(EOS_INTEGER*));

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
  }

  for (itest = 0; itest < nModelTests; itest++) {

    EOS_INTEGER types[nTablesE]  = { modelTests[itest]->tableType, modelTests[itest]->tableTypeCat0 };
    EOS_INTEGER matIDs[nTablesE] = { modelTests[itest]->matID,     modelTests[itest]->matID         };

    printf ("#\n# *********************************************************************\n");
    {
      EOS_INTEGER num = (EOS_INTEGER)(modelTests[itest]->modelFlag - EOS_INVERT_AT_SETUP - EOS_INSERT_DATA);
      EOS_CHAR str[100];
      sprintf(str, " and EOS_INSERT_DATA=%d option", num);
      printf ("# *** TEST CASE %i: %s for material %i%s%s ***\n# %s\n", itest + 1,
              get_tableType_str(modelTests[itest]->tableType), modelTests[itest]->matID,
              ((modelTests[itest]->modelFlag) ? " with EOS_INVERT_AT_SETUP option" : ""),
              ((modelTests[itest]->modelFlag>EOS_INVERT_AT_SETUP) ? str : ""),
              get_DataTypeDescription(modelTests[itest]->tableType));
    }

    /* initialize setup timer */
    eos_Time (&true, &TIMER_RESULTS_SETUP[itest].wctime, &TIMER_RESULTS_SETUP[itest].cputime,
	      &TIMER_RESULTS_SETUP[itest].cpucycles, &TIMER_RESULTS_SETUP[itest].err);

    /* initialize table data objects */
    eos_CreateTables (&nTables, types, matIDs, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("# eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      for (i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("# %i: eos_CreateTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
      }
      continue;
    }

    { /* conditionally enable EOS_SMOOTH */
      EOS_INTEGER opt1 = EOS_SMOOTH;
      char *myenvar = getenv("TEST_ARTF47921_SMOOTH");
      if (myenvar != NULL) {
	printf ("# --- Set EOS_SMOOTH option: type=%i ---\n", opt1);
	for (i = 0; i < nTables; i++) {
	  eos_SetOption (&tableHandle[i], &opt1, EOS_NullPtr, &errorCode);
	  if (errorCode != EOS_OK) {
	    eos_GetErrorMessage (&errorCode, errorMessage);
	    printf ("# eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
	  }
	}
      }
    }
    if (modelTests[itest]->modelFlag) {
      EOS_INTEGER opt1 = EOS_INVERT_AT_SETUP;
      /* setup options */
      printf ("# --- Set EOS_INVERT_AT_SETUP option: type=%i ---\n", opt1);
      for (i = 0; i < nTables; i++) {
        eos_SetOption (&tableHandle[i], &opt1, EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("# eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
      if (modelTests[itest]->modelFlag > EOS_INVERT_AT_SETUP) {
	EOS_REAL num = (EOS_REAL)(modelTests[itest]->modelFlag - EOS_INVERT_AT_SETUP - EOS_INSERT_DATA);
	EOS_INTEGER opt2 = EOS_INSERT_DATA;
	/* setup options */
	printf ("# --- Set EOS_INSERT_DATA option: type=%i ---\n", opt2);
	for (i = 0; i < nTables; i++) {
	  eos_SetOption (&tableHandle[i], &opt2, &num, &errorCode);
	  if (errorCode != EOS_OK) {
	    eos_GetErrorMessage (&errorCode, errorMessage);
	    printf ("# eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
	  }
	}
      }
    }
    /* Enable data dump to file */
    for (i = 0; i < nTables; i++) {
      if (itest == 0 && i == 0) {
        eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("# eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
      else {
        eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("# eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
        }
      }
    }

    /* load data into table data objects */
    printf ("# --- Load data ---\n");
    eos_LoadTables (&nTables, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("# eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      for (i = 0; i < nTables; i++) {
        tableHandleErrorCode = EOS_OK;
        eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
        if (tableHandleErrorCode != EOS_OK) {
          eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
          printf ("# %i: eos_LoadTables ERROR %i: %s\n",
                  tableHandle[i], tableHandleErrorCode, errorMessage);
        }
      }

      /* Destroy all data objects */
      eos_DestroyAll (&errorCode);
      if (errorCode != EOS_OK) {
	tableHandleErrorCode = EOS_OK;
	eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
	eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	printf ("# eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
		errorMessage);
      }

      continue;
    }

    /* measure setup time */
    eos_Time (&false, &TIMER_RESULTS_SETUP[itest].wctime, &TIMER_RESULTS_SETUP[itest].cputime,
	      &TIMER_RESULTS_SETUP[itest].cpucycles, &TIMER_RESULTS_SETUP[itest].err);

    /* Fetch NR, NT, X, Y, and allocate/initialize memory */
    {
      EOS_INTEGER infoItem[4] = { EOS_NR, EOS_NT, EOS_Fmin, EOS_Fmax };
      EOS_REAL infoVal[4];
      EOS_REAL Fmin, Fmax;

      if (! X0 || ! Y0) {

	eos_GetTableInfo (&tableHandle[1], &FOUR, infoItem, infoVal, &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage (&errorCode, errorMessage);
	  printf ("# %d: %s\n", errorCode, errorMessage);
	  return errorCode;
	}
	NR   = (EOS_INTEGER)(infoVal[0] + 0.5);
	NT   = (EOS_INTEGER)(infoVal[1] + 0.5);
	Fmin = infoVal[2];
	Fmax = infoVal[3];

	X0 = (EOS_REAL*)malloc(NR * sizeof(EOS_REAL));
	infoItem[0] = EOS_R_Array;
	eos_GetTableInfo (&tableHandle[1], &NR, infoItem, X0, &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage (&errorCode, errorMessage);
	  printf ("# %d: %s\n", errorCode, errorMessage);
	  return errorCode;
	}
	X0[0] = 0.75 * X0[1];

	Y0 = (EOS_REAL*)malloc(NT * sizeof(EOS_REAL));
	infoItem[0] = EOS_T_Array;
	eos_GetTableInfo (&tableHandle[1], &NT, infoItem, Y0, &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage (&errorCode, errorMessage);
	  printf ("# %d: %s\n", errorCode, errorMessage);
	  return errorCode;
	}
	Y0[0] = 0.75 * Y0[1];

	__eos_GetExpandedGrid (nInsert, &NR, &X0);
	__eos_GetExpandedGrid (nInsert, &NT, &Y0);

	printf("# NR = %d ; NT = %d ; Fmin = %e ; Fmax = %e\n", NR, NT, Fmin, Fmax);

	if (get_dataTypeCategory(modelTests[itest]->tableType) % 2 == 0)
	  generate_Log10DistributedPoints(Y0, NULL, NT, Fmin, Fmax, 0.0, 0.0);
	else
	  generate_Log10DistributedPoints(X0, NULL, NR, Fmin, Fmax, 0.0, 0.0);

	nXYPairs = NR * NT;

      }

      TIMER_RESULTS_SETUP[itest].num_X = TIMER_RESULTS_INTRP[itest].num_X = NR;
      TIMER_RESULTS_SETUP[itest].num_Y = TIMER_RESULTS_INTRP[itest].num_Y = NT;
      TIMER_RESULTS_SETUP[itest].N     = TIMER_RESULTS_INTRP[itest].N     = nXYPairs;

      X = (EOS_REAL*)malloc(nXYPairs * sizeof(EOS_REAL));
      Y = (EOS_REAL*)malloc(nXYPairs * sizeof(EOS_REAL));
      F = (EOS_REAL*)malloc(nXYPairs * sizeof(EOS_REAL));
      dFx = (EOS_REAL*)malloc(nXYPairs * sizeof(EOS_REAL));
      dFy = (EOS_REAL*)malloc(nXYPairs * sizeof(EOS_REAL));
      extrapCode = (EOS_INTEGER*)malloc(nXYPairs * sizeof(EOS_INTEGER));

      /* store pointers for later use */
      X_history[itest] = X;
      Y_history[itest] = Y;
      F_history[itest] = F;
      dFx_history[itest] = dFx;
      dFy_history[itest] = dFy;
      extrapCode_history[itest] = extrapCode;

      for (i = 0; i < NR; i++) {
        for (j = 0; j < NT; j++) {
	  X[i+j*NR] = X0[i];
	  Y[i+j*NR] = Y0[j];
	}
      }

    }

    /* interpolate */
    for (i = 0; i < 1; i++) {
      printf ("# --- Interpolate using tableType %s for material %i ---\n",
              get_tableType_str(modelTests[itest]->tableType), modelTests[itest]->matID);

      /* initialize interpolation timer */
      eos_Time (&true, &TIMER_RESULTS_INTRP[itest].wctime, &TIMER_RESULTS_INTRP[itest].cputime,
		&TIMER_RESULTS_INTRP[itest].cpucycles, &TIMER_RESULTS_INTRP[itest].err);

      eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy, &errorCode);

      eos_Time (&false, &TIMER_RESULTS_INTRP[itest].wctime, &TIMER_RESULTS_INTRP[itest].cputime,
		&TIMER_RESULTS_INTRP[itest].cpucycles, &TIMER_RESULTS_INTRP[itest].err);

      if (errorCode != EOS_OK) {
	EOS_BOOLEAN equal;
	eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("# eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
	if (equal) {
          eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, extrapCode, &errorCode);
          if (errorCode != EOS_OK) {
            printf ("# eos_CheckExtrap ERROR %i: %s\n", errorCode, errorMessage);
          }
          else {
            for (j = 0; j < nXYPairs; j++) {
	      if (j > 0 && j % NR == 0) printf ("\n\n");
	      if (j == 0 || j % NR == 0)
		printf ("# \ti\t%12s %12s %12s %12s %12s\n", "X", "Y", "F", "dFx", "dFy");
              if (extrapCode[j] != EOS_OK) {
		if (! hideExtrapolatedResults)
		  printf ("\t%i\t%e %e %e %e %e %s\n",
			  j, X[j], Y[j], F[j], dFx[j], dFy[j], ERROR_TO_TEXT(extrapCode[j]));
              }
              else {
                printf ("\t%i\t%e %e %e %e %e\n", j, X[j], Y[j], F[j], dFx[j], dFy[j]);
              }
            }
          }
        }
      }
      else {
        for (j = 0; j < nXYPairs; j++) {
	  if (j > 0 && j % NR == 0) printf ("\n\n");
	  if (j == 0 || j % NR == 0)
	    printf ("# \ti\t%12s %12s %12s %12s %12s\n", "X", "Y", "F", "dFx", "dFy");
          printf ("\t%i\t %e %e %e %e %e\n", j, X[j], Y[j], F[j], dFx[j], dFy[j]);
        }
      }
    }

  }

  /* Destroy all data objects */
  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    tableHandleErrorCode = EOS_OK;
    eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
    eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
    printf ("# eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode, errorMessage);
  }

  { /* Dump interpolated data comparisons for gnuplot heat maps */
    FILE *fp = fopen("test_artf47921a.heatmap.dat", "w");
    if(fp != NULL) {

      for (itest = 1; itest < nModelTests; itest++) {

	EOS_INTEGER num = (EOS_REAL)(modelTests[itest]->modelFlag - EOS_INVERT_AT_SETUP - EOS_INSERT_DATA);
	EOS_CHAR s1[25], s2[25], s3[25];
	sprintf(s1, "'|%s rel.diff.|'",get_dataTypeDepVar_short_str(modelTests[0]->tableType));
	sprintf(s2, "%s",get_dataTypeDepVar_short_str(modelTests[0]->tableType));
	sprintf(s3, "%s",get_dataTypeDepVar_short_str(modelTests[0]->tableType));
	fprintf(fp, "#\n");
	fprintf(fp, "# Relative difference map for %s, where EOS_INSERT_DATA=%d\n",
		get_tableType_str(modelTests[itest]->tableType), MAX(0,num));
	fprintf(fp, "#\n");
	fprintf(fp, "  %21s %23s %23s %20s[0] %20s[%d]\n",
		get_dataTypeIndepVar1_short_str(modelTests[0]->tableType),
		get_dataTypeIndepVar2_short_str(modelTests[0]->tableType),
		s1, s2, s3, itest);
	for (i = 0; i < NR; i++) {
	  for (j = 0; j < NT; j++) {
	    fprintf(fp, "%23.15e %23.15e %23.15e %23.15e %23.15e %6d = %-12s %6d = %-12s\n",
		    X[i+j*NR],
		    Y[i+j*NR],
		    /*( extrapCode_history[itest][i+j*NR] != EOS_OK
		      ? -1.0e-2
		      : */fabs((F_history[itest][i+j*NR] - F_history[0][i+j*NR]) /
			     (F_history[0][i+j*NR] ? F_history[0][i+j*NR]: 1.0))
		    /*)*/,
		    F_history[0][i+j*NR],
		    F_history[itest][i+j*NR],
		    extrapCode_history[0][i+j*NR],
		    ERROR_TO_TEXT(extrapCode_history[0][i+j*NR]),
		    extrapCode_history[itest][i+j*NR],
		    ERROR_TO_TEXT(extrapCode_history[itest][i+j*NR])
		    );
	  }
	  fprintf(fp, "\n");
	}
	fprintf(fp, "\n");
      }

      fclose(fp);
    }
  }

  /* print timer results */
  printf ( "\n\n#\n# TIMER RESULTS\n" );
  printf ( "#       ---SETUP------------------------ ---INTERPOLATION----------------\n" );
  printf ( "# %3s %10s %10s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n#\n",
	   "ITEST", "WCTIME", "CPUTIME", "CPUCYCLES", "WCTIME", "CPUTIME", "CPUCYCLES", "ERR", "nInsert", "num_X", "num_Y", "N" );
  for (i = 0; i < nModelTests; i++) {
    printf ( "# %3d %10g %10g %10g %10g %10g %10g %10d %10d %10d %10d %10d\n",
	     i+1,
	     TIMER_RESULTS_SETUP[i].wctime,
	     TIMER_RESULTS_SETUP[i].cputime,
	     TIMER_RESULTS_SETUP[i].cpucycles,
	     TIMER_RESULTS_INTRP[i].wctime,
	     TIMER_RESULTS_INTRP[i].cputime,
	     TIMER_RESULTS_INTRP[i].cpucycles,
	     TIMER_RESULTS_INTRP[i].err,
	     nInsert,
	     TIMER_RESULTS_INTRP[i].num_X,
	     TIMER_RESULTS_INTRP[i].num_Y,
	     TIMER_RESULTS_INTRP[i].N);
  }
  printf ( "\n\n" );

  /* Clean all memory */
  for (itest = 0; itest < nModelTests; itest++) {
    EOS_FREE(X_history[itest]);
    EOS_FREE(Y_history[itest]);
    EOS_FREE(F_history[itest]);
    EOS_FREE(dFx_history[itest]);
    EOS_FREE(dFy_history[itest]);
    EOS_FREE(extrapCode_history[itest]);
  }
  EOS_FREE(X0);
  EOS_FREE(Y0);
  EOS_FREE(X_history);
  EOS_FREE(Y_history);
  EOS_FREE(F_history);
  EOS_FREE(dFx_history);
  EOS_FREE(dFy_history);
  EOS_FREE(extrapCode_history);
  EOS_FREE(TIMER_RESULTS_INTRP);
  EOS_FREE(TIMER_RESULTS_SETUP);

  eos_DestroyTableListReverseMap();
  destroy_test_entries (&nModelTests, &modelTests);

  return 0;

}
