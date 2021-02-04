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
 *  \brief Demonstrate category 0 and 3 interpolation for EOS and
 *  opacity data.
 *
 * Assuming no conversion factors are specified, verify that eos_Interpolate is expecting eV at the
 * public interface, and then EOSPAC should convert the input values to log10(eV) (see issue #artf1986,
 * https://tf.lanl.gov/sf/go/artf1986).
 * This test code has hooks in place to use EOSPAC 5, if that library is available.
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 3718 13718
 */

#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

#ifdef USE_EOSPAC5

#include "es4types.h"
#include "es4proto.h"
#include "eospac4c.h"

#else

typedef EOS_REAL REAL;
typedef EOS_INTEGER INTEGER;
typedef EOS_BOOLEAN BOOLEAN;
#ifdef TRUE
#undef TRUE
#  endif
#ifdef FALSE
#  undef FALSE
#endif
static const BOOLEAN TRUE = EOS_TRUE;
static const BOOLEAN FALSE = EOS_FALSE;
static const EOS_INTEGER ES5_OK = 0;    // EOS_OK

static const INTEGER ES4_PRCLD = 0;
static const INTEGER ES4_ENCLD = 0;
static const REAL tiny = 1.0e-99;

#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef max
#define max(a,b) ((a<b) ? b : a)
#endif
#ifndef min
#define min(a,b) ((a>b) ? b : a)
#endif

int main ()
{
  //     
  // Demonstrate the usage of ES1TABS, ES1VALS, ES1ERRMSG and ES1INFO
  //     switch-to-other-buffer
  // ES1TABS subroutine parameters
  INTEGER mtyps = 3;            // number of tables to load (including default cold curves)
  INTEGER mregs = 1;            //number of problem regions
  BOOLEAN *llog1 = (BOOLEAN *) malloc (sizeof (BOOLEAN) * mtyps * mregs);
  INTEGER *iopt = (INTEGER *) malloc (sizeof (INTEGER) * mtyps * mregs);
  INTEGER *inams = (INTEGER *) malloc (sizeof (INTEGER) * mtyps);
  REAL *ucons = (REAL *) malloc (sizeof (REAL) * mtyps * 3);
  INTEGER *imids = (INTEGER *) malloc (sizeof (INTEGER) * mtyps * mregs);
#ifdef USE_EOSPAC5
  BOOLEAN lprnt;
  INTEGER iprnt;
  INTEGER idtab;
#endif
  INTEGER mtabs;
  REAL *ktabs;
  INTEGER *ierrs = (INTEGER *) malloc (sizeof (INTEGER) * mtyps * mregs);

  // ES1VALS subroutine parameters
  enum
  { nvalsE = 10 };
  enum
  { nzonsE = nvalsE * nvalsE };
#ifdef USE_EOSPAC5
  INTEGER iregn;
  INTEGER idrvs, intrp, ierr1;
#endif
  const INTEGER nvals = nvalsE; // number of unique X and Y values
  const INTEGER nzons = nzonsE; // number of interpolated F values to return (zones)

  // ES1INFO subroutine parameters
  REAL ycnvt_K;

  // EOSPAC 6 variables
  enum
  { nTablesE = 1 };
  EOS_INTEGER nTables = nTablesE, errorCode = EOS_OK, th_errorCode = EOS_OK,
    tableHandle[nTablesE], nXYPairs = nzons;
  EOS_INTEGER tableType[nTablesE], matID[nTablesE];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER *xyBounds =
    (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * (nXYPairs));

  // Local variables
  INTEGER i, j, k, itest, itype;
  EOS_CHAR str[50];

  REAL X[nzonsE], Y[nzonsE], Y_K[nzonsE], F[nzonsE * 3], minval, maxval, *Y1;

#ifdef USE_EOSPAC5
  enum
  { nLibTestsE = 2 };           // total number of EOSPAC versions to test
#else
  enum
  { nLibTestsE = 1 };
#endif
  typedef struct
  {
    INTEGER matID;
    INTEGER tableType[nLibTestsE];      /* 0->EOSPAC5, 1->EOSPAC6 */
    char *tableTypeLabel[nLibTestsE];
    BOOLEAN loadColdCurves;
    char *description;
  } modelTest_s;

  BOOLEAN equal;

#ifdef USE_EOSPAC5
  enum
  { nModelTestsE = 2 };         // total number of tests to perform
  modelTest_s modelTests[nModelTestsE] = {
    {3718, {ES4_PNTOT, EOS_Pt_DUt}, {"ES4_PNTOT", "EOS_Pt_DUt"}, TRUE,
     "Total Pressure"},
    {13718, {ES4_ZFREE2, EOS_Zfo_DT}, {"ES4_ZFREE2", "EOS_Zfo_DT"}, FALSE,
     "Mean Ion Charge"},
  };
#else
  enum
  { nModelTestsE = 2 };         // total number of tests to perform
  modelTest_s modelTests[nModelTestsE] = {
    {3718, {EOS_Pt_DUt}, {"EOS_Pt_DUt"}, TRUE, "Total Pressure"},
    {13718, {EOS_Zfo_DT}, {"EOS_Zfo_DT"}, FALSE, "Mean Ion Charge"},
  };
#endif

  ycnvt_K = 1.1604505e4;

  minval = 0.1; //1.0;
  maxval = 2.0;
  for (i = 0; i < nvals; i++)
    X[i] = minval + ((REAL) i / (REAL) (nvals - 1)) * (maxval - minval);
  for (i = 0; i < nvals; i++)
    for (j = 0; j < nvals; j++)
      X[i + j * nvals] = X[i];

  minval = 9.691001300000000E-02;
  maxval = 5.0;
  for (i = 0; i < nvals; i++) {
    Y[i * nvals] =
      minval + ((REAL) i / (REAL) (nvals - 1)) * (maxval - minval);
    Y_K[i * nvals] = pow (10.0, Y[i * nvals]) * ycnvt_K;
    for (j = 0; j < nvals; j++) {
      Y[j + i * nvals] = Y[i * nvals];
      Y_K[j + i * nvals] = Y_K[i * nvals];
    }
  }

  // Assign subroutine parameter values
  mtabs = 20000;                // arbitrary intial words in ftabs
#ifdef USE_EOSPAC5
  lprnt = TRUE;                 // dump loaded tables to an ascii file 
  iprnt = 0;                    // arbitrary value -- no longer used 
  idtab = 0;                    // arbitrary value -- no longer used
#endif
  ktabs = NULL;

  for (itest = 0; itest < nModelTestsE; itest++) {
    for (itype = 0; itype < nLibTestsE; itype++) {

      printf
        ("\n*********************************************************************\n");
      printf ("*** EOSPAC %i TEST CASE %i: %s for material %i ***\n%s\n",
              ((itype == 0 &&
                nLibTestsE != 1) ? 5 : 6), itest + 1,
              modelTests[itest].tableTypeLabel[itype],
              modelTests[itest].matID, modelTests[itest].description);

      if (itype == 0 && nLibTestsE != 1) {
        // EOSPAC 5 ----------------------------------------------------------------------

        // Set various arrays
        for (j = 0; j < mtyps; j++) {
          inams[j] =
            ((j == 0 &&
              modelTests[itest].loadColdCurves) ? ES4_PRCLD : (j == 1 &&
                                                               modelTests
                                                               [itest].
                                                               loadColdCurves)
             ? ES4_ENCLD : modelTests[itest].tableType[itype]);
          ucons[j] = 1.;        // table x units conversion factor
          ucons[j + mtyps] = 1.;        // table y units conversion factor
          ucons[j + 2 * mtyps] = 1.;    // table F(x,y) units conversion factor
          for (i = 0; i < mregs; i++) {
            k = j + i * mtyps;
            llog1[k] = FALSE;   // don't return log10 tables for 300-tables
            iopt[k] = 0;
            imids[k] = modelTests[itest].matID;
            ierrs[k] = ES5_OK;  // initialize error flag
          }
        }

        // Allocate memory for tables to be loaded
        ktabs = (REAL *) malloc (sizeof (REAL) * mtabs);

        // Load desired data tables
#ifdef USE_EOSPAC5
        printf ("--- Load data ---\n");
        es1tabs (&llog1[0], &iopt[0], lprnt, iprnt, mtyps,
                 mregs, &inams[0], &ucons[0], &imids[0],
                 idtab, mtabs, &ktabs, ierrs);
        for (j = 0; j < mtyps; j++) {
          for (i = 0; i < mregs; i++) {
            k = j + i * mtyps;
            if (ierrs[k]) {
              es1errmsg (ierrs[k], errorMessage);
              printf ("EOSPAC 5 LOAD ERROR %d, TABLE %d: %s\n", ierrs[k],
                      inams[k], errorMessage);
            }
          }
        }
#endif
      }
      else if (itype == 1 || nLibTestsE == 1) {
        // EOSPAC 6 ----------------------------------------------------------------------

        // Create tables
        for (j = 0; j < nTables; j++) {
          tableType[j] = modelTests[itest].tableType[itype];
          matID[j] = modelTests[itest].matID;
        }
        eos_CreateTables (&nTables, tableType, matID, tableHandle,
                          &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
          for (i = 0; i < nTables; i++) {
	    eos_GetErrorCode (&tableHandle[i], &th_errorCode);
	    if (th_errorCode != EOS_OK) {
	      eos_GetErrorMessage (&th_errorCode, errorMessage);
              printf ("%i: eos_CreateTables ERROR %i: %s\n",
		      tableHandle[i], th_errorCode, errorMessage);
            }
          }
	  eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_WARNING, &th_errorCode, &equal);
	  if (! equal) {
	    errorCode = -2;
	    goto CLEANUP;
	  }
        }

        /* enable data dump */

        for (i = 0; i < nTables; i++) {
          eos_SetOption (&tableHandle[i],
                         ((itest == 0) ? &EOS_DUMP_DATA : &EOS_APPEND_DATA),
                         EOS_NullPtr, &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          }
        }

        // Load data
        eos_LoadTables (&nTables, tableHandle, &errorCode);
        if (errorCode != EOS_OK) {
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
          for (i = 0; i < nTables; i++) {
            eos_GetErrorCode (&tableHandle[i], &th_errorCode);
            if (th_errorCode != EOS_OK) {
              eos_GetErrorMessage (&th_errorCode, errorMessage);
              printf ("%i: eos_LoadTables ERROR %i: %s\n",
                      tableHandle[i], th_errorCode, errorMessage);
            }
          }
	  eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_WARNING, &th_errorCode, &equal);
          if (th_errorCode != EOS_WARNING) {
	    errorCode = -3;
	    goto CLEANUP;
	  }
        }

      }
      else {
        printf ("INVALID itype\n");
	errorCode = -1;
	goto CLEANUP;
      }

      // Perform interpolation for each data type
      printf ("--- Interpolate using tableType %s for material %i ---\n",
              modelTests[itest].tableTypeLabel[itype],
              modelTests[itest].matID);
      if (itype == 0 && nLibTestsE != 1) {
#ifdef USE_EOSPAC5
        idrvs = ES4_DERIVS;     // return F, dF/dx, and dF/dy
        intrp = ES4_BIRATF;     // use bi-rational interpolation
        iregn = 1;              // only 1 region has been used in this example
#endif
      }
      for (i = 2; i < mtyps; i++) {
        Y1 = Y;
        if (itype == 0 && nLibTestsE != 1) {
#ifdef USE_EOSPAC5
          es1vals (inams[i], idrvs, intrp, ktabs, mtabs,
                   nzons, iregn, X, Y, F, 3, ierr1);
          if (ierr1) {
            es1errmsg (ierr1, errorMessage);
            printf ("EOSPAC 5 INTERPOLATION ERROR %d, TABLE %d: %s\n", ierr1,
                    inams[i], errorMessage);
	    errorCode = -4;
	    goto CLEANUP;
          }
#endif
          // convert partial derivatives from dF/dlnx to dFdx and dF/dlny to dFdy
          for (j = 0; j < nzons; j++) {
            F[j + 1 * nzons] /= max (X[j], tiny);
            F[j + 2 * nzons] /= max (Y[j], tiny);
          }
        }
        else if (itype == 1 || nLibTestsE == 1) {
          if (modelTests[itest].matID >= 10000 &&
              modelTests[itest].matID < 30000) {

	      eos_SetOption (&tableHandle[i - 2], &EOS_Y_CONVERT, &ycnvt_K,
			     &errorCode);
	      Y1 = Y_K;
          }
          eos_Interpolate (&tableHandle[i - 2], &nXYPairs, X, Y1, &(F[0]),
                           &(F[nXYPairs]), &(F[2 * nXYPairs]), &errorCode);
          if (errorCode != EOS_OK) {
	    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("%d: %s\n", errorCode, errorMessage);
            if (! equal) {
	      errorCode = -5;
	      goto CLEANUP;
	    }
            else {
	      EOS_INTEGER err;
              eos_CheckExtrap (&tableHandle[i - 2], &nXYPairs, X, Y1,
                               xyBounds, &err);
            }
          }
        }

        printf ("          Y = ");
        for (j = 0; j < nvals; j++)
          printf ("%14.6e", Y1[j * nvals]);
        printf ("\n%7s\n", "X");
        for (j = 0; j < nvals; j++) {
          printf ("%14.6e", X[j]);
          for (i = 0; i < nvals; i++)
            printf ("%14.6e", F[j + i * nvals]);
          printf ("\n");
        }

	eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
        if (equal) {
          printf ("-----\n");
          for (j = 0; j < nvals; j++) {
            printf ("%14s", " ");
            for (i = 0; i < nvals; i++) {
              sprintf (str, "%i", xyBounds[j + i * nvals]);
              printf ("%14s", ERROR_TO_TEXT (xyBounds[j + i * nvals]));
            }
            printf ("\n");
          }
        }

        if (itype == 0 && nLibTestsE != 1 && ktabs)
          free (ktabs);
        else if (itype == 1 || nLibTestsE != 1)
          eos_DestroyAll (&errorCode);
      }
    }
  }

  free(ucons);
  free(llog1);
  free(iopt);
  free(inams);
  free(imids);
  free(ierrs);
  free(xyBounds);

  errorCode = 0;

 CLEANUP:
  {
    EOS_INTEGER err;
    eos_DestroyAll (&err);
  }

  return errorCode;
}
