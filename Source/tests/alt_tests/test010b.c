/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#include "eos_Interface.h"

#ifdef USE_EOSPAC5

#include "es4types.h"
#include "es4proto.h"
#include "eospac4c.h"

#else

typedef EOS_REAL REAL;
typedef EOS_INTEGER INTEGER;
typedef EOS_BOOLEAN BOOLEAN;
static const BOOLEAN TRUE = EOS_TRUE;
static const BOOLEAN FALSE = EOS_FALSE;
static const int ES5_OK = EOS_OK;

static const INTEGER ES4_PRCLD = 0;
static const INTEGER ES4_ENCLD = 0;
static const REAL tiny = 1.0e-99;

#endif

#include <stdio.h>
#include <stdlib.h>

REAL max(REAL a, REAL b) {
  if(a<b) return b;
  return a;
}

REAL min(REAL a, REAL b) {
  if(a>b) return b;
  return a;
}

#define ERROR_TO_TEXT(i) ((i==EOS_xHi_yHi) ? "EOS_xHi_yHi" : \
                          (i==EOS_xHi_yOk) ? "EOS_xHi_yOk" : \
                          (i==EOS_xHi_yLo) ? "EOS_xHi_yLo" : \
                          (i==EOS_xOk_yLo) ? "EOS_xOk_yLo" : \
                          (i==EOS_xLo_yLo) ? "EOS_xLo_yLo" : \
                          (i==EOS_xLo_yOk) ? "EOS_xLo_yOk" : \
                          (i==EOS_xLo_yHi) ? "EOS_xLo_yHi" : \
                          (i==EOS_xOk_yHi) ? "EOS_xOk_yHi" : \
                          (i==EOS_OK     ) ? "           " : \
                          "INVALID    "                      \
                         )

main() {
  //     
  // Demonstrate the usage of ES1TABS, ES1VALS, ES1ERRMSG and ES1INFO
  //     switch-to-other-buffer
  // ES1TABS subroutine parameters
  INTEGER mtyps=3; // number of tables to load (including default cold curves)
  INTEGER mregs=1; //number of problem regions
  BOOLEAN *llog1 = (BOOLEAN*) malloc(sizeof(BOOLEAN)*mtyps*mregs);
  INTEGER *iopt  = (INTEGER*) malloc(sizeof(INTEGER)*mtyps*mregs);
  BOOLEAN lprnt;
  INTEGER iprnt;
  INTEGER *inams = (INTEGER*) malloc(sizeof(INTEGER)*mtyps);
  REAL    *ucons = (REAL*)    malloc(sizeof(REAL)   *mtyps*3);
  INTEGER *imids  = (INTEGER*) malloc(sizeof(INTEGER)*mtyps*mregs);
  INTEGER idtab, mtabs;
  REAL    *ktabs;
  INTEGER *ierrs = (INTEGER*) malloc(sizeof(INTEGER)*mtyps*mregs);

  // ES1VALS subroutine parameters
  INTEGER idrvs, intrp, iregn, ierr1;
  const INTEGER nvals=10; // number of unique X and Y values
  const INTEGER nzons=nvals*nvals; // number of interpolated F values to return (zones)
      
  // ES1INFO subroutine parameters
  INTEGER iname, matid, ifile;
  BOOLEAN llogs;
  REAL    xcnvt, ycnvt, fcnvt, znbar, anbar, dens0;

  // EOSPAC 6 variables
  enum {nTablesE = 1};
  EOS_INTEGER nTables=nTablesE, errorCode=EOS_OK, tableHandle[nTablesE], nXYPairs=nzons;
  EOS_INTEGER tableType[nTablesE], matID[nTablesE];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER *xyBounds = (EOS_INTEGER*) malloc(sizeof(EOS_INTEGER)*(nXYPairs));

  // Local variables
  INTEGER i, j, k, itest, itype;
  EOS_CHAR str[50];

  REAL X[nzons], Y[nzons], F[nzons*3], minval, maxval,
    Xlog10[nzons], Ylog10[nzons], Xexp10[nzons], Yexp10[nzons], *X1, *Y1;
  typedef struct {
    INTEGER matID;
    INTEGER tableType[2];      /* 0->EOSPAC5, 1->EOSPAC6 */
    char*   tableTypeLabel[2];
    BOOLEAN loadColdCurves;
    char*   description;
  } modelTest_s;

  const INTEGER nModelTestsE = 2;

#ifdef USE_EOSPAC5
  modelTest_s modelTests[nModelTestsE] = {
    {3718,  {ES4_PNTOT,  EOS_Pt_DUt}, {"ES4_PNTOT",  "EOS_Pt_DUt"}, TRUE,  "Total Pressure"},
    {13718, {ES4_ZFREE2, EOS_Zfo_DT}, {"ES4_ZFREE2", "EOS_Zfo_DT"}, FALSE, "Mean Ion Charge"},
  };
#else
  EOS_INTEGER DUMMY=0;
  modelTest_s modelTests[nModelTestsE] = {
    {3718,  {DUMMY, EOS_Pt_DUt}, {"DUMMY", "EOS_Pt_DUt"}, TRUE,  "Total Pressure"},
    {13718, {DUMMY, EOS_Zfo_DT}, {"DUMMY", "EOS_Zfo_DT"}, FALSE, "Mean Ion Charge"},
  };
#endif

  minval = 1.0;
  maxval = 2.0;
  for (i=0; i < nvals; i++)
    X[i] = minval + ((REAL)i/(REAL)(nvals-1))*(maxval-minval);
  for (i=0; i < nvals; i++)
    for (j=0; j < nvals; j++) X[i+j*nvals] = X[i];

  minval = 0.0;
  maxval = 7.5;
  for (i=0; i < nvals; i++) {
    Y[i*nvals] = minval + ((REAL)i/(REAL)(nvals-1))*(maxval-minval);
    for (j=0; j < nvals; j++) Y[j+i*nvals] = Y[i*nvals];
  }

  // Assign subroutine parameter values
  mtabs = 20000;             // arbitrary intial words in ftabs
  lprnt = TRUE;              // dump loaded tables to an ascii file 
  iprnt = 0;                 // arbitrary value -- no longer used 
  idtab = 0;                 // arbitrary value -- no longer used
  ktabs = NULL;

  for(itest=0;itest<nModelTestsE;itest++) {
    for(itype=0;itype<2;itype++) {

#ifndef USE_EOSPAC5
      if(itype==0) continue; // skip usage of EOSPAC 5 routines
#endif

      printf("\n*********************************************************************\n");
      printf("*** EOSPAC %i TEST CASE %i: %s for material %i ***\n%s\n",
	     ((itype==0)?5:6),itest+1,
	     modelTests[itest].tableTypeLabel[itype],modelTests[itest].matID,
	     modelTests[itest].description);

      if (itype==0) {
	// EOSPAC 5 ----------------------------------------------------------------------

	// Set various arrays
	for(j=0;j<mtyps;j++) {
	  inams[j        ] = ((j==0 && modelTests[itest].loadColdCurves) ? ES4_PRCLD :
			      (j==1 && modelTests[itest].loadColdCurves) ? ES4_ENCLD :
			      modelTests[itest].tableType[itype]);
	  ucons[j        ] = 1.;   // table x units conversion factor
	  ucons[j+  mtyps] = 1.;   // table y units conversion factor
	  ucons[j+2*mtyps] = 1.;   // table F(x,y) units conversion factor
	  for(i=0;i<mregs;i++) {
	    k = j+i*mtyps;
	    llog1[k] = FALSE;  // don't return log10 tables for 300-tables
	    iopt[k]  = 0;
	    imids[k] = modelTests[itest].matID;
	    ierrs[k] = ES5_OK; // initialize error flag
	  }
	}
    
	// Allocate memory for tables to be loaded
	ktabs = (REAL*)malloc(sizeof(REAL)*mtabs);

	// Load desired data tables
#ifdef USE_EOSPAC5
	printf("--- Load data ---\n");
	es1tabs(&llog1[0], &iopt[0],  lprnt, iprnt, mtyps,
		mregs, &inams[0], &ucons[0], &imids[0],
		idtab, mtabs, &ktabs, ierrs);
	for(j=0;j<mtyps;j++) {
	  for(i=0;i<mregs;i++) {
	    k = j+i*mtyps;
	    if (ierrs[k]) {
	      es1errmsg(ierrs[k], errorMessage);
	      printf("EOSPAC 5 LOAD ERROR %d, TABLE %d: %s\n", ierrs[k], inams[k], errorMessage);
	    }
	  }
	}
#endif
      }
      else if (itype==1) {
	// EOSPAC 6 ----------------------------------------------------------------------

	// Create tables
	for (j=0; j < nTables; j++) {
	  tableType[j] = modelTests[itest].tableType[itype];
	  matID[j] = modelTests[itest].matID;
	}
	eos_CreateTables ( &nTables, tableType, matID, tableHandle, &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage ( &errorCode, errorMessage );
	  printf ( "eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage );
	  for (i=0; i < nTables; i++) {
	    eos_GetErrorCode ( &tableHandle[i], &errorCode );
	    if (errorCode != EOS_OK) {
	      eos_GetErrorMessage ( &errorCode, errorMessage );
	      printf ( "%i: eos_CreateTables ERROR %i: %s\n",
		       tableHandle[i], errorCode, errorMessage );
	    }
	  }
	  return -2;
	}

	// Load data
	eos_LoadTables ( &nTables, tableHandle, &errorCode);
	if (errorCode != EOS_OK) {
	  eos_GetErrorMessage ( &errorCode, errorMessage );
	  printf ( "eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage );
	  for (i=0; i < nTables; i++) {
	    eos_GetErrorCode ( &tableHandle[i], &errorCode );
	    if (errorCode != EOS_OK) {
	      eos_GetErrorMessage ( &errorCode, errorMessage );
	      printf ( "%i: eos_LoadTables ERROR %i: %s\n",
		       tableHandle[i], errorCode, errorMessage );
	    }
	  }
	  return -3;
	}

      }
      else {
	printf("INVALID itype\n");
	return -1;
      }

      // Perform interpolation for each data type
      printf("--- Interpolate using tableType %s for material %i ---\n",
	     modelTests[itest].tableTypeLabel[itype], modelTests[itest].matID);
      if (itype==0) {
#ifdef USE_EOSPAC5
	idrvs = ES4_DERIVS; // return F, dF/dx, and dF/dy
	intrp = ES4_BIRATF; // use bi-rational interpolation
#endif
	iregn = 1;          // only 1 region has been used in this example
      }
      for(i=2;i<mtyps;i++) {
	if (itype==0) {
#ifdef USE_EOSPAC5
	  es1vals(inams[i], idrvs, intrp, ktabs, mtabs,
		  nzons, iregn, X, Y, F, 3, ierr1);
	  if (ierr1) {
	    es1errmsg(ierr1, errorMessage);
	    printf("EOSPAC 5 INTERPOLATION ERROR %d, TABLE %d: %s\n",ierr1, inams[i], errorMessage);
	    return -4;
	  }
#endif
	  // convert partial derivatives from dF/dlnx to dFdx and dF/dlny to dFdy
	  for(j=0;j<nzons;j++) {
	    F[j+1*nzons] /= max(X[j],tiny);
	    F[j+2*nzons] /= max(Y[j],tiny);
	  }
	}
	else if (itype==1) {
	  eos_Interpolate (&tableHandle[i-2], &nXYPairs, X, Y, &(F[0]), &(F[nXYPairs]), &(F[2*nXYPairs]), &errorCode);
	  if (errorCode != EOS_OK) {
	    eos_GetErrorMessage(&errorCode, errorMessage);
	    printf("%d: %s\n", errorCode, errorMessage);
	    if (errorCode != EOS_INTERP_EXTRAPOLATED) {
	      return -5;
	    }
	    else {
	      eos_CheckExtrap (&tableHandle[i-2], &nXYPairs, X, Y, xyBounds, &errorCode);
	      errorCode = EOS_INTERP_EXTRAPOLATED;
	    }
	  }
	}

	printf("          Y = ");
	for(j=0; j < nvals; j++) printf("%14.6e", Y[j*nvals]);
	printf("\n%7s\n","X");
	for(j=0; j < nvals; j++) {
	  printf("%14.6e", X[j]);
	  for(i=0; i < nvals; i++) printf("%14.6e", F[j+i*nvals]);
	  printf("\n");
	}

	if(errorCode == EOS_INTERP_EXTRAPOLATED) {
	  printf("-----\n");
	  for(j=0; j < nvals; j++) {
	    printf("%14s"," ");
	    for(i=0; i < nvals; i++) {
	      sprintf(str,"%i",xyBounds[j+i*nvals]);
	      printf("%14s", ERROR_TO_TEXT(xyBounds[j+i*nvals]));
	    }
	    printf("\n");
	  }
	}

	if(itype==0 && ktabs) free(ktabs);
	else if(itype==1) eos_DestroyAll(&errorCode);
      }
    }
  }
}

