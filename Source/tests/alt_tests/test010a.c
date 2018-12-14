/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

#include "es4types.h"
#include "es4proto.h"
#include "eospac4c.h"
#include "stdio.h"

REAL max(REAL a, REAL b) {
  if(a<b) return b;
  return a;
}

main() {
  //     
  // Demonstrate the usage of ES1TABS, ES1VALS, ES1ERRMSG and ES1INFO
  //     switch-to-other-buffer
  // ES1TABS subroutine parameters
  INTEGER mtyps=1;  // number of tables to load
  INTEGER mregs=20; //number of problem regions
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
  const INTEGER nzons=20; // number of interpolated F values to return (zones)
      
  // ES1INFO subroutine parameters
  INTEGER iname, matid, ifile;
  BOOLEAN llogs;
  REAL    xcnvt, ycnvt, fcnvt, znbar, anbar, dens0;

  // Local variables
  INTEGER i, j, k, itest;
  char    errorMessage[60];

  REAL X[nzons], Y[nzons], F[nzons*3];
  typedef struct {
    INTEGER matID;
    INTEGER tableType;
    char*   tableTypeLabel;
    INTEGER modelFlag;
    BOOLEAN forced;
    char*   description;
  } modelTest_s;

  const INTEGER nModelTestsE = 3;

  modelTest_s modelTests[nModelTestsE] = {
    {3716,  ES4_PRTOT, "ES4_PRTOT", 0, FALSE, "Total Pressure"},
    {31540, ES4_TMELT, "ES4_TMELT", 0, FALSE, "Melt Temperature"},
    {31540, ES4_TMELT, "ES4_TMELT", 1, FALSE, "Melt Temperature"}
  };

  X[0] = 0.85;
  X[1] = 2.0317881088625;
  X[2] = 3.213576217725;
  X[3] = 4.3953643265875;
  X[4] = 5.57715243545;
  X[5] = 6.75894054431249;
  X[6] = 7.94072865317499;
  X[7] = 9.12251676203749;
  X[8] = 10.3043048709;
  X[9] = 11.4860929797625;
  X[10] = 12.667881088625;
  X[11] = 13.8496691974875;
  X[12] = 15.03145730635;
  X[13] = 16.2132454152125;
  X[14] = 17.395033524075;
  X[15] = 18.5768216329375;
  X[16] = 19.0529;
  X[17] = 20.9403978506625;
  X[18] = 22.122185959525;
  X[19] = 23.3039740683875;

  for (i=0; i < nzons; i++)
    Y[i] = 200.;

  // Assign subroutine parameter values
  mtabs = 20000;             // arbitrary intial words in ftabs
  lprnt = TRUE;              // dump loaded tables to an ascii file 
  iprnt = 0;                 // arbitrary value -- no longer used 
  idtab = 0;                 // arbitrary value -- no longer used
  ktabs = NULL;

  for(itest=0;itest<nModelTestsE;itest++) {
    
    printf("\n*********************************************************************\n");
    printf("*** TEST CASE %i: %s for material %i ***\n%s\n",
	   itest+1,modelTests[itest].tableTypeLabel,modelTests[itest].matID,
	   modelTests[itest].description);
    printf("--- Set splitting options: type=%i, forced=%i ---\n",
	   modelTests[itest].modelFlag, modelTests[itest].forced);

    for(j=0;j<mtyps;j++) {
      inams[j        ] = modelTests[itest].tableType;
      ucons[j        ] = 1.;   // table x units conversion factor
      ucons[j+  mtyps] = 1.;   // table y units conversion factor
      ucons[j+2*mtyps] = 1.;   // table F(x,y) units conversion factor
      for(i=0;i<mregs;i++) {
	k = j+i*mtyps;
	llog1[k] = FALSE;  // don't return log10 tables for 300-tables
	iopt[k]  = modelTests[itest].modelFlag;
	imids[k] = modelTests[itest].matID;
	ierrs[k] = ES5_OK; // initialize error flag
      }
    }
    
    // Allocate memory for tables to be loaded
    ktabs = (REAL*)malloc(sizeof(REAL)*mtabs);

    // Load desired data tables
    printf("--- Load data ---\n");
    es1tabs(&llog1[0], &iopt[0],  lprnt, iprnt, mtyps,
	    mregs, &inams[0], &ucons[0], &imids[0],
	    idtab, mtabs, &ktabs, ierrs);
    for(j=0;j<mtyps;j++) {
      for(i=0;i<mregs;i++) {
	k = j+i*mtyps;
	if (ierrs[k]) {
	  es1errmsg(ierrs[k], errorMessage);
	  printf("LOAD ERROR %d, TABLE %d: %s\n", ierrs[k], inams[k], errorMessage);
	}
      }
    }

    // Perform interpolation for each data type
    printf("--- Interpolate using tableType %s for material %i ---\n",
	   modelTests[itest].tableTypeLabel, modelTests[itest].matID);
    idrvs = ES4_DERIVS; // return F, dF/dx, and dF/dy
    intrp = ES4_BIRATF; // use bi-rational interpolation
    iregn = 1;          // only 1 region has been used in this example
    for(i=0;i<mtyps;i++) {
      es1vals(inams[i], idrvs, intrp, ktabs, mtabs,
	      nzons, iregn, X, Y, F, 3, ierr1);
      if (ierr1) {
	es1errmsg(ierr1, errorMessage);
	printf("INTERPOLATION ERROR %d, TABLE %d: %s\n",ierr1, inams[i], errorMessage);
      }
      else {
	// convert partial derivatives from dF/dlnx to dFdx and dF/dlny to dFdy
	for(j=0;j<nzons;j++) {
	  F[j+1*nzons] /= max(X[j],tiny);
	  F[j+2*nzons] /= max(Y[j],tiny);
	}
	for(j=0; j < nzons;j++) {
	  printf("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n",
		 j, X[j], Y[j], F[j], F[j+nzons], F[j+2*nzons]);
	}
      }
    }

    if(ktabs) free(ktabs);
  }
}
