/*********************************************************************
 * Test program
 * ----------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! @file
 *	@ingroup C quick unit tests
 *	@brief Perform the following tests:
 *	   -# Verify that the unexpected floating point exception errors are not returned for a
 *		  specific usage of eos_BiRationalInterpolate on KNL.
 *	   -# This test must be compiled with -fp-model=strict
 *
 *   The floating point exception encountered on the KNL using a compiled of the library with
 *   auto-vectorization enabled and without using the Intel -fp-speculation=safe option looks
 *   like the following:
 *
 *   Program received signal SIGFPE, Arithmetic exception.  0x00000000004092f4 in eos_BiRationalInterpolate (numZones=1,
 *   numXVals=7630448, numYVals=0, XValues=0x702010, YValues=0x7023a0, FValues=0x702630, ixv=0x714950, iyv=0x7199e0,
 *   searchXVals=0x71ea70, searchYVals=0x728b70, searchFVals=0x732c70, searchDFx=0x73cd70, searchDFy=0x746e70,
 *   xyBounds=0x750f70, err=0x7fffffff5094) at eos_Interpolation.c:3707
 *   3707 rx3 = -dxg1 / dxg2;
 *
 *   This issue appears to only arise when using the Intel compiler on the KNL.
 *
 *	 See eos/eospac-dev/eospac6#587 for more details:
 *	 https://git.lanl.gov/eos/eospac-dev/eospac6/issues/587
 *
 *	\note
 *	MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"

void eos_BiRationalInterpolate (EOS_INTEGER numZones, EOS_INTEGER numXVals,
                                EOS_INTEGER numYVals, EOS_REAL *XValues,
                                EOS_REAL *YValues, EOS_REAL **FValues,
                                EOS_INTEGER *ixv, EOS_INTEGER *iyv,
                                EOS_REAL *searchXVals, EOS_REAL *searchYVals,
                                EOS_REAL *searchFVals, EOS_REAL *searchDFx,
                                EOS_REAL *searchDFy, EOS_INTEGER *xyBounds,
                                EOS_INTEGER *err);

#ifdef USE_AVX512PF /* This is only applicable for the KNL chipset while using the Intel compiler */
int mxcsr = 0x1d00; // default, with divide-by-zero and invalid unmasked
const char label[50] = " for AVX512PF";
#else
const char label[50] = "";
#endif

int main (int argc, char *argv[]) {

  EOS_INTEGER numZones; EOS_INTEGER numXVals;
  EOS_INTEGER numYVals; EOS_REAL *XValues;
  EOS_REAL *ptr, *YValues; EOS_REAL **FValues;
  EOS_INTEGER *ixv; EOS_INTEGER *iyv;
  EOS_REAL *searchXVals; EOS_REAL *searchYVals;
  EOS_REAL *searchFVals; EOS_REAL *searchDFx;
  EOS_REAL *searchDFy; EOS_INTEGER *xyBounds;
  EOS_INTEGER err;
  int i, j;

  numZones = 5151; numXVals = 112; numYVals = 81;
#if 0
  XValues = (EOS_REAL *) malloc (8*numXVals);
  YValues = (EOS_REAL *) malloc (8*numYVals);
  FValues = (EOS_REAL **) malloc (8*numYVals);
  for (i = 0; i < numYVals; i++) {
    FValues[i] = (EOS_REAL *) malloc (8*numXVals);
  }
#else
  /* memory is allocated continuously in one chunk for R, T and all tables */
  ptr = (EOS_REAL *) malloc (sizeof (EOS_REAL) * (numYVals + numXVals + numYVals * numXVals));
  XValues = ptr;
  YValues = ptr + numXVals;
  FValues = (EOS_REAL **) malloc (sizeof (EOS_REAL *) * numYVals);
  for (i = 0; i < numYVals; i++) {
    FValues[i] = ptr + numXVals + numYVals + i * numXVals;
  }
#endif

  ixv         = (EOS_INTEGER *) malloc (4*numZones);
  iyv         = (EOS_INTEGER *) malloc (4*numZones);
  searchXVals = (EOS_REAL *) malloc (8*numZones);
  searchYVals = (EOS_REAL *) malloc (8*numZones);
  searchFVals = (EOS_REAL *) malloc (8*numZones);
  searchDFx   = (EOS_REAL *) malloc (8*numZones);
  searchDFy   = (EOS_REAL *) malloc (8*numZones);
  xyBounds    = (EOS_INTEGER *) malloc (4*numZones);

  mt_init();
  
  for (i = 0; i < numXVals; i++) {
    XValues[i] = mt_random();
  }
  for (i = 0; i < numYVals; i++) {
    YValues[i] = mt_random();
    for (j = 0; j < numXVals; j++) {
      FValues[i][j] = mt_random();
    }
  }
  for (i = 0; i < numZones; i++) {
    ixv[i] = 0;
    iyv[i] = 0;
    searchXVals[i] = mt_random();
    searchYVals[i] = mt_random();
    xyBounds[i] = 0;
  }

#ifdef USE_AVX512PF /* This is only applicable for the KNL chipset while using the Intel compiler */
  asm ("ldmxcsr mxcsr");
#endif

  eos_BiRationalInterpolate (numZones, numXVals,
                             numYVals, XValues,
                             YValues, FValues,
                             ixv, iyv,
                             searchXVals, searchYVals,
                             searchFVals, searchDFx,
                             searchDFy, xyBounds,
                             &err);

  printf("This test was successfully compiled and executed%s: 1\n", label);

  free (ptr);
  free (FValues);
  free (ixv);
  free (iyv);
  free (searchXVals);
  free (searchYVals);
  free (searchFVals);
  free (searchDFx);
  free (searchDFy);
  free (xyBounds);
  return 0;
}
