/*********************************************************************
 * Example Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup tests quick
 * \brief Test the eos_TrapezoidIntegrate function with data generate by a
 *        simple polynomial and its integral:
 *             \f$ y=x^2+5x+1 \f$,
 *             \f$ \int_{0}^{x}{y} dx = \frac{x^3}{3}+\frac{5x^2}{2}+x \f$
 *        See SourceForgeÅ© Issue #artf11434 for more details:
 *        https://tf.lanl.gov/sf/go/artf11434
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "eos_Interface.h"

EOS_INTEGER eos_TrapezoidIntegrate (EOS_INTEGER ilower, EOS_INTEGER iupper, EOS_REAL x,
				    EOS_INTEGER nData, EOS_REAL *fData, EOS_REAL *xData,
                                    EOS_INTEGER nInsert, EOS_REAL *result,
				    EOS_BOOLEAN keepTempArays);

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{
  enum
  { N_enum = 100 };

  EOS_INTEGER i;
  EOS_REAL x[N_enum], y[N_enum], integral_y[N_enum], anal_integral_y[N_enum];
  EOS_REAL rdiff, max_rdiff = 0.0;
  EOS_INTEGER errorCode;
  EOS_INTEGER N;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  N = N_enum;

  errorCode = EOS_OK;

  /* define analytically-calculated y and anal_integral_y data */
  for (i = 0; i < N; i++) {
    x[i] = (EOS_REAL)i * 0.01;
    y[i] = x[i] * x[i] + 5.0 * x[i] + 1.0;
    anal_integral_y[i] = x[i] * x[i] * x[i] / 3.0 + 5.0 * x[i] * x[i] / 2.0 + x[i];
  }

  /* calculate integral_y data using the eos_TrapezoidIntegrate function */
  for (i = 0; i < N; i++) {
    errorCode = eos_TrapezoidIntegrate (0, N-1, x[i], N, y, x, 99, &integral_y[i], EOS_FALSE);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("i=%d eos_TrapezoidIntegrate ERROR %d: %s\n", i, errorCode, errorMessage);
    }
  }

  /* Compare (relative difference) anal_integral_y with integral_y data */
  printf ("\n\"--- Compare (relative difference) anal_integral_y with integral_y data ---\"\n");
  printf("%23s%23s%23s%23s%23s\n",
	 "x", "y", "anal_integral_y", "integral_y", "  rel. diff.");
  for (i = 0; i < N; i++) {
    rdiff = (integral_y[i]-anal_integral_y[i])/((anal_integral_y[i] != 0.0) ? anal_integral_y[i] : 1.0);
    max_rdiff = (fabs(max_rdiff) < fabs(rdiff)) ? rdiff : max_rdiff;
    printf("%d. %23.15e%23.15e%23.15e%23.15e #%23.15e\n",
	   i, x[i], y[i], anal_integral_y[i], integral_y[i], rdiff);
  }
  printf("\n# max. rel. diff. = %23.15e\n", max_rdiff);

  return 0;

}
