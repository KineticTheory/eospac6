/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup tests unit
 *  \brief Test code to exercise the Taylor Polynomial class methods and attributes
 *         with manufactured data.
 *
 * Uses the following routines:
 * _eos_Create1DTaylor
 * _eos_Create2DTaylor
 * _eos_GetTaylorCoefficients      -- implemented as a virtual function
 * _eos_Set1DTaylorCoefficients    -- implemented as a virtual function
 * _eos_Set1DTaylorCoefficients    -- implemented as a virtual function
 * _eos_Print1DTaylorCoefficients  -- implemented as a virtual function
 * _eos_Print2DTaylorCoefficients  -- implemented as a virtual function
 * _eos_Evaluate1DTaylorValueArray -- implemented as a virtual function
 * _eos_Evaluate2DTaylorValueArray -- implemented as a virtual function
 * _eos_DestroyTaylor              -- implemented as a virtual function
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include "../src/eos_types_internal.h"
#include "../src/eos_Taylor.h"
#include "../src/eos_Utils.h"

EOS_INTEGER main() {

  int i, j;
  EOS_REAL *B=NULL;
  EOS_REAL **C=NULL;

  EOS_REAL *x=NULL, *y=NULL;
  EOS_REAL a[2] = { -1, 1 };
  EOS_REAL b[2] = { -1, 1 };
  EOS_REAL dx = .75;
  EOS_REAL dy = .75;
  EOS_INTEGER NX=6;
  EOS_INTEGER NY=6;

  EOS_INTEGER w = 23, p = 15; // interpolated numeric output field width and precision

  // ===== UNIVARIATE CALCULATIONS =====
  printf("===================================\n");
  printf("===== UNIVARIATE CALCULATIONS =====\n");
  printf("===================================\n");
  EOS_INTEGER n = 4;
  eos_Taylor *S = _eos_Create1DTaylor(n); // n-order univariate polynomial
  B = (EOS_REAL*) malloc(n * sizeof(EOS_REAL));
  printf("main::B (0x%x)\n{ ", (unsigned int)B);
  for (i=0; i<n; i++) {
    B[i] = (!i)?5:i;
    printf("%g%s", B[i], ((i==n-1)?" }":", "));
  }
  printf("\n\n");

  S->SetIntervals(S,a,b);
  S->divideByFactorial = EOS_TRUE; /* enable internal option */
  S->SetCoefficients(S,&B);

  S->Print(S, 0, stdout);
  printf("\n");

  x =  (EOS_REAL*) malloc(NX * sizeof(EOS_REAL));

  for (i=0; i<NX; i++) {
    x[i] = i*dx;
  }

  printf("--- S->Evaluate ---\n");
  S->Evaluate(S,NX,NY,x,y);
  for (i=0; i<NX; i++)
    printf("g(%.6f) = %.6f\n", x[i], S->f[0][i]);
  printf("\n");

  S->Destroy(S);
  EOS_FREE(x);
  EOS_FREE(B);

  // ===== BIVARIATE CALCULATIONS =====
  printf("==================================\n");
  printf("===== BIVARIATE CALCULATIONS =====\n");
  printf("==================================\n");
  EOS_INTEGER nx = n, ny = 3;
  eos_Taylor *T = _eos_Create2DTaylor(nx, ny); // nx- by ny-order bivariate polynomial
  C = T->GetCoefficients(T);
  printf("main::C (0x%x) %dx%d\n{\n", (unsigned int)C, nx, ny);
  for (i=0; i<nx; i++) {
    printf(" { ");
    for (j=0; j<ny; j++) {
      C[i][j] = ((!i)?5:i)+j+i*j;
      printf("%g%s", C[i][j], ((j==ny-1)?((i<nx)?" },":" }"):", "));
    }
    printf("\n");
  }
  printf("}\n");

  T->SetIntervals(T,a,b);
  T->divideByFactorial = EOS_TRUE; /* enable internal option */
  T->SetCoefficients(T,C);

  T->Print(T, 0, stdout);

  x =  (EOS_REAL*) malloc(NX * sizeof(EOS_REAL));
  y =  (EOS_REAL*) malloc(NY * sizeof(EOS_REAL));

  EOS_INTEGER nux = 1, nuy =1;

  printf("--- T->Evaluate ---\n");
  for (i=0; i<NX; i++) {
    x[i] = i*dx;
    for (j=0; j<NY; j++) {
      y[j] = j*dy;
    }
  }

  T->Evaluate(T,NX,NY,x,y);

  printf("x       y =>");
  for (j=0; j<NY; j++)
    printf("%*.*f", w, p, y[j]);
  printf("\n");
  for (i=0; i<NX; i++) {
    printf("%-*.*f", 12, 6, x[i]);
    for (j=0; j<NY; j++)
      printf("%*.*f", w, p, T->f[i][j]);
    printf("\n");
  }

  for (i=0; i<nx+1; i++) {
    for (j=0; j<ny+1; j++) {

      int ii, jj;

      nux = i; nuy = j;

      printf("--- T->Derivative{%d,%d} ---\n", nux, nuy);
      T->Derivative(T,nux,nuy,NX,NY,x,y);
      printf("x       y =>");
      for (jj=0; jj<NY; jj++)
	printf("%*.*f", w, p, y[jj]);
      printf("\n");
      for (ii=0; ii<NX; ii++) {
	printf("%-*.*f", 12, 6, x[ii]);
	for (jj=0; jj<NY; jj++)
	  printf("%*.*f", w, p, T->df[ii][jj]);
	printf("\n");
      }
    }
  }

  T->Destroy(T);
  EOS_FREE(x);
  EOS_FREE(y);

  return 0;

}
