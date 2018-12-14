/*********************************************************************
* Example Program
* ---------------------------------------------------------
* Filetype: (SOURCE)
*
* Copyright -- see file named COPYRIGHTNOTICE
*
********************************************************************/
/*! \file
 *  \ingroup examples
 *  \brief This is a simple utility to display the platform-specific precision
 *         information used within EOSPAC6.
 *
 * \pre libeospac6.a (version 6.2 or later) is required
 *
 * \author David A. Pimentel
 *
 * \par STDOUT
 *       Platform-specific precision information returned.
 *
 * \par STDERR
 *       All error messages are returned.
 *
 * \par USAGE
 *       get_machprec
 *
 * \arg -h
 *              Display the basic usage of this tool.
*/
#include "common.h"
#include "get_machprec_help.h"

//#define USE_EOSPAC6_INTERNAL_GETMACHINEPRECISION
#ifdef USE_EOSPAC6_INTERNAL_GETMACHINEPRECISION
#include "../src/eos_Interpolation.h"
#endif

#define MIN(x,y) (((x)<(y))? (x) : (y))
#define MAX(x,y) (((x)>(y))? (x) : (y))
#define ABS(x)  (((x)>0)? (x) : -1*(x))

#ifdef USE_EOSPAC6_INTERNAL_GETMACHINEPRECISION

#define getMachinePrecision eos_GetMachinePrecision
#define _machinePrecisionData _eos_machinePrecisionData
void getMachinePrecision (EOS_REAL *eps, EOS_REAL *epsneg);
extern NRmachPrecData _eos_machinePrecisionData;

#else

typedef struct
{                               /* global values used by Newton-Bisection or returned by eos_GetMachinePrecision */
    int gotMachinePrecision;      /* Flag indicating if eos_GetMachinePrecision has been called */
    double eps;                 /* The smallest positive floating-point number such
                                   that (1.0 + eps) > 1, is referred to as the
                                   "floating-point precision." */
    double epsneg;              /* The smallest positive floating-point number such
                                   that (1.0 - eps) < 1, is another way of defining
                                   the "floating-point precision." */
    double maxErr;              /* Newton convergence tolerence */
    int maxIter;          /* Newton maximum iterations */
} NRmachPrecData;

void getMachinePrecision (double *eps, double *epsneg)
{
  double one = (double) (1);
  double two = (double) (2);

  // determine *eps
  *eps = one;
  do {
    *eps /= two;
  } while (one + (*eps / two) > one);

  // determine *epsneg
  *epsneg = one;
  do {
    *epsneg /= two;
  } while (one - (*epsneg / two) < one);
}

NRmachPrecData _machinePrecisionData = {
  0,                            // gotMachinePrecision
  0.0,                          // eps
  0.0,                          // epsneg
  1.0e-04,                      // maxErr (default from original es4invt logic)
  20                            // maxIter (default from original es4invt logic)
};

#endif

int main (int argc, char *argv[])
{
  char    *optionStr = "h";

  /* parse command line options */
  /* EOS_INTEGER errorCode = */ getoptions(argc-1, argv+1, optionStr);

  /* display help */
  if (option['h'].flag) {
    printf("%s\n\n", get_header_str("get_machprec", "$Revision: 1.2 $"));
    printf("%s", help_str);
    return OK;
  }

  /* Set machine precision data */
  if (!_machinePrecisionData.gotMachinePrecision) {
    /* determine the current machine's floating point precision */
    _machinePrecisionData.gotMachinePrecision = 1;
    getMachinePrecision (&_machinePrecisionData.eps, &_machinePrecisionData.epsneg);
    _machinePrecisionData.maxIter = 100;
    _machinePrecisionData.maxErr =
      pow (MAX(_machinePrecisionData.eps, _machinePrecisionData.epsneg),
	   0.75);
  }

  /* determine the current machine's floating point precision */
  getMachinePrecision (&_machinePrecisionData.eps,
		       &_machinePrecisionData.epsneg);
  _machinePrecisionData.maxIter = 100;
  _machinePrecisionData.maxErr =
    pow (MAX
	 (_machinePrecisionData.eps, _machinePrecisionData.epsneg),
	 0.75);

  printf ("gotMachinePrecision = %d\n", _machinePrecisionData.gotMachinePrecision);
  printf ("eps                 = %22.15e\n", _machinePrecisionData.eps);
  printf ("epsneg              = %22.15e\n", _machinePrecisionData.epsneg);
  printf ("maxErr              = %22.15e\n", _machinePrecisionData.maxErr);
  printf ("maxIter             = %d\n", _machinePrecisionData.maxIter);

  return 0;
}
/*********************************************************************/

