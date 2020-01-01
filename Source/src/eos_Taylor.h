/*********************************************************************
 * Class Name : eos_Taylor
 *              store and evaluate a Taylor Polynomial defined by specified coefficients
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 *********************************************************************/
#if !defined(EOS_TAYLOR_H)
#define EOS_TAYLOR_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "eos_types.h"
#include "eos_ErrorHandler.h"
#include "eos_universal_types.h"
#include "eos_Data.h"
#include "eos_SesUtils.h"
#include "ses_defines.h"
#include "assert.h"

typedef EOS_REAL*  EOS_REAL_ptr;
typedef EOS_REAL** EOS_REAL_ptr_ptr;

/*! Enumerated object types
 *  \param T_UNIVARIATE object defined as a collection of 1D Taylor polynomial(s)
 *  \param T_BIVARIATE  object defined as a collection of 2D Taylor polynomial(s)
 */
typedef enum
{
  T_UNDEFINED=-1,
  T_CONSTANT,
  T_UNIVARIATE,
  T_BIVARIATE,
} TaylorObj_type;


/*! Taylor Class struct definition
 */
typedef struct
{

    EOS_BOOLEAN debug;

    TaylorObj_type _type;

    EOS_REAL **f;  /*! array to hold Taylor polynomial values calculated given array(s) of x or (x,y) */
    EOS_REAL **df; /*! array to hold Taylor derivative values calculated given array(s) of x or (x,y) */

    EOS_INTEGER f_NX; /*! f and df array extents corresponding to extent of x array passed to Evaluate() */
    EOS_INTEGER f_NY; /*! f and df array extents corresponding to extent of y array passed to Evaluate() */

    EOS_REAL *P;   /*! array containing intermediate Taylor coefficients for bivariate calculations */
    EOS_REAL **c;  /*! Taylor coefficients; bivariate array dimension: [nx][ny]
		    *                       univariate array dimension: [1][nx] */
    EOS_REAL tx_interval[2], ty_interval[2];
    EOS_REAL a, b;
    EOS_INTEGER Nx;       /*! order of Taylor Polynomial in x */
    EOS_INTEGER Ny;       /*! order of Taylor Polynomial in y */
    EOS_REAL *factorial;  /*! store factorial values up to MAX(Nx!, Ny!) */
    EOS_BOOLEAN divideByFactorial; /*! an internal option to enable division of the input coefficients
				       by appropriate factorial value (default: EOS_FALSE) */

    /* virtual functions */
    void       (*Destroy) (void *ptr);
    void       (*SetIntervals) (void *ptr, EOS_REAL *tx, EOS_REAL *ty);
    void       (*SetCoefficients) (void *ptr, EOS_REAL **C);
    EOS_REAL** (*GetCoefficients) (void *ptr);
    void       (*Print) (void *ptr, EOS_INTEGER dataType, FILE *fh);
    void       (*Evaluate) (void *ptr, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y);
    void       (*Derivative) (void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_INTEGER M, EOS_INTEGER N, EOS_REAL x[], EOS_REAL y[]);
    EOS_REAL   (*EvaluateScalar) (void *ptr, EOS_REAL x, EOS_REAL y);
    EOS_REAL   (*DerivativeScalar) (void *ptr, EOS_INTEGER nux, EOS_INTEGER nuy, EOS_REAL x, EOS_REAL y);

} eos_Taylor;

/* common information populated by _eos_setup_file_to_read_taylor */
typedef struct
{
    ses_file_handle sesFile;
    EOS_INTEGER NTYPS;
    EOS_INTEGER TBLREF_L;
    EOS_INTEGER *NTBLS;
    EOS_INTEGER *TBLREF;
} EOS_COMMON_TAYLOR_INFO;

#include "eos_Taylor.proto.h"

#endif /* !defined(EOS_TAYLOR_H) */
