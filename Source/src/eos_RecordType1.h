/*********************************************************************
 * Class Name : eos_RecordType1
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#ifndef  EOS_RECORDTYPE1_H
#define  EOS_RECORDTYPE1_H

#include "eos_Data.h"
#include "eos_SesUtils.h"
#include "eos_Taylor.h"

/* This class is designed to manipulate Sesame data stored in format #1,
 * which is specific to Sesame tables 301-306, 411, 412, 431, 
 * 432, 502-505, and 601-605.
 */

/* This container is designed to contain the following number of 2-D tables: */
#define MAX_TABLES_RECORDTYPE1 4

typedef struct
{
  eos_Data eosData;             // must be the FIRST, DO NOT MOVE!
  // Number of densities
  EOS_INTEGER NR;
  // Number of temperatures
  EOS_INTEGER NT;
  // bulk data from 201 table
  EOS_REAL avgAtomicNumber;     // mean atomic number
  EOS_REAL avgAtomicWgt;        // mean atomic mass
  EOS_REAL refDensity;          // normal solid density
  EOS_REAL solidBulkModulus;    // solid bulk modulus
  EOS_REAL exchangeCoefficient; // exchange coefficient
  // Density array
  EOS_REAL *R;                  /*[NR] */
  // Temperature array
  EOS_REAL *T;                  /*[NT] */
  EOS_REAL **table1;            /*[NT][NR] */
  EOS_REAL **table2;            /*[NT][NR] */
  EOS_REAL **table3;            /*[NT][NR] */
  EOS_REAL **table4;            /*[NT][NR] */
  EOS_REAL *coldCurve1;         /* [NR} */
  EOS_REAL *coldCurve2;         /* [NR} */
  EOS_REAL *coldCurve3;         /* [NR} */
  EOS_REAL *coldCurve4;         /* [NR} */

  EOS_REAL *temporary; /* a temporary array for internal data exchange */

  eos_Taylor ***Taylor_objects; /* Array (dimension [M*N][4]) of Taylor polynomial object pointers; alternative to table[1-4] */
  EOS_INTEGER M;          /* number of x-dimension Taylor intervals */
  EOS_INTEGER N;          /* number of y-dimension Taylor intervals */
  EOS_REAL *TX;           /* x-dimension Taylor interval boundaries (M+1) */
  EOS_REAL *TY;           /* y-dimension Taylor interval boundaries (N+1) */
  EOS_BOOLEAN tabulated_rhozero_exists; /* to verify extrapolation based upon original tabulated density range */

  /* list of Taylor-specific evaluation function pointers corresponding to subtable, EOS_TYPE_TO_SUB_TAB_NUM(dataType) */
  EOS_INTEGER (*_eos_EvaluateTaylor[4]) (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy);

  EOS_INTEGER isMonotonicX1;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicX2;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicX3;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicX4;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER shouldBeMonotonicX1;
  EOS_INTEGER shouldBeMonotonicX2;
  EOS_INTEGER shouldBeMonotonicX3;
  EOS_INTEGER shouldBeMonotonicX4;
  EOS_INTEGER isMonotonicY1;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicY2;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicY3;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER isMonotonicY4;    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER shouldBeMonotonicY1;
  EOS_INTEGER shouldBeMonotonicY2;
  EOS_INTEGER shouldBeMonotonicY3;
  EOS_INTEGER shouldBeMonotonicY4;
  EOS_INTEGER shouldBeSmooth1;
  EOS_INTEGER shouldBeSmooth2;
  EOS_INTEGER shouldBeSmooth3;
  EOS_INTEGER shouldBeSmooth4;
  EOS_INTEGER shouldBePtSmooth1;
  EOS_INTEGER shouldBePtSmooth2;
  EOS_INTEGER shouldBePtSmooth3;
  EOS_INTEGER shouldBePtSmooth4;
  EOS_INTEGER rt2_handle;       /* handle to internal record type 2 401 table */
  EOS_BOOLEAN found_401;
  EOS_BOOLEAN isInvertedAtSetup; /* indicate if tables are already inverted at setup */
} eos_RecordType1;

#include "eos_RecordType1.proto.h"

#endif
