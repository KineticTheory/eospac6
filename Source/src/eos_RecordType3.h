/*********************************************************************
 * Class Name : eos_RecordType3
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_RECORDTYPE3_H
#define  EOS_RECORDTYPE3_H

#include "eos_Data.h"
#include "eos_SesUtils.h"
//#include "eos_InterpMethods.h"

// This class is designed to manipulate Sesame data stored in format #3 -- this is specific to Sesame tables 501.

/* This container is designed to contain the following number of 1-D tables: */
#define MAX_TABLES_RECORDTYPE3 1

typedef struct
{
  eos_Data eosData;             // must be the FIRST, DO NOT MOVE!
  // Number of temperature-density pairs
  EOS_INTEGER N;
  // bulk data from 201 table
  EOS_REAL avgAtomicNumber;     // mean atomic number
  EOS_REAL avgAtomicWgt;        // mean atomic mass
  EOS_REAL refDensity;          // normal solid density
  EOS_REAL solidBulkModulus;    // solid bulk modulus
  EOS_REAL exchangeCoefficient; // exchange coefficient
  // LOG10 Opacity Grid Boundary: Calculated vs. Interpolated
  /* temperature-density pairs */
  EOS_REAL *T;                  /*[N], temperature */
  EOS_REAL *R;                  /*[N], density */

} eos_RecordType3;

#include "eos_RecordType3.proto.h"

#endif
