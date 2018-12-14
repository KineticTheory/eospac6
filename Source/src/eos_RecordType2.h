/*********************************************************************
 * Class Name : eos_RecordType2
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_RECORDTYPE2_H
#define  EOS_RECORDTYPE2_H

#include "eos_Data.h"
#include "eos_SesUtils.h"

// This class is designed to manipulate Sesame data stored in format #2 -- this is specific to Sesame tables 401.

/* This container is designed to contain the following number of 1-D tables: */
#define MAX_TABLES_RECORDTYPE2 7

typedef struct
{
  eos_Data eosData;             // must be the FIRST, DO NOT MOVE!
  // Number of temperatures
  EOS_INTEGER NT;
  // vaporArrayOffset = *nt401 - num_vapor : determined by _eos_FixTable
  EOS_INTEGER vaporArrayOffset;
  // bulk data from 201 table
  EOS_REAL avgAtomicNumber;     // mean atomic number
  EOS_REAL avgAtomicWgt;        // mean atomic mass
  EOS_REAL refDensity;          // normal solid density
  EOS_REAL solidBulkModulus;    // solid bulk modulus
  EOS_REAL exchangeCoefficient; // exchange coefficient
  // Vapor Pressure
  EOS_REAL *P;                  /*[NT] */
  // Temperature
  EOS_REAL *T;                  /*[NT] */
  // Vapor Density on Coexistence Line
  EOS_REAL *RG;                 /*[NT] */
  // Density of Liquid or Solid on Coexistence Line
  EOS_REAL *RL;                 /*[NT] */
  // Internal Energy of Vapor on Coexistence Line
  EOS_REAL *EG;                 /*[NT] */
  // Internal Energy of Liquid on Coexistence Line
  EOS_REAL *EL;                 /*[NT] */
  // Free Energy of Vapor on Coexistence Line
  EOS_REAL *AG;                 /*[NT] */
  // Free Energy of Liquid on Coexistence Line
  EOS_REAL *AL;                 /*[NT] */

  EOS_INTEGER shouldBeMonotonicX1;
  EOS_INTEGER shouldBeMonotonicX2;
  EOS_INTEGER shouldBeMonotonicX3;
  EOS_INTEGER shouldBeMonotonicX4;
  EOS_INTEGER shouldBeMonotonicX5;
  EOS_INTEGER shouldBeMonotonicX6;
  EOS_INTEGER shouldBeMonotonicX7;

} eos_RecordType2;

#include "eos_RecordType2.proto.h"

#endif
