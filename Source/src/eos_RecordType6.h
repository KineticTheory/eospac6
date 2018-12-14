/*********************************************************************
 * Class Name : eos_RecordType6
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

#ifndef  EOS_RECORDTYPE6_H
#define  EOS_RECORDTYPE6_H

#include "eos_Data.h"
#include "eos_SesUtils.h"

// This class is designed to manipulate Sesame data stored in format #6 -- this is specific to Sesame table 321.

typedef struct
{
  eos_Data eosData;             // must be the FIRST, DO NOT MOVE!
  // Number of densities
  EOS_INTEGER NR;
  // Number of temperatures
  EOS_INTEGER NT;
  // Number of phases
  EOS_INTEGER NP; /* DAP --  is this variable really needed since it's the
		     same value as me->eosData.numSubtablesLoaded? */
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
  // <B>Array containing one of the following kind of data:</B>
  // Mass fraction
  EOS_REAL ***table;            /* [NP][NT][NR] */
} eos_RecordType6;

#include "eos_RecordType6.proto.h"

#endif
