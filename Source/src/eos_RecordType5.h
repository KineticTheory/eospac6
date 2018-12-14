/*********************************************************************
 * Class Name : eos_RecordType5
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/
#ifndef  EOS_RECORDTYPE5_H
#define  EOS_RECORDTYPE5_H

#include "eos_Data.h"
#include "eos_SesUtils.h"

// This class is designed to manipulate Sesame data stored in format #5 -- this is specific to Sesame tables 201.
typedef struct
{
  eos_Data eosData;             // must be the FIRST, DO NOT MOVE!
  // Mean Atomic Number
  EOS_REAL meanAtomicNumber;
  // Mean Atomic Mass
  EOS_REAL meanAtomicMass;
  // Normal (solid) Density
  EOS_REAL normalDensity;
  // Solid Bulk Modulus
  EOS_REAL solidBulkModulus;
  // Exchange Coefficient
  EOS_REAL exchangeCoefficient;

} eos_RecordType5;

#include "eos_RecordType5.proto.h"

#endif
