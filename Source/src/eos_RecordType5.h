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
  eos_Data eosData;             /* must be the FIRST, DO NOT MOVE! */
  EOS_REAL avgAtomicNumber;     /* Mean Atomic Number */
  EOS_REAL avgAtomicWgt;        /* Mean Atomic Mass */
  EOS_REAL refDensity;          /* Normal (solid) Density */
  EOS_REAL solidBulkModulus;    /* Solid Bulk Modulus */
  EOS_REAL exchangeCoefficient; /* Exchange Coefficient */
} eos_RecordType5;

#include "eos_RecordType5.proto.h"

#endif
