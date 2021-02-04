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
#include "eos_HashTable.h"

// This class is designed to manipulate Sesame data stored in format #2 -- this is specific to Sesame tables 401.

/* This container is designed to contain the following number of 1-D tables: */
#define MAX_TABLES_RECORDTYPE2 7

typedef struct
{
  eos_Data eosData;             /* must be the FIRST, DO NOT MOVE! */

  /* bulk data from 201 table */
  EOS_REAL avgAtomicNumber;     /* mean atomic number */
  EOS_REAL avgAtomicWgt;        /* mean atomic mass */
  EOS_REAL refDensity;          /* normal solid density */
  EOS_REAL solidBulkModulus;    /* solid bulk modulus */
  EOS_REAL exchangeCoefficient; /* exchange coefficient */

  EOS_INTEGER NT;               /* Number of temperatures */

  /* SESAME table data */
  EOS_REAL *T;                  /* Temperature [NT] */
  EOS_REAL *P;                  /* Vapor Pressure [NT] */
  EOS_REAL *RG;                 /* Vapor Density on Coexistence Line [NT] */
  EOS_REAL *RL;                 /* Density of Liquid or Solid on Coexistence Line [NT] */
  EOS_REAL *EG;                 /* Internal Energy of Vapor on Coexistence Line [NT] */
  EOS_REAL *EL;                 /* Internal Energy of Liquid on Coexistence Line [NT] */
  EOS_REAL *AG;                 /* Free Energy of Vapor on Coexistence Line [NT] */
  EOS_REAL *AL;                 /* Free Energy of Liquid on Coexistence Line [NT] */

  /* SESAME table data */
  eos_HashTable1D *T_ht;        /**< Hashtable for T, Temperature [NT] */
  eos_HashTable1D *P_ht;        /**< Hashtable for P, Vapor Pressure [NT] */
  eos_HashTable1D *RG_ht;       /**< Hashtable for RG, Vapor Density on Coexistence Line [NT] */
  eos_HashTable1D *RL_ht;       /**< Hashtable for RL, Density of Liquid or Solid on Coexistence Line [NT] */
  eos_HashTable1D *EG_ht;       /**< Hashtable for EG, Internal Energy of Vapor on Coexistence Line [NT] */
  eos_HashTable1D *EL_ht;       /**< Hashtable for EL, Internal Energy of Liquid on Coexistence Line [NT] */
  eos_HashTable1D *AG_ht;       /**< Hashtable for AG, Free Energy of Vapor on Coexistence Line [NT] */
  eos_HashTable1D *AL_ht;       /**< Hashtable for AL, Free Energy of Liquid on Coexistence Line [NT] */

  EOS_INTEGER shouldBeMonotonicX[MAX_TABLES_RECORDTYPE2];

  /* Miscellaneous metadata */
  EOS_INTEGER vaporArrayOffset; /* vaporArrayOffset = *nt401 - num_vapor : determined by _eos_FixTable */

#ifdef DO_OFFLOAD
  EOS_REAL *gpu_xtbls[MAX_TABLES_RECORDTYPE2];
  EOS_REAL *gpu_ftbls[MAX_TABLES_RECORDTYPE2];
#endif /* DO_OFFLOAD */
} eos_RecordType2;

#include "eos_RecordType2.proto.h"

#endif
