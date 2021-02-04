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
#include "eos_HashTable.h"

/* This class is designed to manipulate Sesame data stored in format #1,
 * which is specific to Sesame tables 301-306, 411, 412, 431, 
 * 432, 502-505, and 601-605.
 */

/* This container is designed to contain the following number of 2-D tables: */
#define MAX_TABLES_RECORDTYPE1 5

typedef struct
{
  eos_Data eosData;             /* must be the FIRST, DO NOT MOVE! */

  /* bulk data from 201 table */
  EOS_REAL avgAtomicNumber;     /* mean atomic number */
  EOS_REAL avgAtomicWgt;        /* mean atomic mass */
  EOS_REAL refDensity;          /* normal solid density */
  EOS_REAL solidBulkModulus;    /* solid bulk modulus */
  EOS_REAL exchangeCoefficient; /* exchange coefficient */

  /* SESAME table data */
  EOS_INTEGER NR;               /* Number of densities */
  EOS_INTEGER NT;               /* Number of temperatures */
  EOS_REAL *R;                  /* Density array [NR] */
  EOS_REAL *T;                  /* Temperature array [NT] */
    /* NOTE: The **table[i] and *coldCurve[i] arrays hold data as follows:
       i = 0 : Pressure
       i = 1 : Internal Energy
       i = 2 : Helmholtz Free Energy
       i = 3 : Entropy
       i = 4 : Gibbs Free Energy
     */
  EOS_REAL **table[MAX_TABLES_RECORDTYPE1];    /*[NT][NR] */
  EOS_REAL *coldCurve[MAX_TABLES_RECORDTYPE1]; /* [NR} */

  eos_Taylor ***Taylor_objects; /* Array (dimension [M*N][MAX_TABLES_RECORDTYPE1]) of Taylor polynomial object pointers; alternative to table[i] where i=1..MAX_TABLES_RECORDTYPE1 */
  EOS_INTEGER M;          /* number of x-dimension Taylor intervals */
  EOS_INTEGER N;          /* number of y-dimension Taylor intervals */
  EOS_REAL *TX;           /* x-dimension Taylor interval boundaries (M+1) */
  EOS_REAL *TY;           /* y-dimension Taylor interval boundaries (N+1) */
  EOS_BOOLEAN tabulated_rhozero_exists; /* to verify extrapolation based upon original tabulated density range */

  /* list of Taylor-specific evaluation function pointers corresponding to subtable, EOS_TYPE_TO_SUB_TAB_NUM(dataType) */
  EOS_INTEGER (*_eos_EvaluateTaylor[MAX_TABLES_RECORDTYPE1]) (eos_Taylor *T, EOS_REAL x, EOS_REAL y, EOS_REAL *f, EOS_REAL *dFx, EOS_REAL *dFy);

  /* Miscellaneous metadata */
  EOS_INTEGER isMonotonicX[MAX_TABLES_RECORDTYPE1];    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER shouldBeMonotonicX[MAX_TABLES_RECORDTYPE1];
  EOS_INTEGER isMonotonicY[MAX_TABLES_RECORDTYPE1];    /* 1 for monotonic, 0 for non-monotonic, -1 for unset. */
  EOS_INTEGER shouldBeMonotonicY[MAX_TABLES_RECORDTYPE1];
  EOS_INTEGER shouldBeSmooth[MAX_TABLES_RECORDTYPE1];
  EOS_INTEGER shouldBePtSmooth[MAX_TABLES_RECORDTYPE1];
  EOS_INTEGER rt2_handle;        /* handle to internal record type 2 401 table */
  EOS_BOOLEAN found_401;
  EOS_BOOLEAN isInvertedAtSetup; /* indicate if tables are already inverted at setup */
  EOS_BOOLEAN CreateGhostData;   /* indicate if tables are to be created with ghost data (default: EOS_TRUE) */
  EOS_INTEGER nGhostData;        /* ghost data count at each table boundary */
  EOS_BOOLEAN useTmpGhostData;   /* ghost data is temporarily created during interpolation */
  EOS_BOOLEAN FreeUnusedArrays;  /* indicate usage of _eos_FreeUnusedArraysRecordType1 (default: EOS_TRUE) */

  /* Hashtables for use in eos_Search */
  eos_HashTable1D *R_hashTable;                         /**< Hashtable for density array R */
  eos_HashTable1D *T_hashTable;                         /**< Hashtable for temperature array T */
  eos_HashTable2D *hashTables[MAX_TABLES_RECORDTYPE1];  /**< 2D Hashtables corresponding to each in table[]  */

#ifdef DO_OFFLOAD
  EOS_REAL *gpu_xtbls;  /*gpu offloaded sesame tables */
  EOS_REAL *gpu_ytbls;
  EOS_REAL *gpu_ftbls_th1, *gpu_ftbls_th2, *gpu_ftbls_th3, *gpu_ftbls_th4, *gpu_ftbls_th5;
  EOS_REAL *gpu_xtbls_th1, *gpu_xtbls_th2, *gpu_xtbls_th3, *gpu_xtbls_th4, *gpu_xtbls_th5;
  EOS_REAL *gpu_ytbls_th1, *gpu_ytbls_th2, *gpu_ytbls_th3, *gpu_ytbls_th4, *gpu_ytbls_th5;
  EOS_REAL *gpu_coldCurve_th1, *gpu_coldCurve_th2, *gpu_coldCurve_th3, *gpu_coldCurve_th4, *gpu_coldCurve_th5;

#endif /* DO_OFFLOAD */
} eos_RecordType1;

#include "eos_RecordType1.proto.h"

#endif
