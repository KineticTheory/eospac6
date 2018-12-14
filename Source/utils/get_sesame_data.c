/*********************************************************************
 * Utility Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
/*!
 * \file
 * \ingroup utils
 * \brief
 * Given a Sesame material ID and table number, extract data from Sesame database an serve
 * to stdout in a format compatible with GNUPLOT's input requirements for a 2-D plot.
 *
 * \pre libeospac6.a (version 6.3 or later) is required
 *
 * \author David A. Pimentel
 *
 * \par STDOUT
 * Only data in an appropriate gnuplot format are returned unless <CUSTOM OPTIONS> are provided.
 *
 * \par STDERR
 * All information and error messages are returned.
 *
 * \par USAGE
 * get_sesame_data [<GENERAL OPTIONS>] <sesMaterialNum> <sesTableNum> [ <sesSubtableIndex> ]
 *
 * get_sesame_data [<GENERAL OPTIONS>] id [ <file> ]
 *
 * get_sesame_data [<GENERAL OPTIONS>] tables <sesMaterialNum> [, <sesMaterialNum> [, ... ]]
 *
 * get_sesame_data [<GENERAL OPTIONS>] comments <sesMaterialNum> [, <sesMaterialNum> [, ... ]]
 *
 * get_sesame_data [<GENERAL OPTIONS>] <CUSTOM OPTIONS> <sesMaterialNum>
 *
 * \arg <sesMaterialNum>     Sesame material ID number
 * \arg <sesTableNum>        Sesame table number
 * \arg <sesSubtableIndex>   Optional Sesame subtable number (default=1).
 * <br>
 * \arg id                   Character string, "id" used as option flag to fetch material ID number list
 * \arg <file>               Optional Sesame file name
 * <br>
 * \arg tables               Character string, "tables" used as option flag to fetch table number list and table sizes for one or more material IDs
 * <br>
 * \arg comments             Character string, "comments" used as option flag to fetch all comments for one or more material IDs
 * <br>
 * \arg <GENERAL OPTIONS>
 * \arg -F <file>
 *              Search named Sesame <file> exclusively.
 * \arg -f
 *              Force the manufacture of the required table(s).
 * \arg -h
 *              Display the basic usage of this tool.
 * \arg -V
 *              Display the version number.
 * \arg -v
 *              Enable verbose output. Multiple instances of this option will increase the
 *              verbosity.
 * \arg -X
 *              Force the loaded table(s) to be monotonic with respect to x.
 * \arg -Y
 *              Force the loaded table(s) to be monotonic with respect to y.
 * <br>
 * \arg <CUSTOM OPTIONS>
 * \arg -I N
 *              Number of samples, N, to take from the x-independent variable's range. Default=50.
 * \arg -J N
 *              Number of samples, N, to take from the y-independent variable's range. Default=50.
 * \arg -n N
 *              Number of samples, N, to take from each independent variable's range. Default=50.
 *              This option overrides the '-I' and '-J' options.
 * \arg -r R[:R1]
 *              Specify density (g/cc).
 *              Optional range upper bound, R1, delimited with a colon, ":".
 * \arg -T T[:T1]
 *              Specify temperature (K) -- requires "-r" option.
 *              Optional range upper bound, T1, delimited with a colon, ":".
 * \arg -P P[:P1]
 *              Specify pressure (GPa) -- requires "-r" option.
 *              Optional range upper bound, P1, delimited with a colon, ":".
 * \arg -U U[:U1]
 *              Specify specific internal energy (MJ/kg) -- requires "-r" option.
 *              Optional range upper bound, U1, delimited with a colon, ":".
 * \arg -A A[:A1]
 *              Specify specific Helmholtz free energy (MJ/kg) -- requires "-r" option.
 *              Optional range upper bound, A1, delimited with a colon, ":".
 * \arg -S S[:S1]
 *              Specify specific entropy (MJ/kg/K) -- requires "-r" option.
 *              Optional range upper bound, S1, delimited with a colon, ":".
 *
 *              Note that the EOSPAC table inversion algorithm may produce incorrect
 *              and/or unexpected results when using any of the following options:
 *              -P, -U, -A, -S.
 *
 * The following are applicable values of sesSubtableIndex given sesTableNum:
 * \verbatim
 *       sesTableNum sesSubtableIndex (description)
 *               101 <ignored>        (comment tables)
 *               201 <ignored>        (general information)
 *               301 1                (total pressure)
 *                   2                (total internal energy)
 *                   3                (total free energy)
 *                   4                (total entropy -- derived from Sesame data)
 *               303 1                (ion + cold curve pressure)
 *                   2                (ion + cold curve internal energy)
 *                   3                (ion + cold curvefree energy)
 *                   4                (ion + cold entropy -- derived from Sesame data)
 *               304 1                (electron pressure)
 *                   2                (electron internal energy)
 *                   3                (electron free energy)
 *                   4                (electron entropy -- derived from Sesame data)
 *               305 1                (ion pressure)
 *                   2                (ion internal energy)
 *                   3                (ion free energy)
 *                   4                (ion entropy -- derived from Sesame data)
 *               306 1                (cold curve pressure)
 *                   2                (cold curve internal energy)
 *                   3                (cold curve free energy)
 *               311 1                (maxwell total pressure)
 *                   2                (maxwell total internal energy)
 *                   3                (maxwell total free energy)
 *                   4                (maxwell total entropy -- derived from Sesame data)
 *               321 <num>            (mass fraction data corresponding to material phase <num>)
 *               401 1                (vapor pressure)
 *                   2                (vapor density on coexistence line)
 *                   3                (density of liquid or solid on coexistence line)
 *                   4                (internal energy of vapor on coexistence line)
 *                   5                (internal energy of liquid on coexistence line)
 *                   6                (free energy of vapor on coexistence line)
 *                   7                (free energy of liquid on coexistence line)
 *               411 1                (melt temperature)
 *                   2                (melt pressure)
 *                   3                (internal energy of solid on the melt line)
 *                   4                (free energy of solid on the melt line)
 *               412 1                (melt temperature)
 *                   2                (melt pressure)
 *                   3                (internal energy of liquid on the melt line)
 *                   4                (free energy of liquid on the melt line)
 *               431 1                (shear modulus)
 *               501 1                (LOG10 temperature/density pairs)
 *           502-505 1                (LOG10 opacity quantities [see Ref. 1 for more detail])
 *           601-605 1                (LOG10 conductivity quantities [see Ref. 1 for more detail])
 * \endverbatim
 *
 * \par References
 *           -# D. A. Pimentel,
 *              "EOSPAC USER'S MANUAL: Version 6.3", LA-UR-14-29289,
 *              Los Alamos National Laboratory (2014).
 *           -# S. P. Lyon, J. D. Johnson, Group T-1,
 *              "Sesame: The Los Alamos National Laboratory
 *              Equation of State Database",
 *              LA-UR-92-3407.
 *
 * \retval      0     No errors encounterd
 * \retval      1     Insufficient arguments passed to this utility
 * \retval      2     Invalid sesSubtableIndex
 * \retval      3     sesSubtableIndex is valid given sesTableNum, but the subtable is unavailable
 * \retval      4     Invalid sesTableNum
 * \retval      5     Data dump error; no stdout
 * \retval      6     fprintf function error; probably system-related
 * \retval      7     memory allocation error
 * \retval      8     Invalid sesame file name
 * \retval      9     Missing sesame file
 * \retval     10     Incorrect number of independent variables specified
 * \retval     11     Invalid independent variables specified
 * \retval     12     The -r (density) independent variable is required
 * \retval     13     _QuickSort ERROR exceeded QUICKSORT_RECURSION_LIMIT
 * \retval     14     calculateCategory2 ERROR performing inverse interpolation
 * \retval    255     EOSPAC related error occurred; corresponding message written to stderr
 *
 */

#include "common.h"
#include "get_sesame_data_help.h"
#include <sys/types.h>
#include <sys/stat.h>

// ****************************************************************
// dumpRecordPatternType1
// ****************************************************************
int dumpRecordPatternType1 (EOS_INTEGER th, EOS_INTEGER load_phase) {

  // local variables
  int     i, j, k;
  EOS_INTEGER NR, NT, NP = 1, infoItem[1], errorCode=EOS_OK;
  EOS_REAL val;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_REAL *X=NULL, *Y=NULL, *F=NULL;
  EOS_INTEGER nXYPairs;

  // fetch NR and NT data;
  // return error code if requested is not available
  infoItem[0] = EOS_NR;
  eos_GetTableInfo (&th, &one, infoItem, &val, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }
  NR = (EOS_INTEGER) val;

  infoItem[0] = EOS_NT;
  eos_GetTableInfo (&th, &one, infoItem, &val, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }
  NT = (EOS_INTEGER) val;

  if (load_phase > 0) {
    infoItem[0] = EOS_NUM_PHASES;
    eos_GetTableInfo (&th, &one, infoItem, &val, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
      return eospacError;
    }
    NP = (EOS_INTEGER) val;
  }

  nXYPairs = NR * NT * NP;

  /* allocate memory continuously */
  EOS_FREE(X);
  EOS_FREE(Y);
  EOS_FREE(F);
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  /* initialize array(s) */
  if (! (X && Y && F)) {
    fprintf (stderr, "Memory allocation error!\n");
    return mallocError;
  }

  // fetch R data;
  infoItem[0] = EOS_R_Array;
  eos_GetTableInfo (&th, &NR, infoItem, X, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }

  // fetch T data;
  infoItem[0] = EOS_T_Array;
  eos_GetTableInfo (&th, &NT, infoItem, Y, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }

  // fetch F data;
  infoItem[0] = EOS_F_Array;
  eos_GetTableInfo (&th, &nXYPairs, infoItem, F, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }
  
  // dump each isotherm in in succession; each line contains the
  // following data triplet: density   F(rho,T)   temperature
  fprintf(stderr, "NR:              %d\n", NR);
  fprintf(stderr, "NT:              %d\n", NT);
  if (load_phase > 0) {
    fprintf(stderr, "NP:              %d\n", NP);
    k = ((NP - 1) * (load_phase - 1)) / (NP - 1);
  }
  else {
    k = 0;
  }
  for(j=0; j<NT; j++) {
    for(i=0; i<NR; i++)
      fprintf(stdout, "%23.15E %23.15E %23.15E\n",
              X[i], F[k*NR*NT + i + j*NR], Y[j]);
    fprintf(stdout,"\n");
  }

  EOS_FREE(X);
  EOS_FREE(Y);
  EOS_FREE(F);

  return(EOS_OK);

}

// ****************************************************************
// dumpRecordPatternType2
// ****************************************************************
int dumpRecordPatternType2 (EOS_INTEGER th, EOS_BOOLEAN flag) {

  // local variables
  int     i;
  EOS_INTEGER NT, infoItem[1], errorCode=EOS_OK;
  EOS_REAL val;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_REAL *X=NULL, *F=NULL;
  EOS_INTEGER nXYPairs;

  // fetch NT data;
  // return error code if requested is not available
  infoItem[0] = EOS_NT;
  eos_GetTableInfo (&th, &one, infoItem, &val, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }
  NT = (EOS_INTEGER) val;

  nXYPairs = NT;

  /* allocate memory continuously */
  EOS_FREE(X);
  EOS_FREE(F);
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  F = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  /* initialize array(s) */
  if (! (X && F)) {
    fprintf (stderr, "Memory allocation error!\n");
    return mallocError;
  }

  // fetch T data;
  infoItem[0] = EOS_T_Array;
  eos_GetTableInfo (&th, &NT, infoItem, X, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }

  // fetch F data;
  infoItem[0] = EOS_F_Array;
  if (flag) infoItem[0] = EOS_R_Array;
  eos_GetTableInfo (&th, &nXYPairs, infoItem, F, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
    return eospacError;
  }
  
  // dump each temperature-dependent array in in succession
  fprintf(stderr, "NT:              %d\n", NT);
  for(i=0; i<NT; i++) {
    //                         Temperature     F(T)
    fprintf(stdout, "%23.15E %23.15E\n", X[i], F[i]);
  }

  EOS_FREE(X);
  EOS_FREE(F);

  return(EOS_OK);

}

// ****************************************************************
// dumpRecordPatternType3
// ****************************************************************
int dumpRecordPatternType3 (EOS_INTEGER th) {

  return dumpRecordPatternType2 (th,EOS_TRUE);

}

// ****************************************************************
// dumpData
// ****************************************************************
int dumpData(EOS_INTEGER th,
             EOS_INTEGER sesTableNum,
             EOS_INTEGER sesSubtableIndex) {

  // local variables
  enum { nInfoItemsE = 5};
  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER nInfoItems = nInfoItemsE;
  EOS_REAL infoVals[nInfoItemsE];
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_Mean_Atomic_Num,
    EOS_Mean_Atomic_Mass,
    EOS_Normal_Density,
    EOS_Modulus,
    EOS_Exchange_Coeff
  };
  EOS_INTEGER commentInfoItems[1] = {
    EOS_Cmnt_Len
  };
  EOS_INTEGER Cmnt_Len=0;
  EOS_CHAR *cmntStr=NULL;

  // select table type to dump to stdout
  switch (sesTableNum) {

  case 100:
  case 101:
    eos_GetTableInfo (&th, &one, commentInfoItems, infoVals, &err);
    if (err == EOS_OK) {
      Cmnt_Len = (EOS_INTEGER) (infoVals[0]);
      cmntStr = (EOS_CHAR *) malloc (sizeof (EOS_CHAR) * Cmnt_Len);
      if (! cmntStr) {
        fprintf (stderr, "Memory allocation error!\n");
        return mallocError;
      }
      eos_GetTableCmnts (&th, cmntStr, &err);
      err = fprintf(stdout,"Comments:\n%s\n", cmntStr);
      EOS_FREE(cmntStr);
    }
    if (err < 0)
      err = FprintfError;
    else
      err = EOS_OK;
    break;

  case 201:
    eos_GetTableInfo (&th, &nInfoItems, infoItems, infoVals, &err);
    if (err == EOS_OK)
      err = fprintf(stdout,"Mean atomic number:  %23.15E\nMean atomic mass:    %23.15E\nNormal density:      %23.15E\nSolid bulk modulus:  %23.15E\nExchange coefficient:%23.15E\n",
                    infoVals[0], infoVals[1], infoVals[2], infoVals[3], infoVals[4]);
    if (err < 0)
      err = FprintfError;
    else
      err = EOS_OK;
    break;

  case 301: case 303: case 304: case 305: case 306:
    err = dumpRecordPatternType1(th,0);
    break;

  case 311:
    err = dumpRecordPatternType1(th,0);
    break;

  case 321:
    err = dumpRecordPatternType1(th,sesSubtableIndex);
    break;

  case 401:
    err = dumpRecordPatternType2(th,EOS_FALSE);
    break;

  case 411: case 412:
    err = dumpRecordPatternType1(th,0);
    break;

  case 431:
    err = dumpRecordPatternType1(th,0);
    break;

  case 501:
    err = dumpRecordPatternType3(th);
    break;

  case 502: case 503: case 504: case 505:
  case 601: case 602: case 603: case 604: case 605:
    err = dumpRecordPatternType1(th,0);
    break;

  default: // Invalid sesTableNum
    return(NoStdoutPossible);
  }

  return(err);
}

// ****************************************************************
// calculateCustomQuantities
// ****************************************************************
typedef struct {
  EOS_REAL r; /* density */
  EOS_REAL T; /* temperature */
  EOS_REAL P; /* total pressure */
  EOS_REAL U; /* total specific internal energy */
  EOS_REAL A; /* total specific helmholtz free energy */
  EOS_REAL S; /* total specific entropy */
  EOS_BOOLEAN P_extrap;
  EOS_BOOLEAN U_extrap;
  EOS_BOOLEAN A_extrap;
  EOS_BOOLEAN S_extrap;
  EOS_REAL dPdr_T; /* dP/dr @ constant T */
  EOS_REAL dUdr_T; /* dU/dr @ constant T */
  EOS_REAL dAdr_T; /* dA/dr @ constant T */
  EOS_REAL dSdr_T; /* dS/dr @ constant T */
  EOS_REAL dPdT_r; /* dP/dT @ constant r */
  EOS_REAL dUdT_r; /* dU/dT @ constant r */
  EOS_REAL dAdT_r; /* dA/dT @ constant r */
  EOS_REAL dSdT_r; /* dS/dT @ constant r */
  EOS_REAL dPdr_S; /* dP/dr @ constant S */
  EOS_REAL dPdS_r; /* dP/dS @ constant r */
  EOS_REAL dSdr_P; /* dS/dr @ constant P */
  EOS_REAL dSdP_r; /* dS/dP @ constant r */
  EOS_REAL dPdr_U; /* dP/dr @ constant U */
  EOS_REAL dPdU_r; /* dP/dU @ constant r */
  EOS_REAL adiabaticBulkModulus; /* r*c^2 */
  EOS_REAL isothermalBulkModulus; /*r*cT^2 */
  EOS_REAL thermalExpansionAlpha; /* (dP/dT @ constant r)/isothermalBulkModulus */
  EOS_REAL soundSpeed;     /* (r*(dP/dr @ constant S))^0.5 */
  EOS_REAL soundSpeed_alt; /* ((-dS/dr @ const. P)/(dS/dP @ const. r))^0.5 */
  EOS_REAL soundSpeed_T;   /* ((-dS/dr @ const. P)/(dS/dP @ const. r))^0.5 */
  EOS_REAL Ks; /* isentropic compressibility, 1/(r*c^2) */
  EOS_REAL KT; /* isothermal compressibility, 1/(r*cT^2) */
  EOS_REAL gruneisenCoefficient;
  EOS_REAL Cv; /* constant volume specific heat */
  EOS_REAL Cp; /* constant pressure specific heat */
} customValues_t;
/*
 * EOS_INTEGER sesMaterialNum     -- SESAME material id number
 * EOS_INTEGER N                  -- array dimension of x, y and data
 * EOS_REAL *x                    -- input densities
 * EOS_REAL *y                    -- input temperatures
 * EOS_BOOLEAN displayResults     -- boolean: if true, then this function will display calculated results
 * customValues_t *data           -- array of data structures containing calculated results
 */
int calculateCustomQuantities(EOS_INTEGER sesMaterialNum, EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y,
			      EOS_CHAR y_flag, EOS_BOOLEAN displayResults, customValues_t *data) {

  enum {nTablesE = 7};

  // local variables
  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER nTables = nTablesE;
  EOS_INTEGER tableType[nTablesE] = {EOS_Pt_DT, EOS_Ut_DT, EOS_At_DT, EOS_St_DT, EOS_Pt_DSt, EOS_St_DPt, EOS_Pt_DUt};
  EOS_INTEGER tableHandle[nTablesE], matNum[nTablesE];
  EOS_REAL c, c_alt, rc2, Gamma, rcT2, Cv, Cp;
  EOS_REAL F[nTablesE], dFx[nTablesE], dFy[nTablesE];
  int i, idx;
  EOS_INTEGER indexFileCount;

  /* initialize arrays */
  for (i=0; i<nTables; i++) {
    tableHandle[i] = -1;
    matNum[i] = sesMaterialNum;
  }

  indexFileCount = 0;
  if (option['F'].flag) {
    indexFileCount = createIndexFile(option['F'].arg);
  }

  /* Load required data */
  eos_CreateTables (&nTables, tableType, matNum, tableHandle, &err);
  if (err != EOS_OK)
    return((int)err);

  if (option['f'].flag) {
    for (i=0; i<nTables; i++) {
      eos_SetOption (&tableHandle[i], &EOS_CREATE_TZERO, EOS_NullPtr, &err);
      if (err != EOS_OK)
	return((int)err);
    }
  }

  eos_LoadTables (&nTables, tableHandle, &err);

  if (option['F'].flag) {
    cleanIndexFile(indexFileCount);
  }

  if (err != EOS_OK)
    return((int)err);

  for (idx=0; idx<N; idx++) {

    data[idx].P_extrap = EOS_FALSE;
    data[idx].U_extrap = EOS_FALSE;
    data[idx].A_extrap = EOS_FALSE;
    data[idx].S_extrap = EOS_FALSE;

    /* pressure, P */
    i = 0;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &y[idx], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK) {
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &err, &equal);
      if (! equal)
	return((int)err);
      else
	data[idx].P_extrap = EOS_TRUE;
    }

    /* r * cT^2 */
    rcT2 = x[idx] * dFx[i];

    /* specific internal energy, U */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &y[idx], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK) {
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &err, &equal);
      if (! equal)
	return((int)err);
      else
	data[idx].U_extrap = EOS_TRUE;
    }

    /* specific Helmholtz free energy, A */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &y[idx], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK) {
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &err, &equal);
      if (! equal)
	return((int)err);
      else
	data[idx].A_extrap = EOS_TRUE;
    }

    /* specific entropy, S */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &y[idx], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK) {
      EOS_BOOLEAN equal;
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &err, &equal);
      if (! equal)
	return((int)err);
      else
	data[idx].S_extrap = EOS_TRUE;
    }

    /* sound speed, c = (dP/dr @ const. S)^0.5 */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &F[i-1], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK)
      return((int)err);
    c = pow(dFx[i], 0.5);

    /* adiabatic bulk modulus, r*c^2 */
    rc2 = x[idx] * dFx[i];

    /* alternative sound speed calculation, c = ((-dS/dr @ const. P)/(dS/dP @ const. r))^0.5 */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &F[i-1], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK)
      return((int)err);
    c_alt = pow(-dFx[i]/dFy[i], 0.5); /* this introduces interpolation error */

    /* gruneisen coefficient, Gamma = 1/r * dP/dU @ constant r */
    i++;
    eos_Interpolate(&tableHandle[i], &one, &x[idx], &F[1], &F[i], &dFx[i], &dFy[i], &err);
    if (err != EOS_OK)
      return((int)err);
    Gamma = dFy[i] / ((x[idx] > 1.0e-13) ? x[idx] : ((x[idx] < -1.0e-13) ? x[idx] : ((x[idx] > 0) ? 1.0e-13 : -1.0e-13)));

    /* constant volume specific heat, Cv */
    Cv = dFy[1];

    /* Constant pressure specific heat, Cp = T * dS/dT @ constant P is not calculated, because
     * S(P,T) is unavailable and (dU/dT @ const. P) is unreliable in EOSPAC 6.
     */
    Cp = rc2 / rcT2 * Cv;

    /* store results */
    data[idx].r = x[idx]; /* density */
    data[idx].T = y[idx]; /* temperature */

    i = 0;
    data[idx].P      = F[i];   /* total pressure */
    data[idx].dPdr_T = dFx[i]; /* dP/dr @ constant T */
    data[idx].dPdT_r = dFy[i]; /* dP/dT @ constant r */

    i++;
    data[idx].U      = F[i]; /* total specific internal energy */
    data[idx].dUdr_T = dFx[i]; /* dU/dr @ constant T */
    data[idx].dUdT_r = dFy[i]; /* dU/dT @ constant r */

    i++;
    data[idx].A      = F[i]; /* total specific helmholtz free energy */
    data[idx].dAdr_T = dFx[i]; /* dA/dr @ constant T */
    data[idx].dAdT_r = dFy[i]; /* dA/dT @ constant r */

    i++;
    data[idx].S      = F[i]; /* total specific entropy */
    data[idx].dSdr_T = dFx[i]; /* dS/dr @ constant T */
    data[idx].dSdT_r = dFy[i]; /* dS/dT @ constant r */

    i++;
    data[idx].dPdr_S = dFx[i]; /* dP/dr @ constant S */
    data[idx].dPdS_r = dFy[i]; /* dP/dS @ constant r */

    i++;
    data[idx].dSdr_P = dFx[i]; /* dS/dr @ constant P */
    data[idx].dSdP_r = dFy[i]; /* dS/dP @ constant r */

    i++;
    data[idx].dPdr_U = dFx[i]; /* dP/dr @ constant U */
    data[idx].dPdU_r = dFy[i]; /* dP/dU @ constant r */

    data[idx].adiabaticBulkModulus  = rc2;   /* r*c^2 */
    data[idx].isothermalBulkModulus = rcT2;  /* r*cT^2 */
    data[idx].thermalExpansionAlpha =
      data[idx].dPdT_r / data[idx].isothermalBulkModulus; /* (dP/dT @ constant r)/isothermalBulkModulus */
    data[idx].soundSpeed            = c;     /* (r*(dP/dr @ constant S))^0.5 */
    data[idx].soundSpeed_alt        = c_alt; /* ((-dS/dr @ const. P)/(dS/dP @ const. r))^0.5 */

    data[idx].Ks = 1.0 / data[idx].adiabaticBulkModulus; /* isentropic compressibility, 1/(r*c^2) */
    data[idx].KT = 1.0 / data[idx].isothermalBulkModulus; /* isothermal compressibility, 1/(r*cT^2) */

    data[idx].gruneisenCoefficient = Gamma;
    data[idx].Cv = Cv; /* constant volume specific heat */
    data[idx].Cp = Cp; /* constant pressure specific heat */

    if (displayResults) {
      /* display results */
      char pad[50], *p;
      sprintf(pad, "%e", 1.0);
      p = pad;
      while (*p) {
	*p = ' ';
	p++;
      }
      printf("r = %e T = %e P = %e dP/dr = %e dP/dT = %e\n", data[idx].r, data[idx].T, data[idx].P, data[idx].dPdr_T, data[idx].dPdT_r);
      printf("    %s     %s => r*cT^2 = %e\n", pad, pad, data[idx].isothermalBulkModulus);
      printf("\n");
      printf("    %s     %s U = %e dU/dr = %e dU/dT = %e\n", pad, pad, data[idx].U, data[idx].dUdr_T, data[idx].dUdT_r);
      printf("\n");
      printf("    %s     %s A = %e dA/dr = %e dA/dT = %e\n", pad, pad, data[idx].A, data[idx].dAdr_T, data[idx].dAdT_r);
      printf("\n");
      printf("    %s     %s S = %e dS/dr = %e dS/dT = %e\n", pad, pad, data[idx].S, data[idx].dSdr_T, data[idx].dSdT_r);
      printf("\n");
      printf("    %s     %s Cv = %e Cp = %e\n", pad, pad, data[idx].Cv, data[idx].Cp);
      printf("\n");
      printf("    %s S = %e P = %e dP/dr = %e dP/dS = %e\n", pad, data[idx].S, data[idx].P, data[idx].dPdr_S, data[idx].dPdS_r);
      printf("    %s     %s => sound speed, c  = (dP/dr @ const. S)^0.5 = %23.15e\n", pad, pad, data[idx].soundSpeed);
      printf("    %s     %s => adiabatic bulk modulus, r*c^2   = %e\n", pad, pad, data[idx].adiabaticBulkModulus);
      printf("    %s     %s => isothermal bulk modulus, r*cT^2 = %e\n", pad, pad, data[idx].isothermalBulkModulus);
      printf("    %s     %s => cT = ((r*cT^2)/r)^0.5 = %23.15e\n", pad, pad, pow(data[idx].isothermalBulkModulus/data[idx].r,0.5));
      printf("    %s     %s => thermal expansion alpha, (dP/dT @ const. r)/(r*cT^2) = %e\n", pad, pad, data[idx].thermalExpansionAlpha);
      printf("    %s     %s => isentropic compressibility, Ks=1/(r*c^2)  = %e\n", pad, pad, data[idx].Ks);
      printf("    %s     %s => isothermal compressibility, KT=1/(r*cT^2) = %e\n", pad, pad, data[idx].KT);
      printf("\n");
      printf("    %s P = %e S = %e dS/dr = %e dS/dP = %e\n", pad, data[idx].P, data[idx].S, data[idx].dSdr_P, data[idx].dSdP_r);
      printf("    %s     %s => sound speed, c = ((-dS/dr)/(dS/dP))^0.5 = %23.15e\n", pad, pad, data[idx].soundSpeed_alt);
      printf("\n");
      printf("    %s U = %e P = %e dP/dr = %e dP/dU = %e\n", pad, data[idx].U, data[idx].P, data[idx].dPdr_U, data[idx].dPdU_r);
      printf("    %s     %s => gruneisen coefficient = %e\n", pad, pad, Gamma);

      printf("cT^2 <= c^2 ? %s ; Cv <= Cp ? %s\n",
	     ((rcT2<=rc2)?"TRUE":"FALSE (numerical error)"), ((Cv<=Cp)?"TRUE":"FALSE (numerical error)"));
    }
  }

  if (displayResults) {
    printf("\nUNITS\n");
    printf("\tr:  g/cc          T:  K\n");
    printf("\tP:  GPa           U:  MJ/kg         A: MJ/kg          S:  MJ/kg/K\n");
    printf("\tCv: MJ/kg/K       Cp: MJ/kg/K       c: km/s           cT: km/s\n");
    printf("\tKs: 1/GPa         KT: 1/GPa\n");

    if(option['v'].flag > 1) {
      printf("\ntableHandlesMap:\n");
      for (i=0; i<nTables; i++)
	printf("\tth %d => %d\n",tableHandle[i],get_tableHandlesMap_val(tableHandle[i]));
    }
  }

  /* clean up */
  eos_DestroyTables(&nTables, tableHandle, &err);

  return((int)err);
}

int printTabularCustomData(int N, customValues_t *data, char *type) {
#define FLAG_CNT 13
  int i, j;
  int typeFlag[FLAG_CNT];
  char *p1, *p2;

  for (i=0; i<FLAG_CNT; i++) typeFlag[i] = 0;

  p1 = type;
  p2 = NULL;
  while (1) { /* split into substrings at ',' and ignore leading ' ' and '\t' */
    EOS_BOOLEAN last = EOS_FALSE;
    p2 = strchr(p1, ',');
    if (! p2) {
      p2 = p1 + strlen(p1);
      last = EOS_TRUE;
    }

    i = 0;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"P",p2-p1))                     typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"U",p2-p1))                     typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"A",p2-p1))                     typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"S",p2-p1))                     typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"adiabaticBulkModulus",p2-p1))  typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"isothermalBulkModulus",p2-p1)) typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"thermalExpansionAlpha",p2-p1)) typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"soundSpeed",p2-p1))            typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"soundSpeed_alt",p2-p1))        typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"cT",p2-p1))                    typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"Ks",p2-p1))                    typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"KT",p2-p1))                    typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"gruneisenCoefficient",p2-p1))  typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"Cv",p2-p1))                    typeFlag[i++] = 1; else i++;
    if (! strncmp(p1,"all",p2-p1) || ! strncmp(p1,"Cp",p2-p1))                    typeFlag[i++] = 1; else i++;

    if (last) break;
    p1 = p2 + 1;
    p2 = NULL;
  }

  {
    char *fmt   = "%-23.15e    %-23.15e    %-23.15e%s\n";
    char *fmt_t = "\n%-23s    %-23s    %-23s\n";
    i = 0;
    if (typeFlag[i++]) { /* P */
      printf(fmt_t,"r:  g/cc", "T:  K", "P:  GPa");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].P, ((data[j].P_extrap)?" extrapolated":""));
      }
    }
    if (typeFlag[i++]) { /* U */
      printf(fmt_t,"r:  g/cc", "T:  K", "U:  MJ/kg");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].U, ((data[j].U_extrap)?" extrapolated":""));
      }
    }
    if (typeFlag[i++]) { /* A */
      printf(fmt_t,"r:  g/cc", "T:  K", "A:  MJ/kg");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].A, ((data[j].A_extrap)?" extrapolated":""));
      }
    }
    if (typeFlag[i++]) { /* S */
      printf(fmt_t,"r:  g/cc", "T:  K", "S:  MJ/kg/K");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].S, ((data[j].S_extrap)?" extrapolated":""));
      }
    }
    if (typeFlag[i++]) { /* adiabaticBulkModulus */
      printf(fmt_t,"r:  g/cc", "T:  K", "adiabaticBulkModulus: GPa");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].adiabaticBulkModulus, "");
      }
    }
    if (typeFlag[i++]) { /* thermalExpansionAlpha */
      printf(fmt_t,"r:  g/cc", "T:  K", "thermalExpansionAlpha: 1/K");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].thermalExpansionAlpha, "");
      }
    }
    if (typeFlag[i++]) { /* isothermalBulkModulus */
      printf(fmt_t,"r:  g/cc", "T:  K", "isothermalBulkModulus: GPa");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].isothermalBulkModulus, "");
      }
    }
    if (typeFlag[i++]) { /* soundSpeed */
      printf(fmt_t,"r:  g/cc", "T:  K", "c  = (dP/dr @ const. S)^0.5: m/s");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].soundSpeed, "");
      }
    }
    if (typeFlag[i++]) { /* isothermal soundSpeed */
      printf(fmt_t,"r:  g/cc", "T:  K", "c = ((-dS/dr)/(dS/dP))^0.5: m/s");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].soundSpeed_alt, "");
      }
    }
    if (typeFlag[i++]) { /* isothermal soundSpeed */
      printf(fmt_t,"r:  g/cc", "T:  K", "cT: m/s");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, pow(data[j].isothermalBulkModulus/data[j].r,0.5), "");
      }
    }
    if (typeFlag[i++]) { /* Ks */
      printf(fmt_t,"r:  g/cc", "T:  K", "Ks: 1/GPa");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].Ks, "");
      }
    }
    if (typeFlag[i++]) { /* KT */
      printf(fmt_t,"r:  g/cc", "T:  K", "KT: 1/GPa");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].KT, "");
      }
    }
    if (typeFlag[i++]) { /* gruneisenCoefficient */
      printf(fmt_t,"r:  g/cc", "T:  K", "gruneisenCoefficient");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].gruneisenCoefficient, "");
      }
    }
    if (typeFlag[i++]) { /* Cv */
      printf(fmt_t,"r:  g/cc", "T:  K", "Cv: MJ/kg/K");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].Cv, "");
      }
    }
    if (typeFlag[i++]) { /* Cp */
      printf(fmt_t,"r:  g/cc", "T:  K", "Cp: MJ/kg/K");
      for (j=0; j<N; j++) {
	printf(fmt, data[j].r, data[j].T, data[j].Cp, "");
      }
    }
  }

  return(EOS_OK);
}

void display_usage(char *argv[]) {
  fprintf(stderr,
	  "\nUSAGE: %s [<GENERAL OPTIONS>] <sesMaterialNum> <sesTableNum> [ <sesSubtableIndex> ]\n"
	  "\n       %s [<GENERAL OPTIONS>] id [ <file> ]\n"
	  "\n       %s [<GENERAL OPTIONS>] tables <sesMaterialNum> [, <sesMaterialNum> [, ... ]]\n"
	  "\n       %s [<GENERAL OPTIONS>] <CUSTOM OPTIONS> <sesMaterialNum>\n"
	  "\n"
	  "       <sesMaterialNum>  \t- Sesame material ID number\n"
	  "       <sesTableNum>     \t- Sesame table number\n"
	  "       <sesSubtableIndex>\t- Optional Sesame subtable number (default=1)\n"
	  "       <file>            \t- Optional Sesame file name\n\n"
	  "See %s.readme or use -h option for more details and a complete description of the <GENERAL OPTIONS> and <CUSTOM OPTIONS>.\n\n",
	  argv[0],argv[0],argv[0],argv[0],argv[0]);
}

// ****************************************************************
// MAIN
// ****************************************************************
int main(int argc, char *argv[]) {

  // main arguments
  EOS_INTEGER sesMaterialNum;
  EOS_INTEGER sesTableNum;
  EOS_INTEGER sesSubtableIndex;

  // Local Variables
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER NP = 1, infoItem[1], errorCode=EOS_OK;
  EOS_REAL val;
  EOS_INTEGER err=EOS_OK;
  EOS_INTEGER nTables=one;
  EOS_INTEGER tableType;
  EOS_INTEGER tableHandle=0;
  EOS_INTEGER *matidlist, *tableList, *tableSize;
  EOS_INTEGER nmats, ntabs;
  EOS_INTEGER cDate, cDate_m, cDate_d, cDate_y, mDate, mDate_m, mDate_d, mDate_y;
  char    *p = NULL;
  char    *generalOpts = "F:fhI:J:n:VvXY";
  char    *indepVarOpts = "r:T:P:U:A:S:";
  char    *optionStr = NULL;
  int i, indepVarOptsCnt, minArgs = 2;
  EOS_CHAR *sesFileName = NULL;
  EOS_INTEGER indexFileCount;
  EOS_CHAR *depVarStr, *indepVar1Str, *indepVar2Str, str[50];

  /* parse command line options */
  optionStr = (char*) malloc((strlen(generalOpts) + strlen(indepVarOpts) + 1)*sizeof(char));
  optionStr = strcpy(optionStr, generalOpts);
  optionStr = strcat(optionStr, indepVarOpts);
  err = getoptions(argc-1, argv+1, optionStr);
  EOS_FREE(optionStr); /* we are done with this temporary memory here */
  if (err) return(err);

  /* display usage */
  if (argc < 2) {
    display_usage(argv);
    return OK;
  }

  /* display help */
  if (option['h'].flag) {
    printf("%s\n\n", get_header_str("get_sesame_data", "$Revision: 1.42 $"));
    printf("%s", help_str);
    return OK;
  }

  /* display version */
  if (option['V'].flag) {
    printf("%s\n", get_version_str("get_sesame_data", "$Revision: 1.42 $"));
    return OK;
  }

  /* count independent variable options */
  indepVarOptsCnt = countIndependentVariableOptions(indepVarOpts);

  if (indepVarOptsCnt && indepVarOptsCnt != 2) { /* only allow two independent variable options */
    err = IncorrectNumberOfIndepVars;
      return(fatal(err, argv[0], argsc, args, localErrorMessages[err]));
  }

  if (indepVarOptsCnt == 2) { /* reset minimum required arg-uments */
    minArgs = 1;
    if (! option['r'].flag) {
      err = rIndepVarRequired;
      return(fatal(err, argv[0], argsc, args, localErrorMessages[err]));
    }
  }

  if (! strcmp(args[0],"id"))
    minArgs = 1;

  // check main arguments

  if (argsc < minArgs) {
    display_usage(argv);
    err = InsufficientArguments;
    return(fatal(err, argv[0], argsc, args, localErrorMessages[err]));
  }

  if (! strcmp(args[0],"id")) { /* Get all material id's */

    EOS_BOOLEAN fileExists;
    struct stat file_statbuf;

    if (option['F'].flag) {
      fileExists = (!(stat (option['F'].arg, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
      if (fileExists) {
	/* push option['F'].arg onto args[] */
	args = realloc(args,(++argsc)*sizeof(char*));
	args[argsc-1] = (char*) malloc((strlen(option['F'].arg)+1)*sizeof(char));
	strcpy(args[argsc-1],option['F'].arg);
      }
    }

    if (argsc == minArgs) { /* Get all material id's from all known files */

      EOS_BOOLEAN equal;

      err = get_matIdList(&matidlist);

      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_READ_TOTAL_MATERIALS_FAILED, &err, &equal);

      if (err < 0) {
	return((int)err);
      }
      else if (err >= EOS_MIN_ERROR_CODE_VALUE && ! equal) {
	eos_GetErrorMessage (&err, errorMessage);
	return(fatal(err, "get_matIdList", argsc, args, errorMessage));
      }

      for (nmats=0;1;nmats++) /* count material id's */
	if (matidlist[nmats] < 0) break;

    }
    else { /* Get all material id's from the specified file */

      fileExists = (!(stat (args[1], &file_statbuf)))?EOS_TRUE:EOS_FALSE;
      if (! fileExists) {
	err = -3;
      }
      else {
	err = get_matIdListFromFile(&matidlist, args[1]);
      }

      if (err < 0) {
	if (err == -1) err = InvalidsesFileName;
	if (err == -3) err = MissingsesFile;
	return(fatal(err, argv[0], argsc, args, localErrorMessages[err]));
      }
      else if (err >= EOS_MIN_ERROR_CODE_VALUE) {
	eos_GetErrorMessage (&err, errorMessage);
	return(fatal(err, "get_matIdListFromFile", argsc, args, errorMessage));
      }

      for (nmats=0;1;nmats++)
	if (matidlist[nmats] < 0) break;

    }

    /* print all material id's to stdout */
    fprintf(stdout, "%i materials found\n", nmats);
    for (i=0;i<nmats;i++) {
      fprintf(stdout, "%i",matidlist[i]);
      if (i < nmats-1) fprintf(stdout, " ");
    }
    fprintf(stdout, "\n");

    return((int)err);
  }
  else if (argsc < minArgs) {
    display_usage(argv);
    return(InsufficientArguments);
  }

  if (! strcmp(args[0],"tables") && argsc > 1) { /* Get all table numbers and table sizes for given matid(s) */

    int j, rerror = 0;

    indexFileCount = 0;
    if (option['F'].flag) {
      indexFileCount = createIndexFile(option['F'].arg);
    }

    for (j=1;j<argsc;j++) {
      sesMaterialNum   = (EOS_INTEGER)strtol(args[j], &p, 10);

      /* get the table list and associated sizes */
      err = get_matidTableInfo (sesMaterialNum, &tableList, &tableSize);
      if (err < 0) {
	if (err == -3) err = MissingsesFile;
	if (err == -1) err = InvalidsesFileName;
	fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %s %s)\n\n",
		argv[0], err,  localErrorMessages[err], args[0], args[1]);
	rerror = (int)err;
	break;
      }
      else if (err >= EOS_MIN_ERROR_CODE_VALUE) {
	eos_GetErrorMessage (&err, errorMessage);
	fprintf (stderr, "get_matIdListFromFile ERROR %d: %s\n\t(args: %s %s)\n\n",
		 err,  errorMessage, args[0], args[1]);
	rerror = (int)err;
	break;
      }

      for (ntabs=0;1;ntabs++)
	if (tableList[ntabs] < 0) break;

      /* print the table list and associated sizes to stdout */
      fprintf(stdout, "%i tables found for material %d (table:size)\n", ntabs, sesMaterialNum);
      for (i=0;i<ntabs;i++) {
	fprintf(stdout, "%i:%i",tableList[i],tableSize[i]);
	if (i < ntabs-1) fprintf(stdout, " ");
      }
      fprintf(stdout, "\n");
    }

    if (option['F'].flag) {
      cleanIndexFile(indexFileCount);
    }

    if (option['F'].flag) {
      cleanIndexFile(indexFileCount);
    }

    return(rerror);
  }

  if (! strcmp(args[0],"comments") && argsc > 1) { /* Get all comments for given matid(s) */

    int j, rerror = 0;

    fprintf (stdout, "\n");

    indexFileCount = 0;
    if (option['F'].flag) {
      indexFileCount = createIndexFile(option['F'].arg);
    }

    for (j=1;j<argsc;j++) {

      sesMaterialNum   = (EOS_INTEGER)strtol(args[j], &p, 10);
      sesTableNum   = (EOS_INTEGER)101;
      sesSubtableIndex = 1;
      err = get_DataType(sesTableNum, sesSubtableIndex, &tableType);
      if (err == -1)
	err = InvalidsesTableNum;
      if (err == -2)
	err = InvalidsesSubtableIndex;
      if (err) {
	fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
		localErrorMessages[err],
		sesMaterialNum, sesTableNum, sesSubtableIndex);
	rerror = (int)err;
	break;
      }

      // Initialize the table handle
      eos_CreateTables (&nTables, &tableType, &sesMaterialNum, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	fprintf (stdout, "Material Number %d Comments:\n%s\n", sesMaterialNum, errorMessage);
	continue;
      }

      /* Load data */
      eos_LoadTables (&nTables, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	fprintf (stdout, "Material Number %d Comments:\n%s\n", sesMaterialNum, errorMessage);
	continue;
      }

      /* write the comments for current matid */
      fprintf(stdout,"Material Number %d ", sesMaterialNum);
      err = dumpData(tableHandle, sesTableNum, sesSubtableIndex);
      fprintf (stdout, "\n");

      if (err < 0) {
	fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
		localErrorMessages[err],
		sesMaterialNum, sesTableNum, sesSubtableIndex);
	rerror = (int)err;
	break;
      }

      /* Destroy data */
      eos_DestroyAll (&err);
      rerror = (int)err;
    }

    if (option['F'].flag) {
      cleanIndexFile(indexFileCount);
    }

    return(rerror);
  }

  if(argsc < minArgs) { /* assume numeric arguments */
    display_usage(args);
    return(InsufficientArguments);
  }
  else {
    sesMaterialNum   = (EOS_INTEGER)strtol(args[0], &p, 10);
    sesTableNum = 101; /* default value */
    if (argsc > 1) {
      sesTableNum      = (EOS_INTEGER)strtol(args[1], &p, 10);
      sesTableNum      = (sesTableNum < 200 && sesTableNum > 99) ? 101 : sesTableNum;
    }
    if (argsc > 2) {
      sesSubtableIndex = (EOS_INTEGER)strtol(args[2], &p, 10);
    } else {
      sesSubtableIndex = (EOS_INTEGER)1;
    }
  }

  // verify that sesTableNum and sesSubtableIndex are valid, and the fetch data type
  if (sesTableNum == 321) {
    if (sesSubtableIndex < 0 )
      err = InvalidsesSubtableIndex;
    else
      err = get_DataType(sesTableNum, 1, &tableType);
  }
  else if (sesTableNum == 311) {
    err = get_DataType(301, sesSubtableIndex, &tableType);
  }
  else {
    err = get_DataType(sesTableNum, sesSubtableIndex, &tableType);
  }
  if (err == -1)
    err = InvalidsesTableNum;
  if (err == -2)
    err = InvalidsesSubtableIndex;
  if (err) {
    fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
            localErrorMessages[err],
            sesMaterialNum, sesTableNum, sesSubtableIndex);
    return((int)err);
  }

  indexFileCount = 0;
  if (option['F'].flag) {
    indexFileCount = createIndexFile(option['F'].arg);
  }

  // Initialize the table handle
  eos_CreateTables (&nTables, &tableType, &sesMaterialNum, &tableHandle, &err);
  if (err != EOS_OK) {
    eos_GetErrorMessage (&err, errorMessage);
    fprintf (stderr, "eos_CreateTables ERROR %i: %s\n", err, errorMessage);

    if (option['F'].flag) {
      cleanIndexFile(indexFileCount);
    }
    return((int)err);
  }

  if (option['f'].flag) {
    eos_SetOption (&tableHandle, &EOS_CREATE_TZERO, EOS_NullPtr, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      fprintf (stderr, "eos_SetOption ERROR %i: %s\n", err, errorMessage);

      if (option['F'].flag) {
	cleanIndexFile(indexFileCount);
      }
      return((int)err);
    }
  }

  if (option['X'].flag) {
    eos_SetOption (&tableHandle, &EOS_MONOTONIC_IN_X, EOS_NullPtr, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      fprintf (stderr, "eos_SetOption ERROR %i: %s\n", err, errorMessage);

      if (option['F'].flag) {
	cleanIndexFile(indexFileCount);
      }
      return((int)err);
    }
  }

  if (option['Y'].flag) {
    eos_SetOption (&tableHandle, &EOS_MONOTONIC_IN_Y, EOS_NullPtr, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      fprintf (stderr, "eos_SetOption ERROR %i: %s\n", err, errorMessage);

      if (option['F'].flag) {
	cleanIndexFile(indexFileCount);
      }
      return((int)err);
    }
  }

  if (sesTableNum == 311) {
    eos_SetOption (&tableHandle, &EOS_USE_MAXWELL_TABLE, EOS_NullPtr, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      fprintf (stderr, "eos_SetOption ERROR %i: %s\n", err, errorMessage);

      if (option['F'].flag) {
	cleanIndexFile(indexFileCount);
      }
      return((int)err);
    }
  }

  /* Load data */
  eos_LoadTables (&nTables, &tableHandle, &err);
  if (err != EOS_OK) {
    EOS_BOOLEAN equal;
    eos_GetErrorMessage (&err, errorMessage);
    fprintf (stderr, "eos_LoadTables ERROR %i: %s\n", err, errorMessage);

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_NO_DATA_TABLE, &err, &equal);
    if (equal)
      fprintf (stderr, "\nIf you wish to force the manufacture of %s, then use the -f option.\n\n",
	       get_tableType_str(tableType));

    if (option['F'].flag) {
      cleanIndexFile(indexFileCount);
    }
    return((int)err);
  }

  if (option['F'].flag) {
    cleanIndexFile(indexFileCount);
  }

  if (sesTableNum == 321) {
    infoItem[0] = EOS_NUM_PHASES;
    eos_GetTableInfo (&tableHandle, &one, infoItem, &val, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf (stderr, "%d: %s\n", errorCode, errorMessage);
      return eospacError;
    }
    NP = (EOS_INTEGER) val;

    if (NP < sesSubtableIndex || sesSubtableIndex < 0 )
      err = InvalidsesSubtableIndex;

    if (err) {
      fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
	      localErrorMessages[err],
	      sesMaterialNum, sesTableNum, sesSubtableIndex);
      return((int)err);
    }
  }

  /* get the filename */
  err = get_matidFileName(sesMaterialNum, &sesFileName);
  if (err < 0) {
    fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
            localErrorMessages[err],
            sesMaterialNum, sesTableNum, sesSubtableIndex);
    return((int)err);
  }
  else if (err >= EOS_MIN_ERROR_CODE_VALUE) {
    eos_GetErrorMessage (&err, errorMessage);
    fprintf (stderr, "dumpData ERROR %i: %s\n\t(args: %i %i %i)\n",
             err, errorMessage, sesMaterialNum, sesTableNum, sesSubtableIndex);
    return((int)err);
  }

  // print Sesame File information to stderr

  cDate = get_creationDate(tableHandle);
  cDate_m = cDate/10000;
  cDate_d = (cDate-cDate_m*10000)/100;
  cDate_y = cDate-cDate_m*10000-cDate_d*100;
  if (cDate_m > 13) { /* assume YYMMDD notation instead of expected MMDDYY notation */
    EOS_INTEGER tmp = cDate_y;
    cDate_y = cDate_m;
    cDate_m = cDate_d;
    cDate_d = tmp;
  }
  if (cDate_y < 70) cDate_y += 2000; /* change 2-digit year to 4-digit year */
  else              cDate_y += 1900;

  mDate = get_modificationDate(tableHandle);
  mDate_m = mDate/10000;
  mDate_d = (mDate-mDate_m*10000)/100;
  mDate_y = mDate-mDate_m*10000-mDate_d*100;
  if (mDate_m > 13) { /* assume YYMMDD notation instead of expected MMDDYY notation */
    EOS_INTEGER tmp = mDate_y;
    mDate_y = mDate_m;
    mDate_m = mDate_d;
    mDate_d = tmp;
  }
  if (mDate_y < 70) mDate_y += 2000; /* change 2-digit year to 4-digit year */
  else              mDate_y += 1900;
  

  fprintf(stderr, "\n");
  fprintf(stderr, "Material Number:       %d\n", sesMaterialNum);
  fprintf(stderr, "Table Number:          %d\n", sesTableNum);
  fprintf(stderr, "Data file name:        %s\n", sesFileName);
  fprintf(stderr, "Creation date:         %d/%d/%d\n", cDate_m, cDate_d, cDate_y);
  fprintf(stderr, "Latest update date:    %d/%d/%d\n", mDate_m, mDate_d, mDate_y);

  depVarStr    = get_VarStr(get_dataTypeDepVar(tableType), 1);
  indepVar1Str = get_VarStr(get_dataTypeIndepVar1(tableType), 1);
  indepVar2Str = get_VarStr(get_dataTypeIndepVar2(tableType), 1);
  
  if (sesTableNum == 321) sprintf(str, " Phase %d ", sesSubtableIndex);
  else                    sprintf(str, " ");
  printf ("# %s%s :: %s\n", str, get_tableType_description(tableType), get_tableHandleFileName(tableHandle));
  printf ("#%12s%24s%24s\n", indepVar1Str, depVarStr, indepVar2Str);

  // write requested data to stdout
  if (indepVarOptsCnt == 2) { /* only allow two independent variable options */

    EOS_REAL *r, *y, *y_in, r_lower, y_lower, r_upper, y_upper;
    EOS_BOOLEAN r_rangeExists = EOS_FALSE, y_rangeExists = EOS_FALSE;
    char y_flag = '\0';
    
    customValues_t *data;

    err = EOS_OK;

    /* Set independent (x,y) values and identify the EOSPAC 6 data type to use.
     * This assumes that two independent variable command line options are set.
     */
    if (option['r'].flag) {
      r_lower = atof(option['r'].arg);
      /* Is a range specified? */
      r_rangeExists = get_UpperRangeValue(option['r'].arg, &r_upper);
      if (! r_rangeExists) r_upper = r_lower;
    }
    else err = InvalidIndepVar;

    if (option['T'].flag) y_flag = 'T';
    if (option['P'].flag) y_flag = 'P';
    if (option['U'].flag) y_flag = 'U';
    if (option['A'].flag) y_flag = 'A';
    if (option['S'].flag) y_flag = 'S';

    if (option[(int)y_flag].flag) {
      y_lower = atof(option[(int)y_flag].arg);
      /* Is a range specified? */
      y_rangeExists = get_UpperRangeValue(option[(int)y_flag].arg, &y_upper);
      if (! y_rangeExists) y_upper = y_lower;
    }
    else err = InvalidIndepVar;

    if (! err) {
      int N=1, NR=1, Ny=1, j;
      if (r_rangeExists) {
	NR = 50;
	if (option['I'].flag) NR = atoi(option['I'].arg);
	if (option['n'].flag) NR = atoi(option['n'].arg);
      }
      if (y_rangeExists) {
	Ny = 50;
	if (option['J'].flag) Ny = atoi(option['J'].arg);
	if (option['n'].flag) Ny = atoi(option['n'].arg);
      }
      N = NR * Ny;
      r = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
      y_in = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
      getSamples(NR, r_lower, r_upper, r);
      getSamples(Ny, y_lower, y_upper, y_in);

      if (y_flag == 'T') {
	y = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
	for (i=0; i<Ny; i++)
	  y[i] = y_in[i];
      }
      else if (y_flag == 'P') {
	y = calculateCategory2(sesMaterialNum, EOS_T_DPt, Ny, r, y_in);
      }
      else if (y_flag == 'U') {
	y = calculateCategory2(sesMaterialNum, EOS_T_DUt, Ny, r, y_in);
      }
      else if (y_flag == 'A') {
	y = calculateCategory2(sesMaterialNum, EOS_T_DAt, Ny, r, y_in);
      }
      else if (y_flag == 'S') {
	y = calculateCategory2(sesMaterialNum, EOS_T_DSt, Ny, r, y_in);
      }
      else  /* invalid y_flag value */
	return(fatal(err, argv[0], argsc, args, localErrorMessages[InvalidIndepVar]));

      if (!y)
	return(fatal(err, argv[0], argsc, args, localErrorMessages[inverseInterpolationError]));

      y = (EOS_REAL*) realloc(y, N * sizeof(EOS_REAL));

      for (j=Ny-1; j>=0; j--) { /* spread samples in arrays */
	for (i=0; i<NR; i++) {
	  r[i+j*NR] = r[i];
	  y[i+j*NR] = y[j];
	}
      }
      data = (customValues_t*) malloc(N * sizeof(customValues_t));
      err = calculateCustomQuantities(sesMaterialNum, N, r, y, y_flag, (N==1)?EOS_TRUE:EOS_FALSE, data);
      if (N!=1) /* print tabular data */
	printTabularCustomData(N, data, "gruneisenCoefficient,all");
      EOS_FREE(data);
      EOS_FREE(r);
      EOS_FREE(y);
    }
  }
  else
    err = dumpData(tableHandle, sesTableNum, sesSubtableIndex);

  if (err < 0) {
    fprintf(stderr,"\n%s ERROR %d: %s\n\t(args: %i %i %i)\n\n", argv[0], err, 
            localErrorMessages[err],
            sesMaterialNum, sesTableNum, sesSubtableIndex);
    return((int)err);
  }
  else if (err >= EOS_MIN_ERROR_CODE_VALUE) {
    EOS_BOOLEAN equal;
    eos_GetErrorMessage (&err, errorMessage);
    fprintf (stderr, "dumpData ERROR %i: %s\n\t(args: %i %i %i)\n",
             err, errorMessage, sesMaterialNum, sesTableNum, sesSubtableIndex);

    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_NO_DATA_TABLE, &err, &equal);
    if (equal)
      fprintf (stderr, "\nIf you wish to force the manufacture of the required table(s), then use the -f option.\n\n");

    return((int)err);
  }

  // linefeedto stderr
  fprintf(stderr, "\n");

  // return EOS_OK code
  return((int)EOS_OK);
}
