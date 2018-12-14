/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup tests
 *  \brief Using linear ideal gas data, verify results of all 5 interpolation
 *  categories for the EOS_LINEAR interpolation option.
 *
 * \note
 * MATIDS TO TEST: 9981
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

/*
 * Calculate the pressure of a mixture of one or more ideal gases
 *
 * INPUT:
 * EOS_INTEGER   N          Total ideal gases to use here
 * EOS_REAL      rho        mixture density (g/cc)
 * EOS_REAL      y          mixture temperature (K) or internal energy (MJ/kg)
 * EOS_CHAR      yFlag      'T' indicates y=temperature (K)
 *                          'U' indicates y=internal energy (MJ/kg)
 * EOS_REAL      Pc(3)      cold curve pressure (GPa) and derivative
 * EOS_REAL      Uc(3)      cold curve internal energy (MJ/kg) and derivative
 * EOS_REAL      A(N)       ideal gas atomic weight (g/mol)
 * EOS_REAL      gamma(N)   ideal gas specific heat ratio
 * EOS_REAL      R          Universal Gas Constant (J/mol-K)
 * EOS_REAL      C(N)       Number fraction concentrations of ideal gas
 *
 * OUTPUT:
 * EOS_REAL      P(3)       Pressure (GPa) and derivatives
 * EOS_REAL      Abar       Averaged atomic weight
 * EOS_REAL      gamma_bar  Averaged gamma
 */
void idealP(EOS_INTEGER N, EOS_REAL rho, EOS_REAL y, EOS_CHAR yFlag,
	    EOS_REAL R, EOS_REAL *Pc, EOS_REAL *Uc, EOS_REAL *A, EOS_REAL *C,
	    EOS_REAL *gamma, EOS_REAL *P, EOS_REAL *gamma_bar, EOS_REAL *Abar) {

  EOS_INTEGER i;
  *Abar = 0.0;
  *gamma_bar = 0.0;
  for(i=0; i<N; i++) {
    *Abar = *Abar + C[i] * A[i];
    *gamma_bar = *gamma_bar + C[i] / (gamma[i] - 1.0);
  }
  if (yFlag == 'T') {
    P[0] = rho * R * y / *Abar / 1000.0;   /* GPa */
    P[1] = R * y / *Abar / 1000.0 + Pc[1]; /* dP/d(rho) */
    P[2] = rho * R / *Abar / 1000.0;       /* dP/dT */
    P[0] = P[0] + Pc[0];
  }
  else if (yFlag == 'U') {
    P[0] = rho * y / *gamma_bar;                              /* GPa */
    P[1] = y / *gamma_bar + Pc[1] - rho / *gamma_bar * Uc[1]; /* dP/d(rho) */
    P[2] = rho / *gamma_bar;                                  /* dP/dU */
    P[0] = P[0] + Pc[0];
  }
  else {
    printf("idealP ERROR: invalid yFlag = %c\n", yFlag);
    exit(-1);
  }
}
/*
 * Calculate the internal energy of a mixture of one or more ideal gases
 *
 * INPUT:
 * EOS_INTEGER   N          Total ideal gases to use here
 * EOS_REAL      rho        mixture density (g/cc)
 * EOS_REAL      y          mixture temperature (K) or internal energy (MJ/kg)
 * EOS_CHAR      yFlag      'T' indicates y=temperature (K)
 *                          'U' indicates y=internal energy (MJ/kg)
 * EOS_REAL      Pc(3)      cold curve pressure (GPa) and derivative
 * EOS_REAL      Uc(3)      cold curve internal energy (MJ/kg) and derivative
 * EOS_REAL      A(N)       ideal gas atomic weight (g/mol)
 * EOS_REAL      gamma(N)   ideal gas specific heat ratio
 * EOS_REAL      R          Universal Gas Constant (J/mol-K)
 * EOS_REAL      C(N)       Number fraction concentrations of ideal gas
 *
 * OUTPUT:
 * EOS_REAL      U(3)       Internal energy (MJ/kg) and derivatives
 * EOS_REAL      Abar       Averaged atomic weight
 * EOS_REAL      gamma_bar  Averaged gamma
 */
void idealU(EOS_INTEGER N, EOS_REAL rho, EOS_REAL y, EOS_CHAR yFlag,
	    EOS_REAL R, EOS_REAL *Pc, EOS_REAL *Uc, EOS_REAL *A, EOS_REAL *C,
	    EOS_REAL *gamma, EOS_REAL *U, EOS_REAL *gamma_bar, EOS_REAL *Abar) {

  EOS_INTEGER i;
  *Abar = 0.0;
  *gamma_bar = 0.0;
  for(i=0; i<N; i++) {
    *Abar = *Abar + C[i] * A[i];
    *gamma_bar = *gamma_bar + C[i] / (gamma[i] - 1.0);
  }
  if (yFlag == 'T') {
    U[0] = R * y / *Abar * *gamma_bar / 1000.0; /* MJ/kg */
    U[1] = Uc[1];                               /* dU/d(rho) */
    U[2] = R / *Abar * *gamma_bar / 1000.0;     /* dU/dT */
    U[0] = U[0] + Uc[0];
  }
  else if (yFlag == 'P') {
    U[0] = *gamma_bar * y / rho;                         /* MJ/kg */
    U[1] = Uc[1] - *gamma_bar / rho * ( y/rho + Pc[1] ); /* dU/d(rho) */
    U[2] = *gamma_bar / rho;                             /* dU/dT */
    U[0] = U[0] + Uc[0];
  }
  else {
    printf("idealU ERROR: invalid yFlag = %c\n", yFlag);
    exit(-1);
  }
}

int main ()
{
  enum
  { nTablesE = 4 };
  enum
  { nXYPairsE = 10 };
  enum
  { nInfoItemsE = 7 };

  EOS_INTEGER i, j;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], X0[nXYPairsE], Y0[nXYPairsE],
    F[nXYPairsE], F0[nXYPairsE], dFx[nXYPairsE], dFy[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE], _type_;
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs, xyBounds[nXYPairsE];
  EOS_REAL infoVals[nInfoItemsE];
  EOS_INTEGER nInfoItems;
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_Rmin,
    EOS_Rmax,
    EOS_Tmin,
    EOS_Tmax,
    EOS_Fmin,
    EOS_Fmax,
    EOS_Normal_Density
  };
  EOS_REAL Rmin, Rmax, Tmin, Tmax;
  EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_Pic_DT",
    "EOS_T_DPic",
    "EOS_D_PtT",
    "EOS_At_DPt"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_REAL atomicWeight, gamma, gamma_bar, Abar;
  EOS_REAL Fideal[nXYPairsE], *dFx_ideal=NULL, *dFy_ideal=NULL, _F_[3];
  EOS_INTEGER _infoItemTH_, _infoItem_;

  EOS_REAL zero = 0.0, one = 1.0;
  EOS_INTEGER one_i = 1;
  EOS_REAL R = 8.31440000; /* Universal Gas Constant (J/mol-K) */

  EOS_REAL e1 = 1.0e-10; /* relative difference tolerance */
  EOS_REAL e2 = 1.0e-20; /* absolute difference tolerance */

  nTables = nTablesE;
  nXYPairs = nXYPairsE;
  nInfoItems = nInfoItemsE;

  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_T_DPt;
  tableType[2] = EOS_D_PtT;
  tableType[3] = EOS_At_DPt;

  gamma = 1.4; /* specific to matID 9981 */

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    matID[i] = 9981;
    tableHandle[i] = 0;
    numIndVars[i] = 2;
  }

  /*
   * initialize table data objects
   */

  eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s\n", tableHandleErrorCode,
              errorMessage);
    }
  }
  _type_ = (EOS_INTEGER) EOS_Info;
  eos_CreateTables (&one_i, &_type_, &(matID[0]), &_infoItemTH_, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
  }

  /*
   * set some options
   */

  eos_SetOption (&_infoItemTH_, &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 0; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      return 0;
    }
    eos_SetOption (&tableHandle[i], &EOS_LINEAR, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      return 0;
    }
  }

  /*
   * load data into table data objects
   */
  eos_LoadTables (&one_i, &_infoItemTH_, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
  }
  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i (TH=%i): %s\n", tableHandleErrorCode,
              tableHandle[i], errorMessage);
    }
  }

  /*
   * fetch Rmin, Rmax, Tmin, and Tmax
   */
  eos_GetTableInfo (&tableHandle[0], &nInfoItems, infoItems, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  Rmin = infoVals[0];
  Rmin = infoVals[6]; // alternatively use normal density
  Rmin = 200.;        // alternatively use 200 g/cc
  Rmax = infoVals[1];
  Tmin = infoVals[2];
  Tmin = 100.;        // alternatively use 100 K
  Tmax = infoVals[3];

  /*
   * fetch atomicWeight
   */
  _infoItem_ = (EOS_INTEGER) EOS_Mean_Atomic_Mass;
  eos_GetTableInfo (&_infoItemTH_, &one_i, &_infoItem_, infoVals, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%i: eos_GetTableInfo ERROR %i for %s: %s\n",
	    tableHandle[i], errorCode, get_tableType_str(tableType[i]), errorMessage);
    return errorCode;
  }
  atomicWeight = infoVals[0];

  /*
   * interpolate
   */
  for (i = 0; i < nTables; i++) {

    for (j = 0; j < nXYPairs; j++)
      xyBounds[j] = EOS_OK;

    if (tableType[i] == EOS_Pt_DT) {
      generate_RandomPoints(X, Y, nXYPairs, Rmin, Rmax, Tmin, Tmax, EOS_FALSE);

      dFx_ideal = (EOS_REAL*) malloc(nXYPairs * sizeof(EOS_REAL));
      dFy_ideal = (EOS_REAL*) malloc(nXYPairs * sizeof(EOS_REAL));

      for (j = 0; j < nXYPairs; j++) {
	EOS_REAL Pc[3] = {zero, zero, zero};
	EOS_REAL Uc[3] = {zero, zero, zero};
	X0[j] = X[j];
	Y0[j] = Y[j];
	
	idealP(1, X[j], Y[j], 'T', R, Pc, Uc, &atomicWeight, &one,
	       &gamma, _F_, &gamma_bar, &Abar);
	Fideal[j] = _F_[0];
	dFx_ideal[j] = _F_[1];
	dFy_ideal[j] = _F_[2];
      }
    }
    else if (tableType[i] == EOS_T_DPt) {
      for (j = 0; j < nXYPairs; j++) {
	X[j] = X0[j];
	Y[j] = F0[j] = F[j];

	Fideal[j] = Y0[j];
      }
      EOS_FREE(dFx_ideal);
      EOS_FREE(dFy_ideal);
    }
    else if (tableType[i] == EOS_D_PtT) {
      for (j = 0; j < nXYPairs; j++) {
	X[j] = F0[j];
	Y[j] = Y0[j];

	Fideal[j] = X0[j];

	e1 = 1.0e-7; /* relative difference tolerance */
	e2 = 1.0e-20; /* absolute difference tolerance */
      }
      EOS_FREE(dFx_ideal);
      EOS_FREE(dFy_ideal);
    }
    else if (tableType[i] == EOS_At_DPt)
      continue;
    else {
      printf ("WARNING -- unknown tableType: %i\n", tableType[i]);
      continue;
    }

    printf ("\n--- Interpolate using tableType %s for material %d ---\n", tableTypeLabel[i], matID[i]);
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                     &errorCode);
    printf ("%s Interpolation Results:\n", tableTypeLabel[i]);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("# eos_Interpolate ERROR %i (TH=%i): %s\n", errorCode,
              tableHandle[i], errorMessage);
      if (errorCode != EOS_INTERP_EXTRAPOLATED)
	continue;
      eos_CheckExtrap (&tableHandle[i], &nXYPairs, X, Y, xyBounds, &errorCode);
    }
    if (numIndVars[i] == 1)
      printf ("%16s %22s %22s %22s\n", "i","X","F","dFx");
    if (numIndVars[i] == 2) {
      printf ("%16s %22s %22s %22s %22s %22s %22s", "i","X","Y","F","dFx","dFy","Fideal");
      if (dFx_ideal) printf (" %22s", "dFx_ideal");
      if (dFy_ideal) printf (" %22s", "dFy_ideal");
      printf ("\n");
    }
    for (j = 0; j < nXYPairs; j++) {

      if (fcmp(Fideal[j], F[j], e1, e2) ||
	  (dFx_ideal && fcmp(dFx_ideal[j], dFx[j], e1, e2)) ||
	  (dFy_ideal && fcmp(dFy_ideal[j], dFy[j], e1, e2))) {
	printf ("%s", ((tableType[i] == EOS_D_PtT) ? "#" : " "));
	printf(" FAILED ");
      }
      else
	printf("# PASSED ");

      printf ("%7i %22.15e", j, X[j]);
      if (numIndVars[i] == 2) printf (" %22.15e", Y[j]);
      printf (" %22.15e %22.15e", F[j], dFx[j]);
      if (numIndVars[i] == 2) {
	printf (" %22.15e %22.15e", dFy[j], Fideal[j]);
	if (dFx_ideal) printf (" %22.15e", dFx_ideal[j]);
	if (dFy_ideal) printf (" %22.15e", dFy_ideal[j]);
      }
      printf (" %s\n", ((xyBounds[j]!=EOS_OK) ? ERROR_TO_TEXT(xyBounds[j]) : ""));
    }
  }

  /*
   * Destroy all data objects
   */

  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", tableHandleErrorCode,
              errorMessage);
    }
  }

  return 0;

}
