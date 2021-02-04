/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup C tests
 *  \brief Verify that eos_Mix stores the expected data associated with the
 *         EOS_SAVE_SPECIES_DATA option (see issue #artf10761, https://tf.lanl.gov/sf/go/artf10761).
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_GetErrorMessage
 * eos_Mix
 *
 * \note
 * MATIDS TO TEST: 2030 2140 3336 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

int main ()
{
  int i, j, k, optionFlag;
  enum
  { nTablesE = 4 };
  enum
  { nTableTypesE = 6 };
  enum
  { nXYPairsE = 4 };

  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE], dFy[nXYPairsE];
  EOS_REAL xSpecies[nTablesE][nXYPairsE];
  EOS_REAL ySpecies[nTablesE][nXYPairsE];
  EOS_REAL FSpecies[nTablesE][nXYPairsE];
  EOS_REAL dFxSpecies[nTablesE][nXYPairsE];
  EOS_REAL dFySpecies[nTablesE][nXYPairsE];
  EOS_REAL atomicMass[nTablesE];
  EOS_REAL C[nTablesE*nXYPairsE];
  EOS_INTEGER xyBounds[nXYPairsE];

  EOS_INTEGER tableType[nTablesE], _infoItem_;
  EOS_INTEGER tableTypeList[nTableTypesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, nXYPairs, nTableTypes, one = 1;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;
  nTableTypes = nTableTypesE;

  tableTypeList[0] = EOS_Pt_DT;
  tableTypeList[1] = EOS_Pt_DUt;
  tableTypeList[2] = EOS_Ut_DT;
  tableTypeList[3] = EOS_Ut_DPt;
  tableTypeList[4] = EOS_T_DPt;
  tableTypeList[5] = EOS_T_DUt;

  matID[0] = 2140;
  matID[1] = 2030;
  matID[2] = 3720;
  matID[3] = 3336;

  for (j = 0; j < nTableTypes; j++) {

    errorCode = EOS_OK;

    for (i = 0; i < nTables; i++) {
      tableType[i] = tableTypeList[j];
      /* equal concentrations of all but material 2030 */
      for (k = 0; k < nXYPairs; k++)
	C[k+i*nXYPairs] = (matID[i] == 2030) ? 0.0 : 1./3.;
    }

    eos_CreateTables (&nTables, tableType, matID, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_CreateTables ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      for (i = 0; i < nTables; i++) {
	tableHandleErrorCode = EOS_OK;
	eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
	if (tableHandleErrorCode != EOS_OK) {
	  eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	  printf ("%i: eos_CreateTables ERROR %i: %s (fcmp_ignore)\n",
		  tableHandle[i], tableHandleErrorCode, errorMessage);
	}
      }
      goto CLEANUP;
    }

    if (j == 0)
      optionFlag = EOS_DUMP_DATA;
    for (i = 0; i < nTables; i++) {
      /* Force data output to TablesLoaded.dat */
      eos_SetOption (&tableHandle[i], &optionFlag, EOS_NullPtr,
		     &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
	goto CLEANUP;
      }
      optionFlag = EOS_APPEND_DATA;

      /* Enable EOS_SAVE_SPECIES_DATA */
      eos_SetOption (&tableHandle[i], &EOS_SAVE_SPECIES_DATA, EOS_NullPtr,
		     &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
	goto CLEANUP;
      }
    }

    eos_LoadTables (&nTables, tableHandle, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_LoadTables ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      for (i = 0; i < nTables; i++) {
	tableHandleErrorCode = EOS_OK;
	eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
	if (tableHandleErrorCode != EOS_OK) {
	  eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
	  printf ("%i: eos_LoadTables ERROR %i: %s (fcmp_ignore)\n",
		  tableHandle[i], tableHandleErrorCode, errorMessage);
	}
      }
      goto CLEANUP;
    }

    printf("Mixture for table type %s\n", get_tableType_str(tableType[0]));

    for (i = 0; i < nXYPairs; i++) {
      X[i] = 123. + i * 123.;
      if (tableType[0] == EOS_Ut_DPt || tableType[0] == EOS_T_DPt)
	Y[i] = 1.801801e+06 + i * 1111.;
      else if (tableType[0] == EOS_Pt_DUt || tableType[0] == EOS_T_DUt)
	Y[i] = 1.026945e+04 + i * 11111.;
      else
	Y[i] = 2345. + i * 1111.;
      xyBounds[i] = EOS_OK;
    }

    eos_Mix (&nTables, tableHandle, &nXYPairs, C, X, Y, F,
	     dFx, dFy, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_Mix ERROR: %s\n\n", errorMessage);

      eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds,
		       &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_CheckExtrap ERROR: %s\n\n", errorMessage);
      }
    }

    /* Fetch species-specific data */
    for (k = 0; k < nTables; k++) {

      /* Typicaly, one would fetch nXYPairs using eos_GetTableInfo,
         but it should be consistent with the current value. */

      /* get species-specific atomic mass */
      _infoItem_ = (EOS_INTEGER) EOS_Mean_Atomic_Mass;
      eos_GetTableInfo (&tableHandle[k], &one, &_infoItem_, &atomicMass[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }

      /* get species-specific X Data */
      _infoItem_ = (EOS_INTEGER) EOS_X_Species_Data;
      eos_GetTableInfo (&tableHandle[k], &nXYPairs, &_infoItem_, xSpecies[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }

      /* get species-specific Y Data */
      _infoItem_ = (EOS_INTEGER) EOS_Y_Species_Data;
      eos_GetTableInfo (&tableHandle[k], &nXYPairs, &_infoItem_, ySpecies[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }

      /* get species-specific F Data */
      _infoItem_ = (EOS_INTEGER) EOS_F_Species_Data;
      eos_GetTableInfo (&tableHandle[k], &nXYPairs, &_infoItem_, FSpecies[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }

      /* get species-specific dF/dX Data */
      _infoItem_ = (EOS_INTEGER) EOS_dFx_Species_Data;
      eos_GetTableInfo (&tableHandle[k], &nXYPairs, &_infoItem_, dFxSpecies[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }

      /* get species-specific dF/dY Data */
      _infoItem_ = (EOS_INTEGER) EOS_dFy_Species_Data;
      eos_GetTableInfo (&tableHandle[k], &nXYPairs, &_infoItem_, dFySpecies[k], &errorCode);
      if (errorCode != EOS_OK) {
	eos_GetErrorMessage (&errorCode, errorMessage);
	printf ("eos_GetTableInfo ERROR: %s\n", errorMessage);
      }
    }

    for (i = 0; i < nXYPairs; i++) {
      printf("\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\t%s\n",
	     X[i], Y[i], F[i], dFx[i], dFy[i], ERROR_TO_TEXT(xyBounds[i]));
    }

    for (k = 0; k < nTables; k++) {
      printf("\n\tSpecies-specific values for table type %s and material %d with atomic mass, %e\n",
	     get_tableType_str(tableType[0]), matID[k], atomicMass[k]);
      for (i = 0; i < nXYPairs; i++) {
	printf("\t\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, C = %e\t%s\n",
	       xSpecies[k][i], ySpecies[k][i], FSpecies[k][i], dFxSpecies[k][i], dFySpecies[k][i],
	       C[i+k*nXYPairs], ERROR_TO_TEXT(xyBounds[i]));
      }
    }
    printf("\n");

  }

 CLEANUP:
  {
    EOS_INTEGER err;
    eos_DestroyAll (&err);
  }

  return 0;
}
