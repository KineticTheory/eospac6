/*********************************************************************
 * Example Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup examples
 * \brief This is a simple C++ example of how to use EOSPAC6 interface.
 */

#include <iostream>
#include <iomanip>
#include "eos_Interface.h"

using namespace std;

int main ()
{

  const EOS_INTEGER nTablesE = 5;
  const EOS_INTEGER nXYPairsE = 4;
  const EOS_INTEGER nInfoItemsE = 12;

  EOS_INTEGER i, j;
  EOS_REAL X[nXYPairsE], Y[nXYPairsE], F[nXYPairsE], dFx[nXYPairsE],
    dFy[nXYPairsE];
  EOS_INTEGER tableType[nTablesE], numIndVars[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode;
  EOS_INTEGER tableHandleErrorCode;
  EOS_INTEGER nTables;
  EOS_INTEGER nXYPairs;
  EOS_REAL infoVals[nInfoItemsE];
  EOS_INTEGER nInfoItems;
  EOS_INTEGER infoItems[nInfoItemsE] = {
    EOS_Cmnt_Len,
    EOS_Exchange_Coeff,
    EOS_F_Convert_Factor,
    EOS_Log_Val,
    EOS_Material_ID,
    EOS_Mean_Atomic_Mass,
    EOS_Mean_Atomic_Num,
    EOS_Modulus,
    EOS_Normal_Density,
    EOS_Table_Type,
    EOS_X_Convert_Factor,
    EOS_Y_Convert_Factor
  };
  const EOS_CHAR *infoItemDescriptions[nInfoItemsE] = {
    "The length in characters of the comments available for the specified data table",
    "The exchange coefficient",
    "The conversion factor corresponding to the dependent variable, F(x,y)",
    "Non-zero if the data table is in a log10 format",
    "The SESAME material identification number",
    "The mean atomic mass",
    "The mean atomic number",
    "The solid bulk modulus",
    "The normal density",
    "The type of data table. Corresponds to the parameters in APPENDIX B and APPENDIX C",
    "The conversion factor corresponding to the primary independent variable, x",
    "The conversion factor corresponding to the secondary independent variable, y"
  };
  const EOS_CHAR *tableTypeLabel[nTablesE] = {
    "EOS_Pt_DT",
    "EOS_Dv_T",
    "EOS_Ogb",
    "EOS_Comment",
    "EOS_Info"
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  EOS_INTEGER one = 1;

  nTables = nTablesE;
  nXYPairs = nXYPairsE;
  nInfoItems = nInfoItemsE;

  /*
   * EOS_Pt_DT, material 2140 works for Sesame table 301 (record type 1)
   * EOS_Dv_T, material 2140 works for Sesame table 401 (record type 2)
   * EOS_Ogb, material 12140 works for Sesame table 501 (record type 3)
   * EOS_Comment, material 2140 works for Sesame tables 101-199 (record type 4)
   * EOS_Info, material 2140 works for Sesame table 201 (record type 5)
   */
  tableType[0] = EOS_Pt_DT;
  tableType[1] = EOS_Dv_T;
  tableType[2] = EOS_Ogb;
  tableType[3] = EOS_Comment;
  tableType[4] = EOS_Info;

  numIndVars[0] = 2;
  numIndVars[1] = 1;
  numIndVars[2] = 0;
  numIndVars[3] = 0;
  numIndVars[4] = 0;

  matID[0] = 2140;
  matID[1] = 2140;
  matID[2] = 12140;
  matID[3] = 2140;
  matID[4] = 2140;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
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
      cout << "eos_CreateTables ERROR " << tableHandleErrorCode
	   << ": " << errorMessage << '\n';
    }
  }

  /*
   * set some options
   */

  for (i = 0; i < nTables; i++) {
    /* enable smoothing */
    eos_SetOption (&tableHandle[i], &EOS_SMOOTH, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      cout << "eos_SetOption ERROR " << errorCode << ": " << errorMessage << '\n';
    }
  }

  /*
   * load data into table data objects
   */

  eos_LoadTables (&nTables, tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    cout << "eos_LoadTables ERROR " << errorCode << ": " << errorMessage << '\n';
    for (i = 0; i < nTables; i++) {
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
      cout << "eos_LoadTables ERROR " << tableHandleErrorCode << "(TH="
	   << tableHandle[i] << "): " << errorMessage << '\n';
    }
  }

  /*
   * interpolate -- errors codes are intentionally produced
   */
  X[0] = 3000.;
  X[1] = 6000.;
  X[2] = 8200.;
  X[3] = 8300.;

  Y[0] = 20000.0;
  Y[1] = 620000.0;
  Y[2] = 4000000.0;
  Y[3] = 200000000.0;

  for (i = 0; i < nTables; i++) {
    cout << "\n--- Interpolate using tableType " << tableTypeLabel[i] << " ---\n";
    eos_Interpolate (&tableHandle[i], &nXYPairs, X, Y, F, dFx, dFy,
                     &errorCode);
    cout << tableTypeLabel[i] << " Interpolation Results:\n";
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      cout << "eos_Interpolate ERROR " << errorCode << "(TH="
	   << tableHandle[i] << "): " << errorMessage << '\n';
    }
    else {
      for (j = 0; j < nXYPairs; j++) {
        if (numIndVars[i] == 1)
	  cout << "\ti=" << j
	       << "\tX = " << scientific << X[j]
	       << ", F = " << scientific << F[j]
	       << ", dFx = " << scientific << dFx[j]
	       << ", errorCode: " << errorCode << '\n';
        if (numIndVars[i] == 2)
	  cout << "\ti=" << j
	       << "\tX = " << scientific << X[j]
	       << ", Y = " << scientific << Y[j]
	       << ", F = " << scientific << F[j]
	       << ", dFx = " << scientific << dFx[j]
	       << ", dFy = " << scientific << dFy[j]
	       << ", errorCode: " << errorCode << '\n';
      }
    }
  }

  /*
   * retrieve table info -- errors codes are intentionally produced
   */

  for (i = 0; i < nTables; i++) {
    cout << "\n--- Table information for tableType " << tableTypeLabel[i]
	 << " , tableHandle=" << tableHandle[i]
	 << " ---\n";
    for (j = 0; j < nInfoItems; j++) {
      EOS_BOOLEAN equal;
      eos_GetTableInfo (&(tableHandle[i]), &one, &(infoItems[j]),
                        &(infoVals[j]), &errorCode);
      eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INVALID_INFO_FLAG, &errorCode, &equal);
      if (errorCode == EOS_OK) {
	cout.setf(ios::fixed,ios::floatfield);
	cout << setprecision(2) << setiosflags(ios::fixed)
	     << setw(2) << right << j + 1 << ". "
	     << setw(82) << left << infoItemDescriptions[j] << ": "
	     << setprecision(6) << setiosflags(ios::fixed)
	     << setw(13) << right << infoVals[j] << '\n';
      }
      else if (! equal) {
        /* Ignore EOS_INVALID_INFO_FLAG since not all infoItems are currently
           applicable to a specific tableHandle. */
        eos_GetErrorMessage (&errorCode, errorMessage);
	cout << "eos_GetTableInfo ERROR " << errorCode
	     << ": " << errorMessage << '\n';
      }
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
      cout << "eos_DestroyAll ERROR " << tableHandleErrorCode
	   << ": " << errorMessage << '\n';
    }
  }

  return 0;

}
