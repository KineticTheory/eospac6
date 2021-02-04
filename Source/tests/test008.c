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
 *  \brief Perform the following tests:
 *    -# Ensure tables can be created, loaded, packed, unpacked
 *       and destroyed.
 *    -# Then test interpolation using unpacked data.
 *
 * Test the deletion of the first loaded table and then set it again with packed data.
 * Test additional error checking that was added for many inverted data types so that monotonicity
 * with respect to density is not forced. This means that table inversion with respect to density
 * will only occur if the corresponding Sesame data is monotonic with respect to density. See
 * SourceForge© Issue #artf1964 for more details: https://tf.lanl.gov/sf/go/artf1964
 * Fixed a problem that occurred when a host code used eos_GetPackedTables and eos_SetPackedTables
 * in an unexpected fashion -- packing and unpacking the tables individually, instead of all tables
 * at one time. See SourceForge© Issue #artf1970 for more details: https://tf.lanl.gov/sf/go/artf1970
 * Modified test008 to test types which can not be made monotonic as a side effect test002 output now
 * has an extra error message.
 * Test the added error checking for eos_SetOption and eos_LoadTables
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_CreateTables
 * eos_DestroyAll
 * eos_DestroyTables
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_GetPackedTables
 * eos_GetPackedTablesSize
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 * eos_SetPackedTables
 *
 * \note
 * MATIDS TO TEST: 3720 9991
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i, j;
  EOS_INTEGER one = 1, packedTablesSize;
#define N_TABLES 21
  enum
  { nTablesE = N_TABLES };

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen], *tableTypeStr[nTablesE],
    *packedData = NULL;

  EOS_INTEGER nTables;

  EOS_INTEGER nXYPairs, *xyBounds;
  EOS_REAL *xVals, *yVals, *fVals, *dFx, *dFy;

  nTables = nTablesE;

  tableType[0] = EOS_Pic_DT;    /* cat 0 */
  tableType[1] = EOS_Uic_DT;    /* cat 0 */
  tableType[2] = EOS_T_DUic;    /* cat 2 */
  tableType[3] = EOS_T_DPic;    /* cat 2 */
  tableType[4] = EOS_Pic_DUic;  /* cat 4: EOS_Pic_DT and EOS_T_DUic */
  tableType[5] = EOS_Uic_DPic;  /* cat 4: EOS_Uic_DT and EOS_T_DPic, but EOS_MONOTONIC_IN_X set */
  tableType[6] = EOS_Uic_DT;   /* cat 0 , but EOS_MONOTONIC_IN_X set */
  tableType[7] = EOS_Pv_T;     /* cat 0, EOS_RECORD_TYPE2 */
  tableType[8] = EOS_Dv_T;     /* cat 0, EOS_RECORD_TYPE2 */
  tableType[9] = EOS_Dls_T;    /* cat 0, EOS_RECORD_TYPE2 */
  tableType[10] = EOS_Uv_T;     /* cat 0, EOS_RECORD_TYPE2 */
  tableType[11] = EOS_Uls_T;    /* cat 0, EOS_RECORD_TYPE2 */
  tableType[12] = EOS_Av_T;     /* cat 0, EOS_RECORD_TYPE2 */
  tableType[13] = EOS_Als_T;    /* cat 0, EOS_RECORD_TYPE2 */
  tableType[14] = EOS_T_Pv;     /* cat 1, EOS_RECORD_TYPE2 */
  tableType[15] = EOS_T_Uls;    /* cat 1, EOS_RECORD_TYPE2 */
  tableType[16] = EOS_T_Uv;     /* cat 1, EOS_RECORD_TYPE2 */
  tableType[17] = EOS_Uls_Pv;   /* cat 3, EOS_RECORD_TYPE2 */
  tableType[18] = EOS_Pv_Uls;   /* cat 3, EOS_RECORD_TYPE2 */
  tableType[19] = EOS_Pv_Uv;    /* cat 3, EOS_RECORD_TYPE2 */
  tableType[20] = EOS_Uv_Pv;    /* cat 3, EOS_RECORD_TYPE2 */
#if (N_TABLES > 21)
  tableType[N_TABLES - 1] = EOS_Pic_DT; /* cat 0, but EOS_MONOTONIC_IN_X set */
#endif

  tableTypeStr[0] = "EOS_Pic_DT";       /* cat 0 */
  tableTypeStr[1] = "EOS_Uic_DT";       /* cat 0 */
  tableTypeStr[2] = "EOS_T_DUic";       /* cat 2 */
  tableTypeStr[3] = "EOS_T_DPic";       /* cat 2 */
  tableTypeStr[4] = "EOS_Pic_DUic";     /* cat 4: EOS_Pic_DT and EOS_T_DUic */
  tableTypeStr[5] = "EOS_Uic_DPic";     /* cat 4: EOS_Uic_DT and EOS_T_DPic, but EOS_MONOTONIC_IN_X set */
  tableTypeStr[6] = "EOS_Uic_DT";      /* cat 0 , but EOS_MONOTONIC_IN_X set */
  tableTypeStr[7] = "EOS_Pv_T";        /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[8] = "EOS_Dv_T";        /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[9] = "EOS_Dls_T";       /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[10] = "EOS_Uv_T";        /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[11] = "EOS_Uls_T";       /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[12] = "EOS_Av_T";        /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[13] = "EOS_Als_T";       /* cat 0, EOS_RECORD_TYPE2 */
  tableTypeStr[14] = "EOS_T_Pv";        /* cat 1, EOS_RECORD_TYPE2 */
  tableTypeStr[15] = "EOS_T_Uls";       /* cat 1, EOS_RECORD_TYPE2 */
  tableTypeStr[16] = "EOS_T_Uv";        /* cat 1, EOS_RECORD_TYPE2 */
  tableTypeStr[17] = "EOS_Uls_Pv";      /* cat 3, EOS_RECORD_TYPE2 */
  tableTypeStr[18] = "EOS_Pv_Uls";      /* cat 3, EOS_RECORD_TYPE2 */
  tableTypeStr[19] = "EOS_Pv_Uv";       /* cat 3, EOS_RECORD_TYPE2 */
  tableTypeStr[20] = "EOS_Uv_Pv";       /* cat 3, EOS_RECORD_TYPE2 */
#if (N_TABLES > 21)
  tableTypeStr[N_TABLES - 1] = "EOS_Pic_DT";    /* cat 0, but EOS_MONOTONIC_IN_X set */
#endif

  for (i = 0; i < nTables; i++) {
    if (i >= 7 && i < N_TABLES)
      matID[i] = 3720;
    else
      matID[i] = 9991;
  }

  errorCode = EOS_OK;
  printf ("TH\n--\n");
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
    printf ("%2i. creating table %s (%i)\n", i, tableTypeStr[i],
            tableType[i]);
    eos_CreateTables (&one, &tableType[i], &matID[i], &tableHandle[i],
                      &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
      return -1;
    }
  }

  /* Enable data dump to file */
  for (i = 0; i < nTables; i++) {
    if (i == 0)
      eos_SetOption (&tableHandle[i], &EOS_DUMP_DATA, EOS_NullPtr,
                     &errorCode);
    else
      eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                     &errorCode);
    if ((i == 6) || i > (N_TABLES - 1))
      eos_SetOption (&tableHandle[i], &EOS_MONOTONIC_IN_X, EOS_NullPtr,
                     &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
      return -1;
    }
  }

/* packed tables: delete the first table and then set it again with packed data */
  printf ("\n*** call Get/SetPackedTables BEFORE calling eos_LoadTables()\n");
  eos_GetPackedTablesSize (&nTables, tableHandle, &packedTablesSize,
                           &errorCode);
  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&nTables, tableHandle, packedData, &errorCode);
  eos_DestroyTables (&nTables, tableHandle, &errorCode);
  eos_DestroyAll (&errorCode);
  eos_SetPackedTables (&nTables, &packedTablesSize, packedData, tableHandle,
                       &errorCode);
  free (packedData);

  printf ("\nTH\n--\n");
  for (i = 0; i < nTables; i++) {
    printf ("%2i. loading table %s (%i)\n", tableHandle[i], tableTypeStr[i],
            tableType[i]);
    eos_LoadTables (&one, &tableHandle[i], &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("    eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
      tableHandleErrorCode = EOS_OK;
      eos_GetErrorCode (&tableHandle[i], &tableHandleErrorCode);
      if (tableHandleErrorCode != EOS_OK) {
        eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
        printf ("    eos_LoadTables ERROR %i: %s\n", tableHandleErrorCode,
                errorMessage);
      }
    }
  }

/* packed tables: delete the first table and then set it again with packed data */
  printf ("\n*** call Get/SetPackedTables AFTER calling eos_LoadTables()\n");
  eos_GetPackedTablesSize (&nTables, tableHandle, &packedTablesSize,
                           &errorCode);
  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  eos_GetPackedTables (&nTables, tableHandle, packedData, &errorCode);
  eos_DestroyTables (&nTables, tableHandle, &errorCode);
  eos_DestroyAll (&errorCode);
  eos_SetPackedTables (&nTables, &packedTablesSize, packedData, tableHandle,
                       &errorCode);
  free (packedData);

#define FREE_INTERP_ARRAYS free(xyBounds); free(xVals); free(yVals); free(fVals); free(dFx); free(dFy);

  /* Interpolate some data */
  nXYPairs = 4;
  xyBounds = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));
  xVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  yVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  fVals = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFx = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFy = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  i = 2;                        /* indicate table handle index to use: EOS_T_DUic */
  xVals[0] = 4.4192528E-007;
  xVals[1] = 4.4192528E-007;
  xVals[2] = 4.4192528E-007;
  xVals[3] = 4.4192528E-007;
  yVals[0] = 6.4494245E-002;
  yVals[1] = 9.5735247E-002;
  yVals[2] = 9.5735248E-002;
  yVals[3] = 1.0538984E-001;
  printf ("\nInterpolating %s data\n", tableTypeStr[i]);
  eos_Interpolate (&tableHandle[i], &nXYPairs, xVals, yVals, fVals, dFx, dFy,
                   &errorCode);

  if (errorCode != EOS_OK) {
    EOS_BOOLEAN equal;
    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
    if (equal) {
      eos_CheckExtrap (&tableHandle[i], &nXYPairs, xVals, yVals, xyBounds,
                       &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_CheckExtrap ERROR: %s\n", errorMessage);
        return 1;
      }
      for (j = 0; j < nXYPairs; j++)
        printf ("\tX = %.7e, Y = %.7e, F = %.7e, extrapCode = %d\n",
                xVals[j], yVals[j], fVals[j], xyBounds[j]);
    }
    else {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_Interpolate ERROR(%d): %s\n", errorCode, errorMessage);
    }
  }
  else {
    for (j = 0; j < nXYPairs; j++)
      printf ("\tX = %.7e, Y = %.7e, F = %.7e\n", xVals[j], yVals[j],
              fVals[j]);

  }
  FREE_INTERP_ARRAYS;

  eos_DestroyAll (&errorCode);

  return 0;

}
