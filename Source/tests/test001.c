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
 *  \brief Verify results of category 0, 2 and 4 interpolation for both
 *  EOS_RATIONAL and EOS_LINEAR interpolation options.
 *
 * Test data setup with default options settings (except EOS_DUMP_DATA and EOS_APPEND_DATA were used):
 * eos_CreateTables
 * eos_SetOption
 * eos_GetErrorMessage
 * eos_GetErrorCode
 * eos_LoadTables
 *
 * Test data packing and unpacking and data object destruction:
 * eos_GetPackedTablesSize
 * eos_GetPackedTables
 * eos_DestroyTables
 * eos_DestroyAll
 * eos_SetPackedTables
 *
 * Test data interpolation:
 * eos_Interpolate using record type 1, category 0 data and default options
 * eos_Interpolate using record type 1, category 1 data and default options
 * eos_Interpolate using record type 1, category 2 data and default options
 * eos_Interpolate using record type 1, category 4 data and default options
 * eos_CheckExtrap
 * eos_Interpolate using record type 1, category 0 data and EOS_LINEAR option
 * eos_Interpolate using record type 1, category 0 data and EOS_RATIONAL option
 *
 * \note
 * MATIDS TO TEST: 2140
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

int main ()
{
  int i, j;
  EOS_REAL *X, *Y, *FXY, FXY0[4][2], *dFx, *dFy, *Y2, *X2, *ptr;
  EOS_INTEGER xyBounds[4];
  enum
  { nTablesE = 4 };
  enum
  { nXYPairsE = 4 };
  //    enum {nTablesE = 9};

  EOS_INTEGER tableType[nTablesE];
  EOS_INTEGER matID[nTablesE];
  EOS_INTEGER tableHandle[nTablesE];
  EOS_INTEGER errorCode, tableHandleErrorCode;

  EOS_INTEGER nTables, nXYPairs, packedTablesSize, four = 4;
  EOS_CHAR *packedData = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nTables = nTablesE;
  nXYPairs = nXYPairsE;

  /* EOS_Pt_DT, material 2140 works for table 301 (record type 1) */
  /* EOS_Uls_T, material 2030 works for table 401 (record type 2) */
  /* EOS_Ogb, material 13718 works for table 501 (record type 3) */
  /* EOS_Comment, material 4100 works for tables 101-199 (record type 4) */
  /* EOS_Info, material 2140 works for tables 201 (record type 5) */
  //    tableType[0] = EOS_Pt_DT;  /*record type 1, subtable 1 */
  //    tableType[1] = EOS_Uls_T; /*record type 2, subtable 5 */
  //    tableType[2] = EOS_Ogb;
  //    tableType[3] = EOS_Comment;
  //    tableType[4] = EOS_Info;
  tableType[0] = EOS_Pt_DT;     /* record type 1, subtable 1 , cat 0 */
  tableType[1] = EOS_T_DPt;     /* record type 1, subtable 1 , cat 2 */
  tableType[2] = EOS_D_PtT;     /* record type 1, subtable 1 , cat 1 */
  tableType[3] = EOS_Pt_DUt;    /* record type 1, category 3: EOS_Pt_DT, EOS_T_DUt (where EOS_T_DUt is subtable 1, category 2) */

  //    tableType[5] = EOS_Pt_DT;
  //    tableType[6] = EOS_Ut_DT;  record type 1, subtable 2
  //    tableType[7] = EOS_Uls_T;  record type 2, subtable 5
  //    tableType[8] = EOS_Uv_T;   record type 2, subtable 4

  matID[0] = 2140;
  matID[1] = 2140;
  matID[2] = 2140;
  matID[3] = 2140;
  //    matID[1] = 2030;
  //    matID[2] = 13718;
  //    matID[3] = 4100;
  //    matID[4] = 2140;

  //    matID[5] = 2140;
  //    matID[6] = 2140;
  //    matID[7] = 2030;
  //    matID[8] = 2030;

  errorCode = EOS_OK;
  for (i = 0; i < nTables; i++) {
    tableHandle[i] = 0;
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
    return 0;
  }

  eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    return 0;
  }

  for (i = 1; i < nTables; i++) {
    eos_SetOption (&tableHandle[i], &EOS_APPEND_DATA, EOS_NullPtr,
                   &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
      return 0;
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
    return 0;
  }

  /* packed tables: delete the tables and then set them again with packed data */

  printf("\n*** call eos_GetPackedTablesSize from main in %s(%d) ... fcmp_ignore\n", __FILE__, __LINE__);
  eos_GetPackedTablesSize (&four, tableHandle, &packedTablesSize, &errorCode);
  packedData = (EOS_CHAR *) malloc (packedTablesSize);
  printf("\n*** call eos_GetPackedTables from main in %s(%d)     ... fcmp_ignore\n", __FILE__, __LINE__);
  eos_GetPackedTables (&four, tableHandle, packedData, &errorCode);
  printf("\n*** call eos_DestroyTables from main in %s(%d)       ... fcmp_ignore\n", __FILE__, __LINE__);
  eos_DestroyTables (&four, tableHandle, &errorCode);
  printf("\n*** call eos_DestroyAll from main in %s(%d)          ... fcmp_ignore\n", __FILE__, __LINE__);
  eos_DestroyAll (&errorCode);
  printf("\n*** call eos_SetPackedTables from main in %s(%d)     ... fcmp_ignore\n", __FILE__, __LINE__);
  eos_SetPackedTables (&four, &packedTablesSize, packedData, tableHandle,
                       &errorCode);
  free (packedData);

  /* testing interpolation methods */

  /* allocate memory continuously */
  ptr = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs * 3);
  X = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  Y2 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  X2 = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  FXY = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
  dFy = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);

  X[0] = .56;                   //4.906250000000e-01, 8.111664050000e-01
  X[1] = 8.42;                  //8.392435000000e+00, 8.535148000000e+00
  X[2] = 17.0;                  //1.670213100000e+01, 1.721490870000e+01
  X[3] = 108.0;                 //1.019480280000e+02, 1.121428230000e+02

  Y[0] = 20000.0;               //1.160484999999998E+04, 2.320969999999995E+04 (1.277665349999999 2.280032270000007 3.257297170000001 5.272329460000009)
  Y[1] = 620000;                //5.802425000000000E+05, 1.160485000000000E+06 (5.805866370000003E+03 6.022057830000005E+03 1.533697850000003E+04 1.590773070000001E+04)
  Y[2] = 4000000.0;             //3.481455000000000E+06, 5.802425000000000E+06 (1.370582999999998E+05 1.410762460000003E+05 2.735364440000001E+05 2.814615289999992E+05)
  Y[3] = 200000000.0;           //1.160485000000000E+08, 2.320970000000000E+08 (4.693332700000000E+07 5.160415090000010E+07 9.477940909999990E+07 1.042485970000000E+08)

  /* extrapolation 
     X[0] = -.01; Y[0] = 381355200.0; // x low y hi
     X[1] = 157010.0;  Y[1] = 381355200.0; // x hi y hi
     X[2] = 157010.0;  Y[2] = 361355200.0; // x hi y ok
     X[3] = 156010.0;  Y[3] = -1.0; // x ok y low

     X[0] = -.01; Y[0] = -1.0; // x low y lo
     X[1] = -1.0;  Y[1] = -1.0; // x lo y lo
     X[2] = 156010.0;  Y[2] = 361355200.0; // x ok y ok
     X[3] = 156010.0;  Y[3] = 381355200.0; // x ok y hi
   */
  printf
    ("\n--- TEST eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
  eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        //F(x,y)
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  for (i = 0; i < 4; i++) {
    printf
      ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
       i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
  }
  eos_CheckExtrap (&tableHandle[0], &nXYPairs, X, Y, xyBounds, &errorCode);

  /* since we no longer take log10 but still use biLinear I will assign it to prev results */
  /* result: 2.829             3.12?
     6249.1212         6505.38
     162613.60944      170390.37
     82562601.322359    86379046.505346 */
  //    FXY[0] = 2.829;
  //    FXY[1] = 6249.1212;
  //    FXY[2] = 162613.60944;
  //    FXY[3] = 82562601.322359;

  /* extrapolation 
     FXY[0] = -0.2; //lo
     FXY[1] = 1000082562601.322359; //hi
     FXY[2] =  216932436408.09  ; //ok
     FXY[3] =  216932436408.09 ; //ok
     X[0] = 156010.0; //ok
     X[1] = 156010.0; // ok
     X[2] = 156010.0; //ok
     X[3] = 381355200.0; //hi
   */

  printf
    ("\n--- TEST eos_Interpolate using category 2 tableType: EOS_T_DPt ---\n");
  eos_Interpolate (&tableHandle[1], &nXYPairs, X, FXY, Y2, dFx, dFy, &errorCode);       //y( x,F)
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  for (i = 0; i < 4; i++) {
    printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n",
            i, X[i], FXY[i], Y2[i], dFx[i], dFy[i]);
  }
  eos_CheckExtrap (&tableHandle[1], &nXYPairs, X, FXY, xyBounds, &errorCode);

  /* extrapolation 
     FXY[0] = -.01; Y[0] = -1.0; // x low y lo
     FXY[1] = -1.0;  Y[1] = 361355200.0; // x lo y ok
     FXY[2] = 1000082562601.322359; ;  Y[2] = 361355200.0; // x hi y ok
     FXY[3] = 156010.0;  Y[3] = 381355200.0; // x ok y hi
   */

  printf
    ("\n--- TEST eos_Interpolate using category 1 tableType: EOS_D_PtT ---\n");
  eos_Interpolate (&tableHandle[2], &nXYPairs, FXY, Y, X2, dFx, dFy, &errorCode);       //x( F,y)
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  for (i = 0; i < 4; i++) {
    printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n",
            i, FXY[i], Y[i], X2[i], dFx[i], dFy[i]);
  }

  eos_CheckExtrap (&tableHandle[2], &nXYPairs, FXY, Y, xyBounds, &errorCode);

  X[0] = .15;                   /* 1.170121000000e-01 1.727000000000e-01 */
  X[1] = .6;                    /* 4.906250000000e-01 8.111664050000e-01 */
  X[2] = 8.55;                  /* 8.535148000000e+00 8.677861000000e+00 */
  X[3] = 11.0;                  /* 1.075342460000e+01 1.129496470000e+01 */

  Y[0] = 17.0;                  /* 1.302885020000002E+01 1.302770479999998E+01 2.150784209999995E+01 1.975271069999997E+01 */
  Y[1] = 50.0;                  /* 1.690384129999995E+01 1.611996550000003E+01 6.643819140000005E+01 6.227892930000007E+01 */
  Y[2] = 900.0;                 /* 4.338561799999996E+03 4.324848809999996E+03 1.174836770000000E+04 1.171932890000002E+04 */
  Y[3] = 200000.0;              /* 1.694242999999998E+05 1.690812180000003E+05 3.942975910000000E+05 3.940299920000006E+05 */

  printf
    ("\n--- TEST eos_Interpolate using category 4 tableType: EOS_Pt_DUt ---\n");
  eos_Interpolate (&tableHandle[3], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);        // Pt(D,Ut)
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    printf ("%d: %s\n", errorCode, errorMessage);
  }
  for (i = 0; i < 4; i++) {
    printf ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e\n",
            i, X[i], Y[i], FXY[i], dFx[i], dFy[i]);
  }

  //    eos_CreateTables ( 3, &(tableType[5]), &(matID[5]), &(tableHandle[5]), &errorCode);
  //    eos_LoadTables ( 3, &(tableHandle[5]), &errorCode);
  //...
  // interpolate
  //...

  //    eos_DestroyTables( 3, &(tableHandle[5]), &errorCode );
  //   eos_DestroyTables( 2, tableHandle, &errorCode );

  // Compare Linear and Rational function interpolator results for EOS_Pt_DT
  X[0] = .56;                   //4.906250000000e-01, 8.111664050000e-01
  X[1] = 8.42;                  //8.392435000000e+00, 8.535148000000e+00
  X[2] = 17.0;                  //1.670213100000e+01, 1.721490870000e+01
  X[3] = 108.0;                 //1.019480280000e+02, 1.121428230000e+02

  Y[0] = 20000.0;               //1.160484999999998E+04, 2.320969999999995E+04 (1.277665349999999 2.280032270000007 3.257297170000001 5.272329460000009)
  Y[1] = 620000;                //5.802425000000000E+05, 1.160485000000000E+06 (5.805866370000003E+03 6.022057830000005E+03 1.533697850000003E+04 1.590773070000001E+04)
  Y[2] = 4000000.0;             //3.481455000000000E+06, 5.802425000000000E+06 (1.370582999999998E+05 1.410762460000003E+05 2.735364440000001E+05 2.814615289999992E+05)
  Y[3] = 200000000.0;           //1.160485000000000E+08, 2.320970000000000E+08 (4.693332700000000E+07 5.160415090000010E+07 9.477940909999990E+07 1.042485970000000E+08)
  for (j = 0; j < 2; j++) {
    if (j == 0) {
      eos_SetOption (&tableHandle[0], &EOS_LINEAR, EOS_NullPtr, &errorCode);
      printf
        ("\n--- TEST EOS_LINEAR eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
    }
    else {
      eos_SetOption (&tableHandle[0], &EOS_RATIONAL, EOS_NullPtr, &errorCode);
      printf
        ("\n--- TEST EOS_RATIONAL eos_Interpolate using category 0 tableType: EOS_Pt_DT ---\n");
    }
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    }
    eos_Interpolate (&tableHandle[0], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    for (i = 0; i < 4; i++) {
      printf
        ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
         i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }

    FXY0[0][j] = FXY[0];
    FXY0[1][j] = FXY[1];
    FXY0[2][j] = FXY[2];
    FXY0[3][j] = FXY[3];

  }

  // Compare Linear and Rational function interpolator results for EOS_T_DPt
  X[0] = .56;                   //4.906250000000e-01, 8.111664050000e-01
  X[1] = 8.42;                  //8.392435000000e+00, 8.535148000000e+00
  X[2] = 17.0;                  //1.670213100000e+01, 1.721490870000e+01
  X[3] = 108.0;                 //1.019480280000e+02, 1.121428230000e+02

  for (j = 0; j < 2; j++) {

    Y[0] = FXY0[0][j];
    Y[1] = FXY0[1][j];
    Y[2] = FXY0[2][j];
    Y[3] = FXY0[3][j];

    if (j == 0) {
      eos_SetOption (&tableHandle[1], &EOS_LINEAR, EOS_NullPtr, &errorCode);
      printf
        ("\n--- TEST EOS_LINEAR eos_Interpolate using category 2 tableType: EOS_T_DPt ---\n");
    }
    else {
      eos_SetOption (&tableHandle[1], &EOS_RATIONAL, EOS_NullPtr, &errorCode);
      printf
        ("\n--- TEST EOS_RATIONAL eos_Interpolate using category 2 tableType: EOS_T_DPt ---\n");
    }
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_SetOption ERROR %i: %s (fcmp_ignore)\n", errorCode, errorMessage);
    }
    eos_Interpolate (&tableHandle[1], &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
    for (i = 0; i < 4; i++) {
      printf
        ("\ti=%i\tX = %e, Y = %e, F = %e, dFx = %e, dFy = %e, errorCode: %d\n",
         i, X[i], Y[i], FXY[i], dFx[i], dFy[i], errorCode);
    }
  }

  eos_DestroyAll (&errorCode);

  free(ptr);
  free(X);
  free(Y);
  free(Y2);
  free(X2);
  free(FXY);
  free(dFx);
  free(dFy);

  return 0;

}
