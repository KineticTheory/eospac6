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
 *  \brief Test the EOS_INVERT_AT_SETUP option, and compare interpolation results
 *         of tables loaded both with and without EOS_INVERT_AT_SETUP enabled after
 *         ordered usage of eos_GetPackedTables, eos_DestroyAll, and eos_SetPackedTables.
 *         The output is expected to be identical to that of the test_artf35695.c test code.
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf35695">artf35695</a>
 *  for details.
 *
 * Uses the following routines:
 * eos_CheckExtrap
 * eos_CreateTables
 * eos_DestroyAll
 * eos_GetErrorCode
 * eos_GetErrorMessage
 * eos_Interpolate
 * eos_LoadTables
 * eos_SetOption
 *
 * \note
 * MATIDS TO TEST: 3720 7247
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

int main ()
{
  enum
  { nTablesE = 1 };             /* This is the maximum size of the modelTest_s.matID array */
  enum
  { nXYPairsE = 100 };

  EOS_INTEGER MaterialList[] = {3720, 7247};
  EOS_INTEGER nMaterialList, imatid;

  EOS_INTEGER i, j, itest;
  EOS_REAL rho[nXYPairsE], T[nXYPairsE];

  /* Group 1: 301 tables */
  EOS_REAL P1[nXYPairsE], U1[nXYPairsE];
  EOS_REAL                 dFx0[nXYPairsE],  dFy0[nXYPairsE];
  EOS_REAL F1[nXYPairsE],  dFx1[nXYPairsE],  dFy1[nXYPairsE];
  EOS_REAL F2[nXYPairsE],  dFx2[nXYPairsE],  dFy2[nXYPairsE];
  EOS_REAL F1a[nXYPairsE], dFx1a[nXYPairsE], dFy1a[nXYPairsE];
  EOS_REAL F2a[nXYPairsE], dFx2a[nXYPairsE], dFy2a[nXYPairsE];
  EOS_REAL F3[nXYPairsE],  dFx3[nXYPairsE],  dFy3[nXYPairsE];
  EOS_REAL F4[nXYPairsE],  dFx4[nXYPairsE],  dFy4[nXYPairsE];
  EOS_REAL F5[nXYPairsE],  dFx5[nXYPairsE],  dFy5[nXYPairsE];
  EOS_REAL F6[nXYPairsE],  dFx6[nXYPairsE],  dFy6[nXYPairsE];
  EOS_REAL F7[nXYPairsE],  dFx7[nXYPairsE],  dFy7[nXYPairsE];
  EOS_REAL F8[nXYPairsE],  dFx8[nXYPairsE],  dFy8[nXYPairsE];
  EOS_REAL F9[nXYPairsE],  dFx9[nXYPairsE],  dFy9[nXYPairsE];
  EOS_REAL F10[nXYPairsE], dFx10[nXYPairsE], dFy10[nXYPairsE];

  /* Group 2: 303 tables */
  EOS_REAL P2[nXYPairsE], U2[nXYPairsE];
  EOS_REAL                 dFx11[nXYPairsE], dFy11[nXYPairsE];
  EOS_REAL                 dFx12[nXYPairsE], dFy12[nXYPairsE];
  EOS_REAL F13[nXYPairsE], dFx13[nXYPairsE], dFy13[nXYPairsE];
  EOS_REAL F14[nXYPairsE], dFx14[nXYPairsE], dFy14[nXYPairsE];
  EOS_REAL F13a[nXYPairsE], dFx13a[nXYPairsE], dFy13a[nXYPairsE];
  EOS_REAL F14a[nXYPairsE], dFx14a[nXYPairsE], dFy14a[nXYPairsE];
  EOS_REAL F15[nXYPairsE], dFx15[nXYPairsE], dFy15[nXYPairsE];
  EOS_REAL F16[nXYPairsE], dFx16[nXYPairsE], dFy16[nXYPairsE];
  EOS_REAL F17[nXYPairsE], dFx17[nXYPairsE], dFy17[nXYPairsE];
  EOS_REAL F18[nXYPairsE], dFx18[nXYPairsE], dFy18[nXYPairsE];

  /* Group 3: 304 tables */
  EOS_REAL P3[nXYPairsE], U3[nXYPairsE];
  EOS_REAL                 dFx19[nXYPairsE], dFy19[nXYPairsE];
  EOS_REAL                 dFx20[nXYPairsE], dFy20[nXYPairsE];
  EOS_REAL F21[nXYPairsE], dFx21[nXYPairsE], dFy21[nXYPairsE];
  EOS_REAL F22[nXYPairsE], dFx22[nXYPairsE], dFy22[nXYPairsE];
  EOS_REAL F21a[nXYPairsE], dFx21a[nXYPairsE], dFy21a[nXYPairsE];
  EOS_REAL F22a[nXYPairsE], dFx22a[nXYPairsE], dFy22a[nXYPairsE];
  EOS_REAL F23[nXYPairsE], dFx23[nXYPairsE], dFy23[nXYPairsE];
  EOS_REAL F24[nXYPairsE], dFx24[nXYPairsE], dFy24[nXYPairsE];
  EOS_REAL F25[nXYPairsE], dFx25[nXYPairsE], dFy25[nXYPairsE];
  EOS_REAL F26[nXYPairsE], dFx26[nXYPairsE], dFy26[nXYPairsE];

  /* Group 4: 305 tables */
  EOS_REAL P4[nXYPairsE], U4[nXYPairsE];
  EOS_REAL                 dFx29[nXYPairsE], dFy29[nXYPairsE];
  EOS_REAL                 dFx30[nXYPairsE], dFy30[nXYPairsE];
  EOS_REAL F31[nXYPairsE], dFx31[nXYPairsE], dFy31[nXYPairsE];
  EOS_REAL F32[nXYPairsE], dFx32[nXYPairsE], dFy32[nXYPairsE];
  EOS_REAL F31a[nXYPairsE], dFx31a[nXYPairsE], dFy31a[nXYPairsE];
  EOS_REAL F32a[nXYPairsE], dFx32a[nXYPairsE], dFy32a[nXYPairsE];
  EOS_REAL F33[nXYPairsE], dFx33[nXYPairsE], dFy33[nXYPairsE];
  EOS_REAL F34[nXYPairsE], dFx34[nXYPairsE], dFy34[nXYPairsE];
  EOS_REAL F35[nXYPairsE], dFx35[nXYPairsE], dFy35[nXYPairsE];
  EOS_REAL F36[nXYPairsE], dFx36[nXYPairsE], dFy36[nXYPairsE];

  EOS_INTEGER extrapCode[nXYPairsE];
  EOS_INTEGER *tableHandle;
  EOS_INTEGER errorCode, tableHandleErrorCode, nXYPairs, nModelTests;

  typedef struct
  {
      EOS_INTEGER nTables;
      EOS_INTEGER matID[nTablesE];
      EOS_INTEGER tableType[nTablesE];
      EOS_BOOLEAN isLoaded[nTablesE];
      const EOS_INTEGER modelFlag;
      EOS_CHAR *modelFlagLabel;
      EOS_REAL *x_ptr;
      EOS_REAL *y_ptr;
      EOS_REAL *f_ptr;
      EOS_REAL *dFx_ptr;
      EOS_REAL *dFy_ptr;
      EOS_BOOLEAN dump;
  } modelTest_s;

  EOS_INTEGER dump_cntr = 0;

  modelTest_s modelTests[] = {
    /* Group 1: 301 tables */
    { nTablesE, {0}, {EOS_Pt_DT},    {EOS_FALSE}, 0,                   "",                    rho, T,  P1,  dFx0,  dFy0,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ut_DT},    {EOS_FALSE}, 0,                   "",                    rho, T,  U1,  dFx0,  dFy0,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPt},    {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P1, F1,  dFx1,  dFy1,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPt},    {EOS_FALSE}, 0,                   "",                    rho, P1, F2,  dFx2,  dFy2,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DUt},    {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U1, F1a, dFx1a, dFy1a, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DUt},    {EOS_FALSE}, 0,                   "",                    rho, U1, F2a, dFx2a, dFy2a, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ut_DPt},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P1, F3,  dFx3,  dFy3,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ut_DPt},   {EOS_FALSE}, 0,                   "",                    rho, P1, F4,  dFx4,  dFy4,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pt_DUt},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U1, F5,  dFx5,  dFy5,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pt_DUt},   {EOS_FALSE}, 0,                   "",                    rho, U1, F6,  dFx6,  dFy6,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_D_PtT},    {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", P1,  T,  F7,  dFx7,  dFy7,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_D_PtT},    {EOS_FALSE}, 0,                   "",                    P1,  T,  F8,  dFx8,  dFy8,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ut_PtT},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", P1,  T,  F9,  dFx9,  dFy9,  EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ut_PtT},   {EOS_FALSE}, 0,                   "",                    P1,  T,  F10, dFx10, dFy10, EOS_TRUE }
    /* Group 2: 303 tables */
    ,{ nTablesE, {0}, {EOS_Pic_DT},   {EOS_FALSE}, 0,                   "",                    rho, T,  P2,  dFx11, dFy11, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Uic_DT},   {EOS_FALSE}, 0,                   "",                    rho, T,  U2,  dFx12, dFy12, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPic},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P2, F13, dFx13, dFy13, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPic},   {EOS_FALSE}, 0,                   "",                    rho, P2, F14, dFx14, dFy14, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_T_DUic},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U2, F13a, dFx13a, dFy13a, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DUic},   {EOS_FALSE}, 0,                   "",                    rho, U2, F14a, dFx14a, dFy14a, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_Uic_DPic}, {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P2, F15, dFx15, dFy15, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Uic_DPic}, {EOS_FALSE}, 0,                   "",                    rho, P2, F16, dFx16, dFy16, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pic_DUic}, {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U2, F17, dFx17, dFy17, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pic_DUic}, {EOS_FALSE}, 0,                   "",                    rho, U2, F18, dFx18, dFy18, EOS_TRUE }
    /* Group 3: 304 tables */
    ,{ nTablesE, {0}, {EOS_Pe_DT},    {EOS_FALSE}, 0,                   "",                    rho, T,  P3,  dFx19, dFy19, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ue_DT},    {EOS_FALSE}, 0,                   "",                    rho, T,  U3,  dFx20, dFy20, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPe},    {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P3, F21, dFx21, dFy21, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPe},    {EOS_FALSE}, 0,                   "",                    rho, P3, F22, dFx22, dFy22, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_T_DUe},    {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U3, F21a, dFx21a, dFy21a, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DUe},    {EOS_FALSE}, 0,                   "",                    rho, U3, F22a, dFx22a, dFy22a, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_Ue_DPe},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P3, F23, dFx23, dFy23, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Ue_DPe},   {EOS_FALSE}, 0,                   "",                    rho, P3, F24, dFx24, dFy24, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pe_DUe},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U3, F25, dFx25, dFy25, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Pe_DUe},   {EOS_FALSE}, 0,                   "",                    rho, U3, F26, dFx26, dFy26, EOS_TRUE }
    /* Group 4: 305 tables */
    ,{ nTablesE, {0}, {EOS_Piz_DT},   {EOS_FALSE}, 0,                   "",                    rho, T,  P4,  dFx29, dFy29, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Uiz_DT},   {EOS_FALSE}, 0,                   "",                    rho, T,  U4,  dFx30, dFy30, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPiz},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P4, F31, dFx31, dFy31, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DPiz},   {EOS_FALSE}, 0,                   "",                    rho, P4, F32, dFx32, dFy32, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_T_DUiz},   {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U4, F31a, dFx31a, dFy31a, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_T_DUiz},   {EOS_FALSE}, 0,                   "",                    rho, U4, F32a, dFx32a, dFy32a, EOS_TRUE }

    ,{ nTablesE, {0}, {EOS_Uiz_DPiz}, {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, P4, F33, dFx33, dFy33, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Uiz_DPiz}, {EOS_FALSE}, 0,                   "",                    rho, P4, F34, dFx34, dFy34, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Piz_DUiz}, {EOS_FALSE}, EOS_INVERT_AT_SETUP, "EOS_INVERT_AT_SETUP", rho, U4, F35, dFx35, dFy35, EOS_TRUE }
    ,{ nTablesE, {0}, {EOS_Piz_DUiz}, {EOS_FALSE}, 0,                   "",                    rho, U4, F36, dFx36, dFy36, EOS_TRUE }
  };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  nModelTests   = sizeof(modelTests)/sizeof(modelTests[0]);
  nMaterialList = sizeof(MaterialList)/sizeof(MaterialList[0]);

  tableHandle = (EOS_INTEGER *) malloc (nModelTests * sizeof(EOS_INTEGER));
  for (itest = 0; itest < nModelTests; itest++)
    tableHandle[itest] = 0;

  errorCode = EOS_OK;

  nXYPairs = 0;
#if 1
  rho[nXYPairs] = 2.701050234646199e+00; nXYPairs++;
  rho[nXYPairs] = 2.702992978658409e+00; nXYPairs++;
  rho[nXYPairs] = 2.705978874029471e+00; nXYPairs++;
  rho[nXYPairs] = 2.708498050041613e+00; nXYPairs++;
  rho[nXYPairs] = 2.711139559197156e+00; nXYPairs++;
  rho[nXYPairs] = 2.712746939211559e+00; nXYPairs++;
  rho[nXYPairs] = 2.715419028444546e+00; nXYPairs++;
  rho[nXYPairs] = 2.718460286993291e+00; nXYPairs++;
  rho[nXYPairs] = 2.720347218388423e+00; nXYPairs++;
  rho[nXYPairs] = 2.732757092008996e+00; nXYPairs++; // normal density for 3720
  rho[nXYPairs] = 2.725596746314689e+00; nXYPairs++;
  rho[nXYPairs] = 2.728286088655770e+00; nXYPairs++;
  rho[nXYPairs] = 2.730455980590884e+00; nXYPairs++;
  rho[nXYPairs] = 2.733141751137595e+00; nXYPairs++;
  rho[nXYPairs] = 2.736190287156191e+00; nXYPairs++;
  rho[nXYPairs] = 2.738645243834738e+00; nXYPairs++;
  rho[nXYPairs] = 2.740794639659765e+00; nXYPairs++;
  rho[nXYPairs] = 2.743396621161582e+00; nXYPairs++;
  rho[nXYPairs] = 2.745177003194153e+00; nXYPairs++;
  rho[nXYPairs] = 2.748258711095145e+00; nXYPairs++;
  rho[nXYPairs] = 4.792706386856783e+00; nXYPairs++;
  rho[nXYPairs] = 8.571343490605672e+00; nXYPairs++;
  rho[nXYPairs] = 1.437890998731982e+01; nXYPairs++;
  rho[nXYPairs] = 1.927870733093673e+01; nXYPairs++;
  rho[nXYPairs] = 2.441644263846775e+01; nXYPairs++;
  rho[nXYPairs] = 2.754279676648271e+01; nXYPairs++;
  rho[nXYPairs] = 3.274001032464207e+01; nXYPairs++;
  rho[nXYPairs] = 3.865525820195158e+01; nXYPairs++;
  rho[nXYPairs] = 4.232533976548302e+01; nXYPairs++;
  rho[nXYPairs] = 4.785933945471405e+01; nXYPairs++;
  rho[nXYPairs] = 5.253567158206964e+01; nXYPairs++;
  rho[nXYPairs] = 5.776644243547145e+01; nXYPairs++;
  rho[nXYPairs] = 6.198688224926868e+01; nXYPairs++;
  rho[nXYPairs] = 6.721070596262247e+01; nXYPairs++;
  rho[nXYPairs] = 7.314010851879200e+01; nXYPairs++;
  rho[nXYPairs] = 7.791499925856537e+01; nXYPairs++;
  rho[nXYPairs] = 8.209557413824265e+01; nXYPairs++;
  rho[nXYPairs] = 8.715642815927718e+01; nXYPairs++;
  rho[nXYPairs] = 9.061927121262863e+01; nXYPairs++;
  rho[nXYPairs] = 9.661319308005639e+01; nXYPairs++;
#endif
  rho[nXYPairs] = 2.110364268933100e-02; nXYPairs++;
  rho[nXYPairs] = 5.995658018952331e-02; nXYPairs++;
  rho[nXYPairs] = 1.196715017153775e-01; nXYPairs++;
  rho[nXYPairs] = 1.700525027822127e-01; nXYPairs++;
  rho[nXYPairs] = 2.228800443839160e-01; nXYPairs++;
  rho[nXYPairs] = 2.550260372919732e-01; nXYPairs++;
  rho[nXYPairs] = 3.084651498624765e-01; nXYPairs++;
  rho[nXYPairs] = 3.692872795788326e-01; nXYPairs++;
  rho[nXYPairs] = 4.070240205500745e-01; nXYPairs++;
  rho[nXYPairs] = 4.639260564292166e-01; nXYPairs++;
  rho[nXYPairs] = 5.120093295474606e-01; nXYPairs++;
  rho[nXYPairs] = 5.657934870267318e-01; nXYPairs++;
  rho[nXYPairs] = 6.091891558370819e-01; nXYPairs++;
  rho[nXYPairs] = 6.629018810007644e-01; nXYPairs++;
  rho[nXYPairs] = 7.238695528366690e-01; nXYPairs++;
  rho[nXYPairs] = 7.729662314509249e-01; nXYPairs++;
  rho[nXYPairs] = 8.159519985556375e-01; nXYPairs++;
  rho[nXYPairs] = 8.679890266104804e-01; nXYPairs++;
  rho[nXYPairs] = 9.035948868798767e-01; nXYPairs++;
  rho[nXYPairs] = 9.652259631917983e-01; nXYPairs++;
  rho[nXYPairs] = 1.000457493914799e+00; nXYPairs++;
  rho[nXYPairs] = 1.056119365655867e+00; nXYPairs++;
  rho[nXYPairs] = 1.103475617879381e+00; nXYPairs++;
  rho[nXYPairs] = 1.170145913630051e+00; nXYPairs++;
  rho[nXYPairs] = 1.203956781381579e+00; nXYPairs++;
  rho[nXYPairs] = 1.260060608673328e+00; nXYPairs++;
  rho[nXYPairs] = 1.303279598930722e+00; nXYPairs++;
  rho[nXYPairs] = 1.352752584039008e+00; nXYPairs++;
  rho[nXYPairs] = 1.425001864288628e+00; nXYPairs++;
  rho[nXYPairs] = 1.455483649810371e+00; nXYPairs++;
  rho[nXYPairs] = 1.512847668691631e+00; nXYPairs++;
  rho[nXYPairs] = 1.570999256972138e+00; nXYPairs++;
  rho[nXYPairs] = 1.615335230011535e+00; nXYPairs++;
  rho[nXYPairs] = 1.657417920401189e+00; nXYPairs++;
  rho[nXYPairs] = 1.715953009748530e+00; nXYPairs++;
  rho[nXYPairs] = 1.763119024389630e+00; nXYPairs++;
  rho[nXYPairs] = 1.812348957693162e+00; nXYPairs++;
  rho[nXYPairs] = 1.874325659622648e+00; nXYPairs++;
  rho[nXYPairs] = 1.907317553962643e+00; nXYPairs++;
  rho[nXYPairs] = 1.969285478243236e+00; nXYPairs++;

  for (i = 0; i < nXYPairs; i++)
    T[i] = 1.0e7;

  /* EOSPAC 6 tests */

  for (imatid = 0; imatid < nMaterialList; imatid++) {

    for (itest = 0; itest < nModelTests; itest++) {

      for (i = 0; i < modelTests[itest].nTables; i++) {
        tableHandle[itest] = 0;
        modelTests[itest].matID[i] = MaterialList[imatid];
      }

      printf ("\n*********************************************************************\n");
      printf ("*** EOSPAC 6 TEST CASE %i ***\n", itest+imatid*nModelTests);

      /* initialize table data objects */
      eos_CreateTables (&(modelTests[itest].nTables),
                        modelTests[itest].tableType, modelTests[itest].matID,
                        &tableHandle[itest], &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_CreateTables ERROR %i: %s\n", errorCode, errorMessage);
        for (i = 0; i < modelTests[itest].nTables; i++) {
          tableHandleErrorCode = EOS_OK;
          eos_GetErrorCode (&tableHandle[itest], &tableHandleErrorCode);
          if (tableHandleErrorCode != EOS_OK) {
            eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
            printf ("%i: eos_CreateTables ERROR %i: %s\n",
                    tableHandle[itest], tableHandleErrorCode, errorMessage);
          }
        }
        continue;
      }

      if (modelTests[itest].modelFlag) {
        printf ("--- Set option: type=%s (%i) ---\n",
                modelTests[itest].modelFlagLabel, modelTests[itest].modelFlag);
        /* set additional option */
        for (i = 0; i < modelTests[itest].nTables; i++) {
          eos_SetOption (&tableHandle[itest], &modelTests[itest].modelFlag, EOS_NullPtr,
                         &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          }
        }
      }
      /* Enable data dump to file */
      for (i = 0; i < modelTests[itest].nTables; i++) {
        if (modelTests[itest].dump == EOS_FALSE) continue;
        dump_cntr++;
        if (dump_cntr == 0) {
          eos_SetOption (&tableHandle[0], &EOS_DUMP_DATA, EOS_NullPtr,
                         &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          }
        }
        else {
          eos_SetOption (&tableHandle[itest], &EOS_APPEND_DATA, EOS_NullPtr,
                         &errorCode);
          if (errorCode != EOS_OK) {
            eos_GetErrorMessage (&errorCode, errorMessage);
            printf ("eos_SetOption ERROR %i: %s\n", errorCode, errorMessage);
          }
        }
      }

      /* load data into table data objects */
      printf ("--- Load data:");
      for (i = 0; i < modelTests[itest].nTables; i++)
        printf ("%c%s (%i)", (i>0?',':' '), get_tableType_str(modelTests[itest].tableType[i]), modelTests[itest].matID[i]);
      printf (" ---\n");
      eos_LoadTables (&(modelTests[itest].nTables), &tableHandle[itest], &errorCode);
      if (errorCode != EOS_OK) {
        eos_GetErrorMessage (&errorCode, errorMessage);
        printf ("eos_LoadTables ERROR %i: %s\n", errorCode, errorMessage);
        for (i = 0; i < modelTests[itest].nTables; i++) {
          tableHandleErrorCode = EOS_OK;
          eos_GetErrorCode (&tableHandle[itest], &tableHandleErrorCode);
          if (tableHandleErrorCode != EOS_OK) {
            eos_GetErrorMessage (&tableHandleErrorCode, errorMessage);
            printf ("%i: eos_LoadTables ERROR %i: %s\n",
                    tableHandle[itest], tableHandleErrorCode, errorMessage);
          }
          else {
            modelTests[itest].isLoaded[i] = EOS_FALSE;
          }
        }
        continue;
      }
      else {
        for (i = 0; i < modelTests[itest].nTables; i++) {
          modelTests[itest].isLoaded[i] = EOS_TRUE;
        }
      }
    }                             /* itest loop */

    { /* pack, destroy and unpack tables */

      EOS_CHAR *packedData = NULL;
      EOS_INTEGER packedTablesSize;

      /* get tables */
      eos_GetPackedTablesSize (&nModelTests, tableHandle, &packedTablesSize, &errorCode);
      packedData = (EOS_CHAR *) malloc (packedTablesSize);
      eos_GetPackedTables (&nModelTests, tableHandle, packedData, &errorCode);

      /* destroy all tables */
      eos_DestroyAll (&errorCode);

      /* pack tables */
      eos_SetPackedTables (&nModelTests, &packedTablesSize, packedData, tableHandle, &errorCode);
      free (packedData);

    }

    for (itest = 0; itest < nModelTests; itest++) {

      /* interpolate */
      for (i = 0; i < modelTests[itest].nTables; i++) {

        if (! modelTests[itest].isLoaded[i])
          continue;

        printf ("\n*********************************************************************\n");
        printf ("*** EOSPAC 6 TEST CASE %i ***\n", itest+imatid*nModelTests);
        if (modelTests[itest].modelFlag) {
          printf ("--- Set options: type=%s (%i) ---\n",
                  modelTests[itest].modelFlagLabel, modelTests[itest].modelFlag);
        }
        printf ("--- Loaded data:");
        for (j = 0; j < modelTests[itest].nTables; j++)
          printf ("%c%s (%i)", (i>0?',':' '), get_tableType_str(modelTests[itest].tableType[j]), modelTests[itest].matID[j]);
        printf (" ---\n");

        printf ("--- Interpolate using tableType %s for material %i ---\n",
                get_tableType_str(modelTests[itest].tableType[i]), modelTests[itest].matID[i]);
        eos_Interpolate (&tableHandle[itest], &nXYPairs,
                         modelTests[itest].x_ptr, modelTests[itest].y_ptr, modelTests[itest].f_ptr,
                         modelTests[itest].dFx_ptr, modelTests[itest].dFy_ptr,
                         &errorCode);
        if (errorCode != EOS_OK) {
          EOS_BOOLEAN equal;
          eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);
          eos_GetErrorMessage (&errorCode, errorMessage);
          printf ("eos_Interpolate ERROR %i: %s\n", errorCode, errorMessage);
          if (equal) {
            eos_CheckExtrap (&tableHandle[itest], &nXYPairs, modelTests[itest].x_ptr, modelTests[itest].y_ptr, extrapCode,
                             &errorCode);
            if (errorCode != EOS_OK) {
              printf ("eos_CheckExtrap ERROR %i: %s\n", errorCode, errorMessage);
            }
            else {
              for (j = 0; j < nXYPairs; j++) {
                if (extrapCode[j] != EOS_OK) {
                  printf
                    ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e, extrap: %i\n",
                     j, modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
                     modelTests[itest].dFx_ptr[j], modelTests[itest].dFy_ptr[j], extrapCode[j]);
                }
                else {
                  printf
                    ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n", j,
                     modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
                     modelTests[itest].dFx_ptr[j], modelTests[itest].dFy_ptr[j]);
                }
              }
            }
          }
        }
        else {
          for (j = 0; j < nXYPairs; j++) {
            printf ("  i=%i\tX = %13.6e, Y = %13.6e, F = %13.6e, dFx = %13.6e, dFy = %13.6e\n",
                    j, modelTests[itest].x_ptr[j], modelTests[itest].y_ptr[j], modelTests[itest].f_ptr[j],
                    modelTests[itest].dFx_ptr[j], modelTests[itest].dFy_ptr[j]);
          }
        }
      }
    }                             /* itest loop */


    /* Compare interpolation results of test */

    printf ("\n*********************************************************************\n");
    printf ("*** EOSPAC 6 INTERPOLATION RELATIVE DIFFERENCE COMPARISON(S) ***\n");

    for (itest = 0; itest < nModelTests; itest++) {

      EOS_INTEGER itest0 = itest + 1;
      EOS_REAL sum[3] = {0.0, 0.0, 0.0}, mean[3], stddev[3] = {0.0, 0.0, 0.0}, rdiff[3];

      if (get_dataTypeCategory(modelTests[itest].tableType[0]) == 0)
        continue;

      if (modelTests[itest].modelFlag != EOS_INVERT_AT_SETUP)
        continue;

      for (i = 0; i < modelTests[itest].nTables; i++) {

        if (! modelTests[itest].isLoaded[i])
          continue;

        printf ("--- TEST CASE %i vs. TEST CASE %i for material %i ---\n",
                itest+imatid*nModelTests, itest0+imatid*nModelTests, modelTests[itest].matID[i]);

#define reldiff(x,y) (y-x)/fabs(x)
        for (j = 0; j < nXYPairs; j++) {
          sum[0] += reldiff( modelTests[itest0].f_ptr[j],   modelTests[itest].f_ptr[j]   );
          sum[1] += reldiff( modelTests[itest0].dFx_ptr[j], modelTests[itest].dFx_ptr[j] );
          sum[2] += reldiff( modelTests[itest0].dFy_ptr[j], modelTests[itest].dFy_ptr[j] );
        }
        mean[0] = sum[0] / nXYPairs;
        mean[1] = sum[1] / nXYPairs;
        mean[2] = sum[2] / nXYPairs;

        for (j = 0; j < nXYPairs; j++) {
          rdiff[0] = reldiff( modelTests[itest0].f_ptr[j],   modelTests[itest].f_ptr[j]   );
          rdiff[1] = reldiff( modelTests[itest0].dFx_ptr[j], modelTests[itest].dFx_ptr[j] );
          rdiff[2] = reldiff( modelTests[itest0].dFy_ptr[j], modelTests[itest].dFy_ptr[j] );

          printf ("  i=%i\t                                      F = %13.6e, dFx = %13.6e, dFy = %13.6e\tfmcp_ignore\n",
                  j, rdiff[0], rdiff[1], rdiff[2]);

          stddev[0] += pow(rdiff[0] - mean[0], 2.0);
          stddev[1] += pow(rdiff[1] - mean[1], 2.0);
          stddev[2] += pow(rdiff[2] - mean[2], 2.0);
        }

        stddev[0] = sqrt(stddev[0] / nXYPairs);
        stddev[1] = sqrt(stddev[1] / nXYPairs);
        stddev[2] = sqrt(stddev[2] / nXYPairs);

        printf ("  mean\t                                      F = %13.6e, dFx = %13.6e, dFy = %13.6e\tfmcp_ignore\n",
                mean[0], mean[1], mean[2]);
        printf ("stddev\t                                      F = %13.6e, dFx = %13.6e, dFy = %13.6e\tfmcp_ignore\n",
                stddev[0], stddev[1], stddev[2]);
      }

    }                             /* itest loop */

    /* Destroy all data objects */
    eos_DestroyAll (&errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      printf ("eos_DestroyAll ERROR %i: %s\n", errorCode, errorMessage);
    }

  }                             /* imatid loop */

  free(tableHandle);

  return 0;

}
