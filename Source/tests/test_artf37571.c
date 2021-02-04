/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

/*! \file
 *  \ingroup C tests
 *  \brief Verify the public function, eos_GetTableMetaData, returns meta data,
 *         per a user's specific request, functions as advertised within a C code,
 *         and handles errors properly.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf37571">artf37571</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"

int main ()
{
  EOS_INTEGER err1, err2;

  enum {
    NTABLES_enum = 22
  };

  EOS_INTEGER tablehandles[NTABLES_enum];
  EOS_INTEGER itabletypes[NTABLES_enum] = {
    EOS_Comment, /* 101 table */
    EOS_Info,    /* 201 table */
    EOS_Pt_DT,   /* 301 table */
    EOS_Pic_DT,  /* 303 table */
    EOS_Pe_DT,   /* 304 table */
    EOS_Piz_DT,  /* 305 table */
    EOS_Pc_D,    /* 306 table */
    EOS_M_DT,    /* 321 table */
    EOS_Pv_T,    /* 401 table */
    EOS_Pm_D,    /* 411 table */
    EOS_Pf_D,    /* 412 table */
    EOS_Gs_D,    /* 431 table */
    EOS_Ogb,     /* 501 table */
    EOS_Kr_DT,   /* 502 table */
    EOS_Keo_DT,  /* 503 table */
    EOS_Zfo_DT,  /* 504 table */
    EOS_Kp_DT,   /* 505 table */
    EOS_Zfc_DT,  /* 601 table */
    EOS_Kec_DT,  /* 602 table */
    EOS_Ktc_DT,  /* 603 table */
    EOS_B_DT,    /* 604 table */
    EOS_Kc_DT    /* 605 table */
  };
  EOS_INTEGER ntables = NTABLES_enum;
  EOS_INTEGER matidList[] = { 3720 };
  EOS_INTEGER matids[NTABLES_enum];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR infoString[EOS_META_DATA_STRLEN];

  int i, j, k, passed = 0, failed = 0;

  EOS_INTEGER infoTypes[8] = {
    EOS_Material_Name,
    EOS_Material_Source,
    EOS_Material_Date,
    EOS_Material_Ref,
    EOS_Material_Composition,
    EOS_Material_Codes,
    EOS_Material_Phases,
    EOS_Material_Classification
  };
  EOS_CHAR *infoTypes_str[8] = {
    "EOS_Material_Name          ",
    "EOS_Material_Source        ",
    "EOS_Material_Date          ",
    "EOS_Material_Ref           ",
    "EOS_Material_Composition   ",
    "EOS_Material_Codes         ",
    "EOS_Material_Phases        ",
    "EOS_Material_Classification"
  };

  for (j=0; j<sizeof(matidList)/sizeof(matidList[0]); j++) {

    /* populate matidList[] */

    for (i=0; i<sizeof(matids)/sizeof(matids[0]); i++)
      matids[i] = matidList[j];

    /* create table handle */

    eos_CreateTables(&ntables,itabletypes,matids,tablehandles,&err1);
    if (err1 != EOS_OK) {
      eos_GetErrorMessage (&err1, errorMessage);
      printf("eos_CreateTables ERROR %d: %s\n", err1, errorMessage);
      for (i=0; i<ntables; i++) {
        eos_GetErrorCode (&tablehandles[i], &err2);
        if (err2 == EOS_OK) continue;
        eos_GetErrorMessage (&err2, errorMessage);
        printf("     TH %-3d ERROR %d: %s\n", tablehandles[i], err2, errorMessage);
      }
      failed++; /* increment failed test counter */
    }
    else {
      errorMessage[0] = '\0';
      passed++; /* increment passed test counter */
    }
    if (err1 != EOS_OK) {
      printf("*** %2i of %i tests passed\n", passed, passed+failed);
    }

    /* Load */
    eos_LoadTables(&ntables, tablehandles, &err1);
    if (err1 != EOS_OK) {
      eos_GetErrorMessage (&err1, errorMessage);
      printf("eos_LoadTables ERROR %d: %s\n", err1, errorMessage);
      for (i=0; i<ntables; i++) {
        eos_GetErrorCode (&tablehandles[i], &err2);
        if (err2 == EOS_OK) continue;
        eos_GetErrorMessage (&err2, errorMessage);
        printf("     TH %-3d ERROR %d: %s\n", tablehandles[i], err2, errorMessage);
      }
      failed++; /* increment failed test counter */
    }
    else {
      errorMessage[0] = '\0';
      passed++; /* increment passed test counter */
    }

    if (err1 != EOS_OK) {
      printf("*** %2i of %i tests passed\n", passed, passed+failed);
    }

    printf("\n Test eos_GetTableMetaData using material id %d\n", matids[j]);

    for (k=0; k<ntables; k++) {

      EOS_INTEGER category = EOS_Table_Type;
      eos_GetMetaData (&itabletypes[k], &category, infoString, &err1);
      if (err1 == EOS_OK)
        printf("\n     --- %s ---\n", infoString);

      for (i=0; i<sizeof(infoTypes)/sizeof(infoTypes[0]); i++) {

        /* fetch meta data for tablehandles[k] */
        infoString[0] = '\0';
        eos_GetTableMetaData (&tablehandles[k], &infoTypes[i], infoString, &err1);
        if (err1 == EOS_OK) {
          errorMessage[0] = '\0';
          printf("     %s: %s\n", infoTypes_str[i], infoString);
        }
        else {
          eos_GetErrorMessage(&err1, errorMessage);
          printf("     %s: %s\n", infoTypes_str[i], errorMessage);
          failed++; /* increment failed test counter */
          continue;
        }
        passed++; /* increment passed test counter */

      }

    }

    printf("     ---\n");

    eos_DestroyAll(&err1);

  }

  printf("\n*** %2i tests passed, %2i tests failed ***\n", passed, failed);

  return(0);
}
