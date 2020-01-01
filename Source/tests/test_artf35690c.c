/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 *********************************************************************/

/*! \file
 *  \ingroup tests
 *  \brief Verify the new public function, eos_GetTableMetaData, returns meta data,
 *         per a user's specific request, functions as advertised within a C code.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf35690">artf35690</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 2161 3720
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"

int main ()
{
  EOS_INTEGER err1;

  EOS_INTEGER tablehandle;
  EOS_INTEGER itabletype = EOS_Comment;
  EOS_INTEGER ntables = 1;
  EOS_INTEGER matids[2] = { 2161, 3720 };
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR infoString[EOS_META_DATA_STRLEN];

  int i, j, count = 0;

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

  for (j=0; j<sizeof(matids)/sizeof(matids[0]); j++) {

    /* create table handle */

    eos_CreateTables(&ntables,&itabletype,&matids[j],&tablehandle,&err1);
    if (err1 != EOS_OK) {
      eos_GetErrorMessage (&err1, errorMessage);
    }
    else {
      errorMessage[0] = '\0';
    }
    printf("\n AFTER create tables err1=%6d: %s\n",err1,errorMessage);
    printf(" tablehandle = %6d\n",tablehandle);

    if (err1 != EOS_OK) {
      printf("\n%2i %s\n", count, "tests passed");
      return(-count-1);
    }
    count++; /* increment passed test counter */

    /* Load */
    eos_LoadTables(&ntables, &tablehandle, &err1);
    if (err1 != EOS_OK) {
      eos_GetErrorMessage (&err1, errorMessage);
    }
    else {
      errorMessage[0] = '\0';
    }
    printf(" AFTER load tables err1=%6d: %s\n",err1,errorMessage);

    if (err1 != EOS_OK) {
      printf("\n%2i %s\n", count, "tests passed");
      return(-count-1);
    }
    count++; /* increment passed test counter */

    printf("\n Test eos_GetTableMetaData using material id %d\n", matids[j]);

    for (i=0; i<sizeof(infoTypes)/sizeof(infoTypes[0]); i++) {

      /* fetch meta data for tablehandle */
      infoString[0] = '\0';
      eos_GetTableMetaData (&tablehandle, &infoTypes[i], infoString, &err1);
      if (err1 != EOS_OK) {
        eos_GetErrorMessage (&err1, errorMessage);
        printf(" AFTER eos_GetTableMetaData err1=%6d: %s\n",err1,errorMessage);
        printf("\n%2i %s\n", count, "tests passed");
        return(-count-1);
      }
      else {
        errorMessage[0] = '\0';
        printf("     %s: %s\n", infoTypes_str[i], infoString);
      }
      count++; /* increment passed test counter */

    }

    eos_DestroyAll(&err1);

  }

  printf("\n%2i %s\n", count, "tests passed");

  return(0);
}
