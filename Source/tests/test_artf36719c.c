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
 *  <a href="https://tf.lanl.gov/sf/go/artf36719">artf36719</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 3720 3721 3722
 * REQUIRED FILE: data/ses003721
 * REQUIRED FILE: data/ses003722
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"

/* prototype defined in src/eos_Utils.proto.h, and function declared in src/eos_Utils.c */
EOS_BOOLEAN _eos_fileExistsAndValid (EOS_CHAR * filename);

int main ()
{
  EOS_INTEGER err1, err2;

  EOS_INTEGER firsthandle;
  EOS_INTEGER secondhandle;
  EOS_INTEGER thirdhandle;

  EOS_INTEGER itabletype;
  EOS_INTEGER infotype;
  EOS_INTEGER ntables;
  EOS_INTEGER matid;
  EOS_CHAR filename[100];
  EOS_CHAR tmp[200];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR infoString[EOS_META_DATA_STRLEN];

  int i, count;

  EOS_BOOLEAN equal;

  ntables = 1;
  count = 0;

  /* FIRST TABLE */

  matid = 3721;
  itabletype =  EOS_T_DUt;
  strcpy(filename, "./tests/data/ses003721");
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(filename)) break;
    sprintf(tmp, "./.%s", filename);
    strcpy(filename, tmp);
  }
  if (! _eos_fileExistsAndValid(filename)) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }

  /* create first table handle */

  eos_CreateTables(&ntables,&itabletype,&matid,&firsthandle,&err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER FIRST create tables err1=%6d: %s\n",err1,errorMessage);
  printf(" firsthandle = %6d\n",firsthandle);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* set the file name for first table handle */
  eos_SetDataFileName(&firsthandle, &matid, &itabletype, filename, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER FIRST SetDataFileName err1=%6d itabletype=%6d: %s\n",err1,itabletype,errorMessage);
  printf("               fcmp_ignore filename = %s\n", filename);

  err2 = EOS_OPEN_SESAME_FILE_FAILED;
  eos_ErrorCodesEqual(&err2, &err1, &equal);
  if ( equal ) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* Load first table handle */
  eos_LoadTables(&ntables, &firsthandle, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER FIRST load tables err1=%6d: %s\n",err1,errorMessage);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* fetch meta data for firsthandle */
  infoString[0] = '\0';
  infotype = EOS_File_Name;
  eos_GetTableMetaData (&firsthandle, &infotype, infoString, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
    printf(" AFTER FIRST eos_GetTableMetaData err1=%6d: %s\n",err1,errorMessage);
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  else {
    errorMessage[0] = '\0';
    printf(" AFTER FIRST eos_GetTableMetaData filename: .../tests/data%s\n",strrchr(infoString,'/'));
  }
  count++; /* increment passed test counter */

  if ( ! strcmp(filename, infoString) ) {
    printf(" AFTER FIRST eos_GetTableMetaData filename == infoString 1\n");
    count++; /* increment passed test counter */
  }
  else {
    printf(" AFTER FIRST eos_GetTableMetaData filename != infoString 0\n");
  }

  /* SECOND TABLE */

  matid = 3722;
  itabletype =  EOS_Pt_DUt;
  strcpy(filename, "./tests/data/ses003722");
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(filename)) break;
    sprintf(tmp, "./.%s", filename);
    strcpy(filename, tmp);
  }
  if (! _eos_fileExistsAndValid(filename)) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }

  /* create second table handle */

  eos_CreateTables(&ntables,&itabletype,&matid,&secondhandle,&err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER SECOND create tables err1=%6d: %s\n",err1,errorMessage);
  printf(" secondhandle = %6d\n",secondhandle);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* set the file name for second table handle */
  eos_SetDataFileName(&secondhandle, &matid, &itabletype, filename, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER SECOND SetDataFileName err1=%6d itabletype=%6d: %s\n",err1,itabletype,errorMessage);
  printf("               fcmp_ignore filename = %s\n", filename);

  err2 = EOS_OPEN_SESAME_FILE_FAILED;
  eos_ErrorCodesEqual(&err2, &err1, &equal);
  if ( equal ) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* Load second table handle */
  eos_LoadTables(&ntables, &secondhandle, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER SECOND load tables err1=%6d: %s\n",err1,errorMessage);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* fetch meta data for secondhandle */
  infoString[0] = '\0';
  infotype = EOS_File_Name;
  eos_GetTableMetaData (&secondhandle, &infotype, infoString, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
    printf(" AFTER SECOND eos_GetTableMetaData err1=%6d: %s\n",err1,errorMessage);
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  else {
    errorMessage[0] = '\0';
    printf(" AFTER SECOND eos_GetTableMetaData filename: .../tests/data%s\n",strrchr(infoString,'/'));
  }
  count++; /* increment passed test counter */

  if ( ! strcmp(filename, infoString) ) {
    printf(" AFTER SECOND eos_GetTableMetaData filename == infoString 1\n");
    count++; /* increment passed test counter */
  }
  else {
    printf(" AFTER SECOND eos_GetTableMetaData filename != infoString 0\n");
  }

  /* THIRD TABLE */

  matid = 3720;
  itabletype =  EOS_Pt_DT;

  /* create third table handle */

  eos_CreateTables(&ntables,&itabletype,&matid,&thirdhandle,&err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER THIRD create tables err1=%6d: %s\n",err1,errorMessage);
  printf(" thirdhandle = %6d\n",thirdhandle);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* Load third table handle */
  eos_LoadTables(&ntables, &thirdhandle, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER THIRD load tables err1=%6d: %s\n",err1,errorMessage);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* fetch meta data for thirdhandle */
  infoString[0] = '\0';
  infotype = EOS_File_Name;
  eos_GetTableMetaData (&thirdhandle, &infotype, infoString, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
    printf(" AFTER THIRD eos_GetTableMetaData err1=%6d: %s\n",err1,errorMessage);
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  else {
    errorMessage[0] = '\0';
    printf(" AFTER THIRD eos_GetTableMetaData filename: .../projects/eos/data%s\n",strrchr(infoString,'/'));
  }
  count++; /* increment passed test counter */

  printf("\n%2i %s\n", count, "tests passed");

  eos_DestroyAll (&err1);

  return(0);
}

