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
 *  \brief Verify the user-define file name(s) are not lost in successive
 *         calles to eos_CreateTables when mixed with eos_SetDataFileName.
 *
 *  See SourceForge issue
 *  <a href="https://tf.lanl.gov/sf/go/artf36931">artf36931</a>
 *  for details.
 *
 * \note
 * MATIDS TO TEST: 9991 9992
 */

#include <stdio.h>
#include <stdlib.h>
#include "eos_Interface.h"
#include "unistd.h"

int main ()
{
  EOS_INTEGER err1, err2;

  EOS_INTEGER th[2];

  EOS_INTEGER itabletype;
  EOS_INTEGER infotype;
  EOS_INTEGER ntables, one = 1;
  EOS_INTEGER matid;
  EOS_CHAR filename[100];
  EOS_CHAR *symlinkname2 = "./sesameu";
  EOS_CHAR tmp[100];
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_CHAR infoString[EOS_META_DATA_STRLEN];

  int i, k, count;

  EOS_BOOLEAN lexist;
  EOS_BOOLEAN equal;

  ntables = 2;
  count = 0;

  /* create symlinks */
  strcpy(filename, "./tests/data/special_sesame1");
  for (i = 0; i < 10; i++) {
    if (_eos_fileExistsAndValid(filename)) break;
    sprintf(tmp, "./.%s", filename);
    strcpy(filename, tmp);
  }
  if (! _eos_fileExistsAndValid(filename)) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }

  /* create symbolic link to filename for secondhandle */
  symlink(filename, symlinkname2);
  if (! _eos_fileExistsAndValid(symlinkname2)) {
    printf("\n%2i %s\n", count, "tests passed");
    printf("ERROR: symlink(%s, %s) failed\n", filename, symlinkname2);
    return(-count-1);
  }

  /* FIRST TABLE */

  matid = 9991;
  itabletype =  EOS_T_DUt;

  /* create first table handle */

  eos_CreateTables(&one,&itabletype,&matid,&th[0],&err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER FIRST create tables err1=%6d: %s\n",err1,errorMessage);
  printf(" firsthandle = %6d\n",th[0]);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  eos_SetOption(&th[0], &EOS_DUMP_DATA, EOS_NullPtr, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", err1, errorMessage);
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* set the file name for first table handle */
  eos_SetDataFileName(&th[0], &matid, &itabletype, filename, &err1);
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

  /* SECOND TABLE */

  matid = 9992;
  itabletype =  EOS_T_DUt;

  /* create second table handle */

  eos_CreateTables(&one,&itabletype,&matid,&th[1],&err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
  }
  else {
    errorMessage[0] = '\0';
  }
  printf(" AFTER SECOND create tables err1=%6d: %s\n",err1,errorMessage);
  printf(" secondhandle = %6d\n",th[1]);

  if (err1 != EOS_OK) {
    printf("\n%2i %s\n", count, "tests passed");
    return(-count-1);
  }
  count++; /* increment passed test counter */

  eos_SetOption(&th[1], &EOS_APPEND_DATA, EOS_NullPtr, &err1);
  if (err1 != EOS_OK) {
    eos_GetErrorMessage (&err1, errorMessage);
    printf ("eos_SetOption ERROR %i: %s\n", err1, errorMessage);
    return(-count-1);
  }
  count++; /* increment passed test counter */

  /* Load all table handles */
  eos_LoadTables(&ntables, th, &err1);
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

  /* fetch meta data for firsthandle */
  infoString[0] = '\0';
  infotype = EOS_File_Name;
  eos_GetTableMetaData (&th[0], &infotype, infoString, &err1);
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

  /* fetch meta data for secondhandle */
  infoString[0] = '\0';
  infotype = EOS_File_Name;
  eos_GetTableMetaData (&th[1], &infotype, infoString, &err1);
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

  if ( ! strcmp(symlinkname2, infoString) ) {
    printf(" AFTER SECOND eos_GetTableMetaData filename == infoString 1\n");
    count++; /* increment passed test counter */
  }
  else {
    printf(" AFTER SECOND eos_GetTableMetaData filename != infoString 0\n");
  }

  printf("\n%2i %s\n", count, "tests passed");

  return(0);
}

