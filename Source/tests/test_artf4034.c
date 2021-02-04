/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup C tests
 * \brief Test end of file search token usage within "sesameFilesDir.txt".
 *        Test MATID matid-to-file association token usage within "sesameFilesDir.txt".
 *        See SourceForge© Issue #artf4034 for more details:
 *        https://tf.lanl.gov/sf/go/artf4034
 *
 * \note
 * MATIDS TO TEST: 3720 9001
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

EOS_BOOLEAN _eos_fileExistsAndValid(EOS_CHAR *filename);
int copy_file(char *source, char *target);

int main ()
{

  int i;
  char *indexFileName = "sesameFilesDir.txt";
  FILE *fp = NULL;
  EOS_CHAR *file_str = NULL;

  extern EOS_CHAR **sesameFiles;
  extern EOS_INTEGER sesameFilesL;

  int err = EOS_OK;

  printf ("Comprehensive file list:\n");
  err = print_FileList(-1, "<...>/");

  if (err) return(err);

  /* Reset the file list */
  if (sesameFiles) {
    for (i = 0; i < sesameFilesL; i++) {
      EOS_FREE(sesameFiles[i]);
    }
    EOS_FREE(sesameFiles);
    sesameFilesL = 0;
  }

  /*  Read sesameFilesDir.txt into memory */
  err = get_fileContent(indexFileName, &file_str);
  if(err)
    return(err);

  /* Modify sesameFilesDir.txt to include END token */
  if (_eos_fileExistsAndValid(indexFileName)) {
    fp = fopen (indexFileName, "a");  /* open indexFileName */
    if (fp) {
      fprintf(fp, "\n    END    \n"); /* append END token with leading and trailing white space */
      fclose (fp);              /* close indexFileName */
    }
    else {
      err = -2;
      return(err);
    }
  }

  printf ("\nLimited file list:\n");
  err = print_FileList(-1, "<...>/");
  if (err)
    return(err);

  {     /* load data to test usage of sesame3 */
    EOS_INTEGER nTables = 1;
    EOS_INTEGER tableType = EOS_Info;
    EOS_INTEGER matID = 9001;
    EOS_INTEGER tableHandle;
    EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      printf ("eos_CreateTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
	      matID, err, errorMessage);
    }

    if (err == EOS_OK) {
      eos_SetOption (&tableHandle, &EOS_DUMP_DATA, EOS_NullPtr, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_SetOption (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

    if (err == EOS_OK) {
      eos_LoadTables (&nTables, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_LoadTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

  }

  {     /* load data to test usage of sesame3 and unknown material id */
    EOS_INTEGER nTables = 1;
    EOS_INTEGER tableType = EOS_Info;
    EOS_INTEGER matID = 999999;
    EOS_INTEGER tableHandle;
    EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      printf ("eos_CreateTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
	      matID, err, errorMessage);
    }

    if (err == EOS_OK) {
      eos_SetOption (&tableHandle, &EOS_APPEND_DATA, EOS_NullPtr, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_SetOption (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

    if (err == EOS_OK) {
      eos_LoadTables (&nTables, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_LoadTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

  }

  /*  Write file_str to sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s", file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -3;
    goto CLEANUP;
  }

  /* copy sesameFiles[0] to ./sesame3.copy */
  err = copy_file(sesameFiles[0], "./sesame3.copy");
  if (err)
    goto CLEANUP;

  /* Modify sesameFilesDir.txt to include MATID tokens */
  if (_eos_fileExistsAndValid(indexFileName)) {
    fp = fopen (indexFileName, "a");  /* open indexFileName */
    if (fp) {
      /* append sesame3.copy references to original index file */
      fprintf(fp, "%s\n", "MATID 9001 sesame3.copy");
      fprintf(fp, "%s\n", "MATID 3720 sesame3.copy");
      fclose (fp);              /* close indexFileName */
    }
    else {
      err = -2;
      goto CLEANUP;
    }
  }

  printf ("\nExtended file list:\n");
  err = print_FileList(-1, "<...>/");
  if (err)
    goto CLEANUP;

  /*  Write file_str to sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s", file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -3;
    goto CLEANUP;
  }

  {     /* load data to test usage of sesame3.copy */
    EOS_INTEGER nTables = 1;
    EOS_INTEGER tableType = EOS_Info;
    EOS_INTEGER matID = 9001;
    EOS_INTEGER tableHandle;
    EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      printf ("eos_CreateTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
	      matID, err, errorMessage);
    }

    if (err == EOS_OK) {
      eos_SetOption (&tableHandle, &EOS_APPEND_DATA, EOS_NullPtr, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_SetOption (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

    if (err == EOS_OK) {
      eos_LoadTables (&nTables, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_LoadTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

  }

  {     /* load data to test usage of sesame3.copy */
    EOS_INTEGER nTables = 1;
    EOS_INTEGER tableType = EOS_Info;
    EOS_INTEGER matID = 3720;
    EOS_INTEGER tableHandle;
    EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

    eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &err);
    if (err != EOS_OK) {
      eos_GetErrorMessage (&err, errorMessage);
      printf ("eos_CreateTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
	      matID, err, errorMessage);
    }

    if (err == EOS_OK) {
      eos_SetOption (&tableHandle, &EOS_APPEND_DATA, EOS_NullPtr, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_SetOption (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

    if (err == EOS_OK) {
      eos_LoadTables (&nTables, &tableHandle, &err);
      if (err != EOS_OK) {
	eos_GetErrorMessage (&err, errorMessage);
	printf ("eos_LoadTables (matid=%d) ERROR %i: %s (fcmp_ignore)\n",
		matID, err, errorMessage);
      }
    }

  }

  /*  Write file_str to sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s", file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -3;
    goto CLEANUP;
  }

  /* delete ./sesame3.copy */
  err = unlink("./sesame3.copy");
  if (err)
    goto CLEANUP;

 CLEANUP:
  EOS_FREE (file_str);
  {
    EOS_INTEGER e;
    eos_DestroyAll(&e);
  }
  return(err);

}

int copy_file(char *source, char *target) {

  FILE *from = NULL;
  FILE *to = NULL;
  char buffer;


  if((from = fopen(source, "rb"))==NULL) {
    printf("Cannot open source file, %s\n", source);
    exit(1);
  }

  /* open destination file */
  if((to = fopen(target, "wb"))==NULL) {
    printf("Cannot open destination file, %s\n", target);
    return(1);
  }

  /* copy the file */
  while(!feof(from)) {
    buffer = fgetc(from);
    if(ferror(from)) {
      printf("Error reading source file, %s\n", source);
      return(2);
    }
    if(!feof(from)) fputc(buffer, to);
    if(ferror(to)) {
      printf("Error writing destination file, %s\n", target);
      return(3);
    }
  }

  if(fclose(from)==EOF) {
    printf("Error closing source file, %s\n", source);
    return(4);
  }

  if(fclose(to)==EOF) {
    printf("Error closing destination file, %s\n", target);
    return(5);
  }

  return(0);
}
