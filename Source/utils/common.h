/*!
 * \file
 * \ingroup utils
 */
#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <utime.h>
#include <assert.h>
#include <math.h>
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 255
#endif
#include "TEST_FUNCTIONS.h"
#include "eos_Interface.h"
#include "getopt.h"

#include "eos_SaferMemoryAllocation.h"

EOS_INTEGER one   = 1;
EOS_INTEGER two   = 2;
EOS_INTEGER three = 3;
EOS_INTEGER four  = 4;
EOS_INTEGER five  = 5;
EOS_INTEGER six   = 6;
EOS_INTEGER seven = 7;
EOS_INTEGER eight = 8;
EOS_INTEGER nine  = 9;
EOS_INTEGER ten   = 10;

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

// define internal error codes
enum { OK = 0,                          /*   0 */
       InsufficientArguments,           /*   1 */
       InvalidsesSubtableIndex,         /*   2 */
       sesSubtableUnavailable,          /*   3 */
       InvalidsesTableNum,              /*   4 */
       NoStdoutPossible,                /*   5 */
       FprintfError,                    /*   6 */
       mallocError,                     /*   7 */
       InvalidsesFileName,              /*   8 */
       MissingsesFile,                  /*   9 */
       IncorrectNumberOfIndepVars,      /*  10 */
       InvalidIndepVar,                 /*  11 */
       rIndepVarRequired,               /*  12 */
       exceededQuicksortRecursionLimit, /*  13 */
       inverseInterpolationError,       /*  14 */
       InvalidDataType,                 /*  15 */
       fopenError,                      /*  16 */
       MissingConfigFile,               /*  17 */
       eospacError = 255                /* 255 */
};

char localErrorMessages[][120] = {
  /*   0 */  "No errors encountered",
  /*   1 */  "Insufficient arguments provided, given the defined options",
  /*   2 */  "Invalid sesSubtableIndex (3rd argument)",
  /*   3 */  "sesSubtableIndex is valid given sesTableNum, but the subtable is unavailable",
  /*   4 */  "Invalid sesTableNum (2nd argument)",
  /*   5 */  "Data dump error; no stdout",
  /*   6 */  "fprintf function error",
  /*   7 */  "malloc error",
  /*   8 */  "Invalid sesame file name",
  /*   9 */  "Missing sesame file",
  /*  10 */  "Incorrect number of independent variables specified",
  /*  11 */  "Invalid independent variables specified",
  /*  12 */  "The -r (density) independent variable is required",
  /*  13 */  "_QuickSort ERROR exceeded QUICKSORT_RECURSION_LIMIT",
  /*  14 */  "calculateCategory2 ERROR performing inverse interpolation",
  /*  15 */  "Invalid data type (2nd argument)",
  /*  16 */  "Cannot open specified input file",
  /*  17 */  "Configuration file was not found"
  /*  18 */  "The -x independent variable is required"
  /* 255 */
};

/*
 */
#define HEADER_L          1024
#define REVISION_NUMBER_L 128
#define TOOL_NAME_L       128
EOS_CHAR header[HEADER_L];
EOS_CHAR revision_number[REVISION_NUMBER_L];
EOS_CHAR tool_name[TOOL_NAME_L];
EOS_CHAR* get_header_str(EOS_CHAR *n, EOS_CHAR *r) {
  //get_machprec(1.11)                                                    get_machprec(1.11)
  EOS_CHAR *prefix = "Revision: ";
  EOS_CHAR tmp[HEADER_L];
  EOS_INTEGER start = 0;
  strncpy(tool_name,       n, TOOL_NAME_L-1);
  if (strstr(r,prefix))    start = strlen(prefix)+1;
  int l = strcspn(r+start,"$");
  strncpy(revision_number, r+start, l-1);
  l = sprintf(tmp,"%s(%s)",tool_name,revision_number);
  sprintf(header,"%s%*s",tmp,100-l,tmp);
  return header;
}

EOS_CHAR* get_version_str(EOS_CHAR *n, EOS_CHAR *r) {
  //get_machprec 1.11
  EOS_CHAR *prefix = "Revision: ";
  EOS_CHAR tmp[HEADER_L];
  EOS_INTEGER start = 0;
  strncpy(tool_name,       n, TOOL_NAME_L-1);
  if (strstr(r,prefix))    start = strlen(prefix)+1;
  int l = strcspn(r+start,"$");
  strncpy(revision_number, r+start, l-1);
  l = sprintf(tmp,"%s %s",tool_name,revision_number);
  sprintf(header,"%s",tmp);
  return header;
}

// **********************************************************************
//      Sort an array a[] into ascending numerical order using a recursive
//      Quicksort algorithm.
// 
// The arguments are:
// 	EOS_INTEGER n   input: number of elements in a[].
// 	EOS_REAL*   a   in/out: replaced on output by its sorted rearrangement.
// 	EOS_INTEGER err output: error code.
// 
// Returned Values:
//      EOS_INTEGER _QuickSort    Return maximum number levels of recursion
//                                reached during sorting.
// 
// **********************************************************************/
#define QUICKSORT_RECURSION_LIMIT 500
static EOS_INTEGER max_recursion_level = 0;
EOS_INTEGER _QuickSort (EOS_INTEGER N, EOS_REAL a[], EOS_INTEGER lvl,
			EOS_INTEGER *err)
{
  EOS_INTEGER i = 0, j = N - 1;
  EOS_REAL x = a[N / 2], h;

  *err = EOS_OK;

  if (lvl == 0)
    max_recursion_level = 0;

  if (lvl > max_recursion_level)
    max_recursion_level = lvl;

  //  partition
  do {
    while (a[i] < x)
      i++;
    while (a[j] > x)
      j--;
    if (i <= j) {
      h = a[i];
      a[i] = a[j];
      a[j] = h;
      i++;
      j--;
    }
  } while (i <= j);

  if (lvl > QUICKSORT_RECURSION_LIMIT) {
    *err = exceededQuicksortRecursionLimit;
    return max_recursion_level;
  }

  //  recursion
  if (0 < j)
    _QuickSort (j + 1, &(a[0]), lvl + 1, err);
  if (*err != EOS_OK)
    return max_recursion_level;

  if (i < N - 1)
    _QuickSort (N - i, &(a[i]), lvl + 1, err);

  return max_recursion_level;
}

// ****************************************************************
// calculateCategory2
// ****************************************************************
/*!
 * \brief This function will return interpolated values of a category 2 data type
 *        (i.e., inverted w.r.t. 2nd independent variable).
 *
 * \par STDERR
 *       All information and error messages are returned
 *
 * \param[in] sesMaterialNum  - EOS_INTEGER : material id number;
 *
 * \param[in] type            - EOS_INTEGER : EOSPAC 6 data type;
 *
 * \param[in] N               - EOS_INTEGER : extent of x[], y[] and returned array, f[];
 *
 * \param[in] *x              - EOS_REAL : first independent values;
 *
 * \param[in] *y              - EOS_REAL : second independent values;
 *
 * \param[out] *f             - EOS_REAL : interpolated values;
 *
 */
EOS_REAL* calculateCategory2(EOS_INTEGER sesMaterialNum, EOS_INTEGER type,
			     EOS_INTEGER N, EOS_REAL *x, EOS_REAL *y) {
  // local variables
  EOS_INTEGER err = EOS_OK, alt_TH = -1;
  EOS_REAL *f = NULL, *dfx = NULL, *dfy = NULL;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  eos_CreateTables (&one, &type, &sesMaterialNum, &alt_TH, &err);
  if (err != EOS_OK)
    return(NULL);

  eos_LoadTables (&one, &alt_TH, &err);
  if (err != EOS_OK)
    return(NULL);

  f = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
  dfx = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
  dfy = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));

  eos_Interpolate(&alt_TH, &N, x, y, f, dfx, dfy, &err);
  if (err != EOS_OK) {
    EOS_BOOLEAN equal;
    eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &err, &equal);
    if (! equal) {
      EOS_FREE(f);
      EOS_FREE(dfx);
      EOS_FREE(dfy);
    }
    eos_GetErrorMessage (&err, errorMessage);
    fprintf(stderr, "\n%s ERROR %d: %s\n", "calculateCategory2", err, errorMessage);
    if (! equal) return(NULL);
  }

  EOS_FREE(dfx);
  EOS_FREE(dfy);
  
  eos_DestroyTables(&one, &alt_TH, &err);
  if (err != EOS_OK) {
    EOS_FREE(f);
    return(NULL);
  }

  return(f);
}

/*!
 * \brief This function returns a random sampling of values using the latin hypercube logic.
 *
 * \par STDERR
 *       All information and error messages are returned
 *
 * \param[in] *argc       - int : number of variable arguments;
 *
 * \param[in] **argv      - char** : pointer to array of cmdline arguments;
 *
 */
int getSamples(int N, EOS_REAL v_lower, EOS_REAL v_upper, EOS_REAL *v) {
  int i, nparam=1;
  double *vdata, a=0, b=1;
  EOS_INTEGER err = EOS_OK;
  vdata = (double*)malloc(N*nparam*sizeof(EOS_REAL));
  latinCube(vdata, &a, &b, nparam, N);
  for (i=0;i<N;i++)
    v[i] = v_lower + (v_upper - v_lower) * (EOS_REAL)vdata[i];
  EOS_FREE(vdata);
  max_recursion_level = _QuickSort (N, v, 0, &err);

  if (option['v'].flag > 1) {
    printf("\n\n");
    printf("\tv_lower = %23.15e\tv_upper = %23.15e\n", v_lower, v_upper);
    for (i=0;i<N;i++)
      printf("\tgetSamples: v[%d] = %23.15e\n",i, v[i]);
    printf("\n");
  }

  return(err);
}

/* \brief Count independent variable options
 */
int countIndependentVariableOptions (char *p) {
  int indepVarOptsCnt = 0;

  while(*p != '\0') {
    if (option[(int)*p].flag && *p != ':') indepVarOptsCnt++;
    p++;
  }

  return indepVarOptsCnt;
}

/* \brief Print an error message if provided, and return original error code.
 */
int fatal(int err, char *caller, int argsc, char **args, char *msg) {
  char c = ' ';
  int i;

  fprintf(stderr, "\n%s ERROR %d: %s\n", caller, err, msg);
  fprintf(stderr,"\t(args:");
  for(i = 0; i<argsc; i++) {
    fprintf (stderr, "%c%s", c, args[i]);
    c = ',';
  }
  fprintf(stderr,")\n\n");
  return(err);
}

/* \brief Parse the second EOS_REAL value in a colon-delimited string (i.e., <x1>:<x2>).
 *        Return value is EOS_TRUE or EOS_FALSE upon exit.
 */
EOS_BOOLEAN get_UpperRangeValue(char *p, EOS_REAL *val) {
  while(*p != '\0') {
    if (*p == ':' && isdigit(*(p+1))) {
      *val = atof(p+1);
      return(EOS_TRUE);
    }
    p++;
  }
  return(EOS_FALSE);
}

/* \brief Copy file, old -> new.
 */
int copy(char *old, char *new) {
  FILE *from, *to;
  char ch;

  /* open source file */
  if((from = fopen(old, "rb"))==NULL) {
    printf("copy ERROR: Cannot open source file, %s\n", old);
    exit(1);
  }

  /* open destination file */
  if((to = fopen(new, "wb"))==NULL) {
    printf("copy ERROR: Cannot open destination file, %s\n", new);
    exit(1);
  }

  /* copy the file */
  while(!feof(from)) {
    ch = fgetc(from);
    if(ferror(from)) {
      printf("copy ERROR: Cannot read source file, %s\n", old);
      exit(1);
    }
    if(!feof(from)) fputc(ch, to);
    if(ferror(to)) {
      printf("copy ERROR: Cannot write destination file, %s\n", new);
      exit(1);
    }
  }

  if(fclose(from)==EOF) {
    printf("copy ERROR: Cannot close source file, %s\n", old);
    exit(1);
  }

  if(fclose(to)==EOF) {
    printf("copy ERROR: Cannot close destination file, %s\n", new);
    exit(1);
  }

  return OK;
}

/* sesame index file names */
static char *indexFileName = "./sesameFilesDir.txt";
static char *indexFileNameBackup = "./sesameFilesDir.get_sesame_data.bak";
static struct utimbuf timestamp;

/* \brief Create a sesame index file.
 */
EOS_INTEGER createIndexFile (EOS_CHAR *file) {
  int file_cnt = 0;
  FILE *fp = NULL;
  EOS_BOOLEAN fileExists;
  struct stat file_statbuf;

  fileExists = (!(stat (indexFileName, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
  if (fileExists) { /* move existing indexFile out of the way */
    /*
     * copy indexFileName to indexFileNameBackup
     */
    copy(indexFileName, indexFileNameBackup);
    /*
     * reset indexFileNameBackup times to those of indexFileName
     */
    timestamp.actime = file_statbuf.st_atime;
    timestamp.modtime = file_statbuf.st_mtime;
    utime(indexFileNameBackup, &timestamp);
    file_cnt++;
  }

  fp = fopen (indexFileName, "w");  /* try to open indexFileName */
  if (fp) {
    /* write *file to sesameFilesDir.txt */
    fprintf(fp, "%s\n", file);
    fprintf(fp, "END\n");
    fclose (fp);              /* close indexFileName */
    file_cnt++;
  }

  return(file_cnt); /* value to pass to cleanIndexFile function */
}

/* \brief Create a sesame index file.
 */
void cleanIndexFile (EOS_INTEGER file_cnt) {
  EOS_BOOLEAN fileExists;
  struct stat file_statbuf;

  if (file_cnt > 1) {
    fileExists = (!(stat (indexFileNameBackup, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
    if (fileExists) { /* restore indexFileNameBackup */
      /*
       * copy indexFileNameBackup to indexFileName
       */
      copy(indexFileNameBackup, indexFileName);
      /*
       * reset indexFileName times to those of indexFileNameBackup
       */
      timestamp.actime = file_statbuf.st_atime;
      timestamp.modtime = file_statbuf.st_mtime;
      utime(indexFileName, &timestamp);
      remove(indexFileNameBackup);
    }
  }
  else if (file_cnt > 0) {
    fileExists = (!(stat (indexFileName, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
    if (fileExists) { /* delete indexFileName */
      remove(indexFileName);
    }
  }
}

static EOS_CHAR fName[PATH_MAX];
static EOS_CHAR fNameBak[PATH_MAX];
static int fNameBakCreated = 0;

#include <time.h>

/* \brief Backup an existing TablesLoaded.dat file.
 */
int backupTablesLoadedFile() {
  EOS_BOOLEAN fileExists;
  struct stat file_statbuf;
  time_t current_time;
  EOS_CHAR *c_time_string = NULL;

  if (strlen(option['D'].arg) > 0) strncpy(fName, option['D'].arg, PATH_MAX);
  else                             strncpy(fName, "TablesLoaded.dat", PATH_MAX);

  /* If TablesLoaded.dat file does not exist, then do nothing more */
  fileExists = (!(stat ("TablesLoaded.dat", &file_statbuf)))?EOS_TRUE:EOS_FALSE;
  if (! fileExists) return 0;

  /* Get current time */
  current_time = time(NULL);

  /* Conditionally convert time to string */
  if (current_time != ((time_t)-1)) {
    c_time_string = (EOS_CHAR*)malloc(100 * sizeof(EOS_CHAR));
    if (c_time_string)
      sprintf(c_time_string, ".%d", (int)current_time);
  }

  {
    int err = 0;
    sprintf(fNameBak, "%s%s%s", "TablesLoaded.dat", ".bak", (c_time_string ? c_time_string : ""));
    EOS_FREE(c_time_string);
    err = rename("TablesLoaded.dat", fNameBak);
    if (err) {
      fprintf (stderr, "\nrename ERROR %d: could not rename file, %s -> %s\n\n", err, "TablesLoaded.dat", fNameBak);
      return err;
    }
    fprintf (stderr, "rename file, %s -> %s\n", "TablesLoaded.dat", fNameBak);
    fNameBakCreated++;
  }

  return 0;
}

/* \brief Rename TablesLoaded.dat to requested name and conditionally recover an existing
 *        TablesLoaded.dat.bak.* file.
 */
int recoverTablesLoadedFile() {
  EOS_BOOLEAN fileExists;
  struct stat file_statbuf;

  fileExists = (!(stat ("TablesLoaded.dat", &file_statbuf)))?EOS_TRUE:EOS_FALSE;
  if (fileExists && strcmp("TablesLoaded.dat", fName)) {
    int err = 0;
    err = rename("TablesLoaded.dat", fName);
    if (err) {
      fprintf (stderr, "\nrename ERROR %d: could not rename file, TablesLoaded.dat -> %s\n\n", err, fName);
      return err;
    }
    fprintf (stderr, "rename file, TablesLoaded.dat -> %s\n", fName);
  }

  if (fNameBakCreated && strcmp("TablesLoaded.dat", fName)) {
    int err = 0;
    err = rename(fNameBak, "TablesLoaded.dat");
    if (err) {
      fprintf (stderr, "\nrename ERROR %d: could not rename file, %s -> %s\n\n", err, fNameBak, "TablesLoaded.dat");
      return err;
    }
    fprintf (stderr, "rename file, %s -> %s\n", fNameBak, "TablesLoaded.dat");
  }

  return 0;
}
#endif
