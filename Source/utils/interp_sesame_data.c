/*********************************************************************
 * Utility Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
/*!
 * \file
 * \ingroup utils
 * \brief
 * Given a Sesame material ID, a Sesame table number, an EOSPAC 6 table type and independent
 * variable values (or ranges) appropriate to the table type, interpolate the function
 * value(s) and partial derivative values.
 *
 * \pre libeospac6.a (version 6.3 or later) is required
 *
 * \author David A. Pimentel
 *
 * \par STDOUT
 * Interpolated values are returned.
 *
 * \par STDERR
 * All error messages are returned.
 *
 * \par USAGE
 * interp_sesame_data [<OPTIONS>] <sesMaterialNum> <tableType> <x>[:<x1>] [ <y>[:<y1>] ]\n
 * interp_sesame_data [<OPTIONS>] <sesMaterialNum> <tableType> -i <file>
 *
 * \arg <sesMaterialNum>
 *                 Sesame material ID number
 * \arg <tableType>
 *                 EOSPAC 6 table type (case insensitive)
 * \arg <x>
 *                 First independent variable value of the table type (64-bit floating point)
 *                 This argument is required unless either the '-i' or the '-x' option is used.
 *                 The optional :<x1> defines an upper bound for a randomly-sampled range of
 *                 values between <x> and <x1>.
 * \arg <y>
 *                 Second independent variable value of the table type (64-bit floating point)
 *                 This argument is required unless either the '-i' or the '-y' option is used.
 *                 The optional :<y1> defines an upper bound for a randomly-sampled range of
 *                 values between <y> and <y1>.
 * <br>
 * \arg <OPTIONS>
 * \arg -d
 *              Display double precision interpolated output.
 * \arg -e
 *              Allow extrapolation to continue with warning(s).
 * \arg -F <file>
 *              Search named Sesame <file> exclusively.
 * \arg -h
 *              Display the basic usage of this tool.
 * \arg -g
 *              Return gnuplot compatible output.
 * \arg -I N
 *              Number of samples, N, to take from the x-independent variable's range. Default=50.
 * \arg -i <file>
 *              Parse <file>, which contains two columns of data where the <xValue> and <yValue> input
 *              correspond to columns 1 and 2 respectively.
 *              Text trailing a # character will be ignored as notes.
 *              If <file> is the '-' character, then stdin is parsed.
 *              This option overrides the '-x' and '-y' options.
 * \arg -J N
 *              Number of samples, N, to take from the y-independent variable's range. Default=50.
 * \arg -L
 *              Use EOSPAC's linear interpolation instead of the default rational interpolation.
 * \arg -M v
 *              Specify the independent variable, v, with respect to which EOSPAC must enforce
 *              monotonicity.
 *              Valid case-insensitive value, v: 'x' or 'y'
 * \arg -n N
 *              Number of samples, N, to take from each independent variable's range. Default=50.
 *              This option overrides the '-I' and '-J' options.
 * \arg -s
 *              Ignored unless '-g' option is used. Ensure the output will be compatible with
 *              gnuplot's 'splot' function.
 * \arg -V
 *              Display the version number.
 * \arg -v
 *              Enable verbose output. Multiple instances of this option will increase the
 *              verbosity.
 * \arg -x (<file>|<list>)
 *              Parse <file>, which contains one column of <xValue> data. This overrides the <x>
 *              command line argument, subsequently making <x> not required.
 *              Text trailing a # character will be ignored as notes.
 *              If <list> is a comma-delimited list of numbers (i.e., <x>[,<x1>[,<x2>[,...]]]),
 *              then it is parsed as a list instead of a <file> containing discreet values.
 *              This option is gnored if the '-i' is used.
 *              
 * \arg -y (<file>|<list>)
 *              Parse <file>, which contains one column of <yValue> data. This overrides the <y>
 *              command line argument, subsequently making <y> not required.
 *              Text trailing a # character will be ignored as notes.
 *              If <list> is a comma-delimited list of numbers (i.e., <y>[,<y1>[,<y2>[,...]]]),
 *              then it is parsed as a list instead of a <file> containing discreet values.
 *              This option is gnored if the '-i' is used.
 *
 * \par References
 *           -# D. A. Pimentel,
 *              "EOSPAC USER'S MANUAL: Version 6.3", LA-UR-14-29289,
 *              Los Alamos National Laboratory (2014).
 *           -# S. P. Lyon, J. D. Johnson, Group T-1,
 *              "Sesame: The Los Alamos National Laboratory Equation of State Database", LA-UR-92-3407,
 *              Los Alamos National Laboratory (1992).
 *
 * \retval      0     No errors encounterd
 * \retval      1     Insufficient arguments passed to this utility
 * \retval      5     Data dump error; no stdout
 * \retval      6     fprintf function error; probably system-related
 * \retval      7     memory allocation error
 * \retval     15     Invalid table type (2nd argument)
 * \retval     16     Cannot open specified input file
 * \retval    255     EOSPAC related error occurred; corresponding message written to stderr
 *
 */

#include "common.h"
#include "interp_sesame_data_help.h"

#ifdef _DEBUG_PACKING_FUNCTIONS
  extern EOS_INTEGER packedBytesResetOverride;
#endif

extern EOS_BOOLEAN disable_SetPackedTablesPrint;

/******************************************************************************/
/* This routine reads the next line of an opened text file referenced by      */
/* a file pointer, fp.                                                        */
/*                                                                            */
/* Returned Values:                                                           */
/* EOS_INTEGER _eos_readLine - status of read (zero indicates failure or EOF) */
/* EOS_CHAR    **line       - character string/array passed by reference      */
/*                                                                            */
/* Input Value:                                                               */
/* FILE        *fp          - text file pointer                               */
/*                                                                            */
/******************************************************************************/
EOS_INTEGER readLine(FILE * fp, EOS_CHAR **line) {
  EOS_CHAR *in = NULL;
  EOS_INTEGER L1 = 0, L2 = 0;
  EOS_CHAR *p1, *p2;
  EOS_INTEGER failed;
  EOS_INTEGER bufsize = 1024;

  in = (EOS_CHAR*) safe_malloc(bufsize, sizeof(EOS_CHAR));

  if (! *line) {
    *line = (EOS_CHAR*) safe_malloc(bufsize, sizeof(EOS_CHAR));
    (*line)[0] = '\0'; /* initialize to empty string */
  }

  while (fgets(in, bufsize, fp)) {
    L1 = strlen(*line);
    L2 = strlen(in);
    *line = safe_realloc (*line, (L1 + L2 + 1), sizeof (EOS_CHAR));
    strcat (*line, in);
    p1 = strchr (*line, '\n');
    p2 = strchr (*line, '\r');
    if (p1 || p2) {
      if (p1) *p1 = '\0';
      if (p2) *p2 = '\0';
      break;
    }
  }

  failed = ((in) ? 0 : 1);

  EOS_FREE(in);

  return((! feof(fp)) && ! failed);
}

/******************************************************************************/
/*                                                                            */
/* Function Name: parseInput                                                  */
/* This routine parses the contents of a text file, fName, with either one or */
/* two real values per line. The real values one each line are x[i] and y[i]  */
/* respectively.                                                              */
/* The parsing rules are defined as follows:                                  */
/*    1. Delimiters are linefeed, carriage return, semi-colon, white space.   */
/*    2. Comments are ignored and begin with #.                               */
/*    3. Leading white space is ignored.                                      */
/*                                                                            */
/* Returned Values:                                                           */
/* EOS_INTEGER _eos_parseInput - error code                                   */
/* EOS_INTEGER *N    - extent of x and y arrays                               */
/* EOS_REAL    **x   - array of double values                                 */
/* EOS_REAL    **y   - array of double values                                 */
/*                                                                            */
/* Input Value:                                                               */
/* EOS_CHAR *fName   - path location of text file                             */
/*                                                                            */
/******************************************************************************/
EOS_INTEGER parseInput (EOS_CHAR *fName, EOS_INTEGER *N, EOS_REAL **x, EOS_REAL **y)
{
  EOS_CHAR *tmp0 = NULL, *tmp = NULL;
  EOS_INTEGER err = EOS_OK;
  EOS_CHAR **split_result = NULL;
  FILE *fp = NULL;
  EOS_CHAR *delim = "	 ,;";
  EOS_CHAR *comma = ",";
  int ntokens, line = 0, totLines = 0;
  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];

  if (strcmp(fName, "-")) {
    EOS_BOOLEAN fileExists;
    struct stat file_statbuf;
    fileExists = (!(stat (fName, &file_statbuf)))?EOS_TRUE:EOS_FALSE;
    if (! fileExists && ! strstr(fName, ",")) {
      err = fopenError;
      return err;
    }

    if (! strstr(fName, ","))
      fp = fopen(fName, "rb");
    else
    { /* comma-delimited list */
      int i;
      *N = split (fName, comma, &split_result);

      if (x)
	*x = (EOS_REAL*) safe_malloc(*N, sizeof(EOS_REAL));
      else
	return mallocError;

      for(i=0;i<*N;i++) {
	EOS_REAL v;
	v = strtod(split_result[i], NULL);
	(*x)[i] = v;
      }

      return OK;
    }
  }
  else {
    fp = stdin;
  }

  if (! fp)
    return fopenError;

  if (x) EOS_FREE (*x);
  if (y) EOS_FREE (*y);

  if (strcmp(fName, "-")) {
    /* count lines in fp */
    while (readLine (fp, &tmp)) { /* tmp is reallocated */
      totLines++;

      EOS_FREE (tmp);
    }
    /* rewind fp */
    err = fseek (fp, 0, SEEK_SET);
  }
  else {
    err = 0;
  }

  if (! err) {

    if (strcmp(fName, "-")) {
      if (x) *x = (EOS_REAL*) safe_malloc(totLines, sizeof(EOS_REAL));
      if (y) *y = (EOS_REAL*) safe_malloc(totLines, sizeof(EOS_REAL));
    }
    else {
      if (x) *x = (EOS_REAL*) safe_malloc(1, sizeof(EOS_REAL));
      if (y) *y = (EOS_REAL*) safe_malloc(1, sizeof(EOS_REAL));
    }

    while (readLine (fp, &tmp)) { /* tmp is reallocated */

      char *endp;
      int l;

      /* remove note(s) from tmp */
      l = strcspn(tmp, "#");
      if (l <= 0) {
	EOS_FREE (tmp);
	continue;
      }
      tmp0 = (EOS_CHAR*) safe_malloc(l+1, sizeof(EOS_CHAR));
      tmp0 = strncpy(tmp0, tmp, l);
      EOS_FREE (tmp);
      tmp = tmp0;

      /* split tmp into tokens */
      ntokens = split (tmp, delim, &split_result);

      if (ntokens <= 0) {
	EOS_FREE (tmp);
	continue;
      }

      errno = 0;
      strtod(split_result[0], &endp);

      if (ntokens <= 0 || !(split_result[0] != endp && *endp == '\0' && ! errno)) continue;

      line++; /* increment line counter */

      sprintf(errorMessage, "input line %d : %s", line, tmp);
      debug(3, (char*)__FILE__, (int)__LINE__, errorMessage);

      if (! strcmp(fName, "-")) {
	if (x) *x = safe_realloc((void*) *x, line, sizeof(EOS_REAL));
	if (y) *y = safe_realloc((void*) *y, line, sizeof(EOS_REAL));
      }

      if (x && ntokens > 0) {
	(*x)[line-1] = strtod(split_result[0], &endp);
	sprintf(errorMessage, "input line %d : *x[%d]=%g", line, line-1, (*x)[line-1]);
	debug(4, (char*)__FILE__, (int)__LINE__, errorMessage);
      }
      if (y && ntokens > 1) {
	(*y)[line-1] = strtod(split_result[1], &endp);
	sprintf(errorMessage, "input line %d : *y[%d]=%g", line, line-1, (*y)[line-1]);
	debug(4, (char*)__FILE__, (int)__LINE__, errorMessage);
      }
      else if (y)
	(*y)[line-1] = 0;

      EOS_FREE (tmp);

    }

  }

  if (strcmp(fName, "-"))
    fclose (fp);

  *N = line;

  return OK;
}

void display_usage(char *argv[]) {
  fprintf(stderr,
	  "\nUSAGE: %s [<OPTIONS>] <sesMaterialNum> <tableType> <x>[:<x1>] [ <y>[:<y1>] ]\n"
	  "\n"
	  "       <sesMaterialNum>  \t- Sesame material ID number\n"
	  "       <tableType>       \t- EOSPAC 6 table type (case insensitive)\n"
	  "       <x>               \t- First independent variable value of the table type (64-bit floating point)\n"
	  "                         \t  The optional :<x1> defines an upper bound for a randomly-sampled range of\n"
	  "                         \t  values between <x> and <x1>. This argument is required unles the '-x' option\n"
	  "                         \t  is used.\n"
	  "       <y>               \t- Second independent variable value of the table type (64-bit floating point)\n"
	  "                         \t  The optional :<y1> defines an upper bound for a randomly-sampled range of\n"
	  "                         \t  values between <y> and <y1>. This argument is required unles the '-y' option\n"
	  "                         \t  is used.\n\n"
	  "See %s.readme or use -h option for more details and a complete description of the <OPTIONS>.\n\n",
	  argv[0],argv[0]);
}

/******************************************************************************/
/*                    MAIN                                                    */
/******************************************************************************/
int main (int argc, char *argv[])
{
  int i;
  EOS_REAL *X = NULL, *Y = NULL, *FXY = NULL, *dFx = NULL, *dFy = NULL;
  EOS_INTEGER *extrap = NULL;
  enum
    { nTablesE = 1 };

  EOS_INTEGER tableType = EOS_NullTable;
  EOS_INTEGER matID     = 0;

  EOS_INTEGER tableHandle;

  EOS_INTEGER errorCode;
  EOS_BOOLEAN equal;

  EOS_INTEGER nTables, nXYPairs;
  EOS_INTEGER depVar, indepVar1, indepVar2, indepVarCnt=0;
  EOS_CHAR *depVarStr, *indepVar1Str, *indepVar2Str, *dFxStr=NULL, *dFyStr=NULL;


  char    *p = NULL;
  char    *generalOpts = "dehF:gI:i:J:LM:n:sVvx:y:";
  char    *indepVarOpts = "";
  int     minArgs = 2;
  char    *optionStr = NULL;

  EOS_CHAR errorMessage[EOS_MaxErrMsgLen];
  EOS_INTEGER indexFileCount;

  /* parse command line options */
  optionStr = (char*) safe_malloc((strlen(generalOpts) + strlen(indepVarOpts) + 1), sizeof(char));
  if (! optionStr) {
    fprintf (stderr, "\nERROR %d: Memory allocation error! (line %d)\n\n", mallocError, __LINE__);
    return mallocError;
  }
  optionStr = strcpy(optionStr, generalOpts);
  errorCode = getoptions(argc-1, argv+1, optionStr);

  sprintf(errorMessage, "argsc=%d", argsc);
  debug(2, (char*)__FILE__, (int)__LINE__, errorMessage);

  EOS_FREE(optionStr); /* we are done with this temporary memory here */
  if (errorCode) return errorCode;

  /* display usage */
  if (argc < 2) {
    printf("%s\n\n", get_header_str("interp_sesame_data", "$Revision: 1.27 $"));
    display_usage(argv);
    return InsufficientArguments;
  }

  /* display help */
  if (option['h'].flag) {
    printf("%s\n\n", get_header_str("interp_sesame_data", "$Revision: 1.27 $"));
    printf("%s", help_str);
    return OK;
  }

  /* display version */
  if (option['V'].flag) {
    printf("%s\n", get_version_str("interp_sesame_data", "$Revision: 1.27 $"));
    return OK;
  }

  /* get matID from argv */
  matID = (EOS_INTEGER)strtol(args[0], &p, 10);

  sprintf(errorMessage, "matID=%d", matID);
  debug(2, (char*)__FILE__, (int)__LINE__, errorMessage);

  /* get tableType from argv */
  tableType = get_tableType_FromStr(args[1]);

  sprintf(errorMessage, "tableType=%d", tableType);
  debug(2, (char*)__FILE__, (int)__LINE__, errorMessage);

  /* check for required arguments */
  if (get_dataTypeIndepVar1(tableType) != EOS_NullTable) indepVarCnt++;
  if (get_dataTypeIndepVar2(tableType) != EOS_NullTable) indepVarCnt++;

  sprintf(errorMessage, "indepVarCnt=%d", indepVarCnt);
  debug(2, (char*)__FILE__, (int)__LINE__, errorMessage);

  if (option['x'].flag)
    minArgs--;
  if (indepVarCnt > 1 && option['y'].flag)
    minArgs--;

  if (argsc < minArgs + ((option['i'].flag)?0:indepVarCnt)) {
    int err = InsufficientArguments;
    fprintf (stderr,"\n%s ERROR %d: %s (%d < %d)\n\n", argv[0], err, localErrorMessages[err],
	     argsc, 2 + ((option['i'].flag)?0:indepVarCnt));
    return err;
  }
  if (tableType <= 0) {
    int err = InvalidDataType;
    fprintf(stderr,"\n%s ERROR %d: %s\n\n", argv[0], err, localErrorMessages[err]);
    return err;
  }

  nTables = nTablesE;
  nXYPairs = 1;

  /* parse optional input file */
  if (option['i'].flag) {
    int err = 0;

    err = parseInput (option['i'].arg, &nXYPairs, &X, &Y);
    if (err) {
      fprintf(stderr,"\n%s ERROR %d: %s\n\n", argv[0], err, localErrorMessages[err]);
      return err;
    }      
  }

  sprintf(errorMessage, "nXYPairs=%d", nXYPairs);
  debug(2, (char*)__FILE__, (int)__LINE__, errorMessage);

  /* determine dependent/independent variable symbols */
  depVar       = get_dataTypeDepVar(tableType);
  indepVar1    = get_dataTypeIndepVar1(tableType);
  indepVar2    = get_dataTypeIndepVar2(tableType);
  depVarStr    = get_VarStr(depVar, 1);
  indepVar1Str = get_VarStr(indepVar1, 1);
  indepVar2Str = get_VarStr(indepVar2, 1);
  dFxStr = (EOS_CHAR*) safe_malloc(4+strlen(depVarStr)+strlen(indepVar1Str), sizeof(EOS_CHAR));
  dFxStr = strcpy(dFxStr, "d");
  dFxStr = strcat(dFxStr, depVarStr);
  dFxStr = strcat(dFxStr, "/d");
  dFxStr = strcat(dFxStr, indepVar1Str);
  if (indepVarCnt > 1) {
    dFyStr = (EOS_CHAR*) safe_malloc(4+strlen(depVarStr)+strlen(indepVar2Str), sizeof(EOS_CHAR));;
    dFyStr = strcpy(dFyStr, "d");
    dFyStr = strcat(dFyStr, depVarStr);
    dFyStr = strcat(dFyStr, "/d");
    dFyStr = strcat(dFyStr, indepVar2Str);
  }

  indexFileCount = 0;
  if (option['F'].flag) {
    indexFileCount = createIndexFile(option['F'].arg);
  }

  eos_CreateTables (&nTables, &tableType, &matID, &tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "\neos_CreateTables ERROR %d: %s\n\n", errorCode, errorMessage);
    return eospacError;
  }

  if (option['M'].flag) {
    EOS_INTEGER opt;
    if (! strncmp(option['M'].arg,"x",1) || ! strncmp(option['M'].arg,"X",1))
      opt = EOS_MONOTONIC_IN_X;
    if (! strncmp(option['M'].arg,"y",1) || ! strncmp(option['M'].arg,"Y",1))
      opt = EOS_MONOTONIC_IN_Y;
    if (option['g'].flag) printf("# ");
    printf("FORCE MONOTONICITY WITH RESPECT TO %s (opt=%d)\n", option['M'].arg, opt);
    eos_SetOption (&tableHandle, &opt, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf (stderr, "\neos_SetOption ERROR %d: %s (fcmp_ignore)\n\n", errorCode, errorMessage);
      return eospacError;
    }
  }

  eos_LoadTables (&nTables, &tableHandle, &errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "\neos_LoadTables ERROR %d: %s\n\n", errorCode, errorMessage);
    return eospacError;
  }

  if (option['F'].flag) {
    cleanIndexFile(indexFileCount);
  }

  if (option['L'].flag) {
    if (option['g'].flag) printf("# ");
    printf("LINEAR INTERPOLATION\n");
    eos_SetOption (&tableHandle, &EOS_LINEAR, EOS_NullPtr, &errorCode);
    if (errorCode != EOS_OK) {
      eos_GetErrorMessage (&errorCode, errorMessage);
      fprintf (stderr, "\neos_SetOption ERROR %d: %s (fcmp_ignore)\n\n", errorCode, errorMessage);
      return eospacError;
    }
  }
  else {
    if (option['g'].flag) printf("# ");
    printf("RATIONAL INTERPOLATION\n");
  }

  if (! option['i'].flag) {

    EOS_REAL x_lower, y_lower, x_upper, y_upper, *x_in = NULL, *y_in = NULL;
    EOS_BOOLEAN x_rangeExists = EOS_FALSE, y_rangeExists = EOS_FALSE;
    int Nx=1, Ny=1, j;

    if (option['x'].flag) { /* parse input file */
      parseInput (option['x'].arg, &Nx, &x_in, NULL);
    }
    else {
      x_lower = (EOS_REAL)strtod(args[2], &p);
      x_rangeExists = get_UpperRangeValue(args[2], &x_upper);
      if (! x_rangeExists) x_upper = x_lower;
    }

    if (indepVarCnt > 1) {

      if (option['y'].flag) { /* parse either input file or comma-delimited list */
	parseInput (option['y'].arg, &Ny, &y_in, NULL);
      }
      else {
	y_lower = (EOS_REAL)strtod(args[3-((option['x'].flag)?1:0)], &p);
	y_rangeExists = get_UpperRangeValue(args[3-((option['x'].flag)?1:0)], &y_upper);
	if (! y_rangeExists) y_upper = y_lower;
      }
    }

    if (x_rangeExists) {
      Nx = 50;
      if (option['I'].flag) Nx = atoi(option['I'].arg);
      if (option['n'].flag) Nx = atoi(option['n'].arg);
    }
    if (y_rangeExists) {
      Ny = 50;
      if (option['J'].flag) Ny = atoi(option['J'].arg);
      if (option['n'].flag) Ny = atoi(option['n'].arg);
    }
    nXYPairs = Nx * Ny;

    /* allocate memory for interpolation */
    X = (EOS_REAL *) safe_malloc(nXYPairs, sizeof(EOS_REAL));
    Y = (EOS_REAL *) safe_malloc(nXYPairs, sizeof(EOS_REAL));

    if (! (X && Y)) {
      fprintf (stderr, "\nERROR %d: Memory allocation error! (line %d)\n\n", mallocError, __LINE__);
      return mallocError;
    }

    if (x_in) {
      for (i=0; i<Nx; i++)
	X[i] = x_in[i];
      EOS_FREE(x_in);
    }
    else
      getSamples(Nx, x_lower, x_upper, X);

    if (y_in) {
      for (i=0; i<Ny; i++)
	Y[i] = y_in[i];
      EOS_FREE(y_in);
    }
    else
      getSamples(Ny, y_lower, y_upper, Y);

    
    for (j=Ny-1; j>=0; j--) { /* spread samples in arrays */
      for (i=0; i<Nx; i++) {
	X[i+j*Nx] = X[i];
	Y[i+j*Nx] = Y[j];
      }
    }

  }

  /* allocate memory for interpolation */
  FXY = (EOS_REAL *) safe_malloc(nXYPairs, sizeof(EOS_REAL));
  dFx = (EOS_REAL *) safe_malloc(nXYPairs, sizeof(EOS_REAL));
  dFy = (EOS_REAL *) safe_malloc(nXYPairs, sizeof(EOS_REAL));
  extrap = (EOS_INTEGER *) safe_malloc(nXYPairs, sizeof(EOS_INTEGER));

  if (! (FXY && dFx && dFy)) {
    fprintf (stderr, "\nERROR %d: Memory allocation error! (line %d)\n\n", mallocError, __LINE__);
    return mallocError;
  }

  /* testing interpolation methods */

  eos_Interpolate (&tableHandle, &nXYPairs, X, Y, FXY, dFx, dFy, &errorCode);
  eos_GetErrorMessage (&errorCode, errorMessage);

  eos_ErrorCodesEqual((EOS_INTEGER*)&EOS_INTERP_EXTRAPOLATED, &errorCode, &equal);

  if (errorCode != EOS_OK && !(option['e'].flag && equal)) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "\neos_Interpolate ERROR %d: %s\n\n", errorCode, errorMessage);
  }
  else {
    EOS_INTEGER err = EOS_OK;
    EOS_CHAR label[64];
    int fieldWidth = (int) log10(nXYPairs) +1 ;
    if (option['g'].flag) printf("# ");
    printf ("%s: %s :: %s\n", get_tableType_str(tableType), get_tableType_description(tableType),
	    get_tableHandleFileName(tableHandle));

    eos_CheckExtrap (&tableHandle, &nXYPairs, X, Y, extrap, &err);

    if (option['d'].flag) {

      if (indepVarCnt > 1) {
	if (option['g'].flag) {
	  printf ("#%12s%24s%24s%24s%24s\n",
		  indepVar1Str, indepVar2Str, depVarStr, dFxStr, dFyStr);
	}
	else {
	  printf ("\t%*s %13s%24s%24s%24s%24s\n", fieldWidth, " ",
		  indepVar1Str, indepVar2Str, depVarStr, dFxStr, dFyStr);
	}
	if (option['g'].flag) {
	  for (i = 0; i < nXYPairs; i++) {
	    if (i>0 && X[i] < X[i-1]) {
	      if (option['s'].flag) printf ("\n");
	      else                  printf ("\n\n");
	    }
	    if (extrap[i]) sprintf(label, " (%s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("%24.15e%24.15e%24.15e%24.15e%24.15e%s\n", X[i], Y[i], FXY[i], dFx[i], dFy[i], label);
	  }
	}
	else {
	  for (i = 0; i < nXYPairs; i++) {
	    if (extrap[i]) sprintf(label, " (%s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("\t%*d.%24.15e%24.15e%24.15e%24.15e%24.15e%s\n",
		    fieldWidth, i+1, X[i], Y[i], FXY[i], dFx[i], dFy[i], label);
	  }
	}
      }
      else {
	if (option['g'].flag) {
	  printf ("#%12s%24s%24s\n", indepVar1Str, depVarStr, dFxStr);
	  for (i = 0; i < nXYPairs; i++) {
	    if (i>0 && X[i] < X[i-1]) {
	      if (option['s'].flag) printf ("\n");
	      else                  printf ("\n\n");
	    }
	    if (extrap[i]) sprintf(label, " (%s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("%24.15e%24.15e%24.15e%s\n", X[i], FXY[i], dFx[i], label);
	  }
	}
	else {
	  printf ("\t%*s %13s%24s%24s\n", fieldWidth, " ", indepVar1Str, depVarStr, dFxStr);
	  for (i = 0; i < nXYPairs; i++) {
	    if (extrap[i]) sprintf(label, " (%s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("\t%*d.%24.15e%24.15e%24.15e%s\n", fieldWidth, i+1, X[i], FXY[i], dFx[i], label);
	  }
	}
      }

    }
    else {

      if (indepVarCnt > 1) {
	if (option['g'].flag) {
	  printf ("#%8s%15s%15s%15s%15s\n",
		  indepVar1Str, indepVar2Str, depVarStr, dFxStr, dFyStr);
	}
	else {
	  printf ("\t%*s %9s%15s%15s%15s%15s\n", fieldWidth, " ",
		  indepVar1Str, indepVar2Str, depVarStr, dFxStr, dFyStr);
	}
	if (option['g'].flag) {
	  for (i = 0; i < nXYPairs; i++) {
	    if (i>0 && X[i] < X[i-1]) {
	      if (option['s'].flag) printf ("\n");
	      else                  printf ("\n\n");
	    }
	    printf ("%15.6e%15.6e%15.6e%15.6e%15.6e\n", X[i], Y[i], FXY[i], dFx[i], dFy[i]);
	  }
	}
	else {
	  for (i = 0; i < nXYPairs; i++) {
	    if (extrap[i]) sprintf(label, " (%s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("\t%*d.%15.6e%15.6e%15.6e%15.6e%15.6e%s\n",
		    fieldWidth, i+1, X[i], Y[i], FXY[i], dFx[i], dFy[i], label);
	  }
	}
      }
      else {
	printf ("\t%*s %9s%15s%15s\n", fieldWidth, " ", indepVar1Str, depVarStr, dFxStr);
	if (option['g'].flag) {
	  for (i = 0; i < nXYPairs; i++) {
	    if (i>0 && X[i] < X[i-1]) {
	      if (option['s'].flag) printf ("\n");
	      else                  printf ("\n\n");
	    }
	    printf ("%15.6e%15.6e%15.6e\n", X[i], FXY[i], dFx[i]);
	  }
	}
	else {
	  for (i = 0; i < nXYPairs; i++) {
	    if (extrap[i]) sprintf(label, " (extrapolated: %s)", ERROR_TO_TEXT(extrap[i]));
	    else strcpy(label, "");
	    printf ("\t%*d.%15.6e%15.6e%15.6e%s\n", fieldWidth, i+1, X[i], FXY[i], dFx[i], label);
	  }
	}
      }

    }
  }

  eos_DestroyAll (&errorCode);
  if (errorCode != EOS_OK) {
    eos_GetErrorMessage (&errorCode, errorMessage);
    fprintf (stderr, "\neos_DestroyAll ERROR %d: %s for tableHandle=%d\n\n",
	     errorCode, errorMessage, tableHandle);
    return eospacError;
  }

  EOS_FREE (X);
  EOS_FREE (Y);
  EOS_FREE (FXY);
  EOS_FREE (dFx);
  EOS_FREE (dFy);
  EOS_FREE (dFxStr);
  EOS_FREE (dFyStr);
  EOS_FREE (extrap);
  return OK;

}
