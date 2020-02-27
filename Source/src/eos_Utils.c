/*! ******************************************************************
 * Class Name : eos_utils
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#define _EOS_INTERNAL_
#define _EOS_UTILS_INTERNAL_
#define _EOS_UTILS_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"

#include "eos_Utils.h"

#ifndef _POSIX_
#define _POSIX_
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 255
#endif

#include <ctype.h>

#include "eos_Interpolation.h"

static _EXCEPTIONS_T _exceptions; /* exclusively used in eos_Utils.c */

/***********************************************************************/
/*!
 * \brief Find the right-most instance of *find in *string
 *
 * \param[in]     *string     - EOS_CHAR* : string to search
 * \param[in]     *find       - EOS_CHAR* : string to find in *string
 *
 * \return        *cp         - EOS_CHAR* : pointer to start of *find within *string
 *
 ***********************************************************************/
EOS_CHAR *
strrstr (const EOS_CHAR * string, const EOS_CHAR * find)
{
  EOS_CHAR *cp;
  int len1 = strlen (string);
  int len2 = strlen (find);

  for (cp = (EOS_CHAR *) string + MAX (0, len1 - len2); cp >= string; cp--)
    {
      if (strncmp (cp, find, len2) == 0)
	return cp;
    }
  return NULL;
}

/*! *********************************************************************
 *
 * Function Name: _eos_ReplaceSubstring
 * This routine will replace all instances of pattern found within original
 * with replacement and return the modified string
 *
 ************************************************************************/
EOS_CHAR* _eos_ReplaceSubstring( const EOS_CHAR *original, const EOS_CHAR *pattern, const EOS_CHAR *replacement )
{
  size_t const replen = strlen(replacement);
  size_t const patlen = strlen(pattern);
  size_t const orilen = strlen(original);

  size_t patcnt = 0;
  const EOS_CHAR * oriptr;
  const EOS_CHAR * patloc;

  // find how many times the pattern occurs in the original string
  for (oriptr = original; ( patloc = strstr(oriptr, pattern) ) != NULL; oriptr = patloc + patlen)
  {
    patcnt++;
  }

  {
    // allocate memory for the new string
    size_t const retlen = orilen + patcnt * (replen - patlen);
    EOS_CHAR * const returned = (EOS_CHAR *) malloc( sizeof(EOS_CHAR) * (retlen + 1) );

    if (returned != NULL)
    {
      // copy the original string, 
      // replacing all the instances of the pattern
      EOS_CHAR * retptr = returned;
      for (oriptr = original; ( patloc = strstr(oriptr, pattern) ) != NULL; oriptr = patloc + patlen)
      {
        size_t const skplen = patloc - oriptr;
        // copy the section until the occurence of the pattern
        strncpy(retptr, oriptr, skplen);
        retptr += skplen;
        // copy the replacement 
        strncpy(retptr, replacement, replen);
        retptr += replen;
      }
      // copy the rest of the string.
      strcpy(retptr, oriptr);
    }
    return returned;
  }
}

/*! *********************************************************************
 *
 * Function Name: _eos_round
 * This routine will round x to specified precision, p
 *
 ************************************************************************/
EOS_REAL _eos_round(EOS_REAL x, EOS_REAL p) {
  EOS_REAL IP = 0.0, v, f;

  v = modf(x, &IP);
  f = pow(10.0,p);

  if (x<0)
    v = ceil(v * f - 0.5) / f + IP;
  else 
    v = floor(v * f + 0.5) / f + IP;

  return v;
}

/*! *********************************************************************
 *
 * Function Name: _eos_littleEndianMachine
 * This routine determines the byte-order of a system at run-time
 * (i.e., Big Endian or Little Endian).
 * This function assigns the value 0001h to a 16-bit integer. A char
 * pointer is then assigned to point at the first (least-significant)
 * byte of the integer value.  If the first byte of the integer is 01h,
 * then the system is little-endian (the 01h is in the lowest, or least-
 * significant, address). If it is 00h then the system is big-endian.
 *
 * Returned Values:
 * EOS_INTEGER _eos_littleEndianMachine - one of the following values:
 *                                  BIG_ENDIAN_MACHINE
 *                                  LITTLE_ENDIAN_MACHINE
 *
 * Uses the following macro(s):
 */
#define BIG_ENDIAN_MACHINE      0
#define LITTLE_ENDIAN_MACHINE   1
/*
 ************************************************************************/
EOS_INTEGER _eos_littleEndianMachine (void)
{
  short int word = 0x0001;
  EOS_CHAR *byte = (EOS_CHAR *) &word;
  return (byte[0] ? LITTLE_ENDIAN_MACHINE : BIG_ENDIAN_MACHINE);
}

/*! *********************************************************************
 *
 * Function Name: _eos_byteSwap
 * This routine converts Sesame data to Little/Big Endian if working on a
 * little endian machine. This routine returns a non-zero error code if
 * the specified wordSize is not an even number.
 * CAUTION: The calling routine is required to provide a valid buffer of
 *          data to be swapped. This routine cannot determine if data is
 *          allowed to be swapped or not.
 *
 * Returned Values:
 * EOS_CHAR    *buf          - array of unsigned char data needing
 *                             bytes swapped; passed by reference
 *
 * Input Value:
 * EOS_INTEGER *nwds         - size of *buf in wordSize increments
 * EOS_INTEGER wordSize      - element size of array in *buf
 * EOS_BOOLEAN bigEndianFile - is the associated data file big endian?
 *
 ************************************************************************/
EOS_INTEGER _eos_byteSwap (EOS_CHAR *buf, EOS_INTEGER nwds, EOS_INTEGER wordSize, EOS_BOOLEAN bigEndianFile)
{
  EOS_INTEGER index, j;
  EOS_CHAR ctmp;
  EOS_BOOLEAN isLittleEndianMachine;

  isLittleEndianMachine = _eos_littleEndianMachine()?EOS_TRUE:EOS_FALSE;
  if ((isLittleEndianMachine && bigEndianFile) || (!isLittleEndianMachine && !bigEndianFile))
    {
    /* convert Sesame data to either Big or Little Endian as needed */
    /* verify wordSize is evenly divisible by 2; otherwise return error */
      if (!(wordSize > 0))
	{
	  if ((EOS_INTEGER) fmod ((double) wordSize, (double) 2) == 0)
	    {
#ifdef DEBUG
	printf ("_eos_byteSwap error!\n");
#endif
	return (-1);
      }
    }

      for (index = 0; index < wordSize * (nwds); index += wordSize)
	{
	  for (j = 0; j < (wordSize / 2); j++)
	    {
        ctmp = buf[index + j];
        buf[index + j] = buf[index + wordSize - j - 1];
        buf[index + wordSize - j - 1] = ctmp;
      }
    }
  }
  return (EOS_OK);
}

/*! Function used to determine if a specific Sesame file is big endian or not.
 *  *esFile is assumed to already be opened by caller
 */
EOS_BOOLEAN
_eos_isBigEndianFile (FILE * esFile)
{
  EOS_INTEGER nRead, err=0;
  EOS_BOOLEAN result = EOS_TRUE;
  EOS_REAL x;
  EOS_INTEGER_UNSIGNED x_int;
  EOS_BOOLEAN loc_known = EOS_TRUE;
  fpos_t loc;

  /* Save current file position. Assume loc=NULL on falure. */
  if (fgetpos (esFile, &loc))
    loc_known = EOS_FALSE;

  /* reset to position 0 */
  err = fseek (esFile, 0, SEEK_SET);
  if (err)
    {
    /* Do some error handling in this function! */
  }

  nRead = fread (&x, sizeof (EOS_REAL), 1, esFile);
  if (nRead <= 0)
    {
    /* Do some error handling in this function! */
  }
  x_int = (EOS_INTEGER_UNSIGNED) x;

  if (_eos_littleEndianMachine ())
    {
    result = (x_int == 0)?EOS_TRUE:EOS_FALSE;
  }
  else
    {
    result = (x_int != 0)?EOS_TRUE:EOS_FALSE;
  }

  /* Reset to position loc */
  if (loc_known) 
    err = fsetpos(esFile, &loc);
  if (err)
    {
    /* Do some error handling in this function! */
  }

  return result;
}

/*! **********************************************************************
 *
 * Function eos_IsErrorCodeValid
 * Description:
 * Checks if the supplied error code is valid
 * 
 * Returned Values:
 * EOS_BOOLEAN   EOS_TRUE for valid, EOS_FALSE for invalid
 *
 * Input Value:
 * EOS_INTEGER err     - the supplied table handle
 *************************************************************************/
EOS_BOOLEAN
eos_IsErrorCodeValid (EOS_INTEGER err)
{
  if (eos_GetStandardErrorCodeFromCustomErrorCode (err) < EOS_MIN_ERROR_CODE_VALUE || eos_GetStandardErrorCodeFromCustomErrorCode (err) > EOS_MAX_ERROR_CODE_VALUE)
    return EOS_FALSE;
  return EOS_TRUE;
}

/*! **********************************************************************
 *
 * Function eos_IsHandleValid
 * Description:
 * Checks if the supplied table handle is valid
 * 
 * Returned Values:
 * EOS_BOOLEAN   EOS_TRUE for valid, EOS_FALSE for invalid
 *
 * Input Value:
 * EOS_INTEGER th     - the supplied table handle
 *************************************************************************/
EOS_BOOLEAN
eos_IsHandleValid (EOS_INTEGER th)
{
  if (th < 0 || th >= gEosDataMap.nAlloc)
    return EOS_FALSE;
  if (gEosDataMap.tableHandlesMap[th] == _EOS_INVALID_TABLE_HANDLE_VALUE)
    return EOS_FALSE;
  return EOS_TRUE;
}

/*! **********************************************************************
 *
 * Function eos_InvalidateHandle
 * Description:
 * Set table handle as invalid
 * 
 * Input Value:
 * EOS_INTEGER th     - the supplied table handle
 *************************************************************************/
void
eos_InvalidateHandle (EOS_INTEGER th)
{
  gEosDataMap.tableHandlesMap[th] = _EOS_INVALID_TABLE_HANDLE_VALUE;
  return;
}

/*! *********************************************************************
 *
 * Function Name: _eos_GetSesameFileName
 * This routine returns the info for the data source from cache
 * if its not cached, it caches it.
 *
 * Returned Values:
 * EOS_CHAR    *fname  - name of the file, a pointer to the static string, should not be destroyed!
 *
 * Input Value:
 * eos_Data    *me     - pointer to an instance of type eos_Data.
 *                     
 ************************************************************************/
EOS_CHAR *
_eos_GetSesameFileName (eos_Data * me)
{
  /* determine if an alternative source was used to load the data */
  if (me->altDataSource)
    return me->altDataSource;

  /* determine if me->dataFileIndex is valid */
  if (sesameFilesL <= me->dataFileIndex || me->dataFileIndex < 0)
    return NULL;

  return sesameFiles[me->dataFileIndex];
}

/***********************************************************************/
/*!
 * \brief Determines whether or not a Sesame file is valid.
 *
 * \param[in]     filename   - EOS_CHAR*       : name of file to test
 * \param[out]    th         - EOS_BOOLEAN     : valid or not
 *
 * \return _eos_fileExistsAndValid - EOS_BOOLEAN : file is valid or not
 * 
 ***********************************************************************/
//#define DEBUG_eos_fileExistsAndValid
EOS_BOOLEAN
_eos_fileExistsAndValid (EOS_CHAR * filename)
{
  EOS_BOOLEAN fileExists=EOS_FALSE, fileIsReadable=EOS_FALSE, fileIsDir=EOS_FALSE;
  struct stat file_statbuf;

  fileExists = (!((EOS_BOOLEAN) stat (filename, &file_statbuf)))?EOS_TRUE:EOS_FALSE;

  if (fileExists) {
    /* now test for access */
    FILE *file;
    file = fopen(filename, "r");
    if (!file) {
      return(EOS_FALSE); /* cannot read file */
    }
    else {
      fclose(file);
      fileIsReadable=EOS_TRUE;
    }
  }
  else {
    return(EOS_FALSE); /* file does not exist */
  }

#ifndef _IFMT
  /* The Windows header files define _S_ forms of this.  */
#  define _IFMT      _S_IFMT
#endif
#ifndef _IFDIR
  /* The Windows header files define _S_ forms of this.  */
#  define _IFDIR     _S_IFDIR
#endif
#ifndef S_ISDIR
  /* The Windows header files do not define this.  */
#  define  S_ISDIR(m) (((m)&_IFMT) == _IFDIR)
#endif
  fileIsDir = (EOS_BOOLEAN) S_ISDIR(file_statbuf.st_mode);

#ifdef DEBUG_eos_fileExistsAndValid
  printf("%s: file_statbuf.st_mode= %o",filename,file_statbuf.st_mode);
  if (fileIsDir)
    printf (" DIRECTORY");
  printf("\n");
#endif

  return ((fileExists && fileIsReadable && ! fileIsDir))?EOS_TRUE:EOS_FALSE;
}

/***********************************************************************/
/*!
 * \brief Determines whether or not a file name is absolute or not.
 *
 * \param[in]     filename   - EOS_CHAR*       : name of file to test
 *
 * \return _eos_is_absoluteFileName - EOS_BOOLEAN : file name is absolute or not
 * 
 ***********************************************************************/
EOS_BOOLEAN
_eos_is_absoluteFileName (EOS_CHAR * f)
{
  if ((f[0] == '/') || (f[0] == '\\' && f[1] == '\\' && isalpha ((int) f[2])) || (isalpha ((int) f[0]) && f[1] == ':' && f[2] == '\\'))
    {
    /* one of the following absolute reference formats is detected:
     *   Unix        --> '/'
     *   DOS/Windows --> '//X'
     *   DOS/Windows --> '\\X'
     *   DOS/Windows --> 'X:\'
     */
    return(EOS_TRUE);
  }
  return(EOS_FALSE);
}

/*! *********************************************************************
 *
 * Function Name: _eos_in_gMatidMap
 * This function returns EOS_TRUE if any matid in the global gMatidMap[] array
 * is cross-referenced to the sesameFiles[idx] file.
 *
 ************************************************************************/
EOS_BOOLEAN
_eos_in_gMatidMap (EOS_INTEGER idx)
{
  int i;
  for (i = 0; i < gMatidMapL; i++)
    {
      if (gMatidMap[i].jfile == idx)
	return EOS_TRUE;
  }
  return EOS_FALSE;
}

/*! *********************************************************************
 *
 * Function Name: _eos_find_matid_in_gMatidMap
 * This function returns the index of the global gMatidMap[] element, which
 * contains matid. A negative value is returned if matid is not found.
 *
 ************************************************************************/
EOS_INTEGER
_eos_find_matid_in_gMatidMap (EOS_INTEGER matid)
{
  EOS_INTEGER i;
  for (i = 0; i < gMatidMapL; i++)
    {
      if (gMatidMap[i].matid == matid)
	return i;
  }
  return -1; /* matid not found */
}

/*! *********************************************************************
 *
 * Function Name: _eos_find_userdefined_fileindex_in_gEosDataMapDataObjects
 * This function returns the index of the global gEosDataMap.dataObjects[]
 * element, which contains a user defined dataFileIndex.
 * A negative value is returned if matid is not found.
 *
 ************************************************************************/
EOS_INTEGER
_eos_find_userdefined_fileindex_in_gEosDataMapDataObjects (EOS_INTEGER fileIndex)
{
  EOS_INTEGER i;
  for (i = 0; i < gEosDataMap.nAlloc; i++)
    {
      if (!gEosDataMap.dataObjects[i])
	continue;		/* skip invalid object */
      if (gEosDataMap.dataObjects[i]->userDefinedDataFile && gEosDataMap.dataObjects[i]->dataFileIndex == fileIndex)
	return i;
  }
  return -1; /* reserved fileIndex not found */
}

/*! *********************************************************************
 *
 * Function Name: _eos_deleteDuplicateNames
 * This function deallocates duplicates from files[] if not referenced by a
 * gMatidMap entry.
 *
 * Returned Value:
 * EOS_INTEGER _eos_deleteDuplicateNames - error code
 * EOS_CHAR    ***files                  - array of character strings/arrays
 *                                         passed by reference
 *
 * Input Values:
 * EOS_INTEGER *filesL                   - number of strings in ***files array
 *                                         passed by reference
 * struct stat *stat_buf                 - File statuses from stat()
 *
 ************************************************************************/
EOS_INTEGER
_eos_deleteDuplicateNames (EOS_CHAR *** files, EOS_INTEGER * filesL, struct stat * stat_buf)
{
  EOS_INTEGER err = EOS_OK;
  int i, j, k;

  for (i = 0; i < *filesL; i++)
    {
      for (j = 0; j < i; j++)
	{
	  if (i == j)
	    continue;
	  if (!eos_compareFiles ((*files)[j], &stat_buf[j], (*files)[i], &stat_buf[i]))
	    {
        /* files[i] and files[j] are identical */
	      if (!_eos_in_gMatidMap (i))
		{
	  /* files[i] is not referenced by a gMatidMap entry */
	  EOS_FREE((*files)[i]);
	}
	      else if (!_eos_in_gMatidMap (j))
		{
	  /* files[j] is not referenced by a gMatidMap entry */
	  EOS_FREE((*files)[j]);
	}
	      else
		{
	  /* keep the most recent entry, files[i] */
	  EOS_FREE((*files)[j]);
	  /* reassign all occurrences of gMatidMap[k].jfile = j to gMatidMap[k].jfile = i */
	  for (k = 0; k < gMatidMapL; k++)
	    if (gMatidMap[k].jfile == j)
	      gMatidMap[k].jfile = i;
	}
        break;                  /* leave j loop */
      }
    }
  }

  return err;
}

/*! *********************************************************************
 *
 * Function Name: eos_getSesameFileNames
 * This function parses a text file named sesameFilesDir.txt that contains
 * available Sesame file names and returns the list to the calling routine.
 *
 * Returned Value:
 * EOS_INTEGER eos_getSesameFileNames   - error code
 * EOS_CHAR    ***files                 - array of character strings/arrays
 *                                        passed by reference
 * EOS_INTEGER *filesL                  - number of strings in ***paths array
 *                                        passed by reference
 * EOS_CHAR    *errMsg                  - custom error message
 *
 * Uses the following global data:
 *  **sesameIndexLocations
 *  sesameIndexLocationsL
 *  *defaultSesamePathList[EOS_NUMDEFAULTSESAMEPATHSL]
 *
 ************************************************************************/
EOS_INTEGER
eos_getSesameFileNames (EOS_CHAR *** files, EOS_INTEGER * filesL, EOS_CHAR ** errMsg)
{
  FILE *fp;
  EOS_CHAR indexFileName[PATH_MAX], indexFilePath[PATH_MAX];
  EOS_INTEGER i, ierr, count, location_cnt;
  EOS_BOOLEAN fileExists;
  struct stat *stat_buf = NULL;

  if (sesameIndexLocationsL <= 0)
  {
    /* initialize sesamePaths to contain CWD */
    sesameIndexLocationsL = 1;
    sesameIndexLocations = (EOS_CHAR **) malloc (sesameIndexLocationsL * sizeof (EOS_CHAR *));
    sesameIndexLocations[0] = (EOS_CHAR *) malloc (2 * sizeof (EOS_CHAR *));
    strcpy (sesameIndexLocations[0], ".");

    /* append paths defined in SESAMEPATH environment variable to
       sesameIndexLocations[] */
    ierr = _eos_parseENV ("SESAMEPATH", &sesameIndexLocations, &sesameIndexLocationsL);

    /* append defaultSesamePathList[] to sesameIndexLocations[] */
    for (i = 0; i < EOS_NUMDEFAULTSESAMEPATHSL; i++)
	{
      sesameIndexLocationsL++;
	  sesameIndexLocations = (EOS_CHAR **) realloc (sesameIndexLocations, (sesameIndexLocationsL) * sizeof (EOS_CHAR *));
	  sesameIndexLocations[sesameIndexLocationsL - 1] = (EOS_CHAR *) malloc (strlen (defaultSesamePathList[i]) * sizeof (EOS_CHAR *));
	  strcpy (sesameIndexLocations[sesameIndexLocationsL - 1], defaultSesamePathList[i]);
    }

    location_cnt = sesameIndexLocationsL;

  }
  else
  {				/* not first time through, only use "." below */
    location_cnt = 1;
  }

  /* loop over all specified search path(s) */
  for (i = 0; i < location_cnt; i++)
  {

    /* if indexFileName will be <= PATH_MAX, then define it and continue */
#ifdef _MSC_VER
    count = strlen(sesameIndexLocations[i]) + strlen("sesameFilesDir.txt") + 2;
#else
    count = strlen (sesameIndexLocations[i]) + strlen ("sesameFilesDir.txt") + 1;
#endif
    if (count > PATH_MAX)
      continue;
    strcpy (indexFilePath, sesameIndexLocations[i]);
    strcpy (indexFileName, sesameIndexLocations[i]);
#ifdef _MSC_VER
    strcat(indexFileName, "\\sesameFilesDir.txt");
#else
    strcat (indexFileName, "/sesameFilesDir.txt");
#endif

    fileExists = _eos_fileExistsAndValid(indexFileName);

    if (fileExists)
      fp = fopen (indexFileName, "r");  /* try to open indexFileName */
    if (fileExists && fp != NULL)
	{
      /* parse sesameFilesDir.txt contents and add to sesameFiles[] */
      ierr = _eos_parseIndexFile (fp, indexFilePath, files, filesL, errMsg);
      fclose (fp);              /* close indexFileName */

	  if (ierr == _EOS_STOP_FILE_SEARCH)
      {			/* add no more names to fileNames[] */
        ierr = EOS_OK;
        break;
      }

    }

    if (location_cnt > 1)
	{
      /* Add any default file names to sesameFiles[] that are found in
         indexFilePath */
      ierr = _eos_addDefaultFileNames (indexFilePath, files, filesL);
    }
  }

  if (*filesL <= 0)
    return (EOS_NO_SESAME_FILES);

  /* Build stat_buf hash */
  stat_buf = (struct stat*) malloc(*filesL * sizeof(struct stat));
  for (i = 0; i < *filesL; i++)
  {
    stat ((*files)[i], &stat_buf[i]);
  }

  /* Remove duplicated file names from files[] if not referenced by a gMatidMap entry*/
  ierr = _eos_deleteDuplicateNames (files, filesL, stat_buf);

  /* Remove all empty strings from files[],
   * and decrement gMatidMap[].jfile values as necessary */
  ierr = _eos_compactFilesArray (files, filesL, EOS_TRUE);

#ifdef DEBUG_FILE_NAMES
  if (*filesL > 0)
    printf ("File list after file comparisons:\n");
  for (i = 0; i < *filesL; i++)
  {
    printf ("%3d. %s\n", i, (*files)[i]);
  }
#endif

  EOS_FREE(stat_buf);

  return (EOS_OK);
}

/*! *********************************************************************
 *
 * Function Name: _eos_parseENV
 * This routine splits an environment variable value into substrings
 * using a ';' or ':' delimiter for PC and UNIX respectively.
 *
 * Returned Values:
 * EOS_INTEGER _eos_parseENV   - error code
 * EOS_CHAR    ***paths       - array of character strings/arrays passed by
 *                              reference
 * EOS_INTEGER *pathsL        - number of strings in ***paths array passed by
 *                              reference
 *
 * Input Value:
 * const EOS_CHAR *envVar     - environment variable name to parse passed by
 *                              reference
 *
 ************************************************************************/
EOS_INTEGER
_eos_parseENV (const EOS_CHAR * envVar, EOS_CHAR *** paths, EOS_INTEGER * pathsL)
{

  EOS_CHAR *envVarVal, *p1;
  EOS_CHAR *envVarVal_init;
  EOS_INTEGER L;
  EOS_CHAR tmp[PATH_MAX];       /* length is platform-dependent using POSIX constant */
  EOS_INTEGER i;
  EOS_CHAR controlString[100];

  envVarVal = getenv (envVar);
  if (envVarVal == NULL)
    return (EOS_OK);            /* *envVar may be unspecified in the environment */

  L = strlen (envVarVal) + 1;

  p1 = strchr (envVarVal, ';');
#if defined(_WIN32) && ! defined(__CYGWIN__)
  if (!p1)
    p1 = envVarVal;		/* override if no semicolon found */
#endif
  if (p1)
    {
    /* Windows PCs use semicolon to separate environment variable values. */
    sprintf (controlString, "%%%d[^;]", PATH_MAX);
  }
  else
    {
    /* *NIX uses colon to separate environment variable values. */
    sprintf (controlString, "%%%d[^:]", PATH_MAX);
  }

  if (*paths == NULL || **paths == NULL)
    *pathsL = 0;
  envVarVal_init = envVarVal;   /* store initial address of envVarVal pointer */

  while ((sscanf (envVarVal, controlString, tmp) == 1) && (envVarVal < envVarVal_init + L))
    {
    i = strlen (tmp) + 1;
      *paths = (EOS_CHAR **) realloc (*paths, sizeof (EOS_CHAR *) * (*pathsL + 1));
    (*paths)[*pathsL] = (EOS_CHAR *) malloc (i * sizeof (EOS_CHAR));
    strcpy ((*paths)[*pathsL], tmp);
    *pathsL += 1;
    envVarVal += i;
  }

  return EOS_OK;
}

/*! *********************************************************************
 *
 * Function Name: _eos_parseIndexFile
 * This routine parses the contents of an opened text file referenced by
 * a file pointer, fp.
 * The parsing rules are defined as follows:
 *    1. Delimiters are linefeed, carriage return, semi-colon.
 *    2. Comments are ignored and begin with #.
 *    3. Leading white space is ignored.
 *    4. Paths that are relative to opened text file are converted to
 *       absolute paths.
 *    5. Leading and trailing white space are removed from each file name.
 *
 * Returned Values:
 * EOS_INTEGER _eos_parseIndexFile - error code
 * EOS_CHAR    ***fileNames       - array of character strings/arrays
 *                                  passed by reference
 * EOS_INTEGER *fileNamesL        - number of strings in ***fileNames
 *                                  array passed by reference
 * EOS_CHAR    **errMsg           - custom error message
 *
 * Input Value:
 * FILE     *fp                   - text file pointer
 * EOS_CHAR *fpPathName           - path location of text file
 *
 ************************************************************************/
EOS_INTEGER
_eos_parseIndexFile (FILE * fp, EOS_CHAR * fpPathName, EOS_CHAR *** fileNames, EOS_INTEGER * fileNamesL, EOS_CHAR ** errMsg)
{
  EOS_CHAR *tmp, *tmp2, *tok;
  EOS_INTEGER count, totChars, line = 0, matid, err = EOS_OK;

  EOS_CHAR *endtok = "END";
  EOS_CHAR *matidtok = "MATID";
  EOS_BOOLEAN newMapMatid = EOS_FALSE;


  if (*fileNames == NULL || **fileNames == NULL)
    *fileNamesL = 0;

  tmp = malloc (sizeof (EOS_CHAR));
  tmp[0] = '\0';
  tok = malloc (sizeof (EOS_CHAR));
  tok[0] = '\0';

  while (_eos_readLine (fp, &tmp))
  {				/* tmp is reallocated -- possibly larger than PATH_MAX */

    line++; /* increment line counter */

    /* printf("sesameFilesDir.txt => %i: %s\n", i++, tmp); */

    /* reallocate tok to size of tmp */
    tok = realloc (tok, (strlen(tmp) + 1) * sizeof (EOS_CHAR));

    sscanf (tmp, "%s", tok); /* read first non-white space character sequence */

    if (!strncmp (tok, endtok, strlen (endtok)))
	{
      /* satisfy request to add no more names to fileNames[] */
      err = _EOS_STOP_FILE_SEARCH;
      break;
    }

    if (!strncmp (tok, matidtok, strlen (matidtok)))
	{			/* user-specified matid to file association */

      /* find end of matidtok in tmp */
      count = (EOS_INTEGER) (strstr(tmp, matidtok) + strlen(matidtok) - tmp);

      sscanf (tmp + count, "%s", tok); /* read first non-white space character sequence after matidtok */
	  if (sscanf (tok, "%d", &matid) != 1)
      {			/* attempt to read matid */
        /* an error occurred if you get here */
        err = EOS_INDEX_FILE_ERROR;
        eos_SetCustomMsg_str (errMsg, "EOS_INDEX_FILE_ERROR: syntax error on line %d of %s", line, fpPathName);
        break;
      }

      /* find end of tok in tmp */
      count = (EOS_INTEGER) (strstr(tmp, tok) + strlen(tok) - tmp);

      /* length of remainder of tmp after matid */
      totChars = strlen (tmp + count);

      /* indicate that a new gMatidMap entry is required */
      newMapMatid = EOS_TRUE;

    }
    else
	{

      totChars = strlen (tmp);
      count = 0;
      newMapMatid = EOS_FALSE;

    }

    tmp2 = malloc ((totChars + 1) * sizeof (EOS_CHAR));
    while (strlen (&tmp[count]) && sscanf (&tmp[count], " %[^;\n\r#]", tmp2))
	{
      int i;

      if (*fileNames)
        *fileNames = (EOS_CHAR **) realloc (*fileNames, sizeof (EOS_CHAR *) * (*fileNamesL + 1));
      else
        *fileNames = (EOS_CHAR **) malloc (sizeof (EOS_CHAR *) * (*fileNamesL + 1));

      /* remove trailing white space from tmp2 */
      for (i=strlen(tmp2)-1; i>0; i--) {
        if (tmp2[i] == ' ' || tmp2[i] == '\t') tmp2[i] = '\0';
        else break;
      }

      count += strlen (tmp2);
	  if (strlen (tmp2) <= PATH_MAX)
      {
        if (_eos_is_absoluteFileName (tmp2))
		{
          /* one of the following absolute reference formats is detected:
           *   Unix        --> '/'
           *   DOS/Windows --> '//X'
           *   DOS/Windows --> '\\X'
           *   DOS/Windows --> 'X:\'
           */
          (*fileNames)[*fileNamesL] = (EOS_CHAR *) malloc ((count + 1) * sizeof (EOS_CHAR));
          strcpy ((*fileNames)[*fileNamesL], tmp2);
        }
        else
		{
          /* need to prepend *fpPathName string to tmp2 to make it an absolute reference */
          totChars = strlen (fpPathName) + 1 + count;
          (*fileNames)[*fileNamesL] = (EOS_CHAR *) malloc ((totChars + 1) * sizeof (EOS_CHAR));
          strcpy ((*fileNames)[*fileNamesL], fpPathName);
          strcat ((*fileNames)[*fileNamesL], "/");
          strcat ((*fileNames)[*fileNamesL], tmp2);
        }

        {
          /* if (*fileNames)[*fileNamesL] exists, then increment *fileNamesL;
           * otherwise current (*fileNames)[*fileNamesL] will be discarded.
           */
          EOS_BOOLEAN fileExists=EOS_FALSE;

          fileExists = _eos_fileExistsAndValid((*fileNames)[*fileNamesL]);

          if (fileExists) {
            *fileNamesL += 1;
          }
          else {
            /* remove extra string and its pointer allocation */
            EOS_FREE((*fileNames)[*fileNamesL]);
            if (*fileNamesL <= 0) {
              EOS_FREE(*fileNames);
            }
            else {
              *fileNames = (EOS_CHAR **) realloc (*fileNames, sizeof (EOS_CHAR *) * (*fileNamesL));
            }
          }
        }

      }

      while (tmp[count] == ';')
        count = count + 1;      /* skip successive ';' delimiters */
    }
    EOS_FREE (tmp2);
    strcpy (tmp, "");

    if (newMapMatid)
	{
      _eos_addMatidMap ( matid, *fileNamesL-1 );
    }
  }

  EOS_FREE (tmp);
  EOS_FREE(tok);

  return (err);
}

/*! *********************************************************************
 *
 * Function Name: _eos_addMatidMap
 * Add a matid and file index to a new gMatidMap[] entry.
 *
 * Input Value:
 * EOS_INTEGER matid          - material id number
 * EOS_INTEGER index          - file index to associate with matid
 *
 ************************************************************************/
void
_eos_addMatidMap (EOS_INTEGER matid, EOS_INTEGER index)
{
  /* allocate/reallocate the global gMatidMap array */
  if (!gMatidMap)
    {
    gMatidMapL = 1;
    gMatidMap = (MatidMap *) malloc (gMatidMapL * sizeof (MatidMap));
  }
  else
    {
    gMatidMapL++;
    gMatidMap = realloc (gMatidMap, gMatidMapL * sizeof (MatidMap));
  }

  /* store new gMatidMap entry */
  gMatidMap[gMatidMapL-1].matid = matid;
  gMatidMap[gMatidMapL-1].jfile = index;
}

/*! *********************************************************************
 *
 * Function Name: _eos_destroyMatidMap
 * Destroy the matid and file index, gMatidMap[].
 *
 * Input Value:
 * EOS_INTEGER matid          - material id number
 * EOS_INTEGER index          - file index to associate with matid
 *
 ************************************************************************/
void
_eos_destroyMatidMap ()
{
  /* allocate/reallocate the global gMatidMap array */
  if (gMatidMap && gMatidMapL > 0)
  {
    EOS_FREE(gMatidMap);
    gMatidMapL = 0;
  }
}

/*! *********************************************************************
 *
 * Function Name: _eos_readLine
 * This routine reads the next line of an opened text file referenced by
 * a file pointer, fp.
 *
 * Returned Values:
 * EOS_INTEGER _eos_readLine - status of read (zero indicates failure or EOF)
 * EOS_CHAR    **line       - character string/array passed by reference
 *
 * Input Value:
 * FILE        *fp          - text file pointer
 *
 ************************************************************************/
EOS_INTEGER
_eos_readLine (FILE * fp, EOS_CHAR ** line)
{
  EOS_CHAR *in = NULL;
  EOS_INTEGER L1 = 0, L2 = 0;
  EOS_CHAR *p1, *p2;
  EOS_INTEGER failed;

  in = (EOS_CHAR*) malloc(PATH_MAX * sizeof(EOS_CHAR));

  while (fgets (in, PATH_MAX, fp))
    {
    L1 = strlen(*line);
    L2 = strlen(in);
    *line = realloc (*line, (L1 + L2 + 1) * sizeof (EOS_CHAR));
    strcat (*line, in);
    p1 = strchr (*line, '\n');
    p2 = strchr (*line, '\r');
      if (p1 || p2)
	{
	  if (p1)
	    *p1 = '\0';
	  if (p2)
	    *p2 = '\0';
      break;
    }
  }

  failed = ((in) ? 0 : 1);

  EOS_FREE(in);

  return((! feof(fp)) && ! failed);
}

/*! *********************************************************************
 *
 * Function Name: _eos_addDefaultFileNames
 * This routine searches for a finite default list of files similarly to
 * previous versions of EOSPAC. The default list is stored in an array
 * named defaultSesamePathList[], and they are pushed onto the list of
 * available *fileNames[] if found in the specified srchPathName.
 *
 * Returned Values:
 * EOS_INTEGER _eos_addDefaultFileNames - error code
 * EOS_CHAR    ***fileNames            - array of character strings/arrays
 *                                       passed by reference
 * EOS_INTEGER *fileNamesL             - number of strings in ***fileNames
 *                                       array passed by reference
 *
 * Input Value:
 * EOS_CHAR *srchPathName              - search path
 *
 * Uses the following global data:
 *  *defaultSesameFileNames[EOS_NUMDEFAULTFILENAMES]
 *
 ************************************************************************/
EOS_INTEGER
_eos_addDefaultFileNames (EOS_CHAR * srchPathName, EOS_CHAR *** fileNames, EOS_INTEGER * fileNamesL)
{
  EOS_CHAR *tmp, *tmp2;
  EOS_INTEGER i, totChars;
  EOS_BOOLEAN fileExists;

  if (*fileNames == NULL || **fileNames == NULL)
    *fileNamesL = 0;

  tmp = malloc (PATH_MAX * sizeof (EOS_CHAR));
  tmp2 = malloc (sizeof (EOS_CHAR));

  for (i = 0; i < EOS_NUMDEFAULTFILENAMES; i++)
    {
#ifdef _MSC_VER
    totChars = strlen(srchPathName) + 3 + strlen(defaultSesameFileNames[i]);
#else
    totChars = strlen (srchPathName) + 2 + strlen (defaultSesameFileNames[i]);
#endif
    tmp2 = realloc (tmp2, totChars * sizeof (EOS_CHAR));

    strcpy (tmp2, srchPathName);
#ifdef _MSC_VER
    strcat(tmp2, "\\");
#else
    strcat (tmp2, "/");
#endif
    strcat (tmp2, defaultSesameFileNames[i]);

    /* if file named tmp2 exists, is not a directory, is not longer than
     * PATH_MAX, and is not a repeat, then push onto *fileNames[] and
     * increment *fileNamesL.
     */

    fileExists = _eos_fileExistsAndValid(tmp2);

      if ((strlen (tmp2) <= PATH_MAX) && fileExists)
	{
	  *fileNames = (EOS_CHAR **) realloc (*fileNames, sizeof (EOS_CHAR *) * (*fileNamesL + 1));
	  (*fileNames)[*fileNamesL] = (EOS_CHAR *) malloc ((totChars + 1) * sizeof (EOS_CHAR));
      strcpy ((*fileNames)[*fileNamesL], tmp2);
      *fileNamesL += 1;
    }
  }

  EOS_FREE (tmp);
  EOS_FREE (tmp2);

  return (EOS_OK);
}

/*! *********************************************************************
 *
 * Function Name: eos_compareFiles
 * This function parses a text file named sesameFilesDir.txt that contains
 * available Sesame file names and returns the list to the calling routine.
 *
 * Returned Value:
 * EOS_INTEGER eos_compareFiles   - if returned value >0 : error code
 *                                                    =0 : no differences found
 *                                                    <0 : differences found
 *
 * Input Values:
 * EOS_CHAR    *file1             - character string of file name #1
 *                                  passed by reference
 * struct stat *stat_buf1         - pointer to stat results for *file1
 * EOS_CHAR    *file2             - character string of file name #2
 *                                  passed by reference
 * struct stat *stat_buf2         - pointer to stat results for *file2
 *
 * Uses the following macro(s):
 */
#define same_file(s,t) ((s).st_ino==(t).st_ino && (s).st_dev==(t).st_dev)
#define same_size(s,t) ((s).st_size==(t).st_size)
#define same_time(s,t) ((s).st_ctime==(t).st_ctime && (s).st_mtime==(t).st_mtime)
/*    same_file(stat1, stat2) is true if given device and inode values are same and valid
 *    same_size(stat1, stat2) is true if two files are same size
 *    same_time(stat1, stat2) is true if mtime and ctime are correspondingly the same
 *    where stat1 and stat2 are
 *    struct stat stat_buf1, stat_buf2; ** File status from stat() **
 *    stat (name1, &stat_buf1);
 *    stat (name2, &stat_buf2);
 *
 ************************************************************************/
static int comparison_counter = 0;
EOS_INTEGER
eos_compareFiles (EOS_CHAR * file1, struct stat *stat_buf1, EOS_CHAR * file2, struct stat *stat_buf2)
{
  EOS_INTEGER err = EOS_OK;

  //#define print_stat(n,s)  printf("%d %d %d %s\n", (int)(s).st_ino, (int)(s).st_dev, (int)(s).st_size, n);
  comparison_counter++;
#ifdef print_stat
  printf ("%d comparison(s) ------------------------------------\n", comparison_counter);
  print_stat(file1, *stat_buf1);
  print_stat(file2, *stat_buf2);
#endif

#ifndef _WIN32
  /* Using same_files(s,t), assume duplicated file names if inodes and
   * file devices are identical.
   * This test is irrelevant in a Win32 environment (i.e., MS Developer Studio),
   * or any environment that doesn't use unique device and/or inode values.
   */
  if (same_file (*stat_buf1, *stat_buf2))
    {
    err = EOS_OK;
    return err;
  }
#endif

#if ( defined _WIN32 || defined __CYGWIN__ || defined __CYGWIN32__ )
  /* Files are unique based upon file device and/or inode numbers;
   * however, this is not accurate in MS Visual Studio due to various
   * drive mapping capabilities in Windows. So, extend device and
   * inode comparison test to testing if same file sizes and times,
   * then do binary file comparison for completeness.
   */
  if (0)
    {
    FILE *fp1, *fp2;
    EOS_INTEGER n, n1, n2;
    EOS_CHAR *tmpStr1, *tmpStr2;

      if (same_size (*stat_buf1, *stat_buf2) && same_time (*stat_buf1, *stat_buf2))
	{

      /* do a binary file comparison here */
      n = BUFSIZ;
      fp1 = fopen (file1, "rb");
      fp2 = fopen (file2, "rb");
      tmpStr1 = (EOS_CHAR *) malloc (n * sizeof (EOS_CHAR));
      tmpStr2 = (EOS_CHAR *) malloc (n * sizeof (EOS_CHAR));
	  while (!feof (fp1) && !feof (fp2))
	    {
	n1 = fread (tmpStr1, sizeof (EOS_CHAR), n, fp1);
	n2 = fread (tmpStr2, sizeof (EOS_CHAR), n, fp2);
	      if (n1 != n2 || memcmp (tmpStr1, tmpStr2, n1))
		{
	  err = -1;
	  break;
	}
      }
      if (feof (fp1) != feof (fp2))
	err = -1;
      if (fp1)
	fclose (fp1);
      if (fp2)
	fclose (fp2);
      if (tmpStr1)
	EOS_FREE (tmpStr1);
      if (tmpStr2)
	EOS_FREE (tmpStr2);

    }
      else
	{

      /* files are unique based upon file sizes and/or times */
      err = -1;

    }
  }
  else
    {

    /* files are always assumed unique */
    err = -1;

  }
#else
  /* Files are unique based upon file device and/or inode numbers. */
  err = -1;
#endif

  return (err);
}

/*! *********************************************************************
 *
 * Function Name: _eos_compactFilesArray
 * This routine removes all empty strings from an array of file names.
 * The gMatidMap[].jfile values are updated if applicable.
 *
 * Returned Value:
 * EOS_INTEGER _eos_compactFilesArray  - error code
 * EOS_CHAR    ***files                - array of character strings/arrays
 *                                       passed by reference
 * EOS_INTEGER *filesL                 - number of strings in ***str array
 *                                       passed by reference
 *
 * Input Value:
 * EOS_BOOLEAN update_gMatidMap         - indicate whether the gMatidMap[].jfile
 *                                       values are updated (EOS_TRUE) or
 *                                       not (EOS_FALSE)
 *
 ************************************************************************/
EOS_INTEGER
_eos_compactFilesArray (EOS_CHAR *** files, EOS_INTEGER * filesL, EOS_BOOLEAN update_gMatidMap)
{
  EOS_INTEGER i, j, k = 0, m;
  EOS_CHAR *tmpStr;

  /* Shift all empty strings to the end of files[] array */
  for (i = 0; i < *filesL; i++)
    {
      if ((*files)[i] == NULL)
	{			/* found an empty string */
	  for (j = i; j < *filesL; j++)
	    {
	      if ((*files)[j] != NULL)
		{		/* found next non-empty string */
          /* swap (*files)[i] and (*files)[j] addresses */
          tmpStr = (*files)[i];
          (*files)[i] = (*files)[j];
          (*files)[j] = tmpStr;
          k++;

		  if (update_gMatidMap)
		    {
	    /* reset gMatidMap[].jfile value as necessary. */
		      for (m = 0; m < gMatidMapL; m++)
			{
			  if (gMatidMap[m].jfile == j)
			    gMatidMap[m].jfile = i;
	    }
	  }

          break;                /* leave j loop */
        }
      }
    }
      else
	{			/* found a non-empty string */
      k = i + 1;
    }
  }

  /* Reallocate files[] excluding all empty strings */
  for (i = k; i < *filesL; i++)
    {
    if ((*files)[i])
      EOS_FREE ((*files)[i]);
  }
  *filesL = k;
  *files = (EOS_CHAR **) realloc (*files, sizeof (EOS_CHAR *) * (k));

  return (EOS_OK);
}

EOS_INTEGER
_eos_mag (EOS_REAL g, EOS_INTEGER gamma)
{
  // calculate the base-10 order-of-_eos_magnitude of g, and
  // place lower limit on returned value
  EOS_REAL ten = (EOS_REAL) 10;
  EOS_REAL returnVal;
  returnVal = log10 (MAX (ABS (g), pow (ten, (EOS_REAL) gamma)));
  return ((EOS_INTEGER) returnVal);
}

#define COMPILE_DEBUG_INSTRUCTIONS

typedef struct
{
    EOS_INTEGER Mi;
    EOS_INTEGER Mim1;
    EOS_INTEGER Ni;
    EOS_INTEGER Oi;
    EOS_INTEGER Mi2;
    EOS_REAL    Pim1;
    EOS_REAL    Pi2;
    EOS_REAL    Qi;
    EOS_REAL    dFmin;
    EOS_REAL    x_ratio;
    EOS_REAL    f_init;
} CALCFMOD_DEBUG_STR;

EOS_REAL
_eos_calcFmod (EOS_REAL Fim1, EOS_REAL Fi, EOS_REAL x0, EOS_REAL xmax, EOS_REAL xi, EOS_REAL trend, void *calcFmod_debug)
{
  EOS_INTEGER zero = (EOS_INTEGER) 0;
  EOS_REAL ten = (EOS_REAL) 10;
  EOS_INTEGER gamma = (EOS_INTEGER) (-6);
  EOS_INTEGER Mi, Mim1, Ni, Oi, Mi2;
  EOS_REAL Pim1, Pi2, Qi, dFmin, x_ratio, Fmod;

  // Calculate some required values
  Mim1 = _eos_mag (Fim1, gamma);
  Pim1 = pow (ten, (EOS_REAL) MIN (zero, Mim1));
  Mi = _eos_mag (Fi, gamma);
  Ni = MAX (zero, Mi);
  Oi = Ni + gamma;
  dFmin = pow (ten, (EOS_REAL) Oi);
  x_ratio = (xi - x0) / (xmax - x0);
  Mi2 = _eos_mag (x_ratio, gamma);
  Pi2 = pow (ten, (EOS_REAL) MIN (zero, Mi2));
  Qi = x_ratio / Pi2;

  // Calculate Fmod
  Fmod = (Fim1 / Pim1 + trend * dFmin * Qi) * Pim1;

#ifdef COMPILE_DEBUG_INSTRUCTIONS
  // Conditionally store calcFmod_debug
  if (calcFmod_debug)
    {
    CALCFMOD_DEBUG_STR *debug_str_ptr = (CALCFMOD_DEBUG_STR*)calcFmod_debug;
    debug_str_ptr->Mi      = Mi;
    debug_str_ptr->Mim1    = Mim1;
    debug_str_ptr->Ni      = Ni;
    debug_str_ptr->Oi      = Oi;
    debug_str_ptr->Mi2     = Mi2;
    debug_str_ptr->Pim1    = Pim1;
    debug_str_ptr->Pi2     = Pi2;
    debug_str_ptr->Qi      = Qi;
    debug_str_ptr->dFmin   = dFmin;
    debug_str_ptr->x_ratio = x_ratio;
    debug_str_ptr->f_init  = Fi;
  }
#endif

  // return Fmod
  return (Fmod);
}

/*
 * ==================================================================
 *
 * name      -- _eos_MakeMonotonic
 *
 * purpose   -- force monotonicity of F w.r.t. to x and/or y.
 * arguments --
 *   nx         = input  INTEGER number of x values.
 *   ny         = input  INTEGER number of y values.
 *   x          = input  REAL    array of x values.
 *   y          = input  REAL    array of y values.
 *   F          = in/out REAL    array of F values.
 *   indep      = input  INTEGER define which independent variables w.r.t.
 *                       which F is to be monotonic:
 *                       if(indep==1) monotonic w.r.t. x
 *                       if(indep==2) monotonic w.r.t. x & y
 *                       if(indep==3) monotonic w.r.t. y
 *   F_shift    = input  REAL    optional array of nx values, which are
 *                               added to F before and after forcing
 *                               monotonicity. Ignored if F_shift=NULL.
 *   enableDebugDump = input EOS_BOOLEAN enable debug output
 *
 * return value -- integer error indicator,
 *                     EOS_OK if there are no errors.
 *
 * ==================================================================
 */
EOS_INTEGER
_eos_MakeMonotonic (EOS_INTEGER nx, EOS_INTEGER ny, EOS_REAL x[], EOS_REAL y[], EOS_REAL F[], EOS_INTEGER indep, EOS_REAL F_shift[], EOS_BOOLEAN enableDebugDump)
{
  EOS_INTEGER zero = (EOS_INTEGER) 0;
  EOS_REAL zero_r = (EOS_REAL) 0;
  EOS_INTEGER i, j, ierr4;
  EOS_REAL trend, Fmod, val0, val1;
#ifdef COMPILE_DEBUG_INSTRUCTIONS
  CALCFMOD_DEBUG_STR _calcFmod_debug;
  CALCFMOD_DEBUG_STR *calcFmod_debug = NULL;
#endif

  if (indep == 1)
    {				// guarantee monotonicity of F w.r.t. x

      if (nx >= 3)
	{			// check for sufficient x-values

      _exceptions.fmin = 1.e99;
	  for (i = 1; i < nx; i++)
	    {			// find exceptions data along first line
	      if (_exceptions.fmin > F[i])
		{
	  _exceptions.fmin = F[i];
	  _exceptions.fmin_index = i;
	}
	      if (F[i] >= 0.)
		{
	  _exceptions.x_threshold_index = i;
	  _exceptions.x_threshold       = x[i];
	  _exceptions.xvals             = x;
	  break;
	}
      }	

      // remove all loops from each x-line of F
      ierr4 = _eos_RemoveLoops (nx, ny, F, (EOS_INTEGER) 1, &_exceptions);

      // force monotonicity of F w.r.t. x according to trend
	  for (j = 0; j < ny; j++)
	    {

	// determine if monotonic-increasing or -decreasing is desired along current constant-y array
	trend = (F[j * nx + (nx-1)] < F[j * nx]) ? (EOS_REAL) (-1) : (EOS_REAL) (1);

	      for (i = 1; i < nx; i++)
		{
          // Calculate Fmod, which is the bounding value of F[i][j]
	  val0 = (F_shift) ? F_shift[i - 1] : zero_r;
	  val1 = (F_shift) ? F_shift[i] : zero_r;
		  Fmod = _eos_calcFmod (F[(i - 1) + nx * j] + val0, F[i + nx * j] + val1, x[0], x[nx - 1], x[i], trend, (void *) calcFmod_debug);

          // Reset F[i][j] if needed
          if (trend < zero)
            F[i + nx * j] = MIN (F[i + nx * j] + val1, Fmod) - val1;
          else
            F[i + nx * j] = MAX (F[i + nx * j] + val1, Fmod) - val1;
        }                       /* nx loop */
      }                         /* ny loop */
    }                           /* if nx > 3 */
  }                             /* wrp to x */
  else if (indep == 2)
    {				// guarantee monotonicity of F w.r.t. x & y

    // w.r.t. x
    ierr4 = _eos_MakeMonotonic (nx, ny, &x[0], &y[0], &F[0], (EOS_INTEGER) 1, F_shift, enableDebugDump);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr4) != EOS_OK)
      return (ierr4);

    // w.r.t. y (disable F0x, F0y and divisor for this iteration)
    ierr4 = _eos_MakeMonotonic (nx, ny, &x[0], &y[0], &F[0], (EOS_INTEGER) 3, F_shift, enableDebugDump);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr4) != EOS_OK)
      return (ierr4);

  }
  else if (indep == 3)
    {				// guarantee monotonicity of F w.r.t. y

      if (ny >= 3)
	{			// check for sufficient y-values

      EOS_REAL global_tiny = HUGE_D;

      // remove all loops from each y-line of F
      ierr4 = _eos_RemoveLoops (nx, ny, F, (EOS_INTEGER) 2, (_EXCEPTIONS_T*)NULL);

      // store the magnitude of value in F[] that has the smallest magnitude
	  for (i = 0; i < nx; i++)
	    {
	      for (j = 0; j < ny; j++)
		{
		  if (ABS (F[i + nx * j]) > 0.0 && global_tiny > ABS (F[i + nx * j]))
		    global_tiny = ABS (F[i + nx * j]);
	}
      }

      // force monotonicity of F w.r.t. y according to trend
	  for (i = 0; i < nx; i++)
	    {

#ifdef COMPILE_DEBUG_INSTRUCTIONS
	      if (i <= 2 && enableDebugDump)
		{
	  //if(i > 0) printf("debug:\ndebug:\n");
		  printf ("debug: %-4s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s\n", "#j", "y", "Fmod", "f_debug", "%", "Mi", "Mim1", "Ni", "Oi", "Mi2", "Pim1", "Pi2", "Qi", "dFmin", "x_ratio", "f_init");
	}
	      if (enableDebugDump)
		{
	  calcFmod_debug = &_calcFmod_debug;
	  calcFmod_debug->Mi      = 0;
	  calcFmod_debug->Mim1    = 0;
	  calcFmod_debug->Ni      = 0;
	  calcFmod_debug->Oi      = 0;
	  calcFmod_debug->Mi2     = 0;
	  calcFmod_debug->Pim1    = 0;
	  calcFmod_debug->Pi2     = 0;
	  calcFmod_debug->Qi      = 0;
	  calcFmod_debug->dFmin   = 0;
	  calcFmod_debug->x_ratio = 0;
	  calcFmod_debug->f_init  = 0;
	}
#endif

	// determine if monotonic-increasing or -decreasing is desired along current constant-x array
	trend = (F[i + (ny-1) * nx] < F[i]) ? (EOS_REAL) (-1) : (EOS_REAL) (1);

	      for (j = 0; j < ny; j++)
		{

		  if (j > 0)
		    {
	    // Calculate Fmod, which is the bounding value of F[i][j]
	    val1 = (F_shift) ? F_shift[i] : zero_r;
#if 0
		      Fmod = _eos_calcFmod (F[i + nx * (j - 1)] + val1, F[i + nx * j] + val1, y[0], y[ny - 1], y[j], trend, (void *) calcFmod_debug);
#else
	    Fmod = MAX(F[i + nx * j] + val1, F[i + nx * (j - 1)] + val1 + global_tiny / 10.0);
#endif
	    /* Reset F[i][j] if needed */
	    if (trend < zero)
	      F[i + nx * j] = MIN (F[i + nx * j] + val1, Fmod) - val1;
	    else
	      F[i + nx * j] = MAX (F[i + nx * j] + val1, Fmod) - val1;
	  }

#ifdef COMPILE_DEBUG_INSTRUCTIONS
		  if (i <= 2 && enableDebugDump)
		    {
	    calcFmod_debug = &_calcFmod_debug;
	    printf ("debug: %-4i %12g %12g %12g %12g %12i %12i %12i %12i %12i %12g %12g %12g %12g %12g %12g\n",
		    /* i + nx * */ j, y[j], F[i + nx * j], F[i + nx * j], 0.0,
			      calcFmod_debug->Mi, calcFmod_debug->Mim1, calcFmod_debug->Ni, calcFmod_debug->Oi, calcFmod_debug->Mi2, calcFmod_debug->Pim1, calcFmod_debug->Pi2, calcFmod_debug->Qi, calcFmod_debug->dFmin, calcFmod_debug->x_ratio, calcFmod_debug->f_init);

	  }
#endif
        }                       /* ny loop */
      }                         /*nx loop */
    }
  }

  return (EOS_OK);
}

/*! *********************************************************************
 * nx         = input  INTEGER x-dimension of F[x][y]
 * ny         = input  INTEGER y-dimension of F[x][y]
 * F          = in/out REAL    2-D data table, F[x][y]
 * dim        = input  INTEGER indicates on which dimension of F to
 *                     remove phase-change loops
 *                     if(dim==1) remove loops holding x fixed
 *                     if(dim==2) remove loops holding y fixed
 * 
 * return 0 if there are no errors,
 *        1 if dim is invalid
 *        2 if invalid search result
 *        3 if calloc failed
 ************************************************************************/
int
_eos_RemoveLoops (EOS_INTEGER nx, EOS_INTEGER ny, EOS_REAL F[], EOS_INTEGER dim, _EXCEPTIONS_T * e)
{
  int i, j, step, stepj, imax, jmax, indexm1, index, indexp1, totPairs, pairIndex, minCntr, maxCntr;
  int *maxIndex, *minIndex;
  EOS_REAL *avgValue;
  EOS_INTEGER backwardDiffSign, forwardDiffSign;
  EOS_REAL zero = (EOS_REAL) 0;
  EOS_REAL two = (EOS_REAL) 2;

  if (dim == 1)
    {
    step = nx;
    stepj = 1;
    imax = nx * ny;
    jmax = nx - 1;
  }
  else if (dim == 2)
    {
    step = 1;
    stepj = nx;
    imax = nx;
    jmax = ny - 1;
  }
  else
    return (EOS_FAILED);

  // allocate space for maximum possible max-min pairs
  if ((maxIndex = (int *) calloc (jmax, sizeof (int))) == NULL)
    return (EOS_FAILED);
  if ((minIndex = (int *) calloc (jmax, sizeof (int))) == NULL)
    return (EOS_FAILED);
  if ((avgValue = (EOS_REAL *) calloc (jmax, sizeof (EOS_REAL))) == NULL)
    return (EOS_FAILED);

  for (i = 0; i < imax; i += step)
    {				// current dim-line
    totPairs = 0;               // intialize max-min pair counter
    maxCntr = 0;                // intialize max counter
    minCntr = 0;                // intialize min counter

    // find all max-min pairs in F for loops on current dim-line
      for (j = 1; j < jmax; j++)
	{
      indexm1 = i + (j - 1) * stepj;
      index = i + (j) * stepj;  // current element of dim-line
      indexp1 = i + (j + 1) * stepj;
      backwardDiffSign = SIGN (F[index] - F[indexm1]);
      forwardDiffSign = SIGN (F[indexp1] - F[index]);

	  if (e != NULL)
	    {			/* exception(s) have been provided */
	if ( e->fmin < F[index] && index <= e->fmin_index )
	  backwardDiffSign = forwardDiffSign;
      }

	  if (backwardDiffSign != forwardDiffSign)
	    {
        // found a local-max or -min

	      if ((maxCntr == 0) && (maxIndex[maxCntr] == 0) && backwardDiffSign < 0)
		{
          // found min first along dim-line
          maxIndex[maxCntr] = i;        // save local-max index
          minIndex[minCntr] = index;    // save local-min index
          maxCntr++;            // increment max counter
          minCntr++;            // increment min counter
        }
	      else if ((backwardDiffSign >= 0) && (forwardDiffSign <= 0))
		{
          // found local-max or start of a Maxwellian region
          maxIndex[maxCntr] = index;    // save local-max index
          maxCntr++;            // increment max counter
        }
	      else if ((backwardDiffSign <= 0) && (forwardDiffSign >= 0))
		{
          // found local-min or end of a Maxwellian region
          minIndex[minCntr] = index;    // save local-min index
          minCntr++;            // increment min counter
        }
        else
          return (EOS_FAILED);  // invalid search result
      }
    }

    // consider that local-max found last instead of local-min
    totPairs = maxCntr;         // reset max-min pair counter
      if (maxCntr > minCntr)
	{
      minIndex[totPairs] = jmax - 1;
      minCntr = maxCntr;
    }
    if (!totPairs)
      continue;                 // no loops in current dim-line

    // calculate averages of all max-min pairs F-values
    for (j = 0; j < totPairs; j++)
      avgValue[j] = (F[maxIndex[j]] + F[minIndex[j]]) / two;

    // Remove loops from current dim-line
    pairIndex = 0;
      for (j = 0; j < jmax; j++)
	{
      if (pairIndex >= totPairs)
        break;                  // all loops removed

      index = i + (j) * stepj;  // current element of dim-line
      indexp1 = i + (j + 1) * stepj;

      // reset F-value
	  if ((F[index] > avgValue[pairIndex]) || ((F[index] < avgValue[pairIndex]) && (index >= maxIndex[pairIndex])))
        F[index] = avgValue[pairIndex];

      // reset arrays' values consider next max-min pair
	  if ((index >= minIndex[pairIndex]) && (F[indexp1] > avgValue[pairIndex]))
	    {
        maxIndex[pairIndex] = 0;
        minIndex[pairIndex] = 0;
        avgValue[pairIndex] = zero;
        pairIndex++;
      }
    }
  }
  // release temporary arrays
  EOS_FREE (maxIndex);
  EOS_FREE (minIndex);
  EOS_FREE (avgValue);
  return (EOS_OK);
}

/*! ********************************************************************
 *
 *  purpose   -- performs data smoothing for a sesame table.
 *            -- force linear temperature dependence for low temp,
 *               force linear density dependence for low and high dens.
 *  arguments --
 *    nX       = input  integer number of X values.
 *    nY       = input  integer number of Y values.
 *    X        = input  real X array.
 *    Y        = input  real Y array.
 *    F        = in/out real F array.
 *
 *  return value -- integer error indicator,
 *                      00 if there are no errors,
 *
 ***********************************************************************/
EOS_INTEGER
_eos_MakeSmooth (EOS_INTEGER nX, EOS_INTEGER nY, EOS_REAL X[], EOS_REAL Y[], EOS_REAL F[])
{
  EOS_INTEGER j, jpre1, jpre2, jpre3, jpres;
  EOS_REAL ftemp, dpres, fdens, fden1, fden3;

  if (nX < 3 || nY < 3)
    return EOS_FAILED;

  // force linear temperature dependence for low temperature.
  ftemp = (Y[1] - Y[0]) / (Y[2] - Y[0]);
  for (j = 0; j < nX; j++)
    {
    jpre1 = j;
    jpre2 = jpre1 + nX;
    jpre3 = jpre2 + nX;
    F[jpre2] = F[jpre3] * ftemp + F[jpre1] * ((EOS_REAL) 1.0 - ftemp);
  }

  // force linear density dependence for low and high density.   
  fdens = (X[1] - X[0]) / (X[2] - X[0]);
  jpres = -nX;
  for (j = 0; j < nY; j++)
    {
    jpres = jpres + nX;
    F[jpres + 1] = F[jpres + 2] * fdens + F[jpres] * ((EOS_REAL) 1.0 - fdens);
  }

  fdens = (X[nX - 2] - X[nX - 1]) / (X[nX - 3] - X[nX - 1]);
  fden1 = X[nX - 2] / X[nX - 1];
  fden3 = X[nX - 2] / X[nX - 3];

  jpres = nX - 1;
  F[nX - 2] = F[nX - 3] * fdens * fden3 + F[nX - 1] * ((EOS_REAL) 1.0 - fdens) * fden1;
  for (j = 1; j < nY; j++)
    {
    jpres = jpres + nX;
    dpres = (F[jpres - 2] - F[nX - 3]) / X[nX - 3];
    F[jpres - 1] = F[nX - 2] + dpres * X[nX - 2];
    F[jpres] = F[nX - 1] + dpres * X[nX - 1];
  }

  return (EOS_OK);
}

/*! ********************************************************************
 *
 *  purpose   -- convert a EOS_REAL value to a string in scientific notation
 *               given a specified maximum string length.
 *  arguments --
 *    x       = input  number to convert to string
 *    fw      = input  field width of converted number
 *    buffer  = output string into which result is placed
 *
 ***********************************************************************/
void
_eos_dbl2String (EOS_REAL x, EOS_INTEGER fw, char *buffer)
{
  EOS_INTEGER p;
  p = fw - 8;
  sprintf (buffer, "%#*.*E", fw, p, x);
}

/*! *********************************************************************
 * 
 * This function returns ANY kind of option: interpolation, loading, general
 * given a table handle
 * 
 * Returned Values:
 * EOS_REAL optionVal
 * EOS_INTEGER *err - error flag
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * EOS_INTEGER optFlag
 * 
 ************************************************************************/
EOS_REAL
eos_getRealOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err)
{
  eos_OptionValue *optVal = NULL;
  EOS_BOOLEAN bv;
  *err = EOS_OK;

  if (!EOS_IS_INTERPOLATION_OPTION (optFlag))
    {
    eos_GetOptionEosDataMap (&gEosDataMap, th, optFlag, &optVal, err);
      if (!optVal || eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
	{
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
        *err = EOS_INVALID_OPTION_FLAG;
      return 0;
    }

    return optVal->rval;
  }
  else
    {				/* interpolation option, they are only boolean! */

    eos_GetOptionEosInterpolation (&gEosInterpolation, th, optFlag, &bv, err);
    return (EOS_REAL) bv;
  }
}


/*! *********************************************************************
 * 
 * This function returns ANY kind of option: interpolation, loading, general
 * given a table handle
 * 
 * Returned Values:
 * EOS_INTEGER optionVal
 * EOS_INTEGER *err - error flag
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * EOS_INTEGER optFlag
 * 
 ************************************************************************/
EOS_INTEGER
eos_getIntOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err)
{
  eos_OptionValue *optVal = NULL;
  EOS_BOOLEAN bv;
  *err = EOS_OK;

  if (!EOS_IS_INTERPOLATION_OPTION (optFlag))
    {
    eos_GetOptionEosDataMap (&gEosDataMap, th, optFlag, &optVal, err);
      if (!optVal || eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
	{
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
        *err = EOS_INVALID_OPTION_FLAG;
      return 0;
    }

    return optVal->ival;
  }
  else
    {				/* interpolation option, they are only boolean! */

    eos_GetOptionEosInterpolation (&gEosInterpolation, th, optFlag, &bv, err);
    return (EOS_INTEGER) bv;
  }
}

/*! *********************************************************************
 * 
 * This function returns ANY kind of option: interpolation, loading, general
 * given a table handle
 * 
 * Returned Values:
 * EOS_BOOLEAN optionVal
 * EOS_INTEGER *err - error flag
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * EOS_INTEGER optFlag
 * 
 ************************************************************************/
EOS_BOOLEAN
eos_getBoolOptionFromTableHandle (EOS_INTEGER th, EOS_INTEGER optFlag, EOS_INTEGER * err)
{
  eos_OptionValue *optVal = NULL;
  EOS_BOOLEAN bv;
  *err = EOS_OK;

  if (!EOS_IS_INTERPOLATION_OPTION (optFlag))
    {
    eos_GetOptionEosDataMap (&gEosDataMap, th, optFlag, &optVal, err);
      if (!optVal || eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
	{
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_OK)
        *err = EOS_INVALID_OPTION_FLAG;
      return EOS_FALSE;
    }

    return optVal->bval;
  }
  else
    {				/* interpolation option, they are only boolean! */

    eos_GetOptionEosInterpolation (&gEosInterpolation, th, optFlag, &bv, err);
    return bv;
  }
}

/*! *********************************************************************
 * 
 * This function returns conversion factors
 * given a table handle, and OPTIONAL dataType, in case we need to search
 * for conv. factors of type other than the one associated with given th,
 * but using the same dataObject.
 * 
 * Returned Values:
 * EOS_INTEGER *err - error flag
 * EOS_REAL *convX, *convY, *convF
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * EOS_INTEGER *dataType (see explanation above)
 * EOS_INTEGER optFlag
 * 
 ************************************************************************/
void
eos_GetConversionFactorsFromTableHandle (EOS_INTEGER th, EOS_INTEGER * dataType, EOS_REAL * convX, EOS_REAL * convY, EOS_REAL * convF, EOS_INTEGER * err)
{
  EOS_INTEGER i, tableHandle = -1;
  *err = EOS_OK;

  if (!eos_IsHandleValid (th))
    {
    *err = EOS_INVALID_TABLE_HANDLE;
    return;
  }

  if (dataType == NULL || gEosDataMap.tableTypes[th] == *dataType)
    tableHandle = th;
  else
    {				/* search for the table handles which shares our object but has type *dataType */

      for (i = 0; i < gEosDataMap.nHandles; i++)
	{
	  if (gEosDataMap.tableHandlesMap[i] == gEosDataMap.tableHandlesMap[th] && gEosDataMap.tableTypes[i] == *dataType)
	    {
        tableHandle = i;
        break;
      }
    }

      if (tableHandle < 0)
	{			/* not found */
      *convX = *convY = *convF = 1.0;
      return;
    }
  }

  *convX = eos_getRealOptionFromTableHandle (tableHandle, EOS_X_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    *convX = 1.0;
  *convY = eos_getRealOptionFromTableHandle (tableHandle, EOS_Y_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    *convY = 1.0;
  *convF = eos_getRealOptionFromTableHandle (tableHandle, EOS_F_CONVERT, err);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    *convF = 1.0;
}

/*! *********************************************************************
 * 
 * This function returns dataType given a table handle
 * 
 * Returned Values:
 * EOS_BOOLEAN optionVal
 * EOS_INTEGER *err - error flag
 *
 * Input Value:
 * EOS_INTEGER th - table handle
 * 
 ************************************************************************/
EOS_INTEGER
eos_GetDataTypeFromTableHandle (EOS_INTEGER th, EOS_INTEGER * err)
{
  *err = EOS_OK;

  if (!eos_IsHandleValid (th))
    {
    *err = EOS_INVALID_TABLE_HANDLE;
    return 0;
  }

  return gEosDataMap.tableTypes[th];
}

/***********************************************************************/
/*!
 * \brief Get the data type description corresponding to the provided table handle, th.
 *
 * \param[in]     th          - EOS_INTEGER : specified table handle
 *
 * \return        description - EOS_CHAR*   : eos_TableList[i].eosTableName; possibly modified from default
 *
 ***********************************************************************/
EOS_CHAR *
eos_GetDataTypeDescriptionFromTableHandle (EOS_INTEGER th)
{
  EOS_CHAR pattern0[40], pattern[20], pattern1[20], *description = NULL;
  EOS_INTEGER err = EOS_OK;
  EOS_INTEGER dataType = eos_GetDataTypeFromTableHandle (th, &err);

  if (!eos_TableListReverseMap)
    _eos_SetTableListReverseMap ();	/* build reverse lookup for eos_TableList[] */

  description = (EOS_CHAR *) malloc (sizeof (EOS_CHAR) * (EOS_META_DATA_STRLEN + 1));
  sprintf (description, "%s", EOS_TYPE_TO_TAB_NAME (dataType));

  switch (EOS_TYPE_TO_INDEP_VAR2 (dataType))
  {
    /* special cases for tables that include cold curve data */
  case EOS_Pt:
    sprintf (pattern, "Total Pressure");
    sprintf (pattern1, " Pressure");
    break;
  case EOS_Ut:
  case EOS_At:
    sprintf (pattern, "Total Specific");
    sprintf (pattern1, " Specific");
    break;
  case EOS_Pic:
    sprintf (pattern, "Ion Pressure");
    sprintf (pattern1, " Ion Pressure");
    break;
  case EOS_Uic:
  case EOS_Aic:
    sprintf (pattern, "Ion Specific");
    sprintf (pattern1, " Ion Specific");
    break;
  default:
    break;
  }

  sprintf (pattern0, "%s", "");
  switch (EOS_TYPE_TO_INDEP_VAR2 (dataType))
  {
    /* special cases for tables that include cold curve data */
  case EOS_Pt:
  case EOS_Pic:
    sprintf (pattern0, "%s", "Density-Normalized ");
  case EOS_Ut:
  case EOS_At:
  case EOS_Uic:
  case EOS_Aic:
    {
      EOS_CHAR *s = NULL, *cp = NULL;
      s = _eos_ReplaceSubstring (EOS_TYPE_TO_TAB_NAME (dataType), pattern, strcat (strcat (pattern0, "Thermal"), pattern1));
      sprintf (description, "%s", s);
      EOS_FREE(s);
      cp = strrstr (description, "GPa)-");	/* find right-most instance of "GPa)-" in description */
      if (cp) {
        EOS_CHAR *s = NULL;
        s = _eos_ReplaceSubstring (cp, "GPa)-", "MJ/kg)-");
        sprintf (cp, "%s", s);
        EOS_FREE(s);
      }
    }
    break;
  default:
    break;
  }

  return description;
}

/*! *********************************************************************
 *
 * function _eos_InterpolateRecordType_1D (helping function to perform 1-D interpolation of F(X) function.)
 * 
 * The input arguments are:
 * 	EOS_INTEGER nX            input: number of elements in either X or F.
 * 	EOS_REAL*   X             input: array of the data corresponding to X.
 * 	EOS_REAL*   F             input: array of the data corresponding to F.
 * 	EOS_REAL*   F2            input: array of the data corresponding to REF2 type corresponding to F.
 * 	EOS_INTEGER th            table handle
 * 	EOS_INTEGER dataType
 * 	EOS_INTEGER nXYPairs	  total number of pairs of independent variable values provided for interpolation.
 * 	EOS_REAL xVals[nXYPairs]  array of the primary independent variable values to use during interpolation. 
 * 
 * The output arguments are:
 * 	EOS_REAL fVals[nXYPairs]  array of the interpolated data corresponding to x and y. 
 * 	EOS_REAL dFx[nXYPairs]	  array of the interpolated partial derivatives of fVals with respect to x. 
 * 	EOS_INTEGER errorCode	  error code of the interpolation: EOS_INTERP_EXTRAPOLATED or EOS_OK
 *      EOS_INTEGER *xyBounds     interpolation errors per xy-pair
 *      EOS_CHAR    **errMsg      custom error message
 * 
 ************************************************************************/

void
_eos_InterpolateRecordType_1D (EOS_INTEGER nX, EOS_REAL * X, EOS_REAL * F, EOS_REAL * F2, EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER nXYPairs, EOS_REAL * srchX, EOS_REAL * fVals, EOS_REAL * dFx, EOS_INTEGER * xyBounds, EOS_INTEGER * errorCode, EOS_CHAR ** errMsg)
{
  EOS_INTEGER err, doRational = 0, i, tabInd1, tabInd2, cat, *xyBounds2;
  EOS_REAL *uVals, *dUx, *xVals;
  EOS_INTEGER nxtbl, nytbl, nGhostData;
  EOS_REAL *xtbls=NULL, *ytbls=NULL, **ftbls=NULL;

  *errorCode = EOS_OK;
  err = EOS_OK;
  cat = EOS_CATEGORY (dataType);

  xVals = srchX;

  switch (cat)
    {
  case EOS_CATEGORY0:          /* indicates the table is not inverted */
    {
      doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);
	if (doRational)
	  {
	/* add "ghost node" data prior to interpolation */
	nGhostData = 1;
	    _eos_CreateGhostData (nGhostData, nX, 0, X, NULL, &F, NULL, &nxtbl, &nytbl, &xtbls, &ytbls, &ftbls, NULL, &err, errMsg);
	    if (eos_GetStandardErrorCodeFromCustomErrorCode (err) != EOS_OK)
	      {
	  *errorCode = err;
	  return;
	}
        err = EOS_OK;
	    eos_RationalInterpolate (nXYPairs, nxtbl, 1, 0, xtbls, *ftbls, xVals, fVals, dFx, 'y', xyBounds, &err);

	/* ignore old extrapolation detection logic */
	if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_INTERP_EXTRAPOLATED)
	  err = EOS_OK;

	/* set extrapolation error codes appropriately */
	    _eos_CheckExtrapCategory0 (nGhostData, nxtbl, nytbl, xtbls, NULL, nXYPairs, xVals, NULL, xyBounds, &err);

	/* free memory containing "ghost node" data */
	_eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, NULL);
      }
	else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err))
	  {			/* interpolate linearly instead */
#define USE_NEW_LINEAR_DERIVATIVE_LOGIC
#ifndef USE_NEW_LINEAR_DERIVATIVE_LOGIC
	    eos_LineInterpolate (EOS_TRUE, nXYPairs, nX, 1, 0, X, &F, xVals, fVals, dFx, 'y', xyBounds, &err);
#else
	    eos_LineInterpolate (eos_getBoolOptionFromTableHandle (th, EOS_DISCONTINUOUS_DERIVATIVES, &err), nXYPairs, nX, 1, 0, X, &F, xVals, fVals, dFx, 'y', xyBounds, &err);
#endif
      }

      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      break;
    }
  case EOS_CATEGORY1:          /* indicates the table is inverted with respect to the independent variable */
    {
      /* means that the user provides F of a function that is defined as x(F)
         so effectively the x given us is F  */

      doRational = eos_getBoolOptionFromTableHandle (th, EOS_RATIONAL, &err);
      /* do inverse interpolation */
	if (doRational)
	  {
        err = EOS_OK;
	    eos_InverseRationalInterpolateF (EOS_FALSE, nXYPairs, xVals, dFx, fVals, nX, X, F, xyBounds, &err);
      }
      else if (eos_getBoolOptionFromTableHandle (th, EOS_LINEAR, &err)) /* interpolate linearly instead */
	  eos_InverseInterpolateF (nXYPairs, xVals, dFx, fVals, nX, X, F, xyBounds, &err);
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) != EOS_OK)
        *errorCode = err;
      if (eos_GetStandardErrorCodeFromCustomErrorCode(err) == EOS_UNDEFINED)
        break;

      /* derivative of our function: x(F), so dF/dX = 1/dFx; */
      for (i = 0; i < nXYPairs; i++)
        dFx[i] = 1 / MAX (TINY_D, dFx[i]);
      break;
    }
  case EOS_CATEGORY3:          /* indicates the table is merged with another function */
    {
      /* get the data pointers and types for these valuables */
      tabInd1 = EOS_EOS_TABLE_TYPE_REF1 (dataType);
      tabInd2 = EOS_EOS_TABLE_TYPE_REF2 (dataType);

      /* We need F(X) given F(U) and U(X) */
      /* 1. find U using second table, 2. find F using 1st table. */
      uVals = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      dUx = (EOS_REAL *) malloc (sizeof (EOS_REAL) * nXYPairs);
      xyBounds2 = (EOS_INTEGER *) malloc (sizeof (EOS_INTEGER) * nXYPairs);

	_eos_InterpolateRecordType_1D (nX, X, F2, NULL, th, tabInd2, nXYPairs, xVals, uVals, dUx, xyBounds, errorCode, errMsg);
	if (eos_GetStandardErrorCodeFromCustomErrorCode (*errorCode) == EOS_UNDEFINED)
	  {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        break;
      }
	_eos_InterpolateRecordType_1D (nX, X, F, NULL, th, tabInd1, nXYPairs, uVals, fVals, dFx /*dFu */ ,
                                     xyBounds2, errorCode, errMsg);
      EOS_FREE(xyBounds2);
	if (eos_GetStandardErrorCodeFromCustomErrorCode (*errorCode) == EOS_UNDEFINED)
	  {
        EOS_FREE (dUx);
        EOS_FREE (uVals);
        break;
      }
      /* derivative dFx = dFu * dUx */
	for (i = 0; i < nXYPairs; i++)
	  {

	/* DAP -- Test multiplication with EOS_CHECK_PRODUCT prior to performing it.
	          This is a temporary kludge to prevent SIGFPE. */
	if (EOS_CHECK_PRODUCT(dUx[i], dFx[i]))
	  dFx[i] = dUx[i] * dFx[i] /*dFu */ ;
	else
	      dFx[i] = SIGN (dUx[i]) * SIGN (dFx[i]) * (EOS_IS_PRODUCT_GT_MAX (dUx[i], dFx[i]) ? DBL_MAX : DBL_MIN);

      }
      EOS_FREE (dUx);
      EOS_FREE (uVals);
      break;
    }
  default:
    {
      *errorCode = EOS_INVALID_TABLE_HANDLE;
      break;
    }
  }

  if (srchX != xVals)
    EOS_FREE (xVals);
}

/*! *********************************************************************
 *
 * function eos_GetCustomErrorMsg
 *      Get a custom error message associated with a specified error code and
 *      table handle.
 * 
 * The arguments are:
 * 	      EOS_INTEGER th      input: associated table handle.
 * 	const EOS_INTEGER err     input: error code.
 * 
 ************************************************************************/
EOS_CHAR *
eos_GetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err)
{
  EOS_INTEGER standard_err;

  if (! eos_IsErrorCodeValid (err))
    return "";

  /* ensure memory access is limited to appropriate bounds */
  standard_err = eos_GetStandardErrorCodeFromCustomErrorCode(err);

  if (th < 0 || th >= gEosDataMap.nAlloc)
    return "";

  if (! gEosDataMap.customErrorMsg[th] )
    return "";

  if (th >= 0 && th < gEosDataMap.nAlloc && eos_IsErrorCodeValid (err))
    {
    if (gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE])
      return gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE]; 
  }

  return "";
}

/*! *********************************************************************
 *
 * function eos_SetCustomMsg
 *      Set a custom message to a character string; allocate if necessary.
 * 
 * The arguments are:
 * 	      EOS_CHAR    **msg  output: character string pointer.
 * 	const EOS_CHAR    *fmt    input: format string.
 * 	                  ...     input: arbitrary number of arguments.
 * 
 ************************************************************************/
void
eos_SetCustomMsg_str (EOS_CHAR ** msg, const EOS_CHAR * fmt, ...)
{
  va_list args;

  if (! msg)
    return;

  if (! *msg)
    *msg = (EOS_CHAR *) malloc (EOS_MaxErrMsgLen * sizeof (EOS_CHAR));

  va_start (args, fmt);
  vsprintf (*msg, fmt, args);
  va_end (args);
}

/*! \bug I'm not sure what it means to have an 'inline' function defined in an
 *       implementation file.  MSVC doesn't like it when creating a dll version 
 *       of the library, so I've added this little hack.  Linux builds seem to
 *       ignore the 'inline' part. */
#ifndef _MSC_VER
inline
#endif 
EOS_INTEGER
eos_GetHandleFromCustomErrorCode (const EOS_INTEGER err)
{
  /* remove the original err value from the err, returning the th (th<0 indicates no th included) */
  static const EOS_REAL fact = 1.0 / EOS_MAGX10_MAX_ERROR_CODE_VALUE;
  EOS_INTEGER th = (EOS_INTEGER)(err * fact) - 1;
  
  return(th);
}

#ifndef _MSC_VER
inline 
#endif 
EOS_INTEGER
eos_GetStandardErrorCodeFromCustomErrorCode (const EOS_INTEGER err)
{
  /* remove the th value from the err, returning the original err */
  static const EOS_INTEGER fact = EOS_MAGX10_MAX_ERROR_CODE_VALUE;
  EOS_INTEGER th = eos_GetHandleFromCustomErrorCode(err);
  EOS_INTEGER original_err = err - fact * MAX(0, th + 1);
  
  return(original_err);
}

EOS_INTEGER
eos_SetCustomErrorCode (EOS_INTEGER th, const EOS_INTEGER err)
{
  if (err >= EOS_MAGX10_MAX_ERROR_CODE_VALUE)
    return err;

  /* prepend the th value to the err, creating a custom_err */
  EOS_INTEGER fact = EOS_MAGX10_MAX_ERROR_CODE_VALUE;
  EOS_INTEGER custom_err = fact * (th + 1) + err;
  
  return(custom_err);
}

/*! *********************************************************************
 *
 * function eos_SetCustomErrorMsg
 *      Set a custom error message associated with a specified error code, and use
 *      vsprintf to allow the parsing of a format string and an arbitrary number of
 *      arguments.
 * 
 * The arguments are:
 * 	      EOS_INTEGER th      input: associated table handle.
 * 	const EOS_INTEGER err     input: error code.
 * 	const EOS_CHAR    *fmt    input: format string.
 * 	                  ...     input: arbitrary number of arguments.
 * 
 ************************************************************************/
#include <assert.h>
EOS_INTEGER
eos_SetCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err, const EOS_CHAR * fmt, ...)
{
  va_list args;
  EOS_INTEGER custom_err;
  EOS_INTEGER standard_err;

  if (! eos_IsErrorCodeValid (err))
    return(err);

  /* ensure memory access is limited to appropriate bounds */
  standard_err = eos_GetStandardErrorCodeFromCustomErrorCode(err);

  if (th < 0 || th >= gEosDataMap.nAlloc)
    return(err);

  if (! gEosDataMap.customErrorMsg[th] )
    return(err);

  if (!gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE])
    gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE] = (EOS_CHAR *) malloc (EOS_MaxErrMsgLen * sizeof (EOS_CHAR));

  va_start (args, fmt);
  vsprintf (gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE], fmt, args);
  va_end (args);

  /* prepend the th value to the err, creating a custom_err */
  custom_err = eos_SetCustomErrorCode(th, standard_err);

  return(custom_err);
}

/*! *********************************************************************
 *
 * function eos_ResetOneCustomErrorMsg
 *      For a given table handle and error code, reset custom error message string to NULL.
 * 
 * The arguments are:
 * 	      EOS_INTEGER th      input: associated table handle.
 * 	const EOS_INTEGER err     input: error code.
 *
 ************************************************************************/
void
eos_ResetOneCustomErrorMsg (EOS_INTEGER th, const EOS_INTEGER err)
{
  EOS_INTEGER standard_err;

  if (th >= 0 && th < gEosDataMap.nAlloc)
    {

    /* ensure memory access is limited to appropriate bounds */
    standard_err = eos_GetStandardErrorCodeFromCustomErrorCode(err);

    if ( gEosDataMap.customErrorMsg[th] )
      EOS_FREE(gEosDataMap.customErrorMsg[th][standard_err - EOS_MIN_ERROR_CODE_VALUE]);

  }
}

/*! *********************************************************************
 *
 * function eos_ResetCustomErrorMsg
 *      For a given table handle, reset all custom error message strings to NULL.
 * 
 * The arguments are:
 * 	      EOS_INTEGER th      input: associated table handle.
 *
 ************************************************************************/
void
eos_ResetCustomErrorMsg (EOS_INTEGER th)
{
  int i;
  int imax = EOS_MAX_ERROR_CODE_VALUE - EOS_MIN_ERROR_CODE_VALUE + 1;

  if (! gEosDataMap.customErrorMsg)
    return;

  if (! gEosDataMap.customErrorMsg[th])
    return;

  if (th >= 0 && th < gEosDataMap.nAlloc)
    {
    for (i=0; i<imax; i++)
      EOS_FREE(gEosDataMap.customErrorMsg[th][i]);
  }
}

/*! *********************************************************************
 *
 * function eos_ResetAllCustomErrorMsg
 *      Reset all custom error message strings to NULL.
 * 
 ************************************************************************/
void
eos_ResetAllCustomErrorMsg (void)
{
  EOS_INTEGER i;

  for (i = 0; i < gEosDataMap.nAlloc; i++)
    eos_ResetCustomErrorMsg(i);
}

/*! *********************************************************************
 *
 * function _eos_QuickSort
 *      Sort an array a[] into ascending numerical order using a recursive
 *      Quicksort algorithm.
 * 
 * The arguments are:
 * 	EOS_INTEGER n   input: number of elements in a[].
 * 	EOS_REAL*   a   in/out: replaced on output by its sorted rearrangement.
 * 	EOS_INTEGER err output: error code.
 * 
 * Returned Values:
 *      EOS_INTEGER _eos_QuickSort    Return maximum number levels of recursion
 *                                    reached during sorting.
 *      EOS_CHAR    **errMsg          Custom error message
 *      EOS_INTEGER *ia               Optional output array (extent n) of indexes corresponding
 *                                    to original location of each value in a[].
 *                                    Ignored if ia==NULL.
 * 
 ************************************************************************/
#define EOS_RECURSION_LIMIT 500
static EOS_INTEGER max_recursion_level = 0;
EOS_INTEGER
_eos_QuickSort (EOS_INTEGER N, EOS_REAL a[], EOS_INTEGER lvl, EOS_INTEGER * err, EOS_CHAR ** errMsg, EOS_INTEGER * ia)
{
  EOS_INTEGER i = 0, j = N - 1;
  EOS_REAL x = a[N / 2], h;

  *err = EOS_OK;

  if (lvl == 0)
    max_recursion_level = 0;

  if (lvl > max_recursion_level)
    max_recursion_level = lvl;

  //  partition
  do
  {
    while (a[i] < x)
      i++;
    while (a[j] > x)
      j--;
    if (i <= j)
	{
      h = a[i];
      a[i] = a[j];
      a[j] = h;
	  if (ia)
	    ia[i] = ia[j];
      i++;
      j--;
    }
  }
  while (i <= j);

  if (lvl > EOS_RECURSION_LIMIT)
  {
    *err = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg, "eos_Utils::_eos_QuickSort ERROR exceeded EOS_RECURSION_LIMIT=%i", EOS_RECURSION_LIMIT);
    return max_recursion_level;
  }

  //  recursion
  if (0 < j)
    _eos_QuickSort (j + 1, &(a[0]), lvl + 1, err, errMsg, ia);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return max_recursion_level;

  if (i < N - 1)
    _eos_QuickSort (N - i, &(a[i]), lvl + 1, err, errMsg, ia);

  return max_recursion_level;
}

/*! *********************************************************************
 *
 * function _eos_RemoveDuplicates
 *      This function is used to eliminate duplicate values in a[].
 *      The array, a[], is assumed to be sorted.
 *      
 * 
 * The arguments are:
 *       EOS_REAL*      a            in/out: array of real values
 *       EOS_INTEGER*   n            in/out: size of array of real values
 *       EOS_INTEGER*   ia           Optional output array (extent n) of indexes corresponding
 *                                   to original location of each value in a[].
 *                                   Ignored if ia==NULL.
 *
 ************************************************************************/
EOS_INTEGER
_eos_RemoveDuplicates (EOS_REAL * a, EOS_INTEGER * n, EOS_INTEGER * ia)
{
//#define DEBUG__eos_RemoveDuplicates
  EOS_INTEGER i, k, err = EOS_OK;

  /* remove duplicates from a[] */
  k = 1;
  for (i = 1; i < *n; i++)
    {
      if (a[i] != a[k - 1])
	{
      a[k] = a[i];
	  if (ia)
	    ia[k] = ia[i];
      k++;
    }
  }
  *n = k;

  return err;
}

/*! *********************************************************************
 *
 * function _eos_CompressArray
 *      This function is used to eliminate approximately-duplicate values in a[].
 *      The array, a[], is assumed to be sorted.
 *      
 * 
 * The arguments are:
 *       EOS_REAL*      a            in/out: array of real values
 *       EOS_INTEGER*   n            in/out: size of array of real values
 *       EOS_INTEGER    n_target     input:  desired size of array of real values
 *                                           ( ignored if n_target<0 )
 *       EOS_CHAR       **errMsg     in/out: Custom error message
 *       EOS_INTEGER    *ia          Optional output array (extent n) of indexes corresponding
 *                                   to original location of each value in a[].
 *                                   Ignored if ia==NULL.
 *
 ************************************************************************/
void
_eos_CompressArray (EOS_REAL * a, EOS_INTEGER * n, EOS_INTEGER n_target, EOS_CHAR ** errMsg, EOS_INTEGER * ia)
{
  //#define DEBUG__eos_CompressArray
  EOS_INTEGER i, j, k, n0, iter = 0, cnt = 0, err, lvl = 0;
  EOS_REAL eps = 0.0, *b = NULL, sumb = 0.0;

  /* remove duplicates from a[] */
  err = _eos_RemoveDuplicates (a, n, ia);

  /*find initial eps based upon an average relative difference */
  n0 = *n;

  /* create list of ratios */
  b = (EOS_REAL *) malloc ((n0 - 1) * sizeof (EOS_REAL));
  for (i = 0; i < n0 - 1; i++)
  {
    b[i] = 0.0;
    if (a[i] == 0.0)
      continue;		/* don't allow division by zero */
    cnt++;
    b[i] = a[i + 1] / a[i];
    sumb += b[i];
  }

  /* sort list of ratios */
  lvl = _eos_QuickSort (n0 - 1, b, lvl, &err, errMsg, ia);

  for (i = 0; i < cnt; i++)
  {
    //sumb += b[i];
  }

  //eps = ABS(sumb / (EOS_REAL)n_target - 1);
  eps = ABS(sumb / (EOS_REAL) *n - 1);

  EOS_FREE(b);

  b = (EOS_REAL *) malloc (n0 * sizeof (EOS_REAL));

  /* iteratively gather representative sample of a[] into b[] */
  while ((n0 != n_target) && (iter < *n * 2))
  {
    iter++;
    k = 1;
    for (i = 0; i < *n; i++)
      b[i] = a[i];              /* set values of temporary array, b[] */
    for (i = 0; i < *n - 1; i++)
    {
      for (j = k; j < *n; j++)
      {
        if (ABS ((b[j] - b[i]) / ((b[i] == 0.0) ? TINY_D : b[i])) > eps)
        {
          break;
        }
      }
      k = j + 1;                /* k points right AFTER the first distinct element */
      if (j < *n)               /* if distinct element found */
        b[i + 1] = b[j];
      if (k >= *n)
        break;                  /* end of table reached */
    }
    n0 = i + 1;

    b[n0++] = a[*n - 1];        /* always keep last element of a[] */

    if (n0 != n_target)
      eps *= 1.0 + (EOS_REAL) (n0 - n_target) / ((EOS_REAL) (n_target + iter));
  }

  if (n0 > n_target)
  {				/* use fallback sampling method */

    EOS_INTEGER step = (EOS_INTEGER)((EOS_REAL)n_target / (EOS_REAL)(n0 - n_target) + 0.5);
    j = 0;
    for (i = 0; i < n0; i++)
    {
      b[j] = b[i];
      if ((i + 1) % step)
        j++;
    }
    n0 = n_target;
    b[n0-1] = a[*n - 1];        /* always keep last element of a[] */
  }

  *n = n0;
  for (i = 0; i < n0; i++)
    a[i] = b[i];

  EOS_FREE (b);
}

/*! *********************************************************************
 *
 * \brief This is used to subtract, as needed, the cold curve data from the table.
 * 
 * \param[in]    th         - EOS_INTEGER : table handle
 * \param[in]    dataType   - EOS_INTEGER : data type
 * \param[in]    *nxtbl     - EOS_INTEGER : x extent of ftbls and coldCurve
 * \param[in]    *nytbl     - EOS_INTEGER : y extent of ftbls
 * \param[in]    **ftbls    - EOS_REAL    : data array
 * \param[in]    *coldCurve - EOS_REAL    : cold curve data array
 *
 * \return _eos_RemoveAndStoreColdCurve - EOS_INTEGER : error code
 *
 ************************************************************************/
EOS_INTEGER
_eos_RemoveAndStoreColdCurve (EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl, EOS_REAL ** ftbls, EOS_REAL * coldCurve)
{

  EOS_INTEGER i, j, err = EOS_OK;

  switch (EOS_TYPE_TO_INDEP_VAR2 (dataType))
    {
    /* special cases for tables that include cold curve data */
  case EOS_Pt:
  case EOS_Pic:
  case EOS_Ut:
  case EOS_Uic:
  case EOS_At:
  case EOS_Aic:
    /* remove cold curve from F */
      for (i = 0; i < *nxtbl; i++)
	{
      for (j = 1; j < *nytbl; j++)
	(*ftbls)[i + j * (*nxtbl)] -= (*ftbls)[i];
	  if (coldCurve)
	    coldCurve[i] = (*ftbls)[i];
      (*ftbls)[i] = 0.0;
    }
    break;
  default:
    // do nothing
    break;
  }

  return(err);

}

/*! *********************************************************************
 *
 * \brief This is used to normalize (i.e., divide) the table with X[].
 * 
 * \param[in]    th         - EOS_INTEGER : table handle
 * \param[in]    dataType   - EOS_INTEGER : data type
 * \param[in]    *nxtbl     - EOS_INTEGER : x extent of ftbls and coldCurve
 * \param[in]    *nytbl     - EOS_INTEGER : y extent of ftbls
 * \param[in]    **ftbls    - EOS_REAL    : data array
 * \param[in]    *coldCurve - EOS_REAL    : cold curve data array
 *
 * \return _eos_NormalizeWithX - EOS_INTEGER : error code
 *
 ************************************************************************/
EOS_INTEGER
_eos_NormalizeWithX (EOS_INTEGER th, EOS_INTEGER dataType, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl, EOS_REAL ** ftbls, EOS_REAL * X)
{

  EOS_INTEGER i, j, err = EOS_OK;

  switch (EOS_TYPE_TO_INDEP_VAR2 (dataType))
    {
      /* special cases for pressure tables that include cold curve data */
    case EOS_Pt:
    case EOS_Pic:
      /* divide by X */
      for (i = 0; i < *nxtbl; i++)
	{
	  for (j = 1; j < *nytbl; j++)
	    (*ftbls)[i + j * (*nxtbl)] /= FLOOR (X[i]);
	}
      break;
    default:
      // do nothing
      break;
    }

  return (err);

}

/*! *********************************************************************
 *
 * \brief This function is used to generate RecordTypes 1 and 2 inverted
 *      tables for dumping to the TablesLoaded.dat file.
 *      
 * \param[in]     th              - const EOS_INTEGER   :  table handle
 * \param[in]     dataType        - const EOS_INTEGER   :  data type of inverted table
 * \param[in]     _xtbls2         -       EOS_REAL*     :  X-values of inverted table, eosTableTypeRef2
 *                                               (used only by category 3 and 4 data types)
 * \param[in]     _ytbls2         -       EOS_REAL*     :  Y-values of inverted table, eosTableTypeRef2
 *                                               (used only by category 3 and 4 data types)
 * \param[in]     _ftbls2         -       EOS_REAL*     :  F-values of inverted table, eosTableTypeRef2
 *                                               (used only by category 3 and 4 data types)
 * \param[in]     _coldCurve2     -       EOS_REAL*     :  coldcurve-values of inverted table, eosTableTypeRef2
 *                                               (used only by category 3 and 4 data types)
 * \param[in]     xtbls           -       EOS_REAL**    :  X-values of inverted table
 * \param[in]     ytbls           -       EOS_REAL**    :  Y-values of inverted table
 * \param[in]     ftbls           -       EOS_REAL**    :  F-values of inverted table
 * \param[in]     coldCurve       -       EOS_REAL*     :  coldcurve-values of inverted table
 * \param[in,out] nxtbls          -       EOS_INTEGER*  :  total X-values in inverted table
 * \param[in,out] nytbls          -       EOS_INTEGER*  :  total Y-values in inverted table
 * \param[out]    xtbls_new       -       EOS_REAL**    :  X-values of inverted table
 * \param[out]    ytbls_new       -       EOS_REAL**    :  Y-values of inverted table
 * \param[out]    ftbls_new       -       EOS_REAL**    :  F-values of inverted table
 * \param[out]    ftbls_invt_mask -       EOS_BOOLEAN** :  which F-values are extrapolations
 * \param[out]    err             -       EOS_INTEGER*  :  error code
 * \param[in]     _target_N       -       EOS_INTEGER   :  reset internal target_N=_target_N if _target_N>0
 *
 ************************************************************************/
/* static EOS_BOOLEAN firstTime = EOS_TRUE; */
#define _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY if(xtbls_invt) EOS_FREE(xtbls_invt); \
                                             if(ytbls_invt) EOS_FREE(ytbls_invt); \
                                             if(ftbls_invt) EOS_FREE(ftbls_invt); \
                                             if(dFx) EOS_FREE(dFx); \
                                             if(dFy) EOS_FREE(dFy);
void
_eos_GetInvertedTable (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL * _xtbls2, EOS_REAL * _ytbls2, EOS_REAL * _ftbls2, EOS_REAL * _coldCurve2, EOS_REAL ** xtbls, EOS_REAL ** ytbls, EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl, EOS_REAL ** xtbls_new, EOS_REAL ** ytbls_new, EOS_REAL ** ftbls_new, EOS_INTEGER **ftbls_invt_mask, EOS_INTEGER *err, EOS_INTEGER _target_N)
{

  EOS_INTEGER i, j, err1 = EOS_OK;
  EOS_REAL *dFx = NULL, *dFy = NULL;
  EOS_INTEGER nXYPairs;
#define __SAVE_CONVERSION_FACTORS__
#ifdef __SAVE_CONVERSION_FACTORS__
  EOS_REAL convX, convY, convF;
#endif
  EOS_REAL *xtbls_invt = NULL, *ytbls_invt = NULL, *ftbls_invt = NULL;

  *err = EOS_OK;

  EOS_FREE (*ftbls_invt_mask);
  *ftbls_invt_mask = NULL;

#ifdef __SAVE_CONVERSION_FACTORS__
  /* Some users have implemented code that sets conversion factors prior to loading data;
     therefore, save conversion factors to be reset later in this loop. */
  eos_GetConversionFactorsFromTableHandle (th, &dataType, &convX, &convY, &convF, &err1);
  if (err1)
    {
    convX = convY = convF = 1.0;
  }
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_X_CONVERT, 1.0, -1, &err1);
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_Y_CONVERT, 1.0, -1, &err1);
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_F_CONVERT, 1.0, -1, &err1);
#endif

  /*
   * Fetch inverted data for subsequent interpolation of the inverted table data.
   */
  switch (EOS_CATEGORY (dataType))
    {
  case EOS_CATEGORY1:          /* x(F,y) */
    {
        _eos_GetInvertedTable_CATEGORY1 (th, dataType, xtbls, ytbls, ftbls, coldCurve, nxtbl, nytbl, &xtbls_invt, &ytbls_invt, &ftbls_invt, err, _target_N, &nXYPairs);
      break;
    }
  case EOS_CATEGORY2:          /* y(x,F) */
    {
        _eos_GetInvertedTable_CATEGORY2 (th, dataType, xtbls, ytbls, ftbls, coldCurve, nxtbl, nytbl, &xtbls_invt, &ytbls_invt, &ftbls_invt, err, _target_N, &nXYPairs);
      break;
    }
  case EOS_CATEGORY3:          /* F(G,y) */
    {
        _eos_GetInvertedTable_CATEGORY1 (th, EOS_EOS_TABLE_TYPE_REF2 (dataType), &_xtbls2, ((ytbls) ? &_ytbls2 : NULL), &_ftbls2, _coldCurve2, nxtbl, nytbl, &xtbls_invt, &ytbls_invt, &ftbls_invt, err, _target_N, &nXYPairs);
      break;
    }
  case EOS_CATEGORY4:          /* F(x,G) */
    {
        _eos_GetInvertedTable_CATEGORY2 (th, EOS_EOS_TABLE_TYPE_REF2 (dataType), &_xtbls2, ((ytbls) ? &_ytbls2 : NULL), &_ftbls2, _coldCurve2, nxtbl, nytbl, &xtbls_invt, &ytbls_invt, &ftbls_invt, err, _target_N, &nXYPairs);
      break;
    }
  default:
    {
      *err = EOS_UNDEFINED;
      *err = eos_SetCustomErrorMsg(th, *err, "eos_Utils::_eos_GetInvertedTable ERROR, invalid data category, %i", EOS_CATEGORY (dataType));
      _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
      return;
    }
  }

  /*
   * Calculate inverted table data.
   */
  ftbls_invt = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFx = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));
  dFy = (EOS_REAL *) malloc (nXYPairs * sizeof (EOS_REAL));

  /* Interpolate the new temporary data table. */
  if (ftbls_invt)
    {
    eos_InterpolateEosInterpolation(&gEosInterpolation, th, nXYPairs, xtbls_invt, ytbls_invt, ftbls_invt, dFx, dFy, &dataType, err);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    {
      if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) == EOS_INTERP_EXTRAPOLATED)
	  {
        /* reset error code */
        *err = EOS_OK;

	/* allocate memory to store extrapolation error codes */
	*ftbls_invt_mask = (EOS_INTEGER *) malloc (nXYPairs * sizeof (EOS_INTEGER));

        eos_CheckExtrapEosInterpolation (&gEosInterpolation, th, nXYPairs, xtbls_invt, ytbls_invt, *ftbls_invt_mask, err);
        if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
        {
          *err = eos_SetCustomErrorMsg (th, *err, "eos_Utils::_eos_GetInvertedTable ERROR, eos_CheckExtrapEosInterpolation returned error code %i", *err);
          _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
          return;
        }
      }
        else
	    {
        _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
        return;
      }
    }
  }

  if (ytbls)
    {
    /* reorganize X and Y data arrays */
      for (j = 1; j < *nytbl; j++)
      {
      for (i = 1; i < *nxtbl; i++)
        xtbls_invt[i] = xtbls_invt[i + j * (*nxtbl)];
      ytbls_invt[j] = ytbls_invt[j * (*nxtbl)];
    }
  }

  /* copy new data */
  if (xtbls_new && *xtbls_new) memcpy (*xtbls_new, xtbls_invt, *nxtbl * sizeof(EOS_REAL));
  if (ytbls_new && *ytbls_new) memcpy (*ytbls_new, ytbls_invt, *nytbl * sizeof(EOS_REAL));
  if (ftbls_new && *ftbls_new) memcpy (*ftbls_new, ftbls_invt, *nxtbl * *nytbl * sizeof(EOS_REAL));

  /* Deallocate all temporary arrays */
  _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;

#ifdef __SAVE_CONVERSION_FACTORS__
  /* restore conversion factors */
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_X_CONVERT, convX, -1, &err1);
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_Y_CONVERT, convY, -1, &err1);
  eos_SetOptionEosDataMap (&gEosDataMap, th, EOS_F_CONVERT, convF, -1, &err1);
#endif
}

/*!
 * Helper function for _eos_GetInvertedTable for x(F,y)
 */
void
_eos_GetInvertedTable_CATEGORY1 (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL ** xtbls, EOS_REAL ** ytbls, EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl, EOS_REAL ** xtbls_invt, EOS_REAL ** ytbls_invt, EOS_REAL ** ftbls_invt, EOS_INTEGER * err, EOS_INTEGER _target_N, EOS_INTEGER * nXYPairs)
{
  EOS_INTEGER i, j;
  EOS_REAL *dFx = NULL, *dFy = NULL;
  EOS_INTEGER new_Nx, target_N;
  EOS_CHAR *errMsg = NULL;

  /* determine ALL possible values for new independent variable, F,
     which corresponds to dataType */
  if (_target_N > 0)
    target_N = _target_N; /* override default value */
  else
    target_N = *nxtbl;
  //new_Nx = *nxtbl * (*nytbl);
  if (ytbls)
    {
    new_Nx = *nxtbl * (*nytbl);
  }
  else
    {
    new_Nx = *nxtbl;
    *nytbl = 1;
  }
  *xtbls_invt = (EOS_REAL *) malloc (new_Nx * (*nytbl) * sizeof (EOS_REAL));
  for (j = 0; j < new_Nx; j++)
    (*xtbls_invt)[j] = (coldCurve) ? (*ftbls)[j] + coldCurve[j - (EOS_INTEGER) (j / (*nxtbl)) * (*nxtbl)] : (*ftbls)[j];
  *nxtbl = new_Nx;

  if (ytbls && *nytbl > 1)
    {
    /* sort values for new independent variable */
    _eos_QuickSort (*nxtbl, *xtbls_invt, 0, err, &errMsg, NULL);
    if (errMsg)
        *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
	{
      _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
      return;
    }
    _eos_CompressArray (*xtbls_invt, nxtbl, target_N, NULL, NULL); /* eliminate *approximately* duplicate values */
  }

  *nXYPairs = *nxtbl * (*nytbl);

  *xtbls_invt = (EOS_REAL *) realloc (*xtbls_invt, *nXYPairs * sizeof (EOS_REAL));
  *ytbls_invt = (EOS_REAL *) malloc (*nXYPairs * sizeof (EOS_REAL));
  for (j = 0; j < *nytbl; j++)
    {
      for (i = 0; i < *nxtbl; i++)
	{
          (*xtbls_invt)[i + j * (*nxtbl)] = (*xtbls_invt)[i];
          (*ytbls_invt)[i + j * (*nxtbl)] = (ytbls) ? (*ytbls)[j] : 0.0;
        }
  }
}

/*!
 * Helper function for _eos_GetInvertedTable for y(x,F)
 */
void
_eos_GetInvertedTable_CATEGORY2 (EOS_INTEGER th, EOS_INTEGER dataType, EOS_REAL ** xtbls, EOS_REAL ** ytbls, EOS_REAL ** ftbls, EOS_REAL * coldCurve, EOS_INTEGER * nxtbl, EOS_INTEGER * nytbl, EOS_REAL ** xtbls_invt, EOS_REAL ** ytbls_invt, EOS_REAL ** ftbls_invt, EOS_INTEGER * err, EOS_INTEGER _target_N, EOS_INTEGER * nXYPairs)
{
  EOS_INTEGER i, j, err1 = EOS_OK;
  EOS_REAL *dFx = NULL, *dFy = NULL;
  EOS_INTEGER target_N;
  EOS_REAL *ytbls_invt_tmp = NULL;
  EOS_CHAR *errMsg = NULL;
  EOS_INTEGER *ia = NULL;
#ifdef __INCLUDE_COLD_CURVE_IN_INVERTED_GRID__
  EOS_INTEGER ny;
#endif

#ifdef __REMOVE_COLD_CURVE_FROM_SELECTED_INVERSIONS__
  if (EOS_TYPE_TO_RECORD_TYPE(dataType) == 1 && eos_getBoolOptionFromTableHandle (th, EOS_INVERT_AT_SETUP, &err1))
  {

    EOS_INTEGER subTableNum = EOS_TYPE_TO_SUB_TAB_NUM(dataType);
    void *ptr = (void*) eos_GetEosDataEosDataMap (&gEosDataMap, th, &dataType, &err1);
    EOS_REAL *CC = NULL;
    EOS_REAL **coldCurve_ptr = &CC;

    coldCurve_ptr = _eos_AllocateColdCurveEosDataMap (ptr, *nxtbl, subTableNum);

    err1 = _eos_RemoveAndStoreColdCurve(th, dataType, nxtbl, nytbl, ftbls, *coldCurve_ptr);

#ifdef __NORMALIZE_WITH_X_FOR_SELECTED_INVERSIONS__
    {
      EOS_BOOLEAN zero_exists = ((*xtbls)[0] == 0.0) ? EOS_TRUE : EOS_FALSE;
      if (zero_exists)
        (*xtbls)[0] = (*xtbls)[1] * 0.75;
      err1 = _eos_NormalizeWithX (th, dataType, nxtbl, nytbl, ftbls, *xtbls);
      //if (zero_exists) (*xtbls)[0] = (*xtbls)[1] * 0.99;
    }
#endif

  }
#endif

  /* determine ALL possible values for new independent variable, F,
     which corresponds to dataType */
  if (_target_N > 0)
    target_N = _target_N; /* override default value */
  else
    target_N = 100; //MAX (*nytbl, 100);
  *nytbl *= *nxtbl;
  *ytbls_invt = (EOS_REAL *) malloc (*nytbl * sizeof (EOS_REAL));

  for (j = 0; j < *nytbl; j++)
  {
#ifdef __REMOVE_COLD_CURVE_FROM_SELECTED_INVERSIONS__
    (*ytbls_invt)[j] = (*ftbls)[j];
#else
    (*ytbls_invt)[j] = (coldCurve) ? (*ftbls)[j] + coldCurve[j % (*nxtbl)] : (*ftbls)[j];
#endif
  }

  if (*nytbl > 1)
  {
    /* sort values for new independent variable */
    _eos_QuickSort (*nytbl, *ytbls_invt, 0, err, &errMsg, ia);
    if (errMsg)
      *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
    EOS_FREE(errMsg);
    if (eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
	{
      _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
      return;
    }
    _eos_CompressArray (*ytbls_invt, nytbl, target_N, NULL, ia);   /* eliminate *approximately* duplicate values */
#ifdef __INCLUDE_COLD_CURVE_IN_INVERTED_GRID__
    switch (EOS_TYPE_TO_INDEP_VAR2 (dataType))
    {
      /* special cases for pressure tables, which include cold curve data */
    case EOS_Pt:
    case EOS_Pic:
      ny = *nytbl;
      *nytbl += *nxtbl;
      *ytbls_invt = (EOS_REAL *) realloc (*ytbls_invt, *nytbl * sizeof (EOS_REAL));

	  for (j = 0; j < *nxtbl; j++)
      {
        (*ytbls_invt)[j+ny] = (coldCurve) ? coldCurve[j] : (*ftbls)[j];
      }

      /* re-sort values for new independent variable */
      _eos_QuickSort (*nytbl, *ytbls_invt, 0, err, &errMsg, ia);
      if (errMsg)
	    *err = eos_SetCustomErrorMsg(th, *err, "%s", errMsg);
      EOS_FREE(errMsg);
      if (eos_GetStandardErrorCodeFromCustomErrorCode (*err) != EOS_OK)
      {
        _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
        return;
      }

      /* remove duplicate values */
      err1 = _eos_RemoveDuplicates (*ytbls_invt, nytbl, ia);
      if (eos_GetStandardErrorCodeFromCustomErrorCode (err1) != EOS_OK)
      {
        *err = eos_SetCustomErrorMsg(th, err1, "%s", errMsg);
        _EOS_GETINVERTEDTABLE_CLEANUP_MEMORY;
        return;
      }
    }
#endif

    EOS_FREE (ia);
  }

  *nXYPairs = *nxtbl * (*nytbl);

  *xtbls_invt = (EOS_REAL *) malloc (*nXYPairs * sizeof (EOS_REAL));
  ytbls_invt_tmp = (EOS_REAL *) malloc (*nXYPairs * sizeof (EOS_REAL));
  for (i = 0; i < *nxtbl; i++)
  {
    for (j = 0; j < *nytbl; j++)
	{
      (*xtbls_invt)[i + j * (*nxtbl)] = (*xtbls)[i];
      ytbls_invt_tmp[i + j * (*nxtbl)] = (*ytbls_invt)[j];
    }
  }
  EOS_FREE (*ytbls_invt);
  *ytbls_invt = ytbls_invt_tmp;
}

/*! *********************************************************************
 *
 * function _eos_DEBUG_PRINT
 *      Write debugging information to stdout using vprintf to allow the
 *      parsing of a format string and an arbitrary number of arguments.
 * 
 * The arguments are:
 * 	const EOS_CHAR    *fmt    input: format string.
 * 	                  ...     input: arbitrary number of arguments.
 * 
 ************************************************************************/
EOS_BOOLEAN enable_DEBUG_PRINT = EOS_FALSE;
void
_eos_DEBUG_PRINT (const EOS_CHAR * fmt, ...)
{
  va_list args;

  if (!enable_DEBUG_PRINT)
    return;

  va_start (args, fmt);
  vprintf (fmt, args);
  va_end (args);
}

/*! *********************************************************************
 *
 * function _eos_DestroyGhostData
 *    Deallocate the memory, which was allocatred in _eos_CreateGhostData
 *
 *    \warning If the memory is allocated differently in _eos_CreateGhostData,
 *             then this function must be changed.
 *
 * The input arguments are:
 *      EOS_INTEGER *nGhostData   number of ghost nodes to add to original table
 * 	EOS_REAL **xtbls          array of tabulated X values
 * 	EOS_REAL **ytbls          array of tabulated Y values (optional: NULL for 1-D)
 * 	EOS_REAL ***ftbls         array of tabulated F(X,Y) values
 * 	EOS_REAL **coldCurve      array of tabulated F(X,Y=0) values (optional)
 *
 * The returned values is:
 *      EOS_INTEGER              error code
 * 
 ************************************************************************/
EOS_INTEGER
_eos_DestroyGhostData (EOS_INTEGER * nGhostData, EOS_REAL ** xtbls, EOS_REAL ** ytbls, EOS_REAL *** ftbls, EOS_REAL ** coldCurve)
{
  if (*nGhostData > 0)
  {
    if (xtbls)
      EOS_FREE (*xtbls);
    /* *ytbls is currently allocated sequentially with xtbls, so no free is needed */
    if (coldCurve)
      EOS_FREE (*coldCurve);
    if (ftbls)
      EOS_FREE (*ftbls);
  }
  return(EOS_OK);
}

/*! *********************************************************************
 *
 * function _eos_CreateGhostData
 *    Add "ghost node" data point at each end of each isotherm and each isochore.
 *    This is used to alleviate birational interpolation oscillations at the table boundaries.
 *    Additionally, this function will operate on 1-D data arrays if nytbl_in=0 and ytbls_in=NULL
 *
 *    \warning If the memory is allocated differently in this function,
 *             then the _eos_DestroyGhostData function must be changed.
 *
 * The input arguments are:
 *      EOS_INTEGER nGhostData   number of ghost nodes to add to original table
 *      EOS_INTEGER nxtbl_in     number of tabulated X values in xtbls_in
 *      EOS_INTEGER nytbl_in     number of tabulated Y values in ytbls_in (optional: 0 for 1-D)
 * 	EOS_REAL *xtbls_in       array of tabulated X values
 * 	EOS_REAL *ytbls_in       array of tabulated Y values (optional: NULL for 1-D)
 * 	EOS_REAL **ftbls_in      array of tabulated F(X,Y) values
 * 	EOS_REAL *coldCurve_in   array of tabulated F(X,Y=0) values (optional)
 *
 * The output arguments are:
 *      EOS_INTEGER *nxtbl       number of tabulated X values in xtbls
 *      EOS_INTEGER *nytbl       number of tabulated Y values in ytbls
 * 	EOS_REAL **xtbls         array of tabulated X values with ghost data
 * 	EOS_REAL **ytbls         array of tabulated Y values with ghost data
 * 	EOS_REAL ***ftbls        array of tabulated F(X,Y) values with ghost data
 * 	EOS_REAL **coldCurve     array of tabulated F(X,Y=0) values (only created
 *                               if coldCurve_in != NULL)
 *      EOS_INTEGER *err         error code
 *      EOS_CHAR    **errMsg     custom error message
 * 
 ************************************************************************/
void
_eos_CreateGhostData (EOS_INTEGER nGhostData, EOS_INTEGER nxtbl_in, EOS_INTEGER nytbl_in, EOS_REAL * xtbls_in, EOS_REAL * ytbls_in, EOS_REAL ** ftbls_in, EOS_REAL * coldCurve_in, EOS_INTEGER * nxtbl_out, EOS_INTEGER * nytbl_out, EOS_REAL ** xtbls_out, EOS_REAL ** ytbls_out, EOS_REAL *** ftbls_out, EOS_REAL ** coldCurve_out, EOS_INTEGER * err, EOS_CHAR ** errMsg)
{
  EOS_INTEGER i, j, k;
  EOS_REAL *ptr=NULL;
  EOS_INTEGER nxtbl, nytbl;
  EOS_REAL *xtbls=NULL, *ytbls=NULL, **ftbls=NULL, *coldCurve=NULL;

  /* initialize output variables */
  *err = EOS_OK;
  *nxtbl_out = 0;
  *nytbl_out = 0;
  *ftbls_out = ftbls;
  *xtbls_out = xtbls;
  *ytbls_out = ytbls;
  if (coldCurve_in && coldCurve_out)
    *coldCurve_out = NULL;

  /* determine if sufficient data exist for expansion */
  if (nxtbl_in < 4)
    {
    *err = EOS_MEM_ALLOCATION_FAILED;
      eos_SetCustomMsg_str (errMsg, "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded because nxtbl<4 in eos_Utils::_eos_CreateGhostData");
    return;
  }
  nxtbl = nxtbl_in + nGhostData*(_EOS_CREATEGHOSTDATA_X_LO + _EOS_CREATEGHOSTDATA_X_HI);

  if (nytbl_in < 1 && ytbls_in)
    {
    *err = EOS_MEM_ALLOCATION_FAILED;
      eos_SetCustomMsg_str (errMsg, "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded because nytbl<1 in eos_Utils::_eos_CreateGhostData");
    return;
  }

  if (nytbl_in >= 4 && ytbls_in) /* expand in y-dimension if sufficient data exist */
    nytbl = nytbl_in + nGhostData*(_EOS_CREATEGHOSTDATA_Y_LO + _EOS_CREATEGHOSTDATA_Y_HI);
  else if (ytbls_in)
    nytbl = nytbl_in;
  else
    nytbl = 1;

  ptr = (EOS_REAL *) malloc ((nxtbl + nytbl + nxtbl * nytbl) * sizeof (EOS_REAL));
  if (!ptr)
    {
    *err = EOS_MEM_ALLOCATION_FAILED;
      eos_SetCustomMsg_str (errMsg, "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded because malloc(ptr) failed in eos_Utils::_eos_CreateGhostData");
    *err = _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);
    return;
  }
  xtbls = ptr;
  ytbls = ptr + nxtbl;

  if (coldCurve_in)
    {
    coldCurve = (EOS_REAL *) malloc (nxtbl * sizeof (EOS_REAL));
      if (!coldCurve)
	{
      *err = EOS_MEM_ALLOCATION_FAILED;
	  eos_SetCustomMsg_str (errMsg, "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded because malloc(coldCurve) failed in eos_Utils::_eos_CreateGhostData");
      *err = _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);
      return;
    }
    if (_EOS_CREATEGHOSTDATA_X_LO)
      coldCurve[0] = coldCurve_in[0] - coldCurve_in[1]; /* assume increasing order */
    else
      coldCurve[0] = coldCurve_in[0];
    if (_EOS_CREATEGHOSTDATA_X_HI)
      coldCurve[nxtbl-1] = coldCurve_in[nxtbl_in-1] + (coldCurve_in[nxtbl_in-1] - coldCurve_in[nxtbl_in-2]);
    else
      coldCurve[nxtbl-1] = coldCurve_in[nxtbl_in-1];
    for (i=1;i<nxtbl-1;i++)
      coldCurve[i] = coldCurve_in[i-1];
  }

  if (_EOS_CREATEGHOSTDATA_X_LO)
    xtbls[0] = 2*xtbls_in[0] - xtbls_in[1]; /* assume increasing order */
  else
    xtbls[0] = xtbls_in[0];
  if (_EOS_CREATEGHOSTDATA_X_HI)
    xtbls[nxtbl-1] = xtbls_in[nxtbl_in-1] + (xtbls_in[nxtbl_in-1] - xtbls_in[nxtbl_in-2]);
  else
    xtbls[nxtbl-1] = xtbls_in[nxtbl_in-1];
  if (nytbl > nytbl_in && ytbls_in)
    {
    if (_EOS_CREATEGHOSTDATA_Y_LO)
      ytbls[0] = 2*ytbls_in[0] - ytbls_in[1]; /* assume increasing order */
    else
      ytbls[0] = ytbls_in[0];
    if (_EOS_CREATEGHOSTDATA_Y_HI)
      ytbls[nytbl-1] = ytbls_in[nytbl_in-1] + (ytbls_in[nytbl_in-1] - ytbls_in[nytbl_in-2]);
    else
      ytbls[nytbl-1] = ytbls_in[nytbl_in-1];
  }
  for (i=1;i<nxtbl-1;i++)
    xtbls[i] = xtbls_in[i-1];
  if (nytbl > nytbl_in && ytbls_in)
    {
    for (j=1;j<nytbl-1;j++)
      ytbls[j] = ytbls_in[j-1];
  }
  else if (ytbls_in)
    {
    for (j=0;j<nytbl;j++)
      ytbls[j] = ytbls_in[j];
  }

  ftbls = (EOS_REAL **) malloc (nytbl * sizeof (EOS_REAL*));
  if (!ftbls)
    {
    *err = EOS_MEM_ALLOCATION_FAILED;
      eos_SetCustomMsg_str (errMsg, "EOS_MEM_ALLOCATION_FAILED: EOS table area cannot be expanded because malloc(ftbls) failed in eos_Utils::_eos_CreateGhostData");
    *err = _eos_DestroyGhostData (&nGhostData, &xtbls, &ytbls, &ftbls, &coldCurve);
    return;
  }

  for (j=0;j<nytbl;j++)
    ftbls[j] = ptr + nxtbl + nytbl + j*nxtbl;

  if (nytbl_in > 1 && ytbls_in)
    {
    for (i=_EOS_CREATEGHOSTDATA_X_LO;i<nxtbl-_EOS_CREATEGHOSTDATA_X_HI;i++)
      for (j=_EOS_CREATEGHOSTDATA_Y_LO;j<nytbl-_EOS_CREATEGHOSTDATA_Y_HI;j++)
	ftbls[j][i] = ftbls_in[j-1][i-1];
  }
  else
    {
    for (i=_EOS_CREATEGHOSTDATA_X_LO;i<nxtbl-_EOS_CREATEGHOSTDATA_X_HI;i++)
      ftbls[0][i] = ftbls_in[0][i-1];
  }

  i=0;
  if (1)
    {
    /* use 1-D linear extrapolation to calculate ghost data */
      if (nytbl != nytbl_in && ytbls_in)
	{			/* expansion in x- and y-dimensions */
	  for (j = 1; j < nytbl - 1; j++)
	    {
	if (_EOS_CREATEGHOSTDATA_X_LO)
		ftbls[j][0] = ftbls[j][1] + (xtbls[0] - xtbls[1]) / (xtbls[2] - xtbls[1]) * (ftbls[j][2] - ftbls[j][1]);
	if (_EOS_CREATEGHOSTDATA_X_HI)
		ftbls[j][nxtbl - 1] = ftbls[j][nxtbl - 3] + (xtbls[nxtbl - 1] - xtbls[nxtbl - 3]) / (xtbls[nxtbl - 2] - xtbls[nxtbl - 3]) * (ftbls[j][nxtbl - 2] - ftbls[j][nxtbl - 3]);
      }
	  for (i = 1; i < nxtbl - 1; i++)
	    {
	if (_EOS_CREATEGHOSTDATA_Y_LO)
		ftbls[0][i] = ftbls[1][i] + (ytbls[0] - ytbls[1]) / (ytbls[2] - ytbls[1]) * (ftbls[2][i] - ftbls[1][i]);
	if (_EOS_CREATEGHOSTDATA_Y_HI)
		ftbls[nytbl - 1][i] = ftbls[nytbl - 3][i] + (ytbls[nytbl - 1] - ytbls[nytbl - 3]) / (ytbls[nytbl - 2] - ytbls[nytbl - 3]) * (ftbls[nytbl - 2][i] - ftbls[nytbl - 3][i]);
      }
    }
      else
	{			/* expansion in x-dimension; no expansion in y-dimension */
      j = 0;
      if (_EOS_CREATEGHOSTDATA_X_LO)
	    ftbls[j][0] = ftbls[j][1] + (xtbls[0] - xtbls[1]) / (xtbls[2] - xtbls[1]) * (ftbls[j][2] - ftbls[j][1]);
      if (_EOS_CREATEGHOSTDATA_X_HI)
	    ftbls[j][nxtbl - 1] = ftbls[j][nxtbl - 3] + (xtbls[nxtbl - 1] - xtbls[nxtbl - 3]) / (xtbls[nxtbl - 2] - xtbls[nxtbl - 3]) * (ftbls[j][nxtbl - 2] - ftbls[j][nxtbl - 3]);
    }
  }
  else
    {
    /* use 1-D rational extrapolation to calculate ghost data */
    /*
     * void eos_RationalInterpolate4 (EOS_REAL x, EOS_REAL *X, EOS_REAL *F,
     *                                EOS_REAL *fvalv, EOS_REAL *dfvalv)
     */
    EOS_REAL X[4], F[4];

      if (nytbl != nytbl_in && ytbls_in)
	{			/* expansion in y-dimension */
	  for (j = 1; j < nytbl - 1; j++)
	    {
	      for (k = 0; k < 4; k++)
		{
	  X[k] = xtbls[k+1];
	  F[k] = ftbls[j][k+1];
	}
	eos_RationalInterpolate4 (xtbls[0], X, F, &(ftbls[j][0]), NULL);
	      for (i = 0; i < 4; i++)
		{
	  X[k] = xtbls[nxtbl-1-(4-k)];
	  F[k] = ftbls[j][nxtbl-1-(4-k)];
	}
	eos_RationalInterpolate4 (xtbls[nxtbl-1], X, F, &(ftbls[j][nxtbl-1]), NULL);
      }
	  for (i = 1; i < nxtbl - 1; i++)
	    {
	      for (k = 0; k < 4; k++)
		{
	  X[k] = ytbls[k+1];
	  F[k] = ftbls[k+1][i];
	}
	eos_RationalInterpolate4 (ytbls[0], X, F, &(ftbls[0][i]), NULL);
	      for (k = 0; k < 4; k++)
		{
	  X[k] = ytbls[nytbl-1-(4-k)];
	  F[k] = ftbls[nytbl-1-(4-k)][i];
	}
	eos_RationalInterpolate4 (ytbls[nytbl-1], X, F, &(ftbls[nytbl-1][i]), NULL);
      }
    }
      else
	{			/* no expansion in y-dimension */
      j = 0;
	  for (k = 0; k < 4; k++)
	    {
	X[k] = xtbls[k+1];
	F[k] = ftbls[j][k+1];
      }
      eos_RationalInterpolate4 (xtbls[0], X, F, &(ftbls[j][0]), NULL);
	  for (i = 0; i < 4; i++)
	    {
	X[k] = xtbls[nxtbl-1-(4-k)];
	F[k] = ftbls[j][nxtbl-1-(4-k)];
      }
      eos_RationalInterpolate4 (xtbls[nxtbl-1], X, F, &(ftbls[j][nxtbl-1]), NULL);
    }
  }

  if (nytbl > 3 && ytbls_in)
    {
    /* ignore corners for now since they are not used by the birational interpolator */
      if (_EOS_CREATEGHOSTDATA_X_LO)
	ftbls[0][0] = 0.0;
      if (_EOS_CREATEGHOSTDATA_X_HI)
	ftbls[0][nxtbl - 1] = 0.0;
      if (_EOS_CREATEGHOSTDATA_Y_LO)
	ftbls[nytbl - 1][0] = 0.0;
      if (_EOS_CREATEGHOSTDATA_Y_HI)
	ftbls[nytbl - 1][nxtbl - 1] = 0.0;
  }
  /* assign new arrays and values to output variables */
  *nxtbl_out = nxtbl;
  *nytbl_out = nytbl;
  *ftbls_out = ftbls;
  *xtbls_out = xtbls;
  *ytbls_out = ytbls;
  if (coldCurve_in && coldCurve_out)
    *coldCurve_out = coldCurve;
}

/***********************************************************************/
/*!
 * \brief This is used to get the required number of subtables for the data type.
 * 
 * \param[in]    *me        - eos_RecordType1 : data object pointer
 * \param[in]    dataType   - EOS_INTEGER : data type
 *
 * \return eos_getRequiredNumSubtables - EOS_INTEGER : required number of subtables for the data type
 *
 ***********************************************************************/
EOS_INTEGER
eos_getRequiredNumSubtables (eos_Data * me, EOS_INTEGER dataType)
{

  EOS_INTEGER n1 = EOS_TYPE_TO_SUB_TAB_NUM(dataType);
  EOS_INTEGER t2 = EOS_EOS_TABLE_TYPE_REF1(dataType);
  EOS_INTEGER t3 = EOS_EOS_TABLE_TYPE_REF2(dataType);
  EOS_INTEGER n2 = EOS_TYPE_TO_SUB_TAB_NUM(t2);
  EOS_INTEGER n3 = EOS_TYPE_TO_SUB_TAB_NUM(t3);
  EOS_INTEGER N  = me->numSubtablesLoaded;

  N = MAX(n1, N);
  N = MAX(n2, N);
  N = MAX(n3, N);

  return N;
}

/*! *********************************************************************
 *
 * This function returns the variable type's index in eos_VarList[].
 *
 ************************************************************************/
EOS_INTEGER
get_VarListIndex (EOS_INTEGER t)
{
  EOS_INTEGER i;
  for (i = 0; i < MAX_VARS; i++)
    {
    if (eos_VarList[i].eosVarType == t)
      return i;
  }
  return 0; // unknown
}

/*! This function returns the specified variable's string representation given:
 *  flag == 1 :: eosVarType_short_s
 *  flag == 2 :: eosVarType_s
 *  flag == 3 :: eosVarDescription
 */
EOS_CHAR *
get_VarStr (EOS_INTEGER t, EOS_INTEGER flag)
{
  EOS_CHAR *sp = NULL;
  EOS_INTEGER i = get_VarListIndex(t);
  if (i < 0)
    return sp;
  if (flag == 1)
    sp = eos_VarList[i].eosVarType_short_s;
  if (flag == 2)
    sp = eos_VarList[i].eosVarType_s;
  if (flag == 3)
    sp = eos_VarList[i].eosVarDescription;
  return sp;
}

/*! *********************************************************************
 *
 * This function returns the option flag's index in eos_OptionFlags[].
 *
 ************************************************************************/
EOS_INTEGER
get_OptionFlagIndex (EOS_INTEGER f)
{
  EOS_INTEGER i;
  for (i = 0; i < EOS_TOTAL_TABLE_OPTIONS; i++)
    {
    if (eos_OptionFlags[i] == f)
      return i;
  }
  return -1; // unknown
}

/*! This function returns the option flag's string representation
 */
EOS_CHAR *
get_OptionFlagStr (EOS_INTEGER f)
{
  EOS_CHAR *sp = NULL;
  EOS_INTEGER i = get_OptionFlagIndex(f);
  if (i < 0 || i >= EOS_TOTAL_TABLE_OPTIONS)
    return sp;
  sp = eos_OptionFlags_str[i];
  return sp;
}

/*!**********************************************************************
 *
 * function _eos_locate
 * find index (jmiddle) such that x[jmiddle*ix] is closest to y,
 * in the interval X[lower], X[upper], where X[lower] <= y < X[upper]
 * return u an l the smallest integer interval of X that maps to y, and jmiddle , s.t. x[jmiddle*ix] is closest to y
 * 
 * Returned Values: 
 * EOS_INTEGER (return value) jmiddle , s.t. x[jmiddle*ix] is closest to y
 * EOS_INTEGER * u an l the smallest integer interval of X that maps to y
 *
 * Input Value:
 * EOS_REAL x[]       - input vector of values to search in.
 * EOS_INTEGER jlower - start search index into array x[]
 * EOS_INTEGER jupper - end search index into array x[]
 * EOS_INTEGER ix     - step to traverse array x[]
 * EOS_REAL    y      - value to search for
 *
 ************************************************************************/
int
_eos_locate (EOS_REAL x[], EOS_INTEGER jlower, EOS_INTEGER jupper, EOS_INTEGER ix, EOS_REAL y, EOS_INTEGER * l, EOS_INTEGER * u)
{
  int jmiddle;

  jmiddle = (jlower + jupper) / 2;
  while ((y != x[jmiddle * ix]) && ((jupper - jlower) > 1))
    {
      if (y > x[jmiddle * ix])
	{
      jlower = jmiddle;
    }
      else
	{
      jupper = jmiddle;
    }
    jmiddle = (jlower + jupper) / 2;
  }
  return (jmiddle);
}

/*!**********************************************************************
 *
 * function _eos_hunt
 * find (lower, upper ) interval where x[lower*ix] <= y < x[upper *ix]
 * 
 * Returned Values: 
 * EOS_INTEGER (return value) jmiddle , s.t. x[jmiddle*ix] is closest to y
 * EOS_INTEGER *err - error code
 *
 * Input Values:
 * EOS_REAL x[]       - input vector of values to search in.
 * EOS_INTEGER n      - number of real elements in array x[]
 * EOS_INTEGER jlower - start search index into array x[]
 * EOS_INTEGER ix     - memory offset between elements of array x[]
 * EOS_REAL    y      - value to search for
 *
 * NOTE: This is a poorly designed function in that the actual dimension
 *       of x[] is not defined when ix>0. This puts unecessary requirements
 *       upon the calling function. CONSIDER REFACTORING! -DAP
 *
 ************************************************************************/
int
_eos_hunt (EOS_REAL x[], EOS_INTEGER n, EOS_INTEGER ix /* step */ ,
               EOS_REAL y, EOS_INTEGER jlower /* start index */ ,
               EOS_INTEGER *err)
     /* find (lower, upper ) interval where x[lower*ix] <= y < x[upper *ix] */
{
  int jupper, inc, res;
  EOS_INTEGER u, l, i;
  EOS_REAL EOS_ROUNDOFF_TOLERANCE=0;

  *err = EOS_OK;

  //#define USE_ROUNDOFF_TOLERANCE
#ifdef USE_ROUNDOFF_TOLERANCE
  /* Use this logic if machine round-off is causing problems with extrapolation
     detection. */
  if (!_eos_machinePrecisionData.gotMachinePrecision)
  {
    /* determine the current machine's floating point precision */
    _eos_machinePrecisionData.gotMachinePrecision = 1;
    eos_GetMachinePrecision (&_eos_machinePrecisionData.eps, &_eos_machinePrecisionData.epsneg);
    _eos_machinePrecisionData.maxIter = 100;
    _eos_machinePrecisionData.maxErr = pow (MAX (_eos_machinePrecisionData.eps, _eos_machinePrecisionData.epsneg), 0.75);
  }
  EOS_ROUNDOFF_TOLERANCE = 100 * MAX (_eos_machinePrecisionData.eps, _eos_machinePrecisionData.epsneg);
#endif

  /* check if all the numbers in X are the same */
  if (x[0] == x[n * ix])
  {
    for (i = 1; i <= n; i++)
      if (x[i * ix] != x[0])
        break;
    if (i > n)
	{			/* all values are the same */
      *err = EOS_UNDEFINED;
      return 0;
    }
  }

  if ((jlower < 0) || jlower >= n)
  {
    jlower = 0;
    jupper = n;
  }
  else
  {				/* if lower, upper are within range */
    inc = 1;
    if (y >= x[jlower * ix])
	{
	  if (jlower == (n - 1))
      {
        if (y > x[n] + EOS_ROUNDOFF_TOLERANCE)
          *err = EOS_xHi_yOk;
        return (n - 1);         /* edge interval */
      }
      jupper = jlower + 1;
      /* make interval of length 1, expand & move (jupper, jlower interval until Y is inside it */
	  while (y >= x[jupper * ix])
      {
        jlower = jupper;
        inc += inc;             /* double increment */
        jupper += inc;
        if (jupper >= (n - 1))
		{
          jupper = n;
          break;
        }
      }                         /* while loop */
    }
    else
	{			/* y >= x[jlower*ix], hunt down */
	  if (jlower == 0)
      {
        if (y < x[0] - EOS_ROUNDOFF_TOLERANCE)
          *err = EOS_xLo_yOk;
        return (0);             /* hit the edge */
      }
      jupper = jlower;
      jlower -= 1;
      /* make interval of length 1, expand & move (jupper, jlower interval until Y is inside it */
	  while (y < x[jlower * ix])
      {
        jupper = jlower;
        inc += inc;
        jlower -= inc;
        if (jlower < 0)
		{
          jlower = 0;
          break;
        }
      }                         /* while loop */
    }                           /* hunt down */
  }                             /* if lower, upper are within range */

  /* now do bisection */

  res = _eos_locate (x, jlower, jupper, ix, y, &l, &u);

  if (y < x[res * ix])
    *err = EOS_xLo_yOk;
  else if (y > x[(res + 1) * ix] && y - x[(res + 1) * ix] > TINY_D)
    *err = EOS_xHi_yOk;
  return res;
}

