/*! ******************************************************************
 * Class Name : eos_SesUtils
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ******************************************************************* */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#define _EOS_SESUTILS_INTERNAL_PROTOTYPES
#include "eos_types_internal.h"
#include "eos_SesUtils.h"

#ifndef _POSIX_
#define _POSIX_
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#ifndef PATH_MAX
#define PATH_MAX 255
#endif

#include "ses_globals.h"
#include "ses_externs.h"

#include <ctype.h>
#include "eos_Interpolation.h"


#undef MY_DEBUG

/*! *********************************************************************
 *
 * Function Name: eos_SesGetFileInfoFromCache
 * This routine returns the info for the file with index ifile from cache
 * if its not cached, it caches it.
 *
 * Returned Values:
 * EOS_INTEGER                   		 - error code
 * ses_file_handle* sesFile      		 - ses_file_handle pointer for open file
 * EOS_INTEGER *nmats                            - number of materials in file
 * ses_material_id_reference* indexdata          - pointer to return array of material names
 *
 * Input Value:
 * EOS_INTEGER  jfile            - file index
 * Uses the following static data:
 *                  SesameFileCache;
 *                     
 ************************************************************************/
#define EOS_RECURSION_LIMIT 500
static EOS_INTEGER max_recursion_level = 0;
EOS_INTEGER _eos_SesQuickSort (EOS_INTEGER N, ses_material_id a[], EOS_INTEGER lvl,
			       EOS_INTEGER *err, EOS_CHAR **errMsg)
{
  EOS_INTEGER i = 0, j = N - 1;
  ses_material_id x = a[N / 2], h;

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

  if (lvl > EOS_RECURSION_LIMIT) {
    *err = EOS_FAILED;
    eos_SetCustomMsg_str (errMsg,
			  "eos_Utils::_eos_QuickSort ERROR exceeded EOS_RECURSION_LIMIT=%i",
			  EOS_RECURSION_LIMIT);
    return max_recursion_level;
  }

  //  recursion
  if (0 < j)
    _eos_SesQuickSort (j + 1, &(a[0]), lvl + 1, err, errMsg);
  if (eos_GetStandardErrorCodeFromCustomErrorCode(*err) != EOS_OK)
    return max_recursion_level;

  if (i < N - 1)
    _eos_SesQuickSort (N - i, &(a[i]), lvl + 1, err, errMsg);

  return max_recursion_level;
}


EOS_INTEGER eos_SesGetFileInfoFromCache (EOS_INTEGER jfile, EOS_INTEGER* nmats,
					 ses_material_id_reference* indexdata, ses_file_handle* sesFile)
{
  EOS_INTEGER i, istart;

  if (!sesameFileCache) {
    /* if global data does not exist, instantiate it */
    istart = 0;
    sesameFileCacheL = sesameFilesL;
    sesameFileCache =
      (SesameFileCache *) malloc (sesameFileCacheL * sizeof (SesameFileCache));
  }
  else {
    /*  if global data exists, and current length is longer than current cache size, reallocate memory for it */
    istart = sesameFileCacheL;
    if (sesameFileCacheL < sesameFilesL) {
      sesameFileCacheL = sesameFilesL;
      sesameFileCache =
	(SesameFileCache *) realloc (sesameFileCache, sesameFileCacheL * sizeof (SesameFileCache));
    }
  }

  /* initialze values in newly added file cache entries */

  for (i = istart; i < sesameFileCacheL; i++) {
      sesameFileCache[i].nmats = 0;
      sesameFileCache[i].sesFile = 0;
      sesameFileCache[i].sesMats = (ses_material_id_reference)NULL;
      sesameFileCache[i].materialListLoaded = SES_FALSE; 
  }

  /*  error return if jfile (cache index that we want) is passed in incorrectly */

  if (sesameFilesL <= jfile || jfile < 0)
    return EOS_OPEN_SESAME_FILE_FAILED;

  /*  if cache for the index that we want has not been loaded, load it */

  if (sesameFileCache[jfile].materialListLoaded == SES_FALSE) {    /* open file, load the index table into cache */

    /*  open and get the ses file handle */

    if (ses_is_valid(sesameFileCache[jfile].sesFile) == SES_FALSE)
      sesameFileCache[jfile].sesFile = ses_open (sesameFiles[jfile], 'R'); /* open file */
    *sesFile = sesameFileCache[jfile].sesFile;

    if (ses_is_valid(sesameFileCache[jfile].sesFile) == SES_FALSE) {
#ifdef DEBUG
      printf ("can't open %s for reading!\n", sesameFiles[jfile]);
#endif
      return EOS_OPEN_SESAME_FILE_FAILED;
    }

    /* load number of materials on data library file. */

    long number_materials[1];
    sesameFileCache[jfile].sesMats = ses_get_materials(sesameFileCache[jfile].sesFile, &number_materials[0]);
    if (sesameFileCache[jfile].sesMats) {
      sesameFileCache[jfile].nmats = number_materials[0];
      *nmats = (int)number_materials[0];
    }
    else {
      sesameFileCache[jfile].nmats = 0;
      *nmats = 0;
    }

#ifdef MY_DEBUG
    printf("eos_SesGetFileInfoFromCache - after ses_get_materials:  nmats is %d\n", *nmats);
    int k2 = 0;
    for (k2=0; k2 < *nmats; k2++) {
	printf("eos_SesGetFileInfoFromCache -- material is %d\n", sesameFileCache[jfile].sesMats[k2]);
    }
#endif

    if (*nmats < 1) {
#ifdef DEBUG
      printf ("can't read number of materials\n");
#endif
      return (EOS_READ_TOTAL_MATERIALS_FAILED);
    }

    *indexdata = sesameFileCache[jfile].sesMats;
    sesameFileCache[jfile].materialListLoaded = SES_TRUE;

  }
  else {                        /* index data already cached in! */


    *indexdata = sesameFileCache[jfile].sesMats;
    *nmats = sesameFileCache[jfile].nmats;
#ifdef MY_DEBUG
    printf("eos_SesGetFileInfoFromCache - after get from cache:  nmats is %d\n", *nmats);
#endif
    if (ses_is_valid(sesameFileCache[jfile].sesFile) == SES_FALSE) { /* reopen file since information is cached */
	sesameFileCache[jfile].sesFile = ses_open(sesameFiles[jfile], 'R');
        if (ses_is_valid(sesameFileCache[jfile].sesFile) == SES_FALSE) {
		return EOS_OPEN_SESAME_FILE_FAILED;
	}
    }
    *sesFile = sesameFileCache[jfile].sesFile;
        
  }

  return EOS_OK;
}


/*! *********************************************************************
 *
 * Function Name: _eos_SesSeekToDataTable
 * This routine locates and reads data associated with Sesame material ID
 * and Sesame table number, caches the material and table ids into file for later loading 
 * and returns first requested number of bytes 
 *
 * Returned Values:
 * EOS_INTEGER _eos_LoadDataTable - error code
 * EOS_REAL    **ptr             - array of real data read from file
 *                                 passed by reference
 * ses_material_id_reference omid- material id for start of table data
 * ses_table_id_reference otid   - table id for start of table data
 * EOS_INTEGER *dataSize         - size of the data stored in file for this table
 *
 * Input/Output Values:
 * EOS_INTEGER *fileIndex        - index of the file where the table is found in cache.
 *
 * Input Values:
 * EOS_CHAR    **sesameFiles     - array of character strings/arrays
 *                                 passed by reference
 * EOS_INTEGER sesameFileL       - number of strings in **sesameFiles
 *                                 array passed by reference
 * EOS_INTEGER matid             - Sesame material ID
 * EOS_INTEGER tableNum          - Sesame table number
 * EOS_REAL    nreals            - size of **ptr passed by reference
 * EOS_BOOLEAN userDefinedDataFile - is the associated sesame file specified by the user?
 *
 * Uses the following static data: 
 *                                sesameFilesL, sesameFiles
 *
 ************************************************************************/

EOS_INTEGER _eos_SesSeekToDataTable (EOS_CHAR **sesameFiles,
				     EOS_INTEGER sesameFilesL, EOS_INTEGER matid,
				     EOS_INTEGER tableNum, EOS_BOOLEAN userDefinedDataFile,
				     EOS_REAL **ptr, EOS_INTEGER nreals, 
				     ses_material_id_reference omid, ses_table_id_reference otid,
				     EOS_INTEGER *fileIndex,
				     EOS_INTEGER *dataSize, EOS_CHAR **errMsg)
{
  EOS_INTEGER nmats, jfile, i, ierr;
  EOS_INTEGER matIdNotFound;

  EOS_REAL* read_data;
  ses_material_id_reference indexdata;
  ses_file_handle sesFile;

#ifdef PC
  struct _stat statinfo;
#endif
  matIdNotFound = 1;

  ierr = EOS_OK;

  read_data = NULL;

  /*  find the index of the material in the global material map */

  i = _eos_find_matid_in_gMatidMap(matid);

  /*  if the index was found */

  if (i >= 0) {

    /* override jfile */
    jfile = gMatidMap[i].jfile;

    /* save the file index so we can return it to the caller */
    *fileIndex = jfile;

    if (jfile < 0) { /* this should never occur */
      ierr = EOS_MATERIAL_NOT_FOUND;
      return (ierr);
    }

    /* verify requested data is available in the specified file */
    sesFile = 0;
    /* get the info from cache */
    ierr = eos_SesGetFileInfoFromCache (jfile, &nmats, &indexdata, &sesFile);
    if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
      return ierr;

    /* search master directory for material id number.
     * if material id number is located, get length of and
     * pointer to material directory. */
     
    for (i = 0; i < nmats; i++) {
	if (matid == (EOS_INTEGER)(indexdata[i])) {
	   matIdNotFound = 0;
           break;	
	}
    }

    /* if material found, break from file loop! */
    if (!matIdNotFound) {
      /* save the file index so we can return it to the caller */
      *fileIndex = jfile;       /* the file is already opened! */
    }

  }
  else {

    /*  if the index of the asked for material was not found in the global material map */

    EOS_INTEGER jfile_start = 0, jfile_end = sesameFilesL;

    if (userDefinedDataFile) {
      jfile_start = *fileIndex;
      jfile_end = *fileIndex+1;
    }

    /* find first occurrence of matid in ordered list of files */
    for (jfile = jfile_start; jfile < jfile_end; jfile++) {

      sesFile = 0;
      /* get the info from cache */
      ierr = eos_SesGetFileInfoFromCache (jfile, &nmats, &indexdata, &sesFile);

      if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
	return ierr;
      /* search master directory for material id number.
       * if material id number is located, get length of and
       * pointer to material directory.
       */

      for (i = 0; i < nmats; i++) {
	if (matid == (EOS_INTEGER)(indexdata[i])) {
	   matIdNotFound = 0;
           break;	
	}
      }

      /* if material found, break from file loop! */
      if (!matIdNotFound) {
	/* save the file index so we can return it to the caller */
	*fileIndex = jfile;       /* the file is already opened! */
	break;
      }

    }                             /* end jfile loop */

  } /* end else */

  if (matIdNotFound) { /* return if matid not found */
    EOS_INTEGER idx = _eos_find_matid_in_gMatidMap(matid);
    ierr = EOS_MATERIAL_NOT_FOUND;
    if (idx < 0)
      eos_SetCustomMsg_str (errMsg, "EOS_MATERIAL_NOT_FOUND: Material ID, %d, is not in library", matid);
    else
      eos_SetCustomMsg_str (errMsg, "EOS_MATERIAL_NOT_FOUND: Material ID, %d, is not in %s",
			    matid, sesameFiles[gMatidMap[idx].jfile]);
    return (ierr);
  }


  /* search the material directory for the data table type */

  ses_error_flag didit_setup1 = ses_setup(sesFile, (ses_material_id)matid, (ses_table_id)tableNum);

  if (didit_setup1 != SES_NO_ERROR) {
    return (EOS_DATA_TYPE_NOT_FOUND);
  }
  long table_size = ses_get_table_size(sesFile, (ses_material_id)matid, (ses_table_id)tableNum); 
						 /*  get the table size of the current ses table */
						 /*  at the end, return to the start of the table */
  read_data = (EOS_REAL *) malloc ((table_size+4) * sizeof (EOS_REAL));

  /*  read nreals words from the ses file handle */

  ses_word_reference the_buffer = (ses_word_reference)NULL;

  int current_index = 0;
  int j = 0;
  ses_number next_size = 0;
  for (j = 0; j < table_size+3; j++) {
	read_data[j] = 0.0;
  }
  j = 0;
  while ((ses_has_next(sesFile) == SES_TRUE) &&  (current_index < nreals)) {
	
	next_size = ses_array_size_next(sesFile);
	the_buffer = ses_read_next(sesFile);

	if (the_buffer == (ses_word_reference)NULL) {
        }
        else {
	   for (j = 0; j < next_size; j++) {
        	read_data[current_index + j] = the_buffer[j];
	   }
           current_index = current_index + next_size;
	   free(the_buffer);
           the_buffer = (ses_word_reference)NULL;
	}
       
  }
  
  if (nreals >= 3) {
    /* get date1, date2 and version */
    read_data[nreals-3] = (EOS_REAL)FILE_LIST[sesFile]->_current_index_record->_date1;
    read_data[nreals-2] = (EOS_REAL)FILE_LIST[sesFile]->_current_index_record->_date2;
    read_data[nreals-1] = (EOS_REAL)FILE_LIST[sesFile]->_current_index_record->_vers;
  }

  *omid = matid;
  *otid = tableNum;
  *dataSize = table_size;
  *ptr = read_data;

  return (EOS_OK);
}

/*! *********************************************************************
 *
 * Function Name: eos_SesSeekToDataTable
 * This routine locates and reads data associated with Sesame material ID
 * and Sesame table number, caches the material id and table number into file for later loading 
 * and returns first requested number of bytes bytes
 *
 * Returned Values:
 * EOS_INTEGER _eos_LoadDataTable - error code
 * EOS_REAL    **ptr             - array of real data read from file
 *                                 passed by reference
 * ses_material_id_reference omid- material id for start of table data
 * ses_table_id_reference otid   - table id for start of table data
 * EOS_INTEGER *fileIndex        - index of the file where the table is found in cache.
 * EOS_INTEGER *dataSize         - size of the data stored in file for this table
 * EOS_BOOLEAN userDefinedDataFile - is the associated sesame file specified by the user?
 *
 * Input Value:                              
 * EOS_INTEGER matid             - Sesame material ID
 * EOS_INTEGER tableNum          - Sesame table number
 * EOS_REAL    nreals            - size of **ptr passed by reference
 *
 * Uses the following static data: 
 *
 ************************************************************************/

EOS_INTEGER eos_SesSeekToDataTable (EOS_INTEGER matid, EOS_INTEGER tableNum,
				    EOS_REAL **ptr, EOS_INTEGER nreals, EOS_INTEGER *fileIndex,
				    ses_material_id_reference omid, ses_table_id_reference otid,
				    EOS_INTEGER *dataSize, EOS_BOOLEAN userDefinedDataFile,
				    EOS_CHAR **errMsg)
{
  EOS_INTEGER ierr = EOS_OK;

  if (( ! sesameFiles || sesameFilesL <= 0 ) && ! userDefinedDataFile) {
    /* create/update the list of data file names */
    ierr = eos_getSesameFileNames (&sesameFiles, &sesameFilesL, errMsg);
    if (ierr)
      return (ierr);
  }

  /* no data file libraries found */
  if (sesameFilesL == 0)
    return (EOS_NO_SESAME_FILES);

  /* read Sesame material data indexes */\
  ierr = _eos_SesSeekToDataTable (sesameFiles, sesameFilesL, matid, tableNum, userDefinedDataFile,
				  ptr, nreals, omid, otid, fileIndex, dataSize, errMsg);

  return ierr;
}

/*! *********************************************************************
 * 
 * This function loads bulk data from 201 table. Some of these values are needed
 * for entropy and free energy data calculations.
 * 
 * Returned Values:
 * EOS_INTEGER eos_GetBulkData - output error code
 *
 * Input Value:
 * EOS_INTEGER materialID        - Sesame material ID
 * EOS_BOOLEAN userDefinedDataFile - is the associated sesame file specified by the user?
 * EOS_INTEGER dataFileIndex     - index of the sesame file for reading the data
 * EOS_REAL    *zbar             - mean atomic number
 * EOS_REAL    *abar             - mean atomic mass
 * EOS_REAL    *dens0            - normal solid density
 * EOS_REAL    *solidBulkModulus - solid bulk modulus
 * EOS_REAL    *exchangeCoeff    - exchange coeffiicient
 * 
 ************************************************************************/

EOS_INTEGER eos_SesGetBulkData(EOS_INTEGER materialID, EOS_BOOLEAN userDefinedDataFile, 
			       EOS_INTEGER dataFileIndex,
			       EOS_REAL *zbar, EOS_REAL *abar, EOS_REAL *dens0,
			       EOS_REAL *solidBulkModulus, EOS_REAL *exchangeCoeff,
			       EOS_CHAR **errMsg)
{

  ses_material_id  mid= 0;
  ses_table_id tid = 0;
  EOS_INTEGER fileIndex, dataSize, ierr = EOS_OK;
  EOS_REAL *read_data;

  fileIndex = dataFileIndex;

  // load bulk data from 201 table

  ierr = _eos_SesSeekToDataTable (sesameFiles, sesameFilesL, materialID, 201, userDefinedDataFile,
				  &read_data, 9, &mid, &tid, &fileIndex, &dataSize, errMsg);

  if (ierr)
    return (ierr);
  *zbar = read_data[0];
  *abar = read_data[1];
  *dens0 = read_data[2];
  *solidBulkModulus = read_data[3];
  *exchangeCoeff = read_data[4];

  EOS_FREE (read_data);

  return (ierr);
}

EOS_INTEGER eos_SesLoadSesameFiles (ses_material_id mid, ses_table_id tid, EOS_INTEGER fileIndex,
				    EOS_REAL **ptr, EOS_INTEGER nreals)
{
  EOS_INTEGER nmats, ierr;
  ses_material_id_reference indexdata = (ses_material_id_reference)NULL;
  EOS_REAL* read_data;
  EOS_INTEGER read_data_len = nreals+10;
  ses_file_handle sesFile;
#ifdef PC
  struct _stat statinfo;
#endif

  read_data = NULL;

  ierr = eos_SesGetFileInfoFromCache (fileIndex, &nmats, &indexdata, &sesFile);

  if (eos_GetStandardErrorCodeFromCustomErrorCode(ierr) != EOS_OK)
    return ierr;

  ses_word_reference the_buffer = (ses_word_reference)NULL;
  int current_index = 0;
  int offset = 2;
  int j = 0;

  offset = 2;                 /* skip NR & NT (default) */
  if (tid == 201) offset = 0; /* skip nothing */
  if (tid == 401 || tid == 501) offset = 1; /* skip NT */

  /*  ses_io has nr and nt as part of the arrays to be gotten this way, eospac doesn't */
  int time_through = 0;

  ses_error_flag didit_setup2 = ses_setup(sesFile, (ses_material_id)mid, (ses_table_id)tid);
  
  if (didit_setup2 != SES_NO_ERROR) {
    return (EOS_READ_DATA_FAILED);
  }
  else {
    read_data = (EOS_REAL*) malloc (read_data_len * sizeof(EOS_REAL));
  }
  
  ses_number next_size = 0;

  if (tid > 100 && tid < 200) {

    ses_string outer = (ses_string)NULL;
    ses_error_flag error = ses_comments(sesFile, &outer);
    if (error != SES_NO_ERROR) {
      EOS_FREE (read_data);
      EOS_FREE (outer);
      return (EOS_READ_DATA_FAILED);
    }

    strncpy ((ses_string)read_data, outer, read_data_len * sizeof(EOS_REAL));

    EOS_FREE(outer);

  } else {

    while ((ses_has_next(sesFile) == SES_TRUE) && (current_index <= nreals)) {
	
      next_size = ses_array_size_next(sesFile);
      the_buffer = ses_read_next(sesFile);
      /*  PUT ERROR HANDLING HERE 	*/
      if (the_buffer == (ses_word_reference)NULL) {
      }
      else {
	if (time_through >= offset) {
	  for (j = 0; j < next_size; j++) {
	    read_data[current_index + j] = the_buffer[j];
	  }
	  current_index = current_index + next_size;
	  free(the_buffer);
	  the_buffer = (ses_word_reference)NULL;
	}
	else {
	  free(the_buffer);
          the_buffer = (ses_word_reference)NULL;
	}
      }
      time_through++;
    }

  }

  *ptr = read_data;
  ses_error_flag didit_setup = ses_setup(sesFile, mid, tid);

  if (didit_setup != SES_NO_ERROR) {
    return (EOS_READ_DATA_FAILED);
  }

  return EOS_OK;
}

EOS_INTEGER eos_SesCleanFileCache ()
{
  EOS_INTEGER i;
  if (sesameFilesL <= 0)
    return EOS_OK;

  /* close all sesFile[]  */
  for (i = 0; i < sesameFilesL; i++) {
    if (sesameFileCache[i].materialListLoaded) {
      if (sesameFileCache[i].sesFile > 0) {
	ses_error_flag didit_close = ses_close (sesameFileCache[i].sesFile);
	if (didit_close != SES_NO_ERROR) {
	  /* DAP: do nothing for now */
	}
      }     
    }
    EOS_FREE (sesameFiles[i]);
  }
  EOS_FREE (sesameFiles);

  /* deallocate all sesMats[]  */
  for( i=0; i<sesameFileCacheL; i++ )
    if( sesameFileCache[i].materialListLoaded )
      EOS_FREE( sesameFileCache[i].sesMats );
  EOS_FREE (sesameFileCache);

  sesameFilesL = 0;

  for (i = 0; i < sesameIndexLocationsL; i++) {
    EOS_FREE(sesameIndexLocations[i]);
  }
  EOS_FREE(sesameIndexLocations);

  sesameIndexLocationsL = 0;

  return EOS_OK;
}


/***********************************************/


long ses_get_table_size(ses_file_handle sesFile, ses_material_id mid, ses_table_id tid)
{

  /*  get the table size of the current ses table */

  /*  assume we are set up to the start of the table */

  long return_value = 0;
  ses_number array_size = 0;
  ses_error_flag didit_skip = SES_NO_ERROR;
  while (ses_has_next(sesFile) == SES_TRUE) {
    array_size = ses_array_size_next(sesFile);
    didit_skip = ses_skip(sesFile);
    if (didit_skip != SES_NO_ERROR) {
      /* DAP: do nothing for now */
    }
    return_value = return_value + (long)array_size;
  }

  ses_error_flag didit_setup = ses_setup(sesFile, mid, tid);  
  if (didit_setup != SES_NO_ERROR) {
    /* DAP: do nothing for now */
  }

  return return_value;
}
 
ses_error_flag ses_table_dates(ses_file_handle sesFile, long* date1, long* date2, long* vers)
{

  ses_table_id_reference the_tables = (ses_table_id_reference)NULL;
  long* nwds = (long*)NULL;
  long* iadr = (long*)NULL;
  long ntbls = ses_access_table_index(sesFile, &the_tables, &nwds, &iadr, date1, date2, vers);
  if (ntbls < 0) {
    /* DAP: do nothing for now */
  }

  return SES_NO_ERROR;
}



