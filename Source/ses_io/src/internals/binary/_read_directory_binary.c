


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"



ses_error_flag _read_directory_binary(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {

  ses_error_flag return_value = SES_NO_ERROR;

  /*******************************************************************/
  /* argument error checking */
  if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary: null directory pointer passed in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary:  null ses file handle in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  /*******************************************************************/

  FILE* pFILE = _getPFILE(pSFH);
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary: Null FILE* \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  return to the start of the file  */

  rewind(pFILE);

  /*  read record 1 */

  long nfiles  = _read_long(pSFH);
 if (nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary: nfiles < 0 in _read_directory\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  else {
    the_directory->_nfiles = nfiles;
  }
 
  the_directory->_date = _read_long(pSFH);

  the_directory->_version = _read_long(pSFH);
  
  /*  read arrays */


  the_directory->_matid = malloc(sizeof(long)*nfiles);
  if (the_directory->_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary: memory allocation error _matid in _read_directory_binary\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);    
    return SES_MEMORY_ALLOCATION_ERROR;
  }

  long i;
  for (i= 0;  i<nfiles; i++) {
    the_directory->_matid[i] = _read_long(pSFH);
  }
 

  the_directory->_nwds = malloc(sizeof(long)*nfiles);
  if (the_directory->_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary; memory allocation error _nwds in _read_directory_binary\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }

  for (i= 0;  i<nfiles; i++) {
    the_directory->_nwds[i] = _read_long(pSFH);
  }
  the_directory->_iadr = malloc(sizeof(long)*nfiles);
  if (the_directory->_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory_binary: memory allocation error _iadr in _read_directory_binary\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }
  
  for (i= 0;  i<nfiles; i++) {
    the_directory->_iadr[i] = _read_long(pSFH);
  }

  the_directory->_has_multiple_files = SES_FALSE;
  the_directory->_ready = SES_TRUE;

  //_releasePFILE(pSFH);

  /*  return */

  return return_value;

}






































