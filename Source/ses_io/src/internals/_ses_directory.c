


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#undef DEBUG_PRINT

struct _ses_directory* _construct_ses_directory() {

  /*  construct a new directory */

  struct _ses_directory* the_directory = (struct _ses_directory*)NULL;

  /*  allocate memory for the return value */


  the_directory = malloc(sizeof(struct _ses_directory)*1);

  if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_directory: malloc error in _construct_ses_directory\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return NULL;
  }

  /*  set the data members */

  the_directory->_nfiles = 0;
  the_directory->_date = 0;
  the_directory->_version = 0;
  the_directory->_matid = (long*)NULL;
  the_directory->_nwds = (long*)NULL;
  the_directory->_iadr = (long*)NULL;
  the_directory->_ready = SES_FALSE;

  /* return */

  the_directory->_material_path = (char**)NULL;
  the_directory->_has_multiple_files = SES_FALSE;

  return the_directory;
}




struct _ses_directory* _construct_ses_directory_from_material_file_list(struct _ses_material_file_list* pMFL, struct _ses_file_handle* pSFH) {

  /*  construct a ses directory from a ses material file list */

  /*  error check the arguments */
#ifdef DEBUG_PRINT
    printf("_ses_directory::_construct_ses_directory_from_material_file_list \n");
#endif

  if (pMFL == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_directory_from_material_file_list: null material file list \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_directory*)NULL; 
  }

  /*  allocate memory for the return value */

  struct _ses_directory* the_directory = malloc(sizeof(struct _ses_directory)*1);
  if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_directory_from_material_file_list: malloc error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_directory*)NULL;
  }

  /*  set the data members */


  the_directory->_ready = SES_TRUE;
  the_directory->_nfiles = _get_nfiles_from_list(pMFL);
  the_directory->_date = 0;
  the_directory->_version = 0;
  the_directory->_matid = _get_materials_from_list(pMFL);

  the_directory->_nwds = _get_nwds_from_list(pMFL);

  long start = _get_directory_size(the_directory, pSFH);
  if (start <= 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_directory_from_material_file_list: directory size <= 0 \n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);

    /*  release memory on error */

    free(the_directory);
    the_directory = (struct _ses_directory*)NULL;

    /*  return on error */
    return (struct _ses_directory*)NULL;
  }

  the_directory->_iadr = _get_iadr_from_list(pMFL, start);
  the_directory->_material_path = malloc(sizeof(char*) * the_directory->_nfiles);
  int i = 0;
  for (i = 0; i < the_directory->_nfiles; i++) {
        char* directory_name = malloc(sizeof(char) * 100);
        sprintf(directory_name, "L%ld/material.xml", the_directory->_matid[i]);
	the_directory->_material_path[i] = malloc(sizeof(char)*( strlen(directory_name) + 1));
	strcpy(the_directory->_material_path[i], directory_name);
        free(directory_name);
        directory_name = (char*)NULL;
  }

  // GINGER print out the results:
//  printf("_construct_ses_directory_from_mfl:: nfiles: %ld, date: %ld, version: %ld\n", the_directory->_nfiles, the_directory->_date, the_directory->_version);
//  for (i=0; i < the_directory->_nfiles; i++){
//    printf("\tmid: %ld, _nwds: %ld, _iadr: %ld\n", the_directory->_matid[i], the_directory->_nwds[i], the_directory->_iadr[i]);
//  }


  /*  return */


  return the_directory;
}

struct _ses_directory* _copy_ses_directory(struct _ses_directory* ptDIR) {

  /*  copy the ses directory */

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: null directory pointer passed in -- returning NULL directory\n");
#endif
    return (struct _ses_directory*)NULL;
  }
 
  /*  allocate memory for the return_value */

  struct _ses_directory* return_value = malloc(sizeof(struct _ses_directory)*1);
  if (return_value == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: malloc error in _copy_ses_directory\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_directory*)NULL;
  }

  /*  copy the ses directory */


  return_value->_nfiles = ptDIR->_nfiles;
  if (return_value->_nfiles < 0) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: nfiles < 0 on copy ses directory\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);

    /*  free memory on error */
    free(return_value);
    return_value = (struct _ses_directory*)NULL;

    return (struct _ses_directory*)NULL;

  }

  return_value->_date = ptDIR->_date;
  return_value->_version = ptDIR->_version;

  return_value->_matid = malloc(sizeof(long)*return_value->_nfiles);
  if (return_value->_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: memory allocation error in _copy ses directory\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error */
    free(return_value);
    return_value = (struct _ses_directory*)NULL;


    return (struct _ses_directory*)NULL;
  }

  return_value->_nwds = malloc(sizeof(long)*return_value->_nfiles);
  if (return_value->_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: memory allocation error in _copy ses directory\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error */
    free(return_value->_matid);
    return_value->_matid = (long*)NULL;
    free(return_value);
    return_value = (struct _ses_directory*)NULL;

    return (struct _ses_directory*)NULL;
  }

  return_value->_iadr = malloc(sizeof(long)*return_value->_nfiles);
  if (return_value->_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_directory: memory allocation error in _copy ses directory\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    /*  free memory on error */
    free(return_value->_matid);
    return_value->_matid = (long*)NULL;
    free(return_value->_nwds);
    return_value->_nwds = (long*)NULL;
    free(return_value);
    return_value = (struct _ses_directory*)NULL;
    return (struct _ses_directory*)NULL;
  }

  int i = 0;
  for (i=0; i < ptDIR->_nfiles; i++) {
    return_value->_matid[i] = ptDIR->_matid[i];
    return_value->_nwds[i] = ptDIR->_nwds[i];
    return_value->_iadr[i] = ptDIR->_iadr[i];
  }

  return_value->_ready = ptDIR->_ready;
  
  return_value->_material_path = (char**)NULL;
  if (ptDIR->_material_path != (char**)NULL) {
  	int i = 0;
        return_value->_material_path = malloc(sizeof(char*)*ptDIR->_nfiles);
  	for (i = 0; i < ptDIR->_nfiles; i++) {
		return_value->_material_path[i] = (char*)NULL;
		if (ptDIR->_material_path[i] != (char*)NULL) {

			int length = strlen(ptDIR->_material_path[i]);
			return_value->_material_path[i] = malloc(sizeof(char)*(length+1));
			strcpy(return_value->_material_path[i], ptDIR->_material_path[i]);
		}
  	}
  }
  // GINGER print out the results:
//  printf("_copy_ses_directory:: nfiles: %ld, date: %ld, version: %ld\n", return_value->_nfiles, return_value->_date, return_value->_version);
//  for (i=0; i < return_value->_nfiles; i++){
//    printf("\tmid: %ld, _nwds: %ld, _iadr: %ld\n", return_value->_matid[i], return_value->_nwds[i], return_value->_iadr[i]);
//  }

  /*  return */
  return return_value;
}



ses_boolean _destruct_ses_directory(struct _ses_directory* ptDIR)  {

  /*  destruct the ses directory */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
    return SES_TRUE;
  }

  long nfiles = ptDIR->_nfiles;

  ptDIR->_nfiles = (long)0;
  ptDIR->_date = (long)0;
  ptDIR->_version = (long)0;
  

  free(ptDIR->_matid);
  ptDIR->_matid = (long*)NULL;

  free(ptDIR->_nwds);
  ptDIR->_nwds = (long*)NULL;

  free(ptDIR->_iadr);
  ptDIR->_iadr = (long*)NULL;

  ptDIR->_ready = SES_FALSE;



  /*  destruct material path for llnl xml */
  if (ptDIR->_material_path != (char**)NULL) {
  	int i = 0;
  	for (i = 0; i < nfiles; i++) {
		if (ptDIR->_material_path[i] != (char*)NULL) {
			free(ptDIR->_material_path[i]);
			ptDIR->_material_path[i] = (char*)NULL;			
		}
  	}
  	free(ptDIR->_material_path);
  	ptDIR->_material_path = (char**)NULL;
  }


  /*  return */

  return return_value;
}

ses_boolean _isit_ready_directory(struct _ses_directory* ptDIR ) {

  /*  return whether the directory is ready */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_isit_ready_directory: null directory pointer passed in to _isit_ready_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */

  return_value = ptDIR->_ready;
  return return_value;
}

long* _get_matid(struct _ses_directory* ptDIR) {

  /*  return the matid array */

  long* return_value = NULL;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_matid: null directory pointer passed in to _get_matid\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (long*)NULL;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_matid: Trying to _get_matid from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (long*)NULL;
  }

  /*  return */

  return_value = ptDIR->_matid;
  return return_value;
}
long* _get_nwds_directory(struct _ses_directory* ptDIR) {

  /*  return the nwds array */

  long* return_value = (long*)NULL;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_directory: null directory pointer passed in to _get_nwds_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (long*)NULL;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_directory: Trying to _get_nwds from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (long*)NULL;
  }

  /*  return */

  return_value = ptDIR->_nwds;
  return return_value;  
}
long* _get_iadr_directory(struct _ses_directory* ptDIR ) {

  /*  return the iadr array */

  long* return_value = NULL;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_directory: null directory pointer passed in to _get_iadr_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (long*)NULL;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_directory: Trying to _get_iadr from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (long*)NULL;
  }

  /*  return */

  return_value = ptDIR->_iadr;
  return return_value; 
}

/*--------------------------------------------*/

ses_boolean _check_directory_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid) {

  /*  this routine reads the directory file and returns true if the material
      is in the index */

  ses_boolean return_value = SES_FALSE;
#ifdef DEBUG_PRINT
    printf("_check_directory_for_material: handle: %ld, mid: %ld\n", ptDIR, the_mid);
#endif

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_check_directory_for_material: directory is null in _check_dir for material\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_check_directory_for_material: invalid mid = %ld in _check_directory_for_material\n", the_mid);
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_FALSE;
  }

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_check_directory_for_material: nfiles <= 0, nfiles: %d\n", ptDIR->_nfiles);
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_FALSE;
  }

  /*  find the material */
#ifdef DEBUG_PRINT
    printf("_check_directory_for_material: 10\n");
#endif

  long i=0;
  for (i=0; i < ptDIR->_nfiles; i++) {
#ifdef DEBUG_PRINT
      printf("_check_directory_for_material: nfiles: %d, i: %d, mid: %ld\n", ptDIR->_nfiles, i, the_mid);
#endif
      
    if (the_mid == ptDIR->_matid[i]) {
#ifdef DEBUG_PRINT
        printf("_check_directory_for_material: ptDIR->_matid[i]: %ld\n", ptDIR->_matid[i]);
#endif
      return_value = SES_TRUE;
    }
  }

  /*  return */
  // GINGER print values:
//  for (i = 0; i < ptDIR->_nfiles; i++) {
//    printf("_check_directory_for_material: _matid: %ld, _nwds: %ld, _iadr: %ld\n",ptDIR->_matid[i],ptDIR->_nwds[i],ptDIR->_iadr[i] );
//  }

  return return_value;
}



ses_boolean _add_materials(struct _ses_directory* ptDIR, long* _matid,
                           long* _nwds, long* _iadr, long _nfiles ) {

  /*  add material information to the directory */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: null directory pointer passed in to _add_materials\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_add_materials: nfiles of 0 passed to _add_materials <= 0\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: null matid passed in to _add_materials\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: null nwds passed in to _add_materials\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  if (_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("null iadr passed in to _add_materials\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  /*  allocate memory for _matid */

  ptDIR->_matid = malloc(sizeof(long)*_nfiles);
  if (ptDIR->_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: memory allocation failed for _matid in _add_materials\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_FALSE;
  }

  ptDIR->_nwds = malloc(sizeof(long)*_nfiles);
  if (ptDIR->_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: memory allocation failed for _nwds in _add_materials\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error */
    free(ptDIR->_matid);
    ptDIR->_matid = (long*)NULL;

    return SES_FALSE;
  }

 
  ptDIR->_iadr = malloc(sizeof(long)*_nfiles);
  if (ptDIR->_iadr == NULL) {
#ifdef DEBUG_PRINT
    printf("_add_materials: memory allocation failed for _iadr in _add_materials\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error */
    free(ptDIR->_matid);
    ptDIR->_matid = (long*)NULL;
    free(ptDIR->_nwds);
    ptDIR->_nwds = (long*)NULL;

    return SES_FALSE;
  }

  /*  copy in the arrays */

  ptDIR->_nfiles = _nfiles;
  int i = 0;
  for (i = 0; i < _nfiles; i++) {
    ptDIR->_matid[i] = _matid[i];
    ptDIR->_nwds[i] = _nwds[i];
    ptDIR->_iadr[i] = _iadr[i];
  }
  // GINGER print values:
//  for (i = 0; i < _nfiles; i++) {
//    printf("_add_materials: _matid: %ld, _nwds: %ld, _iadr: %ld\n",ptDIR->_matid[i],ptDIR->_nwds[i],ptDIR->_iadr[i] );
//  }
  ptDIR->_ready = SES_TRUE; 

  /*  return */

  return return_value;
}


ses_boolean _write_directory(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {


  /*  write the directory to the c file handle */	
	
  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory: null directory passed to _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_directory: null ses file handle passed into _write_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_isit_ready_directory(ptDIR) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_write_directory: Trying to write the directory and it is not ready \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_FALSE;
  }

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_write_directory: _nfiles <= 0 in _write_directory \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_FALSE;
  }

  if (pSFH->pt2_write_directory == NULL) {
#ifdef DEBUG_PRINT
    printf("write_directory:  function pointer null\n");
#endif
    return SES_FALSE;
  }

  return_value = pSFH->pt2_write_directory(ptDIR, pSFH);



  /* return */

  return return_value;
}


struct _ses_directory*  _read_directory(struct _ses_file_handle* pSFH) {

  /*  read the directory from the file handle */
  /*  the directory is always at the beginning of the file */

  struct _ses_directory* the_directory = (struct _ses_directory*)NULL;

  /*  error check the arguments */

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_directory: ses file handle NULL in _read_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_directory*)NULL;
  }

  /*  internal error checks */

  if (pSFH->pt2_read_directory == NULL) {

#ifdef DEBUG_PRINT
    printf("_read_directory: Function pointer to read directory function is null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return (struct _ses_directory*)NULL;
  }

  /*  construct return value */

  if (the_directory == (struct _ses_directory*)NULL) {
     the_directory  = _construct_ses_directory();
     if (the_directory  == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
       printf("_read_directory: construct_ses_directory  error in _read_directory\n");
#endif
       
       _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
       return (struct _ses_directory*)NULL;
     }
  

     /*  read the directory */
  

     ses_error_flag read_errors = pSFH->pt2_read_directory(the_directory, pSFH);
     if (read_errors != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
       printf("_read_directory: read_errors in _read_directory are %d\n", read_errors);
#endif
       _destruct_ses_directory(the_directory);
       free(the_directory);
       the_directory = (struct _ses_directory*)NULL;

       _set_latest_error(read_errors);

       return the_directory;
     }
  }
    
  /*  return */

  return the_directory;

}

ses_boolean _add_new_material(struct _ses_directory* the_directory, ses_material_id the_mid) {

  the_directory->_nfiles++;
  long* _new_matid = malloc(sizeof(long)*the_directory->_nfiles);
  long* _new_nwds = malloc(sizeof(long)*the_directory->_nfiles);
  long* _new_iadr = malloc(sizeof(long)*the_directory->_nfiles);

  int i = 0;
  for (i=0; i < the_directory->_nfiles-1; i++) {
    _new_matid[i] = the_directory->_matid[i];
    _new_nwds[i] = the_directory->_nwds[i];
    _new_iadr[i] = the_directory->_iadr[i];
  }

  _new_matid[i] = the_mid;
  _new_nwds[i] = 0;
  _new_iadr[i] = 0;

  free(the_directory->_matid);
  the_directory->_matid = _new_matid;
  free(the_directory->_nwds);
  the_directory->_nwds = _new_nwds;
  free(the_directory->_iadr);
  the_directory->_iadr = _new_iadr;

  the_directory->_ready = SES_FALSE;

  // GINGER print values:
//  for (i = 0; i < the_directory->_nfiles-1; i++) {
//    printf("_add_new_material: _matid: %ld, _nwds: %ld, _iadr: %ld\n", the_directory->_matid[i],the_directory->_nwds[i],the_directory->_iadr[i] );
//  }

  return SES_TRUE;


}

int _get_material_index(struct _ses_directory* the_directory, ses_material_id the_mid) {

  int return_value = -1;

  if (the_directory != (struct _ses_directory*)NULL) {

	int i = 0;
	for (i=0; i < the_directory->_nfiles;i++) {
		if (the_directory->_matid[i] == the_mid) {
			return_value = i;
		}
  	}
  }
  return return_value;
}




































