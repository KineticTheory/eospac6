
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#define check_errors_get_size_of_directory HEADER(check_errors_get_size_of_directory)
#define check_errors_dmeiof HEADER(check_errors_dmeiof)
#define check_errors_set_directory HEADER(check_errors_set_directory)
#define check_errors_add_material_file HEADER(check_errors_add_material_file)

/***************************************************************/

struct _ses_output_file* _construct_output_file(char _output_file_type) {

  /*  construct the _ses_output_file */

  /*  create memory for the return value */

  struct _ses_output_file* the_file = malloc(sizeof(struct _ses_output_file)*1);
  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_output_file: memory allocation error in _construct_output_file\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  /*  construct and initialize the output file data */

  the_file->_ready_to_write = SES_FALSE;

  the_file->_directory_to_write = (struct _ses_directory*)NULL;
  the_file->_directory_to_write = _construct_ses_directory();
  if (the_file->_directory_to_write == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_output_file: directory failed to construct in _construct output file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_output_file*)NULL;
  }


  the_file->_material_files_to_write = (struct _ses_material_file_list*)NULL;
  the_file->_material_files_to_write = _construct_material_file_list();
  if (the_file->_material_files_to_write == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_output_file: material file list failed to construct in _construct output file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  /*  return */

  the_file->_filetype = _output_file_type;
  the_file->_seen_100 = SES_FALSE;

  //  change state after object constructed successfully to ready_to_write == SES_TRUE

  the_file->_ready_to_write = SES_TRUE;
 
  return the_file;
}
/***************************************************************/

struct _ses_output_file*  _copy_output_file(struct _ses_output_file* pOF) {

  /*  copy the ses output file */

  /*  argument error checking */

  if (pOF == (struct _ses_output_file*)NULL) {
    return (struct _ses_output_file*)NULL;
  }
 
  /*  create memory for the return value */

  struct _ses_output_file* the_file = _construct_output_file(pOF->_filetype);
  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_output_file: memory allocation error in _copy_output_file\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  /*  copy to the return value */

  the_file->_ready_to_write = pOF->_ready_to_write;
  if (the_file->_directory_to_write != (struct _ses_directory*)NULL) {

     /*  free initial constructed directory and copy it in */

     _destruct_ses_directory(the_file->_directory_to_write);
     free(the_file->_directory_to_write);
     the_file->_directory_to_write = (struct _ses_directory*)NULL;
  }

  the_file->_directory_to_write = _copy_ses_directory(pOF->_directory_to_write);
  if (the_file->_directory_to_write == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_output_file: ses directory copy error in _copy_output_file\n");
#endif

    //  release memory for the the_file on error

    _destruct_output_file(the_file);
    free(the_file);
    the_file = (struct _ses_output_file*)NULL;

    _set_latest_error(SES_OBJECT_COPY_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  if (the_file->_material_files_to_write != (struct _ses_material_file_list*)NULL) {

        /*  free initial constructed material file list and copy it in*/

	_destruct_material_file_list(the_file->_material_files_to_write);
        free(the_file->_material_files_to_write); 
        the_file->_material_files_to_write = (struct _ses_material_file_list*)NULL;
  }

  the_file->_material_files_to_write = _copy_material_file_list(pOF->_material_files_to_write);
  if (the_file->_material_files_to_write == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_output_file: ses material file list copy error in _copy_output_file\n");
#endif
    //  release memory for the the_file on error

    _destruct_output_file(the_file);
    free(the_file);
    the_file = (struct _ses_output_file*)NULL;

    _set_latest_error(SES_OBJECT_COPY_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  the_file->_filetype = pOF->_filetype;
  the_file->_seen_100 = pOF->_seen_100;

  /*  return */

  return the_file;

}

/***************************************************************/

struct _ses_output_file*  _combine_into_one(ses_file_handle handle1, 
                                            ses_file_handle handle2) {

  /*  combine two output files into one (as a new one) */

  struct _ses_output_file* return_value = (struct _ses_output_file*)NULL;

  /*  error check the arguments */

  ses_boolean didthey_combine = SES_FALSE;

  struct _ses_output_file* first_file = _read_into_output_file(handle1);
  struct _ses_output_file* second_file = _read_into_output_file(handle2);

  if (first_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_into_one: null output file passed to _combine_into_one\n");
#endif

    if (second_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(second_file);
	free(second_file);
	second_file = (struct _ses_output_file*)NULL;
    }
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  if (second_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_into_one: null output file passed to _combine_into_one\n");
#endif
    if (first_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(first_file);
	free(first_file);
	first_file = (struct _ses_output_file*)NULL;
    }
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  //  Here, both first_file and second_file are valid output files

  /*  get the material file lists for the two files */

  struct _ses_material_file_list* pMFL1 = first_file->_material_files_to_write;
  if (pMFL1 == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_into_one: null first material file list found in _combine_into_one\n");
#endif
    if (first_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(first_file);
	free(first_file);
	first_file = (struct _ses_output_file*)NULL;
    }
    if (second_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(second_file);
	free(second_file);
	second_file = (struct _ses_output_file*)NULL;
    }
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  struct _ses_material_file_list* pMFL2 = second_file->_material_files_to_write;
  if (pMFL2 == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_into_one: null second material file list found in _combine_into_one\n");
#endif
    if (first_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(first_file);
	free(first_file);
	first_file = (struct _ses_output_file*)NULL;
    }
    if (second_file != (struct _ses_output_file*)NULL) {
	_destruct_output_file(second_file);
	free(second_file);
	second_file = (struct _ses_output_file*)NULL;
    }
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  /*  construct the return value */

  return_value = _construct_output_file(FILE_LIST[handle1]->_the_handle->_filetype);

  /*  attch the first material file list to the outgoing material file list */

  if (return_value->_material_files_to_write != (struct _ses_material_file_list*)NULL) {
        /*  remvoe the material file list */

	_destruct_material_file_list(return_value->_material_files_to_write);
        free(return_value->_material_files_to_write);
	return_value->_material_files_to_write = (struct _ses_material_file_list*)NULL;
  }
  return_value->_material_files_to_write = pMFL1;

  /*  one at a time, combine the material files on the second file into the outgoing list */

  struct _ses_material_file* tmp = pMFL2->_head;
  struct _ses_material_file* next = (struct _ses_material_file*)NULL;
 
  while (tmp != (struct _ses_material_file*)NULL) {
    didthey_combine = _combine_in_list(return_value->_material_files_to_write, tmp);
    if (didthey_combine == SES_FALSE) {
	//  free up the memory for tmp if it did not get combined with the first
	next = tmp->_next;
        if (tmp == second_file->_material_files_to_write->_head) {
		second_file->_material_files_to_write->_head = next;
	}
	_destruct_material_file(tmp);
	free(tmp);
	tmp = (struct _ses_material_file*)NULL;	
	tmp = next;
    }
    else {
    	tmp = tmp->_next;
    }
  }

  /*  create a directory for the return_value */

  return_value->_directory_to_write->_nfiles = _get_nfiles_from_list(return_value->_material_files_to_write);
  return_value->_directory_to_write->_date = 0;
  return_value->_directory_to_write->_version = 0;
  return_value->_directory_to_write->_matid = _get_materials_from_list(return_value->_material_files_to_write);
  return_value->_directory_to_write->_nwds = _get_nwds_from_list(return_value->_material_files_to_write);
  return_value->_directory_to_write->_iadr = _get_iadr_from_list(return_value->_material_files_to_write, 0);
  return_value->_directory_to_write->_ready = SES_TRUE;

  //  destruct objects

 _destruct_and_detach_output_file(first_file);
  pMFL1 = (struct _ses_material_file_list*)NULL;
  free(first_file);
  first_file = (struct _ses_output_file*)NULL;

  _destruct_output_file(second_file);

  free(second_file->_material_files_to_write);
  second_file->_material_files_to_write = (struct _ses_material_file_list*)NULL;
  pMFL2 = (struct _ses_material_file_list*)NULL;

  free(second_file);
  second_file = (struct _ses_output_file*)NULL;

  return return_value;
}
/***************************************************************/

ses_boolean _destruct_output_file(struct _ses_output_file* the_file) {

  /*  destruct the ses output file */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (the_file == (struct _ses_output_file*)NULL) {
    return SES_TRUE;
  }

  /* destruct the data members */

  the_file->_ready_to_write = SES_FALSE;

  ses_boolean didit_destruct = _destruct_ses_directory(the_file->_directory_to_write);
  if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_output_file: directory did not destruct in _destruct output file \n");
#endif
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }
  free(the_file->_directory_to_write);
  the_file->_directory_to_write = (struct _ses_directory*)NULL;


  ses_boolean didit_mf_dstrct = _destruct_material_file_list(the_file->_material_files_to_write);
  if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_output_file: material file list did not destruct in _destruct output file \n");
#endif
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }
  free(the_file->_material_files_to_write);
  the_file->_material_files_to_write = (struct _ses_material_file_list*)NULL;

  if (didit_mf_dstrct == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_output_file: material file list did not destruct in _destruct output file\n");
#endif
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }

  the_file->_filetype = _default_format;
  the_file->_seen_100 = SES_FALSE;

  /*  return */

  return return_value;
}

/***************************************************************/

ses_boolean               _destruct_and_detach_output_file(struct _ses_output_file* the_file) {

  /*  destruct the ses output file but leave the material file list around */

  /*  destruct the ses output file */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (the_file == (struct _ses_output_file*)NULL) {
    return SES_TRUE;
  }

  /* destruct the data members */

  the_file->_ready_to_write = SES_FALSE;

  ses_boolean didit_destruct = _destruct_ses_directory(the_file->_directory_to_write);
  if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_output_file: irectory did not destruct in _destruct output file \n");
#endif
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }

  free(the_file->_directory_to_write);
  the_file->_directory_to_write = (struct _ses_directory*)NULL;

  the_file->_filetype = _default_format;
  the_file->_seen_100 = SES_FALSE;

  /*  return */

  return return_value;
}

/***************************************************************/


long _get_size_of_directory(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH) {

  /*  return the size of the directory in the output file */

  /*  function prototypes */

  ses_error_flag check_errors_get_size_of_directory(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  long return_value = 0;

  ses_error_flag check = check_errors_get_size_of_directory(the_file, pSFH);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  /*  call the directory function */
 
  return_value = _get_directory_size(the_file->_directory_to_write, pSFH);

  /*  return */

  return return_value;
}

/***************************************************************/


ses_boolean  _isit_ready_output_file(struct _ses_output_file* the_output_file) {

  /*  return whether or not the output file is ready to be written */

  /*  argument error checking */

  if (the_output_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_isit_ready_output_file: null output_file pointer passed into _isit_ready_output_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }


  /*  return with data member value */

  return the_output_file->_ready_to_write;
}

/***************************************************************/

ses_boolean _does_material_exist_in_output_file(struct _ses_output_file* the_output_file, ses_material_id the_mid) {

 /*  return whether or not the material with the given material id exists in the output file */

 /*  function prototypes */

  ses_error_flag check_errors_dmeiof(struct _ses_output_file* the_file, ses_material_id the_mid);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  ses_error_flag check = check_errors_dmeiof(the_output_file, the_mid);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

 
  /*  call material file list function */

  return_value = _does_material_exist_in_list(the_output_file->_material_files_to_write, the_mid);

  /*  return */
 
  return return_value;
}

/***************************************************************/

ses_boolean  _set_directory(struct _ses_output_file* the_output_file,
                            struct _ses_directory* the_directory) {

  /*  set the directory of the output file */

  /*  function prototypes */

  ses_error_flag check_errors_set_directory(struct _ses_output_file* the_file, struct _ses_directory* the_directory);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  ses_error_flag check = check_errors_set_directory(the_output_file, the_directory);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  set the directory, NOT copy */

  if (the_output_file->_directory_to_write != (struct _ses_directory*)NULL) {

        /*  destruct the directory if it exists */

	_destruct_ses_directory(the_output_file->_directory_to_write);
        free(the_output_file->_directory_to_write);
        the_output_file->_directory_to_write = (struct _ses_directory*)NULL;
  }

  the_output_file->_directory_to_write = the_directory;

  return return_value;
}

/***************************************************************/

ses_boolean  _add_material_file(struct _ses_output_file* the_output_file, 
                                struct _ses_material_file* the_material_file) {

  /*  add a material file to the output file */

  /*  function prototypes */

  ses_error_flag check_errors_add_material_file(struct _ses_output_file* the_file, struct _ses_material_file* the_material_file);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  ses_error_flag check = check_errors_add_material_file(the_output_file, the_material_file);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  put the current material file into the output file */

  if (_does_material_exist_in_output_file(the_output_file, the_material_file->_the_mid) == SES_FALSE) {

     /* put the current material file into the output file if the material doesn't already exist */

    ses_boolean didit_add = _add_to_list(the_output_file->_material_files_to_write, the_material_file);

    //  the_material_file was put on the list as a 'pointed to object, do NOT destruct 

    if (didit_add == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_add_material_file: add to list returned false \n");
#endif
      _set_latest_error(SES_FUNCTION_FAIL_ERROR);
      return SES_FALSE;
    }
  }
  else {

   /* add the current material file to an existing material file if the material already exists */
   /*       for each table in the 'new' material file */
   /*            add the table to the existing file if the table doesn't already exist */
   /*            error if it does   */
    
   ses_boolean didthey_combine = _combine_in_list(the_output_file->_material_files_to_write, the_material_file);
   if (didthey_combine == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_add_material_file: didthey_combine error\n");
#endif
      _set_latest_error(SES_FUNCTION_FAIL_ERROR);
      return SES_FALSE;
   }

  }

  return return_value;
}

/***************************************************************/
ses_boolean               _set_ready_to_write(struct _ses_output_file* the_output_file) {

        the_output_file->_ready_to_write = SES_TRUE;
	return SES_TRUE;

}
/***************************************************************/

struct _ses_output_file*  _read_into_output_file(ses_file_handle the_handle) {

  /*  given a filehandle for a file, read the file into the struct _ses_output_file structure */

  /*  error check the arguments */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_read_into_output_file: invalid file handle passed to _read_into_output_file\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return (struct _ses_output_file*)NULL;
  }

  /*  gather up the file read parameters from the setup */

  struct _ses_output_file* read_file = (struct _ses_output_file*)NULL;
  long nsig = FILE_LIST[the_handle]->_the_setup->_significant_digits;
  ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;

  /*  construct the output file object */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }
  char the_type = pSFH->_filetype;

  read_file = _construct_output_file(the_type);
  if (read_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_into_output_file: construct output file returned null in _read_into_output_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_output_file*)NULL;
  }
 
  ses_boolean needed_to_open = SES_FALSE;
  ses_string filename = FILE_LIST[the_handle]->_the_handle->_filename;
  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_into_output_file: filename null in _read_into_output_file\n");
#endif
    /*  destroy constructed output file on error */

    _destruct_output_file(read_file);
    free(read_file);
    read_file = (struct _ses_output_file*)NULL;

    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  FILE* pFILE = FILE_LIST[the_handle]->_the_handle->_c_file_handle;

  /*  open the file for reading */

  if (pFILE == (FILE*)NULL) {
    pFILE = fopen(filename, "rb");  /*  Note file not exist returns 0 */
    if (pFILE == (FILE*)NULL) {

      /*  destroy constructed output file on error */
      _destruct_output_file(read_file);
      free(read_file);
      read_file = (struct _ses_output_file*)NULL;
      _set_latest_error(SES_READ_ERROR);
      return (struct _ses_output_file*)NULL;
    }
    needed_to_open = SES_TRUE;
  }


  /*  read the directory */

  if (read_file->_directory_to_write != (struct _ses_directory*)NULL) {

        /*  if read_file exists, clear it out */
	_destruct_ses_directory(read_file->_directory_to_write);
	free(read_file->_directory_to_write);
	read_file->_directory_to_write = (struct _ses_directory*)NULL;
  }
  read_file->_directory_to_write = _read_directory(pSFH);
  if (read_file->_directory_to_write == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_into_output_file:  directory_to_write read NULL\n");
#endif

    /*  destroy constructed output file on error */
    _destruct_output_file(read_file);
    free(read_file);
    read_file = (struct _ses_output_file*)NULL;
    _set_latest_error(SES_READ_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  /*  copy in the directory*/

  if (FILE_LIST[the_handle]->_directory != (struct _ses_directory*)NULL) {
    _destruct_ses_directory(FILE_LIST[the_handle]->_directory);
    free(FILE_LIST[the_handle]->_directory);
    FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;
  }
  FILE_LIST[the_handle]->_directory = _copy_ses_directory(read_file->_directory_to_write);

  long* iadr = _get_iadr_directory(read_file->_directory_to_write);

  /*  read the material files */

  if (read_file->_material_files_to_write != (struct _ses_material_file_list*)NULL) {
	_destruct_material_file_list(read_file->_material_files_to_write);
        free(read_file->_material_files_to_write);
        read_file->_material_files_to_write = (struct _ses_material_file_list*)NULL;

  }
  read_file->_material_files_to_write = _read_material_file_list(the_handle, nsig, do_valid, iadr);
  if (read_file->_material_files_to_write == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_into_output_file: the file list null out of _read_material_file_list in _read_into_output_file\n");
#endif
    /*  destroy constructed output file on error */
    _destruct_output_file(read_file);
    free(read_file);
    read_file = (struct _ses_output_file*)NULL;
    _set_latest_error(SES_READ_ERROR);
    return (struct _ses_output_file*)NULL;
  }

  
  /*  close the file */
  if (needed_to_open == SES_TRUE) {
    int close_return = fclose(pFILE);

    FILE_LIST[the_handle]->_the_handle->_c_file_handle = (FILE*)NULL;
    pFILE = (FILE*)NULL;

    if (close_return != 0) {
#ifdef DEBUG_PRINT
      printf("_read_into_output_file: file close error in _read_into_output_file\n");
#endif
      /*  destroy constructed output file on error */
      _destruct_output_file(read_file);
      free(read_file);
      read_file = (struct _ses_output_file*)NULL;
      _set_latest_error(SES_CLOSE_ERROR);
      return (struct _ses_output_file*)NULL;
    }
  }
  

  return read_file;

}

/***************************************************************/

ses_boolean _ses_write(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid, ses_boolean material_sort, long the_date, long the_version) {

  /*  write the output file to the c file handle */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_write: null output_file*, returning false\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_write: null ses file handle, returning false\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /* internal state error checking */

  if (_isit_ready_output_file(the_file) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_ses_write: Trying to write a file that is not ready to be written, returning false \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_FALSE;
  }


  /*  if sort is on, sort material file list */
  ses_boolean didit_sort = _sort_material_file_list(the_file->_material_files_to_write);
  if (didit_sort == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_ses_write: sort material file list failed -- false\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FALSE;
  }
    
   
  

  /*  set the output directory */

  struct _ses_material_file_list* pMFL = the_file->_material_files_to_write;
  if (pMFL == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_write:  material files to write NULL, returning false\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*----------------------------------------------------------------------------*/

  struct _ses_directory* new_directory = (struct _ses_directory*)NULL;

  new_directory = _construct_ses_directory_from_material_file_list(pMFL, pSFH);
  if (new_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
       printf("_ses_write: directory construction error in _write, reeturning false \n");
#endif
       _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
       return SES_FALSE;

  }

  new_directory->_date = the_date;
  new_directory->_version = the_version;

  /*-------------------------------------------------------------------*/

  ses_boolean didit_set = _set_directory(the_file, new_directory);
  if (didit_set == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_ses_write: set directory failed, returning false\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FALSE;
  }

  the_file->_ready_to_write = SES_TRUE;
 
  /*  object is READY TO WRITE, so WRITE! */

  /*  write directory */

  /*  place file handle at beginning of file */

  FILE* pFILE = pSFH->_c_file_handle;
  /* int fseek_return = 0; */
  /* fseek_return = */ fseek(pFILE, 0, SEEK_SET);


  ses_boolean didit_write_dir = _write_directory(
       the_file->_directory_to_write,
       pSFH);
  if (didit_write_dir == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_ses_write: write directory failed, returning false\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return SES_FALSE;
  }


  /*  write material files */
  ses_boolean didit_write_mfl = _write_material_file_list(the_file->_material_files_to_write, pSFH, nsig, do_valid);
  if (didit_write_mfl == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_ses_write: Write material file list failed\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);
    return SES_FALSE;
  }

  if (pSFH->_filetype == ASCII_TYPE) {
	/* ses_error_flag  the_error_flag =   SES_NO_ERROR; */
	/* the_error_flag = */     _write_eof_ascii(pSFH);

  }	

  return return_value;
}
/**************************************************************************/
//  error checking routines

ses_error_flag check_errors_get_size_of_directory(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH) {

  /*  argument error checking */

  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_size_of_directory: the_file is null in _get_size_of directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  internal error checking */

  if (the_file->_directory_to_write == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_size_of_directory: the directory to write is null in _get_size_of directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_size_of_directory: ses file handle null in _get_size_of_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}

ses_error_flag check_errors_dmeiof(struct _ses_output_file* the_output_file, ses_material_id the_mid) {

  /*  argument error checking */

  if (the_output_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_does_material_exist_in_output_file: null output_file pointer passed into _does_material_exist_\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_does_material_exist_in_output_file: invalid mid\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_INVALID_MID;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_set_directory(struct _ses_output_file* the_file, struct _ses_directory* the_directory) {

  /*  argument error checking */

  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_directory: null output_file pointer passed into _set_directory\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_directory: ses directory passed into _set_directory is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_add_material_file(struct _ses_output_file* the_file, struct _ses_material_file* the_material_file) {


  if (the_file == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_material_file:  passed in output file is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_material_file == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_material_file: passed in material_file is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}



