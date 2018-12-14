
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


struct _ses_file*  _construct_dynamic_ses_file(FILE* pFILE, 
                                ses_open_type open_flags, 
                                ses_file_type the_type, 
                                ses_boolean isit_little, 
                                ses_string filename) {

        struct _ses_file* return_value = (struct _ses_file*)NULL;
	return_value = _construct_ses_file(pFILE, 
                                open_flags, 
                                the_type, 
                                isit_little, 
                                filename);

 

        return return_value;
}

      
struct _ses_file  _construct_static_ses_file(FILE* pFILE, 
                                ses_open_type open_flags, 
                                ses_file_type the_type, 
                                ses_boolean isit_little, 
                                ses_string filename) {

        struct _ses_file return_value;

	struct _ses_file* ptSF = _construct_ses_file(pFILE, 
                                open_flags, 
                                the_type, 
                                isit_little, 
                                filename);

        return_value._the_handle = _copy_ses_file_handle(ptSF->_the_handle);
        return_value._the_setup  = _copy_ses_setup(ptSF->_the_setup);
        return_value._directory  = _copy_ses_directory(ptSF->_directory);
        return_value._current_index_record = _copy_index_record(ptSF->_current_index_record);
        return_value._current_data_record  = _copy_ses_data_record(ptSF->_current_data_record);
        return_value._first_time_through_setup = ptSF->_first_time_through_setup;
        return_value.FILE_TO_WRITE = _copy_output_file(ptSF->FILE_TO_WRITE);
        return_value._is_constructed = ptSF->_is_constructed;

        return_value._output_date = ptSF->_output_date;
	return_value._output_date = ptSF->_output_date;

	_destruct_ses_file(ptSF);
        free(ptSF);
        ptSF = (struct _ses_file*)NULL;

        return return_value;
}



struct _ses_file*  _construct_ses_file(FILE* pFILE, 
                                ses_open_type open_flags, 
                                ses_file_type the_type, 
                                ses_boolean isit_little, 
                                ses_string filename) {


  /*  construct a ses file object */

  struct _ses_file* the_library = (struct _ses_file*)NULL;

  /*  error check the arguments */

  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: null c file handle passed into construct ses file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_file*)NULL;
  }

  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: filename null passed into construct ses_file \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_file*)NULL;
  }

  if (_is_valid_open_flag(open_flags) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: invalid open flag passed  into construct ses_file \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (struct _ses_file*)NULL;
    
  }

  if (_is_valid_file_type(the_type) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: invalid file type passed into construct ses_file \n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (struct _ses_file*)NULL;
  }

  /*  allocate memory for the ses file */

  the_library = malloc(sizeof(struct _ses_file)*1);
  if (the_library == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: Memory allocation failed in _construct_ses_file\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_file*)NULL;
  }

  /*  fill in the data structure for the current ses_file_handle */

  the_library->_the_handle = 
     _construct_ses_file_handle(pFILE, open_flags, the_type, isit_little, filename);

  if (the_library->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: NULL ses file handle retrieved in _construct_ses_file_handle in _construct_ses_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  release memory on error */

    free(the_library);
    the_library = (struct _ses_file*)NULL;
   
    return (struct _ses_file*)NULL;
  }

  /*  fill in the data structure for the ses_setup */
  

  the_library->_the_setup = _construct_ses_setup();
  if (the_library->_the_setup == NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file: NULL ses setup retrieved in _construct_ses_setup in _construct_ses_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  release memory on error */

    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_file(the_library);

    return (struct _ses_file*)NULL;
  }

  /*  fill in the data structure for the directory */

  the_library->_directory = (struct _ses_directory*)NULL;

  /*  fill in the data structure for the current index record */

  the_library->_current_index_record = (struct _ses_index_record*)NULL;

  /*  fill in the data structure for the current data record */

  the_library->_current_data_record = (struct _ses_data_record*)NULL;

  /*  fill in the data structure for the output file */

  the_library->FILE_TO_WRITE = (struct _ses_output_file*)NULL;

  the_library->_first_time_through_setup = SES_TRUE;

  /*  if open for write, construct the output file */

  if (open_flags == 'W' || open_flags == 'A') {


   the_library->FILE_TO_WRITE = _construct_output_file(the_type);
   if (the_library->FILE_TO_WRITE == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
      printf("_construct_ses_file: Error constructing output file for write/append in _construct_ses_file\n");
#endif
      _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

      /*  release memory on error */

      ses_boolean didit_destruct = SES_FALSE;
      didit_destruct = _destruct_ses_file(the_library);
      the_library = (struct _ses_file*)NULL;

      return (struct _ses_file*)NULL;
    }
  }

  the_library->_output_date = 0;
  the_library->_output_version = 0;

  the_library->_is_constructed = SES_TRUE;
 
  return the_library;
}


struct _ses_file          _copy_ses_file2(struct _ses_file pSF) {

  /*  copy the ses file */

   struct _ses_file return_value;
 
  /*  copy the data members */
  
  return_value._the_handle = _copy_ses_file_handle(pSF._the_handle);
  return_value._the_setup  = _copy_ses_setup(pSF._the_setup);


  return_value._directory  = _copy_ses_directory(pSF._directory);

  return_value._current_index_record = _copy_index_record(pSF._current_index_record);
  return_value._current_data_record  = _copy_ses_data_record(pSF._current_data_record);
  return_value._first_time_through_setup = pSF._first_time_through_setup;
  return_value.FILE_TO_WRITE = (struct _ses_output_file*)NULL;
  return_value.FILE_TO_WRITE = _copy_output_file(pSF.FILE_TO_WRITE);
  return_value._is_constructed = pSF._is_constructed;
  return_value._output_date = pSF._output_date;
  return_value._output_date = pSF._output_version;
  return return_value;
}


struct _ses_file* _copy_ses_file(struct _ses_file* pSF) {

  /*  copy the ses file */

  struct _ses_file* return_value = (struct _ses_file*)NULL;

  /*  error check the arguments */

  if (pSF == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: NULL ses_file* passes to _copy_ses_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_file*)NULL;
  }

  /*  construct the return value */

  return_value = malloc(sizeof(struct _ses_file)*1);
  if (return_value == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: memory allocation failed in _copy_ses_file\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_file*)NULL;
  }

  /*  copy the data members */
  
  return_value->_the_handle = _copy_ses_file_handle(pSF->_the_handle);
  if (return_value->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: copy ses file handle failed in _copy_ses_file\n");
#endif
    _set_latest_error(SES_OBJECT_COPY_ERROR);

    /*  release memory on error */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_file(return_value);
    free(return_value);
    return_value = (struct _ses_file*)NULL;
 
    return (struct _ses_file*)NULL;
  }

  return_value->_the_setup  = _copy_ses_setup(pSF->_the_setup);
  if (return_value->_the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: copy setup failed in _copy_ses_file\n");
#endif
    _set_latest_error(SES_OBJECT_COPY_ERROR);

    /*  release memory on error */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_file(return_value);
    free(return_value);
    return_value = (struct _ses_file*)NULL;

    return (struct _ses_file*)NULL;
  }

  return_value->_directory  = _copy_ses_directory(pSF->_directory);
  if (return_value->_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: ses directory set to NULL\n");
#endif
  }

  return_value->_current_index_record = _copy_index_record(pSF->_current_index_record);
  if (return_value->_current_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file:  ses index record set to NULL\n");
#endif
  }

  return_value->_current_data_record  = _copy_ses_data_record(pSF->_current_data_record);
  if (return_value->_current_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file:  current_data_record set to NULL\n");
#endif
    _set_latest_error(SES_OBJECT_COPY_ERROR);

  }

  return_value->_first_time_through_setup = pSF->_first_time_through_setup;

  return_value->FILE_TO_WRITE = _copy_output_file(pSF->FILE_TO_WRITE);
  if (return_value->FILE_TO_WRITE == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file: output file set to NULL \n");
#endif
    _set_latest_error(SES_OBJECT_COPY_ERROR);

  }
  return_value->_is_constructed = pSF->_is_constructed;
  return_value->_output_date = pSF->_output_date;
  return_value->_output_date = pSF->_output_version;


  return return_value;
}


ses_boolean _destruct_ses_file(struct _ses_file* the_library) {

  /*  destruct the ses file */

  /*  error check the arguments */

  ses_boolean return_value = SES_TRUE;


  if (the_library == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_ses_file: NULL ses_file* passes to _destruct_ses_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  do the destruction */

  

  the_library->_is_constructed = SES_TRUE;
  if (the_library->_is_constructed == SES_TRUE) {

    ses_boolean didit4 = _destruct_output_file(the_library->FILE_TO_WRITE);
    free(the_library->FILE_TO_WRITE);
    the_library->FILE_TO_WRITE = (struct _ses_output_file*)NULL;
    if (didit4 == SES_FALSE){
#ifdef DEBUG_PRINT
       printf("_destruct_ses_file: destruction error ses_output_file in _destruct_ses_file\n");
#endif
       _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
       return SES_FALSE;
     }

  
  } 



  if (the_library->_is_constructed == SES_TRUE) {

    ses_boolean didit1 = _destruct_ses_setup(the_library->_the_setup);
    free(the_library->_the_setup);
    the_library->_the_setup = (struct _ses_setup*)NULL;
    if (didit1 == SES_FALSE){
#ifdef DEBUG_PRINT
      printf("_destruct_ses_file: destruction error ses_setup in _destruct_ses_file\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
    }
  }


  if ((the_library->_is_constructed == SES_TRUE) && (the_library->_directory != (struct _ses_directory*)NULL)) {
    ses_boolean didit1 = _destruct_ses_directory(the_library->_directory);
    free(the_library->_directory);
    the_library->_directory = (struct _ses_directory*)NULL;
    if (didit1 == SES_FALSE){
#ifdef DEBUG_PRINT
      printf("_destruct_ses_file: destruction error ses_directory in _destruct_ses_file\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
    }


  }

 
  if ((the_library->_is_constructed == SES_TRUE) && (the_library->_the_handle != (struct _ses_file_handle*)NULL)) {

  if (((the_library->_the_handle)->_the_open_mode == 'R')||((the_library->_the_handle)->_the_open_mode == 'C')){
 
     ses_boolean didit3 = _destruct_ses_data_record(the_library->_current_data_record);
     free(the_library->_current_data_record);
     the_library->_current_data_record = (struct _ses_data_record*)NULL;

     if (didit3 == SES_FALSE){
#ifdef DEBUG_PRINT
        printf("_destruct_ses_file: destruction error ses_data_record in _destruct_ses_file\n");
#endif
       _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
       return SES_FALSE;
     }

     ses_boolean didit2 = _destruct_ses_index_record(the_library->_current_index_record);
     free(the_library->_current_index_record);
     the_library->_current_index_record = (struct _ses_index_record*)NULL;
     if (didit2 == SES_FALSE){
#ifdef DEBUG_PRINT
       printf("_destruct_ses_file: destruction error ses_index_record in _destruct_ses_file\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
     }

  }
  else {
	/*  on 'W' and 'A' current data record is on the material file list of the output file, and is destructed there */
        the_library->_current_data_record = (struct _ses_data_record*)NULL;
	ses_boolean didit2 = SES_FALSE;
        if (the_library->_current_index_record != (struct _ses_index_record*)NULL) {
        	didit2 = _destruct_ses_index_record(the_library->_current_index_record);
        	free(the_library->_current_index_record);
        	the_library->_current_index_record = (struct _ses_index_record*)NULL;
	}
	
  }


  if (the_library->_is_constructed == SES_TRUE) {
    ses_boolean didit1 = _destruct_ses_file_handle(the_library->_the_handle);
    free(the_library->_the_handle);
    the_library->_the_handle = (struct _ses_file_handle*)NULL;
    if (didit1 == SES_FALSE){
#ifdef DEBUG_PRINT
      printf("_destruct_ses_file: destruction error ses_file_handle in _destruct_ses_file\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
    }

  }
  }

  

  /*  return */

  the_library->_is_constructed = SES_FALSE;
  the_library->_output_date = 0;
  the_library->_output_version = 0;

  return return_value;
}

struct _ses_directory* _get_directory(struct _ses_file* the_library) {

  /*  return the directory associated with the file */

  /*  error check the arguments */

  struct _ses_directory* return_value = NULL;
  if (the_library == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_directory: NULL pointer passed to _get_directory \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return NULL;
  }

  /*  return */

  return_value = the_library->_directory;

  return return_value;
}

struct _ses_index_record* _get_current_index_record(struct _ses_file* the_library) {

  /*  return the current index record for the file */

  /*  error check the arguments */

  struct _ses_index_record* return_value = NULL;
  if (the_library == (struct _ses_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_directory: NULL pointer passed to _construct_ses_file \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return NULL;
  }

  /*  return */

  return_value = the_library->_current_index_record;
  
  return return_value;
}

struct _ses_data_record* _get_current_data_record(struct _ses_file* the_library) {

  /*  return the current data record for the file */

  /*  error check the arguments */

  struct _ses_data_record*  return_value = NULL;
  if (the_library == NULL) {
#ifdef DEBUG_PRINT
    printf("_get_current_data_record: NULL pointer passed in \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return NULL;
  }

  /*  return */

  return_value = the_library->_current_data_record;

  return return_value;
}


