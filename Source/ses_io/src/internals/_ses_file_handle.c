
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#define BIG_END 1
#define LITTLE_END 0

#define my_sfh_TestByteOrder HEADER(my_sfh_TestByteOrder)


//////////////////DEFINES///////////////////////
//USE_LIBXML - turned on in libxml2.a available, used with llnl xml format
////////////////////////////////////////////////
//////  defines /////////////////////
//  BIG_END - 1 for testing byte order
//  LITTLE_END - 0 for testing byte order
/////////////////////////////////////

#include "xml_utilities.h"

struct _ses_file_handle*  _construct_ses_file_handle(FILE* the_c_file_handle, 
	ses_open_type the_open_mode, ses_file_type the_type, ses_boolean is_little, ses_string filename) {

  /*  construct a ses file handle */

  /*  function prototypes */

  int my_sfh_TestByteOrder();

  /*  end function prototypes */


  /*  error check the arguments */

  if (the_c_file_handle == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file_handle: null c file handle passed into _construct _ses_file_handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_file_handle*)NULL;
  }

  if (_is_valid_open_flag(the_open_mode) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file_handle: invalid open type passed to _construct_ses_file_handle\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE); 
    return (struct _ses_file_handle*)NULL;
  }

  if (_is_valid_file_type(the_type) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file_handle: invalid file type  passed to _construct_ses_file_handle\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE); 
    return (struct _ses_file_handle*)NULL;
  }

  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file_handle: null filename passed to  _construct_ses_file_handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR); 
    return (struct _ses_file_handle*)NULL;
  }

  struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;

  /*  memory allocation for return */

  pSFH = malloc(sizeof(struct _ses_file_handle)*1);
  if (pSFH == NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_file_handle: memory allocation error in _construct_ses_file_handle\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return NULL;
  }

  pSFH->_c_file_handle = the_c_file_handle;
  pSFH->_is_valid = SES_TRUE;
  pSFH->_the_open_mode = the_open_mode;
  pSFH->_current_location = 0;
  pSFH->_is_a_copy = SES_FALSE;
  pSFH->_start_index = 0;
  pSFH->_word_size = 22;

  if (filename != (ses_string)NULL) {
      
	if (strlen(filename) > 0) {

  		pSFH->_filename = malloc(sizeof(char)*SES_MAX_STRING_SIZE);
  		strcpy(pSFH->_filename , filename);
	}
	else {
		pSFH->_filename = (ses_string)NULL;
	}
  }
  else {
	pSFH->_filename = (ses_string)NULL;
  }

  pSFH->_filetype = the_type;
  pSFH->_is_little_endian = is_little;

  int byte_order = my_sfh_TestByteOrder();
  if (byte_order == 0) {
    pSFH->_machine_is_little_endian = SES_TRUE;
  }
  else {
    pSFH->_machine_is_little_endian = SES_FALSE;
  }

  /*  hook up the function pointers based on the type */

  pSFH->_filetype = 'Q';

  pSFH->pt2_read_directory = NULL;
  if (the_type == 'B') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_char = &_read_char_binary;
    pSFH->pt2_read_directory = &_read_directory_binary;
    pSFH->pt2_read_index_record = &_read_index_record_binary;
    pSFH->pt2_read_array = &_read_array_binary;
    pSFH->pt2_read_data_record = &_read_data_record_binary;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_binary;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_binary;
    pSFH->pt2_get_grid = &_get_grid_binary;
    pSFH->pt2_write_directory = &_write_directory_binary;
    pSFH->pt2_write_index_record = &_write_index_record_binary;
    pSFH->pt2_write_array = &_write_array_binary;
    pSFH->pt2_write_data_record = &_write_data_record_binary;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_binary;
    pSFH->pt2_read_long = &_read_long_binary;
    pSFH->pt2_write_long = &_write_long_binary;
    pSFH->pt2_read_double = &_read_double_binary;
    pSFH->pt2_write_double = &_write_double_binary;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_binary;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_binary;
    pSFH->pt2_get_directory_size = &_get_directory_size_binary;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_binary;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_binary;


  }
  if (the_type == 'A') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_char = &_read_char_ascii;
    pSFH->pt2_read_directory = &_read_directory_ascii;
    pSFH->pt2_read_index_record = &_read_index_record_ascii;
    pSFH->pt2_read_array = &_read_array_ascii;
    pSFH->pt2_read_data_record = &_read_data_record_ascii;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_ascii;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_ascii;
    pSFH->pt2_get_grid = &_get_grid_ascii;
    pSFH->pt2_write_directory = &_write_directory_ascii;
    pSFH->pt2_write_index_record = &_write_index_record_ascii;
    pSFH->pt2_write_array = &_write_array_ascii;
    pSFH->pt2_write_data_record = &_write_data_record_ascii;
    pSFH->pt2_read_long = &_read_long_ascii;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_ascii;
    pSFH->pt2_write_long = &_write_long_ascii;
    pSFH->pt2_read_double = &_read_double_ascii;
    pSFH->pt2_write_double = &_write_double_ascii;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_ascii;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_ascii;
    pSFH->pt2_get_directory_size = &_get_directory_size_ascii;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_ascii;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_ascii;
  }
  if (the_type == 'X') {
   pSFH->_filetype = the_type;
    pSFH->pt2_read_char = &_read_char_pFILE_xml;
    pSFH->pt2_read_directory = &_read_directory_xml;
    pSFH->pt2_read_index_record = &_read_index_record_xml;
    pSFH->pt2_read_array = &_read_array_xml;
    pSFH->pt2_read_data_record = &_read_data_record_xml;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_xml;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_xml;
    pSFH->pt2_get_grid = &_get_grid_xml;
    pSFH->pt2_write_directory = &_write_directory_xml;
    pSFH->pt2_write_index_record = &_write_index_record_xml;
    pSFH->pt2_write_array = &_write_array_xml;
    pSFH->pt2_write_data_record = &_write_data_record_xml;
    pSFH->pt2_read_long = &_read_long_xml;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_xml;
    pSFH->pt2_write_long = &_write_long_xml;
    pSFH->pt2_read_double = &_read_double_xml;
    pSFH->pt2_write_double = &_write_double_xml;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_xml;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_xml;
    pSFH->pt2_get_directory_size = &_get_directory_size_xml;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_xml;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_xml;
  }
#ifdef USE_LIBXML2
  if (the_type == 'L') {
    pSFH->_filetype = the_type;
 
    pSFH->pt2_read_directory = &_read_directory_llnl_xml;
    pSFH->pt2_read_index_record = &_read_index_record_llnl_xml;
    pSFH->pt2_read_array = &_read_array_llnl_xml;
    pSFH->pt2_read_data_record = &_read_data_record_llnl_xml;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_llnl_xml;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_llnl_xml;
    pSFH->pt2_get_grid = &_get_grid_llnl_xml;
    pSFH->pt2_write_directory = &_write_directory_llnl_xml;
    pSFH->pt2_write_index_record = &_write_index_record_llnl_xml;
    pSFH->pt2_write_array = &_write_array_llnl_xml;
    pSFH->pt2_write_data_record = &_write_data_record_llnl_xml;
    pSFH->pt2_read_long = &_read_long_llnl_xml;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_llnl_xml;
    pSFH->pt2_write_long = &_write_long_llnl_xml;
    pSFH->pt2_get_directory_size = &_get_directory_size_llnl_xml;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_llnl_xml;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_llnl_xml;
  }
#endif
  /*  return */
  pSFH->_material_filename = (char*)NULL;
  return pSFH;
}

void _set_format_type(struct _ses_file_handle* pSFH, ses_file_type the_type) {

  pSFH->_filetype = 'Q';

  if (the_type == 'B') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_directory = &_read_directory_binary;
    pSFH->pt2_read_index_record = &_read_index_record_binary;
    pSFH->pt2_read_array = &_read_array_binary;
    pSFH->pt2_read_data_record = &_read_data_record_binary;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_binary;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_binary;
    pSFH->pt2_get_grid = &_get_grid_binary;
    pSFH->pt2_write_directory = &_write_directory_binary;
    pSFH->pt2_write_index_record = &_write_index_record_binary;
    pSFH->pt2_write_array = &_write_array_binary;
    pSFH->pt2_write_data_record = &_write_data_record_binary;
    pSFH->pt2_read_long = &_read_long_binary;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_binary;
    pSFH->pt2_write_long = &_write_long_binary;
    pSFH->pt2_read_double = &_read_double_binary;
    pSFH->pt2_write_double = &_write_double_binary;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_binary;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_binary;
    pSFH->pt2_get_directory_size = &_get_directory_size_binary;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_binary;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_binary;
  }
  if (the_type == 'A') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_directory = &_read_directory_ascii;
    pSFH->pt2_read_index_record = &_read_index_record_ascii;
    pSFH->pt2_read_array = &_read_array_ascii;
    pSFH->pt2_read_data_record = &_read_data_record_ascii;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_ascii;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_ascii;
    pSFH->pt2_get_grid = &_get_grid_ascii;
    pSFH->pt2_write_directory = &_write_directory_ascii;
    pSFH->pt2_write_index_record = &_write_index_record_ascii;
    pSFH->pt2_write_array = &_write_array_ascii;
    pSFH->pt2_write_data_record = &_write_data_record_ascii;
    pSFH->pt2_read_long = &_read_long_ascii;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_ascii;
    pSFH->pt2_write_long = &_write_long_ascii;
    pSFH->pt2_read_double = &_read_double_ascii;
    pSFH->pt2_write_double = &_write_double_ascii;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_ascii;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_ascii;
    pSFH->pt2_get_directory_size = &_get_directory_size_ascii;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_ascii;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_ascii;
  }
  if (the_type == 'X') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_directory = &_read_directory_xml;
    pSFH->pt2_read_index_record = &_read_index_record_xml;
    pSFH->pt2_read_array = &_read_array_xml;
    pSFH->pt2_read_data_record = &_read_data_record_xml;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_xml;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_xml;
    pSFH->pt2_get_grid = &_get_grid_xml;
    pSFH->pt2_write_directory = &_write_directory_xml;
    pSFH->pt2_write_index_record = &_write_index_record_xml;
    pSFH->pt2_write_array = &_write_array_xml;
    pSFH->pt2_write_data_record = &_write_data_record_xml;
    pSFH->pt2_read_long = &_read_long_xml;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_xml;
    pSFH->pt2_write_long = &_write_long_xml;
    pSFH->pt2_read_double = &_read_double_xml;
    pSFH->pt2_write_double = &_write_double_xml;
    pSFH->pt2_read_ses_word_array = &_read_ses_word_array_xml;
    pSFH->pt2_write_ses_word_array = &_write_ses_word_array_xml;
     pSFH->pt2_get_directory_size = &_get_directory_size_xml;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_xml;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_xml;
  }
#ifdef USE_LIBXML2
  if (the_type == 'L') {
    pSFH->_filetype = the_type;
    pSFH->pt2_read_directory = &_read_directory_llnl_xml;
    pSFH->pt2_read_index_record = &_read_index_record_llnl_xml;
    pSFH->pt2_read_array = &_read_array_llnl_xml;
    pSFH->pt2_read_data_record = &_read_data_record_llnl_xml;
    pSFH->pt2_go_to_index_record = &_go_to_index_record_llnl_xml;
    pSFH->pt2_go_to_data_record = &_go_to_data_record_llnl_xml;
    pSFH->pt2_get_grid = &_get_grid_llnl_xml;
    pSFH->pt2_write_directory = &_write_directory_llnl_xml;
    pSFH->pt2_write_index_record = &_write_index_record_llnl_xml;
    pSFH->pt2_write_array = &_write_array_llnl_xml;
    pSFH->pt2_write_data_record = &_write_data_record_llnl_xml;
    pSFH->pt2_read_long = &_read_long_llnl_xml;
    pSFH->pt2_go_to_next_array_location = &_go_to_next_array_location_llnl_xml;
    pSFH->pt2_write_long = &_write_long_llnl_xml;
    //pSFH->pt2_read_double = &_read_double_llnl_xml;
    //pSFH->pt2_write_double = &_write_double_llnl_xml;
    //pSFH->pt2_read_ses_word_array = &_read_ses_word_array_llnl_xml;
    //pSFH->pt2_write_ses_word_array = &_write_ses_word_array_llnl_xml;
    pSFH->pt2_get_directory_size = &_get_directory_size_llnl_xml;
    pSFH->pt2_get_address_for_material = &_get_address_for_material_llnl_xml;
    pSFH->pt2_get_address_for_table = &_get_address_for_table_llnl_xml;
  }
#endif

}

struct _ses_file_handle*  _copy_ses_file_handle(struct _ses_file_handle* pSFH) {

  /*  copy the ses file handle */

  /*  error check the arguments */

  struct _ses_file_handle* return_value = (struct _ses_file_handle*)NULL;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file_handle: passed in null _ses_file_handle to _copy_ses_file_handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_file_handle*)NULL;
  }

  /*  construct the return value */

  return_value = _construct_ses_file_handle(pSFH->_c_file_handle, 
	pSFH->_the_open_mode, pSFH->_filetype, pSFH->_is_little_endian, pSFH->_filename);
  return_value->_current_location = pSFH->_current_location;

  return_value->_is_a_copy = SES_TRUE;
  return_value->_c_file_handle = (FILE*)NULL;
  return_value->_start_index = pSFH->_start_index;
  return_value->_word_size = pSFH->_word_size;

  if (return_value == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_file_handle: construct ses file handle failed in _copy_ses_file_handle\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  release memory on error exit */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_file_handle(return_value);

    return (struct _ses_file_handle*)NULL;
  }

  return return_value;
}


ses_boolean _destruct_ses_file_handle(struct _ses_file_handle* the_handle) {

  /*  destruct the ses file handle */

  ses_boolean return_value = SES_TRUE;

  /*  argument error checking */

  if (the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_ses_file_handle: passed in null _ses_file_handle to _destruct_ses_file_handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_TRUE;
  }

  /*  free and set pointers to NULL */

  if (the_handle->_is_a_copy == SES_FALSE) {
  	 if (the_handle->_c_file_handle != (FILE*)NULL) {
		fclose(the_handle->_c_file_handle);
		the_handle->_c_file_handle = (FILE*)NULL;
		the_handle->_is_valid = SES_FALSE;
	  }
	  the_handle->_c_file_handle = (FILE*)NULL;
  }
  else {
  } 
  the_handle->_the_open_mode = '0';

  if (the_handle->_filename != (ses_string)NULL) {
	free(the_handle->_filename);
  }
 
  the_handle->_filename = 0;
  the_handle->_filetype = '0';
  the_handle->_is_little_endian = SES_TRUE;

  free(the_handle->_material_filename);
  the_handle->_material_filename = (char*)NULL;


  /*  return */

  return return_value;
}

FILE* _get_c_file_handle(struct _ses_file_handle* the_handle) {

  /*  return the C file handle from the ses_file_handle */
  
  FILE* return_value = (FILE*)NULL;

  /*  error check the arguments */

  if (the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_c_file_handle: passed in null _ses_file_handle to _get_c_file_handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return NULL;
  }

  /*  return */


  return_value =  the_handle->_c_file_handle;

  return return_value;

}

ses_open_type _get_open_mode(struct _ses_file_handle* the_handle) {

  /*  get the open type of the file handle */

  ses_open_type return_value = '0';

  /*  error check the arguments */

  if (the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_open_mode: passed in null _ses_file_handle to _get_open_mode\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return '0';
  }

  /*  return */

  return_value = the_handle->_the_open_mode;
 
  return return_value;
}

ses_string  _get_filename(struct _ses_file_handle* the_handle) {

  /* return the filename from the ses file handle */

  ses_string return_value = (ses_string)NULL;

  /*  argument error checking */

  if (the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_filename: passed in null _ses_file_handle to _get_filename\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return NULL;
  }

  /*  return */

  return_value = the_handle->_filename;
 
  return return_value;

}

FILE* _getPFILE(struct _ses_file_handle* the_handle) {


       FILE* pFILE = (FILE*)NULL;


        if (the_handle->_c_file_handle == (FILE*)NULL) {
	  
	  ses_string filename = the_handle->_filename;
	  switch(the_handle->_the_open_mode) {
		  case 'R':
  
		  	pFILE = fopen(filename, "r");  /*  Note file not exist returns 0 */
   		  	break;
  
  		  case 'A':

    		  	pFILE = fopen(filename, "r+"); 
    		  	break;

  		case 'W':

			pFILE = fopen(filename, "w"); 
       			break;

  		case 'C':

    			pFILE = fopen(filename, "r+");
    			break;

  
 	 } 
	 the_handle->_c_file_handle = pFILE;
         the_handle->_is_valid = SES_TRUE;

         /*  seek to current location */
	 if (pFILE != 0) {
	 	int fseek_return = 0;
		fseek_return = fseek(the_handle->_c_file_handle, the_handle->_current_location, SEEK_SET);
	 }

   
		
	}
	return the_handle->_c_file_handle;

}

void _releasePFILE(struct _ses_file_handle* the_handle) {



	if (the_handle != (struct _ses_file_handle*)NULL) {
        	if (the_handle->_c_file_handle != (FILE*)NULL) {

			
		       int the_value = ftell(the_handle->_c_file_handle);
		       the_handle->_current_location = the_value;

			int creturn = 0;

	 		creturn = fclose(the_handle->_c_file_handle);
			the_handle->_c_file_handle = (FILE*)NULL;
			the_handle->_is_valid = SES_FALSE;
		}
	}


}

int my_sfh_TestByteOrder()
{

      /*  test the byte order - return endianness */

      short int word = 0x0001;
      char* byte = (char *) &word;
      if (byte[0] == 1) {
        return LITTLE_END;
      }
      else {
        return BIG_END;
      }
}





