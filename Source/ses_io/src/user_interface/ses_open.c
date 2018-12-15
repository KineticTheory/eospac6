

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>

#ifndef WIN32
#include <sys/time.h>
#include <sys/resource.h>
#endif

//////  defines /////////////////////
//  BIG_END - 1 for testing byte order
//  LITTLE_END - 0 for testing byte order
/////////////////////////////////////

#define FIX_OPTIMIZATION_ERRORS


#define BIG_END 1
#define LITTLE_END 0

#define my_construct_file HEADER(my_construct_file)
#define my_determine_file_type HEADER(my_determine_file_type)
#define my_determine_endianness HEADER(my_determine_endiannness)
#define my_is_small HEADER(my_is_small)
#define my_TestByteOrder HEADER(my_TestByteOrder)

#include "../internals/binary/_file_list_binary.h"


ses_file_handle ses_open(ses_string filename, ses_open_type open_flags) {

  /*  open the named file with the given open_flags and 
      return a ses_file_handle */

  /*  function prototypes */

  ses_file_handle my_construct_file(ses_string filename,
				  ses_open_type open_flags);

  /*  end function prototypes */ 

  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_open: filename null in ses_open\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return 0;
  }

  if (_is_valid_open_flag(open_flags) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_open: open type invalid in ses_open\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return 0;
  }

  /*  construct globals upon first usage 
      -- assumes this will always be the first routine called */

  if (_next_empty_file == 0) {
    _construct_globals(open_flags);
  }

  /*  open the sesame file and get a C file handle */

  ses_file_handle return_value = (ses_file_handle)(0);
  if (_globals_contains(filename) == SES_FALSE) {
	
	// if file not already open

  	return_value = my_construct_file(filename, open_flags);
 
  }
  else {

	//  if file already open
	
	return_value = _get_handle_from_globals(filename);
          
  }

#ifdef FIX_OPTIMIZATION_ERRORS
  if (return_value < 1) {
#ifdef DEBUG_PRINT
        printf("ses_open: table handle error (< 1) in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return 0;
  }
#endif


  return return_value;

}

ses_file_handle my_construct_file(ses_string filename,
                                ses_open_type open_flags) {

  /*  open the named file */
  /*  returns: NULL if file handle invalid */

  /*  function prototypes  */

  ses_file_type my_determine_file_type(ses_string filename, ses_open_type open_flags);
  ses_boolean   my_determine_endianness(FILE* pFILE);
  int           my_TestByteOrder();

  /*  end function prototypes */

  ses_file_handle return_value = (ses_file_handle)0;

  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("my_construct_file: filename null in my_construct_file\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return (ses_file_handle)0;
  }
  if (_is_valid_open_flag(open_flags) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("my_construct_file: open type invalid in ses_open\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return (ses_file_handle)0;
  }

 
  /*  get the file type */

  ses_boolean file_exists = SES_FALSE;
  ses_boolean format_exists = SES_FALSE;

  ses_file_type the_type = 'Q';

  if (open_flags == 'R' || open_flags == 'C' || open_flags == 'A' ) {
     the_type = my_determine_file_type(filename, open_flags);
     if (the_type != 'N') {
      file_exists = SES_TRUE;
      if (the_type == 'Q') {
        if (open_flags != 'A') {
		format_exists = SES_FALSE;
        }
	else {
		format_exists = SES_TRUE;
		the_type = 'B';  /*  appending to non-existent file, open as current format */
	}
      }
     }
     else {
      file_exists = SES_FALSE;
      the_type = _current_format;
     }
  }
  else {
    the_type = _current_format;
  }


  if (the_type == 'Q') {
#ifdef DEBUG_PRINT
    printf("my_construct_file:  Could not ascertain file type \n");
#endif
    return -1;
  }

  FILE* pFILE = NULL;

  /*  open the file */
  
  
  switch(open_flags) {
  case 'R':
  
    pFILE = fopen(filename, "r");  //  Note file not exist returns 0 
    break;
  
  case 'A':

    pFILE = fopen(filename, "r+"); 
    break;

  case 'W':

    if (file_exists == SES_FALSE) {
      pFILE = fopen(filename, "w"); 
    }
    else {
      pFILE = 0;
#ifdef DEBUG_PRINT
      printf("my_construct_file:  trying to open for write an existing file \n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return 0;
    }
    break;

  case 'C':

    pFILE = fopen(filename, "r+");
    break;

  default:
    _set_latest_error(SES_OPEN_ERROR);
    return (ses_file_handle)0;
    break;
  
  } 
  
  
  /*  put the file handle on the file handle list */

  /*  check to see if the C file handle limit has been reached */
#ifndef WIN32

  struct rlimit rlim;
  getrlimit(RLIMIT_NOFILE, &rlim);
  int process_open_files = rlim.rlim_cur;
#else
  int process_open_files = INT_MAX;
#endif
  
  if (pFILE == NULL || _next_empty_file >= process_open_files) {
    if (pFILE == NULL) { 
#ifdef DEBUG_PRINT
      printf("my_construct_file:  FILE %s NOT FOUND\n", filename);
#endif
    }
    else {
#ifdef DEBUG_PRINT
      printf("my_construct_file: C file handle null or max file limit reached in ses_open\n");
#endif
   }

    _set_latest_error(SES_OPEN_ERROR);
    return (ses_file_handle)0;
  }
  else {

    /*  file was successfully opened */

    /*  construct the _ses_file* object with all the necessary information */

    /*  get the endianness */


    ses_boolean isit_little = SES_TRUE;
    if (open_flags == 'R' || open_flags == 'C' || open_flags == 'A') {
      isit_little = my_determine_endianness(pFILE);
    } 
    else {
      isit_little = SES_TRUE;
    }

    int index = _get_next_empty_slot_from_globals();
#ifdef FIX_OPTIMIZATION_ERRORS
    if (index >= 1) {
#endif

     /*  construct the file */

    FILE_LIST[index] = _construct_dynamic_ses_file(pFILE, 
                       open_flags, the_type, isit_little, filename);


    /*  set the file handle information */

    ses_boolean isit_big = my_TestByteOrder();
    if (isit_big == SES_TRUE) {
      FILE_LIST[index]->_the_handle->_machine_is_little_endian = SES_FALSE;
    }
    else {
      FILE_LIST[index]->_the_handle->_machine_is_little_endian = SES_TRUE;
    }

    
    if (isit_little == my_TestByteOrder()) {
       FILE_LIST[index]->_the_handle->_needs_flip = SES_TRUE;
    }
    else {
       FILE_LIST[index]->_the_handle->_needs_flip = SES_FALSE;
    }

    if ((open_flags != 'W') && 
        (open_flags != 'A') && (FILE_LIST[index]->_directory == (struct _ses_directory*)NULL)) {
	FILE_LIST[index]->_directory = _read_directory(FILE_LIST[index]->_the_handle);


    }

    
    /* At this point, the data structures for the particular file
       have been filled correctly

       Adding code to:  setup to 100 table
			ifit did the setup correctly, read table definitions
			add those table definitions to the standard tables
    */

    /*  index is the ses file handle before it's been passed back to the user */

    
    ses_boolean done_correctly = _get_more_table_definitions((ses_file_handle)index);

    if (done_correctly == SES_FALSE) {
       return_value = SES_FALSE;

	
    }
    

    /*  pass the sesame file handle back to the user */
    return_value = (ses_file_handle)index;
#ifdef FIX_OPTIMIZATION_ERRORS
    }
#endif


  }

  
  return return_value;

}

ses_file_type my_determine_file_type(ses_string filename, ses_open_type the_flag) {

  /* determine the file type by opening it and checking it */

  ses_file_type return_value = 'N';
  if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("my_determine_file_type: filename null in ses_open\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return return_value;
  }
  if (_is_valid_open_flag(the_flag) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("my_determine_file_type: open type invalid in ses_open\n");
#endif
    _set_latest_error(SES_OPEN_ERROR);
    return return_value;
  }


  return_value = 'Q';
  FILE* pFILE = (FILE*)NULL;
  pFILE = fopen(filename, "r");  /*  Note file not exist returns 0 */


  if (pFILE == (FILE*)NULL) {
    /*  file does not exist, set the default file type to 'N' */
    return 'N';
  }

  /*  go through the list of registered formats and find the correct format */

  ses_boolean found_format = SES_FALSE;

  //  look at the current format first
  found_format = pt2_isit_my_format(pFILE);

  int i = 0;
  if (found_format == SES_FALSE) {
	for (i = 0; i < _number_registered_formats; i++) {
		// already looked at current format, so ignore it 
		if (_current_format != _registered_formats[i]) {

			_set_current_format(_registered_formats[i]);
			found_format = pt2_isit_my_format(pFILE);
			if (found_format == SES_TRUE) {
				_current_format = _registered_formats[i];
				return_value = _current_format;
				i = _number_registered_formats;
				
			}
		}
	}
  }
  else {
	return_value = _current_format;
  }

  int close_return = 0;
  close_return = fclose(pFILE);

  pFILE = (FILE*)NULL;

  return return_value;
}



ses_boolean my_determine_endianness(FILE* pFILE) {

  /*  determine the byte order of the machine upon which we are running */

  /*  SES_TRUE - little, SES_FALSE - big */
  /*  the first thing on any sesame file should be a small integer number (the number of materials) */

  /*  function prototypes */

  ses_boolean my_is_small(long next);
  int my_TestByteOrder();

  /*  end function protytypes */

  ses_boolean return_value = SES_TRUE;

  int machine_endianness = my_TestByteOrder();
  if (pFILE == 0) {
    /*  file does not exist, set the default file type to the machine type*/

    if (machine_endianness == LITTLE_END) {
      return SES_TRUE;
    }
    else {
      return SES_FALSE;
    }
  }

  ses_boolean needs_flip = SES_FALSE;

  long next = _read_long_pFILE_binary(pFILE, needs_flip);
  if (next == 0) {
    /*  needs a flip */
    rewind(pFILE);
    needs_flip = SES_TRUE;
    next = _read_long_pFILE_binary(pFILE, needs_flip);
  }

  /*  if next is small integer, endianness is same as machine */

  if (machine_endianness == LITTLE_END) {
    if (needs_flip == SES_FALSE) {
      return_value = SES_TRUE;  /*  returning little endian */
    }
    else {
      return_value = SES_FALSE;
    }
  }
  else {   /*  machine is big endian */
    if (needs_flip == SES_FALSE) {
      return_value = SES_FALSE;
    }
    else {
      return_value = SES_TRUE;
    }
  }

  return return_value;
}

ses_boolean my_is_small(long next) {

  /*  determine if the passed in long is 'small' (0 < next <= 10000) */

  ses_boolean return_value = SES_TRUE;

  if ((next >= 10000) || (next < 0)) {
    return_value = SES_FALSE;
  }

  return return_value;

}

int my_TestByteOrder()
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



