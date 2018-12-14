
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define my_get_data_as_1d HEADER(my_get_data_as_1d)
#define my_ses_close HEADER(my_ses_close)



ses_error_flag ses_close(ses_file_handle the_handle) {

  /*  close the file */

  /*  function prototypes */

  ses_error_flag my_ses_close(ses_file_handle the_handle);

  /*  end function prototypes */

  ses_error_flag return_value = SES_NO_ERROR;

  /*  error check the arguments */
  
  if (ses_is_valid(the_handle) == SES_FALSE) {

#ifdef DEBUG_PRINT
    printf("ses_close: File handle %d invalid in ses_close\n", the_handle);
#endif

    if (the_handle == _next_empty_file - 1) {
         ses_exit();
    }


    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  } 
  else {

    return_value = my_ses_close(the_handle);

    if (return_value != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
      printf("ses_close: close error \n");
#endif
      _set_latest_error(SES_CLOSE_ERROR);
      return return_value;
    }
    
  }

  return return_value;

}

ses_error_flag my_ses_close(ses_file_handle the_handle) {

  /*  close the current file handle and return a sesame error flag */
  /*  if the file is open for writing, write the file */

  /*  function prototypes */

  ses_word_reference my_get_data_as_1d(struct _ses_data_record* pDR);

  /*  end function prototypes */
     
  ses_error_flag return_value = SES_NO_ERROR;
  ses_open_type open_flags;
  FILE* pFILE = (FILE*)NULL;
  ses_material_id the_mid;

  int close_return = 0;
  if (ses_is_valid(the_handle) == SES_TRUE) {

    struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;
    pSFH = FILE_LIST[the_handle]->_the_handle;
    long nsig = (long)(FILE_LIST[the_handle]->_the_setup->_significant_digits);
    ses_boolean do_valid = FILE_LIST[the_handle]->_the_setup->_do_validation;
    pFILE      = FILE_LIST[the_handle]->_the_handle->_c_file_handle;
    open_flags = FILE_LIST[the_handle]->_the_handle->_the_open_mode;

    struct _ses_material_file* pMF = (struct _ses_material_file*)NULL;
    int didit_write = 0;
    ses_boolean material_sort = FILE_LIST[the_handle]->_the_setup->_order_materials;


    switch(open_flags) {

    case('A'):
    case('W'):

      /*  if the current data record has stuff in it, move it to the material_file */

      if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("my_ses_close:  for W:  setup not complete \n");
#endif
        _releasePFILE(FILE_LIST[the_handle]->_the_handle);
        _set_latest_error(SES_CLOSE_ERROR);
         if (the_handle == _next_empty_file - 1) {
              ses_exit();
         }
         return SES_CLOSE_ERROR;
      }

      the_mid = FILE_LIST[the_handle]->_the_setup->_mid;

      if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
	printf("my_ses_close:  for W:  invalid mid = %ld\n", the_mid);
#endif
        _releasePFILE(FILE_LIST[the_handle]->_the_handle);
        _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
         if (the_handle == _next_empty_file - 1) {
              ses_exit();
         }
        return SES_OBJECT_OUT_OF_RANGE;
      }

      if (FILE_LIST[the_handle]->_current_data_record == (struct _ses_data_record*)NULL) {
      } 
      else {

        if (FILE_LIST[the_handle]->_current_data_record->_has_data == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("my_ses_close: for W: _has_data is FALSE, returning\n");
#endif
          _releasePFILE(FILE_LIST[the_handle]->_the_handle);
          _set_latest_error(SES_NO_DATA_ERROR);

           ses_boolean destructo = SES_FALSE;
           destructo = _destruct_ses_data_record(FILE_LIST[the_handle]->_current_data_record);
           free(FILE_LIST[the_handle]->_current_data_record);
           FILE_LIST[the_handle]->_current_data_record = (struct _ses_data_record*)NULL;

           if (the_handle == _next_empty_file - 1) {
              ses_exit();
           }
          return SES_NO_ERROR;
        }
        else {

          /*  construct the output file on first access of output file */


          if (FILE_LIST[the_handle]->FILE_TO_WRITE == (struct _ses_output_file*)NULL) {
	    char the_type = pSFH->_filetype;
            FILE_LIST[the_handle]->FILE_TO_WRITE = _construct_output_file(the_type);
          }

          
          pMF = _construct_ses_material_file(the_mid);
          if (pMF == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
            printf("my_ses_close: for W: malloc error with material file -- exiting\n");
#endif
            _releasePFILE(FILE_LIST[the_handle]->_the_handle);
            if (the_handle == _next_empty_file - 1) {
                 ses_exit();
            }
            return SES_WRITE_ERROR;
          }


          ses_boolean didit_add = _add_table(pMF, FILE_LIST[the_handle]->_current_data_record);
          if (didit_add == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("my_ses_close: for W: add table error in ses_close_binary\n");
#endif
 
           _destruct_material_file(pMF);
           free(pMF);
	   pMF = (struct _ses_material_file*)NULL;

           _releasePFILE(FILE_LIST[the_handle]->_the_handle);
           if (the_handle == _next_empty_file - 1) {
              ses_exit();
           }
            return SES_WRITE_ERROR;
	  }


          /*  move the material file to the output file */


          ses_boolean didit_move = _add_material_file(FILE_LIST[the_handle]->FILE_TO_WRITE, pMF);
          if (didit_move == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("my_ses_close: for W: add material file error\n");
#endif
           _destruct_material_file(pMF);
           free(pMF);
	   pMF = (struct _ses_material_file*)NULL;

           _releasePFILE(FILE_LIST[the_handle]->_the_handle);
            if (the_handle == _next_empty_file - 1) {
              ses_exit();
            }
	    return SES_WRITE_ERROR;
	  }
          else {
	
		/*  move was successful -- clean up pMF */

		/*  pMF was added as a "Pointed to" object, do not destruct */
                
		pMF = (struct _ses_material_file*)NULL;
                

          }

          ses_boolean didit_set = SES_FALSE;
          didit_set = _set_ready_to_write(FILE_LIST[the_handle]->FILE_TO_WRITE);

        }

      }

      if (_isit_ready_output_file(FILE_LIST[the_handle]->FILE_TO_WRITE)) {

        /*  if the date or version has been changed, change them in the directory */

	if (FILE_LIST[the_handle]->_the_setup->_date_changed == SES_TRUE) {
	  FILE_LIST[the_handle]->FILE_TO_WRITE->_directory_to_write->_date = FILE_LIST[the_handle]->_the_setup->_date;
	}
	if (FILE_LIST[the_handle]->_the_setup->_version_changed == SES_TRUE) {
	  FILE_LIST[the_handle]->FILE_TO_WRITE->_directory_to_write->_version = FILE_LIST[the_handle]->_the_setup->_version;
	}

        /*  write the file */

        pFILE = _getPFILE(pSFH);

	long the_date = FILE_LIST[the_handle]->_output_date;
	long the_version = FILE_LIST[the_handle]->_output_version;
	didit_write = _ses_write(FILE_LIST[the_handle]->FILE_TO_WRITE, pSFH, nsig, do_valid, material_sort, the_date, the_version);         
        if (didit_write == SES_TRUE) {

          _releasePFILE(pSFH);
          close_return = 0;

          if (close_return != 0) {
#ifdef DEBUG_PRINT
            printf("my_ses_close: for W: Close return error -- return is %ld\n", (long)close_return);
#endif
 	    return_value = SES_CLOSE_ERROR;
	  }


        }	
        else {
#ifdef DEBUG_PRINT
          printf("my_ses_close: for W: File write unsuccessful from _write\n");
#endif
           return_value = SES_FILE_WRITE_ERROR;
           _destruct_material_file(pMF);
           free(pMF);
	   pMF = (struct _ses_material_file*)NULL;
	}
      }
      else {
          /*  trying to write the output file when not ready */
#ifdef DEBUG_PRINT
          printf("my_ses_close: for W: trying to write output file when not ready\n");
#endif
          return_value = SES_FILE_READY_ERROR;
      }

      
      break;
    case('R'):

        if (pFILE != 0) {
		close_return = fclose(pFILE);
        	pSFH->_c_file_handle = (FILE*)NULL;
        	pFILE = (FILE*)NULL;
	}

        if (close_return != 0) {
	  return_value = SES_CLOSE_ERROR;
	}
        break;

    case('C'):

	if (pFILE != 0) {
		close_return = fclose(pFILE);
        	pSFH->_c_file_handle = (FILE*)NULL;
		pFILE = (FILE*)NULL;

	}
        if (close_return != 0) {
	  return_value = SES_CLOSE_ERROR;
	}
        break;


    default:
#ifdef DEBUG_PRINT
        printf("my_ses_close: type default not an option\n");
#endif
        return_value = SES_CLOSE_ERROR;
        return return_value;
        break;
    }

  }
  else {

    /*  file handle invalid */

    return_value = SES_INVALID_FILE_HANDLE;
  }


  _releasePFILE(FILE_LIST[the_handle]->_the_handle);

  if (the_handle == _next_empty_file - 1) {
    ses_exit();
  }

  return return_value;
}

ses_word_reference my_get_data_as_1d(struct _ses_data_record* pDR) {

  ses_word_reference return_value = (ses_word_reference)NULL;


  struct _ses_iterator* pIT = pDR->_the_iterator;


  long  num_arrays = _get_number_arrays(pIT);
  long* sizes = _get_array_sizes(pIT);

  int i=0;
  long total_size = 0;
  for (i=0; i < num_arrays; i++) {

    total_size = total_size + sizes[i];
  }


  return_value = malloc(sizeof(ses_word)*total_size);


  int index = 0;
  int j = 0;
  ses_word_reference tmp;
  
  for (i=0; i < num_arrays; i++) {

    if (pDR->_the_data[i] == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("my_get_data_as_1d:  data at index %d is null \n", i);
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return (ses_word_reference)NULL;
    }

    for (j=0; j < sizes[i]; j++) {
      tmp = pDR->_the_data[i];

      return_value[index] = tmp[j];
      index++;
    }
    index = 0;
  }




  return return_value;


}
