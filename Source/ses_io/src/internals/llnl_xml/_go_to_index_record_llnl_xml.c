

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "llnl_tags.h"

#define check_errors_GO_TO_INDEX_RECORD_LLNL_XML HEADER(check_errors_GO_TO_INDEX_RECORD_LLNL_XML)


ses_boolean _go_to_index_record_llnl_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

   /*  function prototypes */

   ses_error_flag check_errors_GO_TO_INDEX_RECORD_LLNL_XML(ses_file_handle the_handle);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_GO_TO_INDEX_RECORD_LLNL_XML(the_handle);
   if (return_flag != SES_NO_ERROR) {
	return SES_FALSE;
   }


  ses_boolean return_value = SES_TRUE;

  //  get the material_path file from the directory and put it on the file handle

  int index = _get_material_index(FILE_LIST[the_handle]->_directory, the_mid);
  if (index >= 0) {
    char* material_path = FILE_LIST[the_handle]->_directory->_material_path[index];

    if (material_path != (char*)NULL) {
        if (FILE_LIST[the_handle]->_the_handle->_material_filename != (char*)NULL) {
		free(FILE_LIST[the_handle]->_the_handle->_material_filename);
		FILE_LIST[the_handle]->_the_handle->_material_filename = (char*)NULL;
	}
  	FILE_LIST[the_handle]->_the_handle->_material_filename = malloc(sizeof(char)*(strlen(material_path)+1));
  	strcpy(FILE_LIST[the_handle]->_the_handle->_material_filename , material_path);
    }
  }

  return return_value;
}

ses_error_flag check_errors_GO_TO_INDEX_RECORD_LLNL_XML(ses_file_handle the_handle) {

   if (FILE_LIST[the_handle]->_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_index_record_llnl_xml: directory null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   if (FILE_LIST[the_handle]->_directory->_material_path == (char**)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_index_record_llnl_xml: _material_path NULL\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_index_record_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }


  return SES_NO_ERROR;
}








