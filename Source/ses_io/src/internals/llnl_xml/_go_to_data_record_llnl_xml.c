#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "llnl_tags.h"
#include <string.h>

#define _go_to_data_record_llnl_xml HEADER(_go_to_data_record_llnl_xml)
#define check_errors_GO_TO_DATA_RECORD_LLNL_XML HEADER(check_errors_GO_TO_DATA_RECORD_LLNL_XML)
#define x HEADER(x)
#define x HEADER(x)

ses_boolean _go_to_data_record_llnl_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

   //  open the file that contains the data record and go to the first array in the data record

   ses_boolean return_value = SES_TRUE;

   /*  function prototypes */

   ses_error_flag check_errors_GO_TO_DATA_RECORD_LLNL_XML(ses_file_handle the_handle);
   int my_get_table_index(ses_file_handle the_handle, ses_table_id the_tid);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_GO_TO_DATA_RECORD_LLNL_XML(the_handle);
   if (return_flag != SES_NO_ERROR) {
	return SES_FALSE;
   }

   //  get the table index 

   struct _ses_index_record* ptIR = FILE_LIST[the_handle]->_current_index_record;
   int table_index = 0;
   table_index = _get_table_index(ptIR, the_tid);

   //  get the file name and address for the first array, then go there (first array is start of data record)
   if (ptIR->_array_filename != (char***)NULL) {

   	char* filename = ptIR->_array_filename[table_index][0];
   	long iadr =      ptIR->_array_iadr[table_index][0];

	//  set the filename and array onto the ses file handle
	
   	FILE_LIST[the_handle]->_the_handle->_array_filename = filename;
	FILE_LIST[the_handle]->_the_handle->_array_address = iadr;

	//  open the file for the array

	fclose(FILE_LIST[the_handle]->_the_handle->_c_file_handle);
   	FILE_LIST[the_handle]->_the_handle->_c_file_handle = fopen(filename, "r");

   	//  go to the first array in the list

        if (iadr >= 0) {
   		fseek(FILE_LIST[the_handle]->_the_handle->_c_file_handle, iadr, SEEK_SET);
	}

	//  initialize the iteration index 

   	FILE_LIST[the_handle]->_the_handle->_iteration_index = 0;
   }

   return return_value;
}

ses_error_flag check_errors_GO_TO_DATA_RECORD_LLNL_XML(ses_file_handle the_handle) {
 
   struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_data_record_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

  FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_data_record_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 

   if (FILE_LIST[the_handle]->_current_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_data_record_llnl_xml: current_index_record null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }


   return SES_NO_ERROR;
}






