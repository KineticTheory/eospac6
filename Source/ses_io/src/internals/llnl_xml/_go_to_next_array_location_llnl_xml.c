#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>

#define _go_to_next_array_location_llnl_xml HEADER(_go_to_next_array_location_llnl_xml)

ses_boolean _go_to_next_array_location_llnl_xml(struct _ses_file_handle* pSFH, long location) {

  /*  function prototypes */

   ses_error_flag check_errors_GO_TO_NAL_LLNL_XML(struct _ses_file_handle* pSFH, long location);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_GO_TO_NAL_LLNL_XML(pSFH, location);
   if (return_flag != SES_NO_ERROR) {
	return SES_FALSE;
   }

   ses_boolean return_value = SES_TRUE;

   //  increment the iteration index

   pSFH->_iteration_index++;

   //  get the filename and array address for the address

   char* filename = pSFH->_array_filename;
   long array_address = pSFH->_array_address;

   if ((array_address < 0) || (filename == (char*)NULL)) {
	return SES_FALSE;
   }
   if (filename != (char*)NULL) {

	//  close the current file handle 

	fclose(pSFH->_c_file_handle);

        //  go to the array address on the filename

	pSFH->_c_file_handle = fopen(filename, "r");
	fseek(pSFH->_c_file_handle, array_address, SEEK_SET);
	
   }
   else {
	return_value = SES_FALSE;
   }


   return return_value;
}

ses_error_flag check_errors_GO_TO_NAL_LLNL_XML(struct _ses_file_handle* pSFH, long location) {

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_next_array_location_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_go_to_next_array_location_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 
   return SES_NO_ERROR;
}

