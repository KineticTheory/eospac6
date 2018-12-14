

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


ses_boolean _go_to_data_record_ascii(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {


  ses_boolean return_value = SES_TRUE;

  /**************************************************************************************/
  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: Invalid handle passed to _go_to_data_record =  %d\n", the_handle);
#endif
    return SES_FALSE;
  }

  if (FILE_LIST[the_handle]->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: ses file handle null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = FILE_LIST[the_handle]->_the_handle->_c_file_handle;
  if (pFILE == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: pFILE is null in _go_to_data record\n");
#endif
    return SES_FALSE;
  }

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  if (ptDIR == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: Directory is null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;

  if (pIR == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: Index record is null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }

  /***********************************************************************************************/

  /*  here, wer're good to go */

  long taddress = _get_address_for_table_ascii(pIR, the_tid, pSFH);
  long file_address = taddress;

  rewind(pFILE);

  int fseek_return = fseek(pFILE, file_address, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_ascii: fseek return in _go_to_data_record error \n");
#endif
    return SES_FALSE;
  }


  //  read the first line -- setting up to the data reord
  char line[128];
  fgets ( line, sizeof line, pSFH->_c_file_handle); 

  return return_value;
}

