

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#undef DEBUG_GO_TO_DATA_RECORD_BINARY

ses_boolean _go_to_data_record_binary(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {


  ses_boolean return_value = SES_TRUE;
#ifdef DEBUG_GO_TO_DATA_RECORD_BINARY
  printf("_go_to_data_record_binary:  entered\n");
#endif
  /***********************************************************************************************/

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: Invalid handle passed to _go_to_data_record =  %d\n", the_handle);
#endif
    return SES_FALSE;
  }

  if (FILE_LIST[the_handle]->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: ses file handle null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = FILE_LIST[the_handle]->_the_handle->_c_file_handle;
  if (pFILE == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: pFILE is null in _go_to_data record\n");
#endif
    return SES_FALSE;
  }

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  if (ptDIR == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: Directory is null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }

  struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;

  if (pIR == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: Index record is null in _go_to_data_record \n");
#endif
    return SES_FALSE;
  }
  /***********************************************************************************************/

  /*  here, wer're good to go */

  long maddress = _get_address_for_material(ptDIR, the_mid, pSFH);
  long taddress = _get_address_for_table(pIR, the_tid, pSFH);
  long file_address = maddress + taddress;

#ifdef DEBUG_GO_TO_DATA_RECORD_BINARY
  printf("_go_to_data_record_binary:  maddress is %d\n", maddress);
  printf("_go_to_data_record_binary:  taddress is %d\n", taddress);
  printf("_go_to_data_record_binary:  file_address is %d\n", file_address);
#endif

  rewind(pFILE);

  int fseek_return = fseek(pFILE, file_address, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record_binary: fseek return in _go_to_data_record error \n");
#endif
    return SES_FALSE;
  }


  
  return return_value;
}

