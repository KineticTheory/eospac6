

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


ses_boolean _go_to_index_record_binary(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {


  ses_boolean return_value = SES_TRUE;

  /***************************************************************************************/
  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record_binary: Invalid handle passed to _go_to_index_record =  %d\n", the_handle);
#endif
    return SES_FALSE;
  }

  if (FILE_LIST[the_handle]->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record_binary: ses file handle null in _go_to_index_record \n");
#endif
    return SES_FALSE;
  }

  FILE* pFILE = FILE_LIST[the_handle]->_the_handle->_c_file_handle;
  if (pFILE == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record_binary: pFILE is null in _go_to_index record\n");
#endif
    return SES_FALSE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;

  if (ptDIR == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record_binary: Directory is null in _go_to_index_record \n");
#endif
    return SES_FALSE;
  }
  /***************************************************************************************/

  /*  here, we're good to go */


  long maddress = _get_address_for_material(ptDIR, the_mid, pSFH);
  long file_address = maddress;

  rewind(pFILE);

  int fseek_return = fseek(pFILE, file_address, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record_binary: fseek return in _go_to_index record_binary error \n");
#endif
    return SES_FALSE;
  }

  
  return return_value;
}








