

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_boolean _go_to_index_record_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  this routine positions the read pointer to the file so that the next 
      thing to be read is the index record */

  ses_boolean return_value = SES_TRUE;


  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = pSFH->_c_file_handle;


  ses_boolean didit_findit = _find_index_record_tag(pFILE, the_mid);
  if (didit_findit == SES_FALSE) {
    return_value = SES_FALSE;
  }
 
  
  return return_value;
}








