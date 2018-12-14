

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"

#include <ctype.h> 

#define my_skip_white_space HEADER(my_skip_white_space)

ses_boolean _go_to_data_record_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  this routine puts the read pointer for the file so that the
      next thing to be read is the data record */


  ses_boolean return_value = SES_TRUE;


  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = pSFH->_c_file_handle;


  ses_boolean didit_findit = _find_data_record_tag(pFILE, the_mid, the_tid);
  if (didit_findit == SES_FALSE) {
    return_value = SES_FALSE;
  }
 
  char* the_tag = _skip_tag(pFILE, "<grid");
  free(the_tag);
  the_tag = (char*)NULL;
  the_tag = _skip_tag(pFILE, "/grid");
  free(the_tag);
  the_tag = (char*)NULL;
  //my_skip_white_space(pFILE);

 
  return return_value;
}

void my_skip_white_space(FILE* pFILE) {
	char the_char = _read_char(pFILE);
        while (isspace(the_char) != 0) {
		the_char = _read_char(pFILE);
        }
}

