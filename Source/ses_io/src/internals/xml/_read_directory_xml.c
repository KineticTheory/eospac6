


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_error_flag _read_directory_xml(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {

  ses_error_flag return_value = SES_NO_ERROR;

  FILE* pFILE = pSFH->_c_file_handle;

  rewind(pFILE);

  /*  position the file to the start of the directory tag */

  /*  PUT THIS HERE */

  /*  read the nmats tag */
  /*  read the dates tag */
  /*  read the version tag */


  the_directory->_nfiles = _read_long_tag(pFILE, "nmats");
  long nfiles = the_directory->_nfiles;
  the_directory->_date = _read_long_tag(pFILE, "date");
  the_directory->_version = _read_long_tag(pFILE, "version");
  
  /*  read arrays */

  the_directory->_matid = _read_long_list_tag(pFILE, "matid", nfiles);

  the_directory->_nwds = _read_long_list_tag(pFILE, "nwds", nfiles);
  the_directory->_iadr =  _read_long_list_tag(pFILE, "iadr", nfiles);

  the_directory->_has_multiple_files = SES_FALSE;
  the_directory->_ready = SES_TRUE;

  /*  return */


  return return_value;

}






































