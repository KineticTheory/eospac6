#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_error_flag _read_index_record_xml(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {
  ses_error_flag return_value = SES_NO_ERROR;

  FILE* pFILE = pSFH->_c_file_handle;

  /*  position the file to the start of the index record tag */

  /*  PUT THIS HERE */

  /*  read the matid tag */
  /*  read the date1 tag */
  /*  read the date2 tag */
  /*  read the version tag */
  /*  read the nrec tag */

#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  reading index record\n");
#endif

  the_index_record->_mid = _read_long_tag(pFILE, "mid");
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  mid is %ld\n", the_index_record->_mid);
#endif
  the_index_record->_date1 = _read_long_tag(pFILE, "date1");
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  date1 is %ld\n", the_index_record->_date1);
#endif
  the_index_record->_date2 = _read_long_tag(pFILE, "date2");
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  date2 is %ld\n", the_index_record->_date2);
#endif
  the_index_record->_vers = _read_long_tag(pFILE, "version");
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  vers is %ld\n", the_index_record->_vers);
#endif
  the_index_record->_nrec = _read_long_tag(pFILE, "nrec");
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  nrec is %ld\n", the_index_record->_nrec);
#endif

  long nrec = the_index_record->_nrec;

  /*  read the tblid list */
  /*  read the nwds list */
  /*  read the iadr list */

  the_index_record->_tblid = (long*)_read_long_list_tag(pFILE, "tblid", nrec);
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  tblid[0] is %ld\n", the_index_record->_tblid[0]);
#endif
  the_index_record->_nwds = (long*)_read_long_list_tag(pFILE, "nwds", nrec);
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  nwds[0] is %ld\n", the_index_record->_nwds[0]);
#endif
  the_index_record->_iadr = (long*)_read_long_list_tag(pFILE, "iadr", nrec);
#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  iadr[0] is %ld\n", the_index_record->_iadr[0]);
#endif

#ifdef DEBUG_READ_INDEX_RECORD_XML
  printf("_read_index_record_xml:  finished reading index record\n");
#endif

  /*  position the file to just after the end of the index record tag */

  /*PUT THIS HERE*/

  /*  return */

  return return_value;
}




