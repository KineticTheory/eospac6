#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"


ses_boolean _write_index_record_binary(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {


  /*  write an index record to the c file handle */

  ses_boolean return_value = SES_TRUE;

  /****************************************************************/
  /*  error check the arguments */

  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_index_record_binary: null index record pointer passed to write index record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_index_record_binary: null ses file handle passed to _write_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  /****************************************************************/

  /*  write the index record to the c file handle */

  _write_long(pSFH, ptIR->_mid);
  _write_long(pSFH, ptIR->_date1);
  _write_long(pSFH, ptIR->_date2);
  _write_long(pSFH, ptIR->_vers);
  _write_long(pSFH, ptIR->_nrec);
#ifdef DEBUG_WRITE_INDEX_RECORD_BINARY
  printf("_write_index_record_binary:  mid %d date1 %d date2 %d vers %d nrec %d\n", ptIR->_mid, ptIR->_date1, ptIR->_date2, ptIR->_vers, ptIR->_nrec);
#endif
  int i=0;


  for (i=0; i < ptIR->_nrec;i++) {
        _write_long(pSFH, ptIR->_tblid[i]);
  }
  for (i=0; i < ptIR->_nrec;i++) {
	_write_long(pSFH, ptIR->_nwds[i]);
  }
  for (i=0; i < ptIR->_nrec;i++) {
	_write_long(pSFH, ptIR->_iadr[i]);
  }


  /*  return */

  return return_value;
}

