#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_boolean _write_index_record_xml(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {


  /*  write an index record to the c file handle */

  ses_boolean return_value = SES_TRUE;

  FILE* pFILE = pSFH->_c_file_handle;

  char* index_tag = malloc(sizeof(char)*30);
  sprintf(index_tag, "<index_record mid = %ld>\n", ptIR->_mid);
  _write_tag(pFILE, index_tag);
  _write_tag(pFILE, "   <mid> ");
  _write_long_xml(pSFH, ptIR->_mid);
  _write_tag(pFILE, "   </mid>\n");
  _write_tag(pFILE, "   <date1>");
  _write_long_xml(pSFH, ptIR->_date1);
  _write_tag(pFILE, "   </date1>\n");
  _write_tag(pFILE, "   <date2>");
  _write_long_xml(pSFH, ptIR->_date2);
  _write_tag(pFILE, "   </date2>\n");
  _write_tag(pFILE, "   <version>");
  _write_long_xml(pSFH, ptIR->_vers);
  _write_tag(pFILE, "   </version>\n");
  _write_tag(pFILE, "   <nrec>");
  _write_long_xml(pSFH, ptIR->_nrec);
  _write_tag(pFILE, "   </nrec>\n");

  _write_tag(pFILE, "   <tblid> ");
  int i = 0;
  for (i = 0; i < ptIR->_nrec; i++) {
  	_write_long_xml(pSFH, ptIR->_tblid[i]);
	if (i < ptIR->_nrec - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </tblid>\n");
  _write_tag(pFILE, "   <nwds> ");
  for (i = 0; i < ptIR->_nrec; i++) {
  	_write_long_xml(pSFH, ptIR->_nwds[i]);
	if (i < ptIR->_nrec - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </nwds>\n");
  _write_tag(pFILE, "   <iadr> ");
  for (i = 0; i < ptIR->_nrec; i++) {
  	_write_long_xml(pSFH, ptIR->_iadr[i]);
	if (i < ptIR->_nrec - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </iadr>\n");
  _write_tag(pFILE, "</index_record>\n");


  free(index_tag);
  index_tag = (char*)NULL;

  return return_value;
}

