


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "tags.h"
#include "../xml_utilities.h"


ses_boolean _write_directory_xml(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {


  /*  write the directory to the c file handle */	

  ses_boolean return_value = SES_TRUE;

  FILE* pFILE = pSFH->_c_file_handle;

  _write_tag(pFILE, "<directory>\n");
  _write_tag(pFILE, "   <nmats> ");
  _write_long_pFILE_xml(pFILE, ptDIR->_nfiles);
  _write_tag(pFILE, "   </nmats>\n");
  _write_tag(pFILE, "   <date>");
  _write_long_pFILE_xml(pFILE, ptDIR->_date);
  _write_tag(pFILE, "   </date>\n");
  _write_tag(pFILE, "   <version>");
  _write_long_pFILE_xml(pFILE, ptDIR->_version);
  _write_tag(pFILE, "   </version>\n");
  _write_tag(pFILE, "   <matid> ");
  int i = 0;
  for (i = 0; i < ptDIR->_nfiles; i++) {
  	_write_long_pFILE_xml(pFILE, ptDIR->_matid[i]);
	if (i < ptDIR->_nfiles - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </matid>\n");
  _write_tag(pFILE, "   <nwds> ");
  for (i = 0; i < ptDIR->_nfiles; i++) {
  	_write_long_pFILE_xml(pFILE, ptDIR->_nwds[i]);
	if (i < ptDIR->_nfiles - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </nwds>\n");
  _write_tag(pFILE, "   <iadr> ");
  for (i = 0; i < ptDIR->_nfiles; i++) {
  	_write_long_pFILE_xml(pFILE, ptDIR->_iadr[i]);
	if (i < ptDIR->_nfiles - 1) {
  		_write_tag(pFILE, "  ");
	}
  }

  _write_tag(pFILE, "   </iadr>\n");
  _write_tag(pFILE, "</directory>\n");

  return return_value;
}








