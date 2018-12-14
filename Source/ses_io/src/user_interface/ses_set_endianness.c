
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_set_endianness(ses_file_handle the_handle, char the_endianness) {


  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_set_endianness: file handle invalid in ses_set_version \n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_set_endianness: file handle null in ses_set_endianness\n");
#endif
    return SES_SETUP_ERROR;
  }

  ses_open_type mode = pSFH->_the_open_mode;
  if (mode == 'W') {

	//  can only change the endianness if file is open for write

	if (the_endianness == 'L') {
		pSFH->_is_little_endian = SES_TRUE;
	}
	if (the_endianness == 'B') {
		pSFH->_is_little_endian = SES_FALSE;
	}

	//  recompute needs flip
	if (pSFH->_is_little_endian == pSFH->_machine_is_little_endian) {
		pSFH->_needs_flip = SES_FALSE;
	}
	if (pSFH->_is_little_endian != pSFH->_machine_is_little_endian) {
		pSFH->_needs_flip = SES_TRUE;
	}

  }
  else {
	return_value = SES_OBJECT_OUT_OF_RANGE;
  }

  return return_value;

}

