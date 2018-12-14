
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_error_flag ses_read_3D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim1, ses_number dim2, ses_number dim3) {

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_3D: Invalid file handle in ses_read_3D\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_3D:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_3D: null buffer passed into ses_read_3D\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  if (dim1 <= 0 || (dim2 <= 0 || dim3 <= 0)) {
#ifdef DEBUG_PRINT
    printf("ses_read_3D: dim1 or dim2  less than or equal to 0 in ses_read_3d\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }
  return_value = ses_read_1D(the_handle, the_buffer, dim1*dim2*dim3);


  return return_value;
}
