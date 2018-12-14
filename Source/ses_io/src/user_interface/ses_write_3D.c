
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"



ses_error_flag ses_write_3D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim1, ses_number dim2, ses_number dim3) {
  
  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_3D:  invalid ses file handle in ses_write_3D\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  return_value = ses_write_1D(the_handle, the_buffer, dim1*dim2*dim3);

  return return_value;
}
