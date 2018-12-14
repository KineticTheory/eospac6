#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define my_swap_array_order HEADER(my_swap_array_order)

ses_error_flag ses_write_2D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim1, ses_number dim2) {

 /*  function prototypes */

  ses_boolean my_swap_array_order(ses_word_reference the_buffer, ses_number dim1, ses_number dim2);    

  /*  end function prototypes */

  ses_error_flag return_value = SES_NO_ERROR;

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_write_2D: invalid ses file handle in ses_write_2D\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  /*  if array order is column major (default), since C is row order, flip the words */
  /*  Note:  C will write in an array in row major order */

  ses_boolean didit_flip;
  if (FILE_LIST[the_handle]->_the_setup->_array_order == 'C') {
    didit_flip = my_swap_array_order(the_buffer, dim1, dim2);    
    if (didit_flip == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("ses_write_2D:  swap array order failed \n");
#endif
     _set_latest_error(SES_FUNCTION_FAIL_ERROR);
     return SES_FUNCTION_FAIL_ERROR;
   }
  }

  return_value = ses_write_1D(the_handle, the_buffer, dim1*dim2);

  return return_value;

}
