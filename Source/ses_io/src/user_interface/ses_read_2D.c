
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define my_swap_array_order HEADER(my_swap_array_order)


ses_error_flag ses_read_2D(ses_file_handle the_handle, ses_word_reference  the_buffer, ses_number dim1, ses_number dim2) {

  /*  function prototypes */

  ses_boolean my_swap_array_order(ses_word_reference the_buffer, ses_number dim1, ses_number dim2);    

  /*  end function prototypes */

  ses_error_flag return_value = SES_NO_ERROR;


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_2D: Invalid file handle in ses_read_2D\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_read_2D:  setup not complete \n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_SETUP_ERROR;
  }

  if (the_buffer == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_read_2D: null buffer passed into ses_read_2D\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  if (dim1 <= 0 || dim2 <= 0) {
#ifdef DEBUG_PRINT
    printf("ses_read_2D: dim1 or dim2  less than or equal to 0 in ses_read_2d\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }


  return_value = ses_read_1D(the_handle, the_buffer, dim1*dim2);
  /*  if array order is column major (default), since C is row order, flip the words */
  /*  Note:  C will read in an array in row major order */

  ses_boolean didit_flip;
  if (FILE_LIST[the_handle]->_the_setup->_array_order == 'C') {
    didit_flip = my_swap_array_order(the_buffer, dim1, dim2);    
    if (didit_flip == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("ses_read_2D:  swap array order failed \n");
#endif
     _set_latest_error(SES_FUNCTION_FAIL_ERROR);
     return SES_FUNCTION_FAIL_ERROR;
   }
  


  }


  

  return return_value;


  
}


ses_boolean my_swap_array_order(ses_word_reference the_buffer, ses_number dim1, ses_number dim2) {

   ses_boolean return_value = SES_TRUE;

   /*  C reads 2,3 data as                X11  X12  X13  X21  X22  X23  - row major order */
   /*  column major order needs data as   X11  X21  X12  X22  X13  X23  -- column major order */

   /* ses_word swapped_array[dim1*dim2]; */
   ses_word * swapped_array;
   swapped_array = (ses_word*)malloc(sizeof(ses_word) * dim1 * dim2);
 
   int i=0;
   int j=0;
   int index;
   int swapped_index;
   /*  dim1 = 2, dim2 = 3 */

   for (i=0; i < dim1; i++) {
     for (j=0; j < dim2; j++) {
       index = i*dim2 + j;
       swapped_index = j*dim1 + i;
       swapped_array[swapped_index] = the_buffer[index];
     }
   }

   /*  Copy the swapped array back into the buffer */

   for (i = 0; i < dim1*dim2; i++) {
     the_buffer[i] = swapped_array[i];
   }

   free(swapped_array);
   return return_value;
}


