
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>


ses_string ses_print_error_condition(ses_file_handle the_handle) {

  /*  function prototype */

  ses_string _get_error(ses_error_flag the_error_flag);

  /*  end function prototypes */


  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_print_error_condition: invalid ses file handle in ses_print_error_condition\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return 0;
  }

  ses_string return_value = _get_error(_latest_error);

  return return_value;
}

ses_string _get_error(ses_error_flag the_error) {

  ses_string return_value;

  switch (the_error) {

  case(SES_NO_ERROR):
    return_value = "No error found";
    break;
  case(SES_ARRAY_SIZE_ERROR):
    return_value = "Array size error";
    break;
  case(SES_MEMORY_ALLOCATION_ERROR):
    return_value = "Memory allocation error";
    break;
  case(SES_OBJECT_CONSTRUCTION_ERROR):
    return_value = "Object construction error";
    break;
  case(SES_NULL_OBJECT_ERROR):
    return_value = "Null object error";
    break;
  case(SES_OBJECT_COPY_ERROR):
    return_value = "Object copy error";
    break;
  case(SES_OBJECT_DESTRUCTION_ERROR):
    return_value = "Object destruction error";
    break;
  case(SES_FUNCTION_FAIL_ERROR):
    return_value = "Function fail error";
    break;
  case(SES_INVALID_FILE_HANDLE):
    return_value = "Invalid File Ses File Handle error";
    break;
  case(SES_INVALID_MID):
    return_value = "Invalid Material ID error";
    break;
  case(SES_OBJECT_READY_ERROR):
    return_value = "Object ready error";
    break;
  case(SES_OPEN_ERROR):
    return_value = "Ses Open error";
    break;
  case(SES_INVALID_TID):
    return_value = "Invalid Table ID error";
    break;
  case(SES_OBJECT_OUT_OF_RANGE):
    return_value = "Object out of range error";
    break;
  case(SES_SETUP_ERROR):
    return_value = "Ses Setup error";
    break;
  case(SES_CLOSE_ERROR):
    return_value = "Ses Close error";
    break;
  case(SES_FILE_READY_ERROR):
    return_value = "Ses File Ready error";
    break;
  case(SES_FILE_WRITE_ERROR):
    return_value = "Ses File Write error";
    break;
  case(SES_READ_ERROR):
    return_value = "Ses Read error";
    break;
  case(SES_WRITE_ERROR):
    return_value = "Ses Write error";
    break;
  case(SES_CHANGE_ERROR):
    return_value = "Ses Change error";
    break;
  case(SES_COMBINE_ERROR):
    return_value = "Ses Combine error";
    break;
  case(SES_COMMENTS_ERROR):
    return_value = "Ses Comments error";
    break;
  case(SES_DELETE_ERROR):
    return_value = "Ses Delete error";
    break;
  case(SES_NOT_IMPLEMENTED):
    return_value = "Not implemented error";
    break;
   case(SES_INVALID_OPEN_TYPE):
    return_value = "Invalid open type";
    break;
   case(SES_DOUBLE_SIZE_ERROR):
    return_value = "Double size error";
    break;
  case(SES_TEST_ERROR):
    return_value = "Test error";
    break;
   case(SES_APPEND_ERROR):
    return_value = "Ses Append Error";
    break;
   case(SES_NO_DATA_ERROR):
    return_value = "Ses No Data Error";
    break;
   

  /*  PUT MORE HERE */

  default:
    return_value = "No error found";
    break;
  }


  return return_value;
}


