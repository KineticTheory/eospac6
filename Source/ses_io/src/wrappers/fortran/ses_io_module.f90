#include "ses_defines_f90.h"
  
module ses_io

   implicit none
   SES_ERROR_FLAG, parameter :: SES_NO_ERROR = 0
   SES_ERROR_FLAG, parameter :: SES_ARRAY_SIZE_ERROR = 1
   SES_ERROR_FLAG, parameter :: SES_MEMORY_ALLOCATION_ERROR = 2
   SES_ERROR_FLAG, parameter :: SES_OBJECT_CONSTRUCTION_ERROR = 3
   SES_ERROR_FLAG, parameter :: SES_NULL_OBJECT_ERROR = 4
   SES_ERROR_FLAG, parameter :: SES_OBJECT_COPY_ERROR = 5
   SES_ERROR_FLAG, parameter :: SES_OBJECT_DESTRUCTION_ERROR = 6
   SES_ERROR_FLAG, parameter :: SES_FUNCTION_FAIL_ERROR = 7
   SES_ERROR_FLAG, parameter :: SES_INVALID_FILE_HANDLE = 8
   SES_ERROR_FLAG, parameter :: SES_INVALID_MID = 9
   SES_ERROR_FLAG, parameter :: SES_OBJECT_READY_ERROR = 10
   SES_ERROR_FLAG, parameter :: SES_OPEN_ERROR = 11
   SES_ERROR_FLAG, parameter :: SES_INVALID_TID = 12
   SES_ERROR_FLAG, parameter :: SES_OBJECT_OUT_OF_RANGE = 13
   SES_ERROR_FLAG, parameter :: SES_SETUP_ERROR = 14
   SES_ERROR_FLAG, parameter :: SES_CLOSE_ERROR = 15
   SES_ERROR_FLAG, parameter :: SES_FILE_READY_ERROR = 16
   SES_ERROR_FLAG, parameter :: SES_FILE_WRITE_ERROR = 17
   SES_ERROR_FLAG, parameter :: SES_READ_ERROR = 18
   SES_ERROR_FLAG, parameter :: SES_WRITE_ERROR = 19
   SES_ERROR_FLAG, parameter :: SES_CHANGE_ERROR = 20
   SES_ERROR_FLAG, parameter :: SES_COMBINE_ERROR = 21
   SES_ERROR_FLAG, parameter :: SES_COMMENTS_ERROR = 22
   SES_ERROR_FLAG, parameter :: SES_DELETE_ERROR = 23
   SES_ERROR_FLAG, parameter :: SES_NOT_IMPLEMENTED = 24
   SES_ERROR_FLAG, parameter :: SES_INVALID_OPEN_TYPE = 25
   SES_ERROR_FLAG, parameter :: SES_DOUBLE_SIZE_ERROR = 26
   SES_ERROR_FLAG, parameter :: SES_TEST_ERROR = 27
   SES_ERROR_FLAG, parameter :: SES_APPEND_ERROR = 28
   SES_ERROR_FLAG, parameter :: SES_NO_DATA_ERROR = 29

   SES_FILE_HANDLE, parameter ::  SES_NULL_HANDLE = 0;

   SES_NUMBER, parameter :: SES_NUMBER_ARRAY_SIZE_ERROR = 0;

   SES_NUMBER, parameter :: SES_MAX_LABEL_SIZE = SES_LEN_LABEL;

   !SES_LABEL, parameter :: SES_NULL_LABEL = 0;
   !SES_STRING, parameter :: NULL_STRING = 0;

   SES_BOOLEAN, parameter :: SES_FALSE = 0;
   SES_BOOLEAN, parameter :: SES_TRUE = 1;

   !SES_ARRAY_ORDER, parameter :: SES_COLUMN_MAJOR = 0;
   !SES_ARRAY_ORDER, parameter :: SES_ROW_MAJOR = 1;

   !SES_TABLE_ID_REFERENCE, parameter :: SES_NULL_TABLES = 0;
   !SES_MATERIAL_ID_REFERENCE, parameter :: SES_NULL_MATERIALS = 0;



CONTAINS

#include "wrap_ses_access_directory.f90"
#include "wrap_ses_access_table_index.f90"
#include "wrap_array_size_next.f90" 
#include "wrap_ses_change_next.f90"  
#include "wrap_ses_close.f90"    
#include "wrap_ses_combine.f90"   
#include "wrap_ses_comments.f90"
#include "wrap_ses_date.f90" 
#include "wrap_ses_define_table.f90"   
#include "wrap_ses_delete_next.f90"
#include "wrap_ses_delete_table.f90"
#include "wrap_ses_exit.f90"
#include "wrap_ses_format.f90"
#include "wrap_ses_get_comments.f90"
#include "wrap_ses_get_available_formats.f90"
#include "wrap_ses_get_date.f90"
#include "wrap_ses_get_grid.f90"
#include "wrap_ses_get_label.f90"
#include "wrap_ses_get_material_id.f90"
#include "wrap_ses_get_materials.f90"
#include "wrap_ses_get_table_ids.f90"
#include "wrap_ses_get_table_sizes.f90"
#include "wrap_ses_get_version.f90"
#include "wrap_ses_has_next.f90"
#include "wrap_ses_indicates_error.f90"
#include "wrap_ses_is_valid.f90"
#include "wrap_ses_open.f90"
#include "wrap_ses_print_error_condition.f90"
#include "wrap_ses_print_error_message.f90"
#include "wrap_ses_read_1D.f90"
#include "wrap_ses_read_2D.f90"
#include "wrap_ses_read_3D.f90"
#include "wrap_ses_read_named_array.f90"
#include "wrap_ses_read_next.f90"
#include "wrap_ses_read_number.f90"
#include "wrap_ses_read_pairs.f90"
#include "wrap_ses_read_word.f90"
#include "wrap_ses_set_array_order.f90"
#include "wrap_ses_set_format.f90"
#include "wrap_ses_set_grid.f90"
#include "wrap_ses_set_label.f90"
#include "wrap_ses_set_material_order.f90"
#include "wrap_ses_set_significant_digits.f90"
#include "wrap_ses_setup.f90"
#include "wrap_ses_set_validate.f90"
#include "wrap_ses_skip.f90"
#include "wrap_ses_set_table_index.f90"
#include "wrap_ses_version.f90"
#include "wrap_ses_write_1D.f90"
#include "wrap_ses_write_2D.f90"
#include "wrap_ses_write_3D.f90"
#include "wrap_ses_write_comments.f90"
#include "wrap_ses_write_next.f90"
#include "wrap_ses_write_number.f90"
#include "wrap_ses_write_pairs.f90"
#include "wrap_ses_write_setup.f90"
#include "wrap_ses_write_word.f90"



end module ses_io

