#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>


//#define DEBUG_WRAP

#ifdef LC_UNDER
ses_boolean ses_get_table_sizes_(ses_file_handle* pt_handle, ses_material_id* mid, ses_table_id_reference the_buffer, long* size) {
#else

#ifdef UC_UNDER
ses_boolean SES_GET_TABLE_SIZES_(ses_file_handle* pt_handle, ses_material_id* mid, ses_table_id_reference the_buffer, long* size) {
#else

#ifdef LC_NOUNDER
ses_boolean ses_get_table_sizes(ses_file_handle* pt_handle, ses_material_id* mid, ses_table_id_reference the_buffer, long* size) {
#else

#ifdef UC_NOUNDER
ses_boolean SES_GET_TABLE_SIZES(ses_file_handle* pt_handle, ses_material_id* mid, ses_table_id_reference the_buffer, long* size) {
#endif

#endif
#endif
#endif

  ses_boolean return_value = SES_TRUE;

  ses_file_handle the_handle = *pt_handle;
  ses_material_id my_mid = *mid;

  long return_size = 0;

#ifdef DEBUG_WRAP
  printf("ses_get_table_sizes.c:  mid is %d\n", my_mid);
#endif

  ses_table_id_reference my_buffer = ses_get_table_sizes(the_handle, my_mid, &return_size);

#ifdef DEBUG_WRAP
  printf("ses_get_table_sizes.c:  return_size is %ld\n", return_size);
  int i = 0;
  for (i = 0; i < return_size; i++) {
	printf("table size[%d] is %d\n", i, my_buffer[i]);
  }
#endif
  int j = 0;
  for (j = 0; j < return_size; j++) {
	the_buffer[j] = my_buffer[j];
  }

  *size = return_size;
#ifdef DEBUG_WRAP
  printf("ses_get_table_sizes.c:  size is %ld return_value is %d\n", *size, return_value);
#endif

  return return_value;

}

 

