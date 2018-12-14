

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
void ses_get_table_ids_(ses_file_handle* pt_handle, ses_material_id* mid, long* the_size, ses_table_id_reference the_return) {
#else

#ifdef UC_UNDER
void SES_get_table_ids_(ses_file_handle* pt_handle, ses_material_id* mid, long* the_size, ses_table_id_reference the_return) {
#else

#ifdef LC_NOUNDER
void ses_get_table_ids(ses_file_handle* pt_handle, ses_material_id* mid, long* the_size, ses_table_id_reference the_return) {
#else

#ifdef UC_NOUNDER
void SES_get_table_ids(ses_file_handle* pt_handle, ses_material_id* mid, long* the_size, ses_table_id_reference the_return) {
#endif

#endif
#endif
#endif

  ses_table_id_reference return_value = (ses_table_id_reference)NULL;

  ses_file_handle the_handle = *pt_handle;
  ses_material_id the_mid = *mid;
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_table_ids.c:  the_handle is %d\n", the_handle);
#endif

  return_value = ses_get_table_ids(the_handle, the_mid, the_size);
  if (return_value != NULL) {
#ifdef DEBUG_WRAP 
    printf("wrap_ses_get_table_ids.c:  return_value[0] is %d\n", return_value[0]);
    printf("wrap_ses_get_table_ids.c:  the_size is %d\n", the_size[0]);
#endif

    int i = 0;
    for (i=0; i < the_size[0]; i++) {
      the_return[i] = return_value[i];
    }
 }
 else {
    the_return = NULL;
 }
}

 
