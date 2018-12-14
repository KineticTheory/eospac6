

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
int ses_get_materials_(ses_file_handle* pt_handle, long* the_size, ses_material_id_reference the_return) {
#else

#ifdef UC_UNDER
int SES_GET_MATERIALS_(ses_file_handle* pt_handle, long* the_size, ses_material_id_reference the_return) {
#else

#ifdef LC_NOUNDER
int ses_get_materials(ses_file_handle* pt_handle, long* the_size, ses_material_id_reference the_return) {
#else

#ifdef UC_NOUNDER
int SES_GET_MATERIALS(ses_file_handle* pt_handle, long* the_size, ses_material_id_reference the_return) {
#endif

#endif
#endif
#endif

  int return_value = 0;

 
  ses_file_handle the_handle = *pt_handle;
#ifdef DEBUG_WRAP
  printf("wrap_ses_get_materials.c:  the_handle is %d\n", the_handle);
#endif

  ses_material_id_reference dummy_array;
  dummy_array = ses_get_materials(the_handle, the_size);

#ifdef DEBUG_WRAP
  printf("wrap_ses_get_materials.c:  the returned size is %d\n", *the_size);
#endif

  int i = 0;
  
  for(i=0; i < *the_size; i++) {
    the_return[i] = dummy_array[i];
  }
#ifdef DEBUG_WRAP 
  printf("wrap_ses_get_materials.c:  the_return[0] is %d\n", the_return[0]);
  printf("wrap_ses_get_materials.c:  the_size[0] is %d\n", the_size[0]); 
#endif
  return_value = *the_size;
  return return_value;
}

 
