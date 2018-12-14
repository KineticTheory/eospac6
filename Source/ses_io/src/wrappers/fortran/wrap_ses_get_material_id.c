

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include "stdio.h"
#include <stdlib.h>

#include <stdio.h>

#undef DEBUG_WRAP

#ifdef LC_UNDER
ses_material_id ses_get_material_id_c_(ses_string the_material_name) {
#else

#ifdef UC_UNDER
ses_material_id SES_GET_MATERIAL_ID_C_(ses_string the_material_name) {
#else

#ifdef LC_NOUNDER
ses_material_id ses_get_material_id_c(ses_string the_material_name) {
#else

#ifdef UC_NOUNDER
ses_material_id SES_GET_MATERIAL_ID_c(ses_string the_material_name) {
#endif

#endif
#endif
#endif

#ifdef DEBUG_WRAP
  printf("wrap_ses_get_material_id.c:  the_material_name  is %s\n", the_material_name);
#endif

  ses_material_id return_value = 0;
/*  removed from user interface

  ses_material_id return_value = ses_get_material_id(the_material_name);

*/

#ifdef DEBUG_WRAP
  printf("wrap_ses_get_material_id.c:  return_value is %d\n", return_value);
#endif

  return return_value;
 
}

 
