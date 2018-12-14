
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


#include <stdlib.h>


ses_material_id ses_get_material_id(ses_string material_name) {

  ses_material_id return_value = 0;

  /*  removing user interface function 4/2/2013 HMA */
/*
  if (material_name == (ses_string)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_get_material_id:  null material name\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  return_value = _get_material_id_from_name(material_name);
*/

  return return_value;

}
