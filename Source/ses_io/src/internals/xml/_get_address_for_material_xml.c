#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


long _get_address_for_material_xml(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH) {

  /*  get the 'file' address for the associated material id */

  long return_value = 0;

  /* argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: null directory pointer passed in to _get_address_for_material\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: invalid mid in _get_address_for_material\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return 0;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: Trying to _get_address_for_material from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return 0;
  }

  /*  find the address */

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: nfiles <= 0 in _get_address_for_material\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return 0;
  }

  int i=0;

  ses_boolean seen_address = SES_FALSE;
  for (i=0; i< ptDIR->_nfiles; i++) {
      if (the_mid == ptDIR->_matid[i]){
        return_value = ptDIR->_iadr[i];
	seen_address = SES_TRUE;
      }
      

  }


  /*  return */

  return return_value;


}
