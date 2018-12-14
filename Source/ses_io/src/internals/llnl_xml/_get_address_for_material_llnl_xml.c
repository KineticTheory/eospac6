#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#define _get_address_for_material_llnl_xml HEADER(_get_address_for_material_llnl_xml)
#define check_errors_gafmlx HEADER(check_errors_gafmlx)

long _get_address_for_material_llnl_xml(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH) {

  //  function prototypes

  ses_error_flag check_errors_gafmlx(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);

  //  end function prototypes


  long return_value = 0;

  ses_error_flag error_check = check_errors_gafmlx(ptDIR, the_mid, pSFH);
  if (error_check != SES_NO_ERROR) {
	return return_value;
  }

  /*  get the 'file' address for the associated material id */

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

  ses_error_flag check_errors_gafmlx(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH) {
  /* argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: null directory pointer passed in to _get_address_for_material\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: invalid mid in _get_address_for_material\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_INVALID_MID;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: Trying to _get_address_for_material from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_OBJECT_READY_ERROR;
  }

  /*  find the address */

  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: nfiles <= 0 in _get_address_for_material\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  return SES_NO_ERROR;
}

