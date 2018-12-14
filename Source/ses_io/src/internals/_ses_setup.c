
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

struct _ses_setup* _construct_ses_setup() {

  /*  construct the ses setup */

  /*  allocate memory for the return value */



  struct _ses_setup* return_value = malloc(sizeof(struct _ses_setup)*1);
  if (return_value == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_setup: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_setup*)NULL;
  }

  return_value->_mid = 0;
  return_value->_tid = 0;
  return_value->_significant_digits = 0;
  return_value->_do_validation = SES_FALSE;
#define FIX_INITIALIZATION
#ifdef FIX_INITIALIZATION
  return_value->_order_materials = SES_TRUE;
  return_value->_date1 = 0;
  return_value->_date2 = 0;
  return_value->_vers = 0;
#endif
  return_value->_setup_complete = SES_FALSE;
  return_value->_nr = 0;
  return_value->_nt = 0;
  return_value->_ntab = 0;
  return_value->_array_order = 'C';
  return_value->_is_user_defined_table = SES_FALSE;
  return_value->_date_changed = SES_FALSE;
  return_value->_version_changed = SES_FALSE;
  return_value->_date = 0;
  return_value->_version = 0;

  /*  return */

  return return_value;

}

struct _ses_setup* _copy_ses_setup(struct _ses_setup* pSET) {

  /*  error check the arguments */

  if (pSET == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_setup: Null setup passed to _copy_ses_setup\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_setup*)NULL;
  }

  /*  memory for the new one */

  struct _ses_setup* return_value = malloc(sizeof(struct _ses_setup)*1);
  if (return_value == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_setup: memory allocation error in _copy_ses_setup\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_setup*)NULL;
  }

  /*  copy the data members */

  return_value->_mid = pSET->_mid;
  return_value->_tid = pSET->_tid;
  return_value->_significant_digits = pSET->_significant_digits;
  return_value->_do_validation = pSET->_do_validation;
  return_value->_array_order = pSET->_array_order;
  return_value->_order_materials = pSET->_order_materials;
  return_value->_setup_complete = pSET->_setup_complete;
  return_value->_nr = pSET->_nr;
  return_value->_nt = pSET->_nt;
  return_value->_ntab = pSET->_ntab;
  return_value->_is_user_defined_table = pSET->_is_user_defined_table;
  return_value->_date_changed = pSET->_date_changed;
  return_value->_version_changed = pSET->_version_changed;
  return_value->_date = pSET->_date;
  return_value->_version = pSET->_version;

  /*  return */


  return return_value;
}


ses_boolean _destruct_ses_setup(struct _ses_setup* the_setup) {

  /*  destruct the ses setup */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_ses_setup: the_setup is NULL error\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_TRUE;
  }

  /*  NOTHING TO DO HERE */

  the_setup->_setup_complete = SES_FALSE;

  return return_value;
}

ses_boolean    _isit_setup(struct _ses_setup* the_setup)  {

  /*  return whether the ses_setup is ready */

  ses_boolean return_value = SES_FALSE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_isit_setup: the_setup passed into _isit_setup is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */

  return_value = the_setup->_setup_complete;

  return return_value;
}




ses_boolean  _get_do_validation(struct _ses_setup* the_setup) {

  /* return whether validation is specified */

  ses_boolean return_value = SES_FALSE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_do_validation: the_setup passed into _get_do_validation is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */

  return_value = the_setup->_do_validation;

  return return_value;
  
}
ses_number _get_significant_digits(struct _ses_setup* the_setup) {

  /*  return the number of significant digits specified */

  ses_number return_value = 0;

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_significant_digits: the_setup passed into _get_significant_digits is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  return */

  return_value = the_setup->_significant_digits;

  return return_value;

}

ses_boolean _init_ses_setup(struct _ses_setup* pSP, ses_material_id the_mid, ses_table_id the_tid, long date, long version) {

  /*   intialize the ses setup */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (pSP == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_init_ses_setup: pSP passed into _init_ses_setup is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  
  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_init_ses_setup: invalid material id passed to _init_ses_setup\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_FALSE;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_init_ses_setup: invalid table id  passed to _init_ses_setup\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return SES_FALSE;
  }

  

  pSP->_mid = the_mid;
  pSP->_tid = the_tid;
  pSP->_significant_digits = 0;
  pSP->_do_validation = SES_FALSE;
  pSP->_setup_complete = SES_TRUE;
  pSP->_nr = 0;
  pSP->_nt = 0;
  pSP->_ntab = 0;
  pSP->_array_order = 'C';
  pSP->_date_changed = SES_FALSE;
  pSP->_version_changed = SES_FALSE;
  pSP->_date = date;
  pSP->_version = version;

  /*  return */
 
  return return_value;

}


ses_boolean    _set_do_validation(struct _ses_setup* the_setup)  {

  /*  set the do vaidation flag */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_do_validation: the_setup passed into _set_do_validation is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /* return */

  the_setup->_do_validation = SES_TRUE;

  return return_value;

}
ses_boolean _set_significant_digits(struct _ses_setup* the_setup, ses_number digits)  {

  /*  set the number of user specified significant digits */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_significant_digits: the_setup passed into _set_significant_digits is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */

  the_setup->_significant_digits = digits;

  return return_value;

}


ses_boolean _set_array_order(struct _ses_setup* the_setup, ses_array_order the_order) {

  /*  set the array order (column or row) */

 ses_boolean return_value = SES_TRUE;

 /*  error check the argument */

 if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_array_order: the_setup passed into _set_array_order is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  return */

  the_setup->_array_order = the_order;

  return return_value;

  
}

ses_boolean _set_material_order(struct _ses_setup* the_setup) {

  /*  set whether the material should be ordered on an output file */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_material_order: the_setup passed into _set_material_order is NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  return */

  the_setup->_order_materials = SES_TRUE;

  return return_value;

}


