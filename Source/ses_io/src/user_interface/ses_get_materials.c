
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"


ses_material_id_reference ses_get_materials(ses_file_handle the_handle, long* size) {

  /*  return the materials in the directory */
  
  ses_material_id_reference return_value = (ses_material_id_reference)NULL;

 if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_get_materials: file handle not valid in ses_get_materials\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_MATERIALS;
  }

  FILE* pFILE = 0;
  pFILE = _getPFILE(FILE_LIST[the_handle]->_the_handle);

  if (size == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_materials:  size pointer null in ses_get_materials\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_MATERIALS;
  }

  struct _ses_directory* ptDIR = (struct _ses_directory*)NULL;
  ses_boolean read_directory = SES_FALSE;


  if (FILE_LIST[the_handle]->_directory == (struct _ses_directory*)NULL) {


    struct _ses_file_handle* pSFH = (struct _ses_file_handle*)NULL;
    pSFH = FILE_LIST[the_handle]->_the_handle;
    ptDIR = _read_directory(pSFH);
    read_directory = SES_TRUE;

    FILE_LIST[the_handle]->_directory = ptDIR;

  
  }
  else {
    ptDIR = FILE_LIST[the_handle]->_directory;

  }

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_materials: directory null in ses_get_materials\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_MATERIALS;
  }


  if (ptDIR->_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_get_materials: material id list null in ses_get_materials\n");
#endif

    //ses_boolean didit_destruct = SES_FALSE;
    //didit_destruct = _destruct_ses_directory(ptDIR);
    //free(ptDIR);
    //FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;
    //ptDIR = (struct _ses_directory*)NULL;

    _set_latest_error(SES_READ_ERROR);
    return SES_NULL_MATERIALS;
  }

  *size = ptDIR->_nfiles;
  return_value = malloc(sizeof(long)*(*size));
  int i = 0;
  for (i=0; i < *size; i++) {
	return_value[i] = ptDIR->_matid[i];
  }


  if ((read_directory == SES_TRUE) && (ptDIR != (struct _ses_directory*)NULL)) {
    //ses_boolean didit_destruct = SES_FALSE;
    //didit_destruct = _destruct_ses_directory(ptDIR);
    //free(ptDIR);
    //ptDIR = (struct _ses_directory*)NULL;
  }
  if ((FILE_LIST[the_handle]->_the_setup->_setup_complete == SES_FALSE)&& (ptDIR != (struct _ses_directory*)NULL)) {
    //ses_boolean didit_destruct = SES_FALSE;
    //didit_destruct = _destruct_ses_directory(ptDIR);
    //free(ptDIR);
    //ptDIR = (struct _ses_directory*)NULL;
  }

  _releasePFILE(FILE_LIST[the_handle]->_the_handle);

  return return_value;
}
