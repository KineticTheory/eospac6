
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

long ses_access_directory (ses_file_handle the_handle,  
		 ses_material_id_reference* return_matid, 
                 long** nwds, 
                 long** iadr,
                 long* date, long* version) {

  /*  This routine gets all information in the directory at one time */

  long return_value = 0;

  /*******************************************************************/
  /*  error checking */

  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_access_directory:  file handle not valid\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return 0;
  }


  /*  if the arrays passed in are NOT null, error */

  if (*return_matid != (ses_material_id*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_directory:  matid pointer NOT NULL\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return 0;
     
  }
  
  if (*nwds != (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_directory:  nwds pointer NOT NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (*iadr != (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_directory:  iadr pointer NOT NULL\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  if the directory is null, error */

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("ses_access_directory:  null directory \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /***************************************************************/

  /*  at this point, we're ready to go */

  long* almost_date = malloc(sizeof(long)*1);
  almost_date[0] = ptDIR->_date;
  *date = almost_date[0];

  free(almost_date);
  almost_date = (long*)NULL;

  long*  almost_version = malloc(sizeof(long)*1);
  almost_version[0] = ptDIR->_version;
  *version = almost_version[0];

  free(almost_version);
  almost_version = (long*)NULL;

  long* size = malloc(sizeof(long)*1);
  ses_material_id_reference matid = ses_get_materials(the_handle, size);
  return_value = size[0];

  free(size);
  size = (long*)NULL;

  long* my_nwds = _get_nwds_directory(ptDIR);
  long* my_iadr = _get_iadr_directory(ptDIR);

  *return_matid = matid;
  *nwds = my_nwds;
  *iadr = my_iadr;

  /*  return the number of materials in the directory */

  return return_value;
}

