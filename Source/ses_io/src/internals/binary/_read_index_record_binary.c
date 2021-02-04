#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "_file_list_binary.h"

#define my_read_grid HEADER(my_read_grid)

#undef DEBUG_PRINT
#undef DEBUG_READ_INDEX_RECORD_BINARY


ses_error_flag _read_index_record_binary(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {

  ses_error_flag return_value = SES_NO_ERROR;

  /*  function prototypes */

  ses_boolean my_read_grid(FILE* pFILE, int grid_address, int* nr, int* nt, ses_boolean needs_flip);
  
  /*  end function ptotoypes */

  /****************************************************************************/
  /* argument error checking */
  if (the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary: null directory pointer passed in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary:  null ses file handle in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

   /****************************************************************************/

  /* read values from the file and put them in member data */


  the_index_record->_mid = _read_long(pSFH);
  the_index_record->_date1 = _read_long(pSFH);
  the_index_record->_date2 = _read_long(pSFH);
  the_index_record->_vers = _read_long(pSFH);
  the_index_record->_nrec = _read_long(pSFH);
#ifdef DEBUG_READ_INDEX_RECORD_BINARY
  printf("_read_index_record_binary:  date1 %ld date2 %ld vers %ld\n", the_index_record->_date1, the_index_record->_date2, the_index_record->_vers);
  printf("_read_index_record_binary:  mid %ld nrec %ld\n", the_index_record->_mid, the_index_record->_nrec);
#endif

  if (_is_valid_mid(the_index_record->_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary: invalid material %ld in read index record binary\n", the_index_record->_mid);
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_INVALID_MID;
  }

  long nrec = the_index_record->_nrec;


  if (the_index_record->_tblid != (ses_table_id*)NULL) {
	free(the_index_record->_tblid);
	the_index_record->_tblid = (ses_table_id*)NULL;
  }
  the_index_record->_tblid = malloc(sizeof(ses_table_id)*nrec);
  if (the_index_record->_tblid == NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary: memory allocation error in read index record _tblid\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }

  if (the_index_record->_nwds != (long*)NULL) {
	free(the_index_record->_nwds);
	the_index_record->_nwds = (long*)NULL;
  }
  the_index_record->_nwds = malloc(sizeof(long)*nrec);
  if (the_index_record->_nwds == NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary: memory allocation error in read index record _nwds\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }
 

  if (the_index_record->_iadr != (long*)NULL) {
	free(the_index_record->_iadr);
	the_index_record->_iadr = (long*)NULL;
  }
  the_index_record->_iadr = malloc(sizeof(long)*nrec);
  if (the_index_record->_iadr == NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record_binary: memory allocation error in read index record _iadr\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_MEMORY_ALLOCATION_ERROR;
  }

  long i=0;
  for (i=0; i < nrec; i++) {
    the_index_record->_tblid[i] = _read_long(pSFH);
  }
  for (i=0; i < nrec; i++) {
    the_index_record->_nwds[i] = _read_long(pSFH);
  }
  for (i=0; i < nrec; i++) {
    the_index_record->_iadr[i] = _read_long(pSFH);
  }

  the_index_record->_ready = SES_TRUE;


  // for each table, put nr and nt into the index record
  the_index_record->_nr = malloc(sizeof(long)*nrec);
  //  PUT ERROR CHECKING HERE
  the_index_record->_nt = malloc(sizeof(long)*nrec);
  //  PUT ERROR CHECKING HERE

  FILE* pFILE = pSFH->_c_file_handle;

  int current_location = ftell(pFILE);
  int nr = 0;
  int nt = 0;

  for (i = 0; i < nrec; i++) {


//      grid_address = offset + _get_address_for_table(the_index_record, the_index_record->_tblid[i], pSFH);
//      didit_read = my_read_grid(pFILE, grid_address, &nr, &nt, pSFH->_needs_flip);
      //  PUT ERROR CHECKING HERE
      the_index_record->_nr[i] = nr;
      the_index_record->_nt[i] = nt;

  }

  fseek(pFILE, current_location, SEEK_SET);
  

  /*  return */

  return return_value;
}


ses_boolean my_read_grid(FILE* pFILE, int grid_address, int* nr, int* nt, ses_boolean needs_flip) {

  ses_boolean return_value = SES_TRUE;

  fseek(pFILE, grid_address, SEEK_SET);

  //  FIX THE SECOND ARGUMENT (needs flip)
  int mynr = _read_long_pFILE_binary(pFILE, needs_flip);
  int mynt = _read_long_pFILE_binary(pFILE, needs_flip);

  *nr = mynr;
  *nt = mynt;

//  PUT FUNCTION HERE

  return return_value;
}





