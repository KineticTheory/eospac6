#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#define destruct_list_string_list HEADER(destruct_list_string_list)

#define check_errors_single_argument HEADER(check_errors_single_argument)
#define check_errors_two_arguments HEADER(check_errors_two_arguments)
#define check_errors_get_address_for_table HEADER(check_errors_get_address_for_table)
#define check_errors_add_table_to_index_record HEADER(check_errors_add_table_to_index_record)
#define check_errors_add_tables HEADER(check_errors_add_tables)
#define check_errors_read_index_record HEADER(check_errors_read_index_record)
#define check_errors_write_index_record HEADER(check_errors_write_index_record)
#define check_errors_remove_table_from_index_record HEADER(check_errors_remove_table_from_index_record)
#define check_errors_add_100_table_to_index_record HEADER(check_errors_add_100_table_to_index_record)

/*****************************************************/

ses_string _ses_print_index_record(struct _ses_index_record* pIR, ses_table_id tid) {

 	ses_string return_value = malloc(sizeof(char) * 10000);
 	ses_string buffer = malloc(sizeof(char) * 500);
	sprintf(return_value, "mid %ld date1 %ld date2 %ld vers %ld nrec %ld ", pIR->_mid, pIR->_date1, pIR->_date2,
										pIR->_vers, pIR->_nrec);
	int i = 0;
	for (i = 0; i < pIR->_nrec; i++) {
		if (pIR->_tblid[i] == tid) {
			sprintf(buffer, "tblid %ld nwds %ld iadr%ld\n", pIR->_tblid[i], pIR->_nwds[i], pIR->_iadr[i]);
		}
	}
	strcat(return_value, buffer);
	return return_value;
}
struct _ses_index_record*  _construct_ses_index_record() {

  /*  construct a ses index record */

  struct _ses_index_record* the_index_record = NULL;

  /*  allocate memory for the return */

  the_index_record = malloc(sizeof(struct _ses_index_record)*1);
  if (the_index_record == NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_index_record: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_index_record*)NULL;
  }
  
  the_index_record->_mid = 0;
  the_index_record->_date1 = 0;
  the_index_record->_date2 = 0;
  the_index_record->_vers = 0;
  the_index_record->_nrec = 0;
  the_index_record->_tblid = (long*)NULL;
  the_index_record->_nwds = (long*)NULL;
  the_index_record->_iadr = (long*)NULL;

  the_index_record->_nr = (ses_number*)NULL;
  the_index_record->_nt = (ses_number*)NULL;
  the_index_record->_function_filename = (char**)NULL;
  the_index_record->_type = (char**)NULL;
  the_index_record->_nfuncs = 0;

  the_index_record->_array_filename = (char***)NULL;
  the_index_record->_narrays = (long*)NULL;
  the_index_record->_array_iadr = (long**)NULL;                



  the_index_record->_ready = SES_FALSE;

  /*  return */

  return the_index_record;
}
struct _ses_index_record* _construct_ses_index_record_for_write(long date1, long date2, long vers) {

	struct _ses_index_record* return_value = _construct_ses_index_record();
	return_value->_date1 = date1;
	return_value->_date2 = date2;
	return_value->_vers = vers;
	return return_value;
}

/************************************************************/

struct _ses_index_record* _copy_index_record(struct _ses_index_record* the_index_record) {

  /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);

  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return (struct _ses_index_record*)NULL;
  }

  struct _ses_index_record* return_value =  (struct _ses_index_record*)NULL;

  /*  allocate memory for the return value */

  return_value = malloc(sizeof(struct _ses_index_record)*1);
  if (return_value == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_index_record: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_index_record*)NULL;
  }

  return_value ->_mid = the_index_record->_mid;
  return_value->_date1 = the_index_record->_date1;
  return_value->_date2 = the_index_record->_date2;
  return_value->_vers = the_index_record->_vers;
  return_value->_nrec = the_index_record->_nrec;  /*  checked for 0 value in check_errors */

  return_value->_tblid = malloc(sizeof(ses_table_id)*return_value->_nrec);
  if (return_value->_tblid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_index_record: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory on error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_ses_index_record(return_value);

    return (struct _ses_index_record*)NULL;
  }

  return_value->_nwds = malloc(sizeof(long)*return_value->_nrec);
  if (return_value->_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_index_record: emory allocation error nwds\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory on error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_ses_index_record(return_value);

    return (struct _ses_index_record*)NULL;
  }

  return_value->_iadr = malloc(sizeof(long)*return_value->_nrec);
  if (return_value->_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_index_record: memory allocation error iadr\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory on error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_ses_index_record(return_value);

    return (struct _ses_index_record*)NULL;
  }

  return_value->_nr = malloc(sizeof(long)*return_value->_nrec);
  return_value->_nt = malloc(sizeof(long)*return_value->_nrec);
  //  PUT MEMORY ERROR CHECKING HERE

  int i;
  //////////////////////////////////////////
  return_value->_type = (char**)NULL;
  return_value->_nfuncs = 0;

  return_value->_function_filename = (char**)NULL;
  return_value->_array_filename = (char***)NULL;
  return_value->_narrays = (long*)NULL;
  return_value->_array_iadr = (long**)NULL; 
	/////////////////////////////////////////////
  for (i=0; i < return_value->_nrec; i++) {
	  return_value->_tblid[i] = the_index_record->_tblid[i];
	  return_value->_nwds[i] = the_index_record->_nwds[i];
	  return_value->_iadr[i] = the_index_record->_iadr[i];
	  return_value->_nr[i] = the_index_record->_nr[i];
	  return_value->_nt[i] = the_index_record->_nt[i];


  }

  return_value->_ready = the_index_record->_ready;
 
  return return_value;
}


/**********************************************************************/

ses_boolean _destruct_ses_index_record(struct _ses_index_record* the_index_record) {

  /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);
  int destruct_list_string_list(char*** the_list, int* size_lists, int size_list);    
 
  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return SES_TRUE;
  }

  /*  destruct the ses index record */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  /*  free and set pointers to null */
  the_index_record->_mid = 0;

  if (the_index_record->_nwds != (long*)NULL) {
  	  free(the_index_record->_nwds);
  	  the_index_record->_nwds = (long*)NULL;
  }


  if (the_index_record->_iadr != (long*)NULL) {
	  free(the_index_record->_iadr);
	  the_index_record->_iadr = (long*)NULL;
  }


  int i = 0;
  if (the_index_record->_nr != (long*)NULL) {
      free(the_index_record->_nr);
      the_index_record->_nr = (long*)NULL;
  }

  if (the_index_record->_nt != (long*)NULL) {
      free(the_index_record->_nt);
      the_index_record->_nt = (long*)NULL;
  }

  if (the_index_record->_function_filename != (char**)NULL) {
	for (i=0; i < the_index_record->_nfuncs; i++) {
		if (the_index_record->_function_filename[i] != (char*)NULL) {
			free(the_index_record->_function_filename[i]);
			the_index_record->_function_filename[i] = (char*)NULL;
		}
        }
	if (the_index_record->_function_filename != (char**)NULL) {
		free(the_index_record->_function_filename);
		the_index_record->_function_filename = (char**)NULL;
	}
  }

  if (the_index_record->_array_filename != (char***)NULL) {
	int j = 0;
	for (i=0; i < the_index_record->_nrec; i++) {
		if (the_index_record->_narrays != (long*)NULL) {
			long narrays = the_index_record->_narrays[i];
			if (the_index_record->_array_filename[i] != (char**)NULL) {
			   for (j = 0; j <narrays; j++) {
				if (the_index_record->_array_filename[i][j] != (char*)NULL) {
					free(the_index_record->_array_filename[i][j]);
					the_index_record->_array_filename[i][j] = (char*)NULL;
				}
			   }
			   free(the_index_record->_array_filename[i]);
			   the_index_record->_array_filename[i] = (char**)NULL;
			}
		}
        }
	free(the_index_record->_array_filename);
	the_index_record->_array_filename = (char***)NULL;
  }

  free(the_index_record->_narrays);
  the_index_record->_narrays = (long*)NULL;

  /*  should delete new_type */
  if (the_index_record->_type != (char**)NULL) {

	for (i=0; i < the_index_record->_nrec; i++) {
		if (the_index_record->_type[i] != (char*)NULL) {
			free(the_index_record->_type[i]);
			the_index_record->_type[i] = (char*)NULL;
		}
        }
	free(the_index_record->_type);
	the_index_record->_type = (char**)NULL;
  }

  if (the_index_record->_array_iadr != (long**)NULL) {
	for (i=0; i < the_index_record->_nrec; i++) {
		if (the_index_record->_array_iadr[i] != (long*)NULL) {
			free(the_index_record->_array_iadr[i]);
			the_index_record->_array_iadr[i] = (long*)NULL;
		}
        }
	free(the_index_record->_array_iadr);
	the_index_record->_array_iadr = (long**)NULL;
  }

  if (the_index_record->_tblid != (long*)NULL) {
  	free(the_index_record->_tblid);
  	the_index_record->_tblid = (long*)NULL;
  }
  the_index_record->_nrec = 0;
  the_index_record->_ready = SES_FALSE;


  /*  return */
  return return_value;
}
/************************************************************/


ses_boolean _isit_ready_index_record(struct _ses_index_record* the_index_record) {

  /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);

  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  return whether the index record is ready to be used */

  return the_index_record->_ready;
}
/************************************************************/


long* _get_tblid(struct _ses_index_record* the_index_record) {

 /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);

  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return (long*)NULL;
  }

  /*  return the table id array in the index record */

  return the_index_record->_tblid;

}
/************************************************************/

long* _get_nwds_index_record(struct _ses_index_record* the_index_record) {

  /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);

  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return (long*)NULL;
  }

  /*  return the nwds array in the index record */

  return the_index_record->_nwds;


}
/************************************************************/

long* _get_iadr_index_record(struct _ses_index_record* the_index_record) {

  /*  function prototypes */

  ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record);

  /*  end function prototypes */

  ses_error_flag check = check_errors_single_argument(the_index_record);
  if (check != SES_NO_ERROR) {
	return (long*)NULL;
  }

  /*  return the iadr array in the index record */

  return the_index_record->_iadr;

}

/**********************************************************/

long _get_address_for_table(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH) {

  /*  function prototypes */

  ses_error_flag check_errors_get_address_for_table(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_get_address_for_table(ptIR, the_tid, pSFH);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  /*  get the 'file' address for the associated table id */

  return pSFH->pt2_get_address_for_table(ptIR, the_tid, pSFH);
}

/*******************************************************/

long _get_table_size(struct _ses_index_record* the_index_record, ses_table_id the_tid) {

  /*  function prototypes */

  ses_error_flag check_errors_two_arguments(struct _ses_index_record* ptIR, ses_table_id the_tid);

  /*  end function prototypes */

  ses_error_flag check = check_errors_two_arguments(the_index_record, the_tid);
  if (check != SES_NO_ERROR) {
	return 0;
  }
  /*  get the 'file' size for the associated table id */

  long return_value = 0;

  /*  get the table size for the id */

  long size = the_index_record->_nrec;  /*  0 check made in check errors routine */

  long i=0;
  for (i=0; i<size; i++) {

    if (the_index_record->_tblid[i] == the_tid){
      return_value = the_index_record->_nwds[i];
    }
    
  }

  return return_value;
}



/*****************************************************************/

ses_boolean _add_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id the_tid, long size) {

  /*  function prototypes */

  ses_error_flag check_errors_add_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id the_tid, long size);

  /*  end function prototypes */

  ses_error_flag check = check_errors_add_table_to_index_record(ptIR, the_tid, size);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  add a table id to the index record */

  ses_boolean return_value = SES_TRUE;

  /*  allocate memory for the new table */

  long  new_size = ptIR->_nrec + 1;
  long  last_index = new_size - 1;

  long* new_tblid = malloc(sizeof(long)* new_size);
  if (new_tblid == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("memory allocation error new_tblid in _add_table_to_index_record\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_FALSE;
  }

  long* new_nwds = malloc(sizeof(long)* new_size);
  if (new_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("memory allocation error new_nwds in _add_table_to_index_record\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error exit */
    free(new_tblid);
    new_tblid = (long*)NULL;

    return SES_FALSE;
  }

  long* new_iadr = malloc(sizeof(long)* new_size);
  if (new_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("memory allocation error new_iadr in _add_table_to_index_record\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  free memory on error exit */
    free(new_tblid);
    new_tblid = (long*)NULL;
    free(new_nwds);
    new_nwds = (long*)NULL;

    return SES_FALSE;
  }


  ses_number* new_nr = malloc(sizeof(ses_number) * new_size);
  //  PUT MEMORY ERROR CHECKS HERE
  ses_number* new_nt = malloc(sizeof(ses_number) * new_size);
  //  PUT MEMORY ERROR CHECKS HERE

  /*  copy the table into the new arrays */

  int i = 0;
  for(i=0; i< ptIR->_nrec; i++) {
    new_tblid[i] = ptIR->_tblid[i];
    new_nwds[i] = ptIR->_nwds[i];
    new_iadr[i] = ptIR->_iadr[i];
  }

  /*  add an new table to the end of the arrays */

  new_tblid[last_index] = the_tid;
  new_nwds[last_index] = size;
  if (last_index >= 1) {
  	new_iadr[last_index] = new_iadr[last_index - 1] + new_nwds[last_index];
  }
  else {
        /*  last_index is 0 */
        new_iadr[last_index] = new_nwds[last_index];
  }


  /*  copy the size */

  ptIR->_nrec = new_size;

  /*  free the old memory  and set the pointers to the new arrays */

  free(ptIR->_tblid);
  ptIR->_tblid = new_tblid;

  free(ptIR->_nwds);
  ptIR->_nwds = new_nwds;

  free(ptIR->_iadr);
  ptIR->_iadr = new_iadr;


  free(ptIR->_nr);
  ptIR->_nr = new_nr;

  free(ptIR->_nt);
  ptIR->_nt = new_nt;



  /*  return */

  return return_value;
}


/*************************************************************************/

ses_boolean _add_tables(struct _ses_index_record* ptIR, ses_table_id* _tblid,
			long* _nwds, long* _iadr, long _nrec) {

 /*  function prototypes */

  ses_error_flag check_errors_add_tables(struct _ses_index_record* ptIR, ses_table_id* _tblid, long* _nwds, long* _iadr, long _nrec);

  /*  end function prototypes */

  ses_error_flag check = check_errors_add_tables(ptIR, _tblid, _nwds, _iadr, _nrec);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  add tables to the index record */

  ses_boolean return_value = SES_TRUE;


  /*  set the values */
  
  ptIR->_nrec = _nrec;
  ptIR->_tblid = _tblid;
  ptIR->_nwds = _nwds;
  ptIR->_iadr = _iadr;
  ptIR->_nr = 0;
  ptIR->_nt = 0;

  ptIR->_ready = SES_TRUE;  

  /*  return */

  return return_value;

}

/******************************************************************************/


struct _ses_index_record* _read_index_record(struct _ses_file_handle* pSFH, long offset) {

 /*  function prototypes */

  ses_error_flag check_errors_read_index_record(struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_read_index_record(pSFH);
  if (check != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
        printf("_read_index_record:  check errors failed\n");
#endif
	return (struct _ses_index_record*)NULL;
  }

  /*  read the index record from the C file handle */

  struct _ses_index_record* the_index_record = (struct _ses_index_record*)NULL;

  /*  construct a new ses index record */

  the_index_record = _construct_ses_index_record();
  if (the_index_record == NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record: received NULL Index record after construct \n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return NULL;
  }


  ses_error_flag read_errors = pSFH->pt2_read_index_record(the_index_record, pSFH, offset);
  
  // Ginger print all of the values read in:
//  printf("_read_index_record:: offset: %ld, mid: %ld, date1: %ld, date2: %ld, version: %ld, nrec: %ld\n",
//         offset, the_index_record->_mid, the_index_record->_date1, the_index_record->_date2, the_index_record->_vers,
//         the_index_record->_nrec);
//  int i = 0;
//  for (i=0;i<the_index_record->_nrec;i++){
//    printf("\ttid: %ld, nwds: %ld, Iadr: %ld\n",the_index_record->_tblid[i], the_index_record->_nwds[i], the_index_record->_iadr[i] );
//  }
  
 
  if (read_errors != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
    printf("_read_index_record: read_errors = %d \n", read_errors);
#endif
    _set_latest_error(read_errors);

    /*  destruct on error */
    /* ses_boolean didit_destruct = SES_FALSE;  */

    /* didit_destruct = */ _destruct_ses_index_record(the_index_record);
    free(the_index_record);
    the_index_record = (struct _ses_index_record*)NULL;
  }

  return the_index_record;

}


/***********************************************************************/

ses_boolean _write_index_record(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {

  ses_error_flag check_errors_write_index_record(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  ses_error_flag check = check_errors_write_index_record(ptIR, pSFH);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  write an index record to the c file handle */

  ses_boolean return_value = SES_TRUE;

  /*  write the index record to the c file handle */

  return_value = pSFH->pt2_write_index_record(ptIR, pSFH);


   return return_value;
}

/******************************************************************************/

ses_boolean _remove_table_from_index_record(struct _ses_index_record* ptIR,
					    struct _ses_data_record* ptDR, ses_table_id the_tid){

  /*  function prototypes */

  ses_error_flag check_errors_remove_table_from_index_record(struct _ses_index_record* ptIR, struct _ses_data_record* ptDRI, ses_table_id the_tid);

  /*  end function prototypes */

  ses_error_flag check = check_errors_remove_table_from_index_record(ptIR, ptDR, the_tid);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  int i=0;
  for (i=0; i<ptIR->_nrec; i++) {
    if (ptIR->_tblid[i] == the_tid) {

      ptIR->_tblid[i] = 0;

    }
  }

  return SES_TRUE;
}


/***********************************************************************************/

ses_boolean _table_exists(struct _ses_index_record* ptIR, ses_table_id tid) {

  ses_error_flag check_errors_two_arguments(struct _ses_index_record* ptIR, ses_table_id tid);

  /*  end function prototypes */

  ses_error_flag check = check_errors_two_arguments(ptIR, tid);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  ses_boolean return_value = SES_FALSE;

  int i=0;
  for (i=0; i < ptIR->_nrec; i++) {
    if (ptIR->_tblid[i] == tid) {
      return_value = SES_TRUE;
    }
  }

  return return_value;
}

/*************************************************************************************/

ses_boolean _add_100_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id the_table_id, int size_table){ 

  /*  function prototypes */

  ses_error_flag check_errors_add_100_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id tid, int size_table);

  /*  end function prototypes */

  ses_error_flag check = check_errors_add_100_table_to_index_record(ptIR, the_table_id, size_table);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }


  ses_boolean return_value = SES_TRUE;

  if (ptIR->_nrec > 0) {
        int i;
        long *_old_tblid = (long*)malloc(ptIR->_nrec * sizeof(long));
	long *_old_nwds  = (long*)malloc(ptIR->_nrec * sizeof(long));
	long *_old_iadr  = (long*)malloc(ptIR->_nrec * sizeof(long));

	for(i=0; i<ptIR->_nrec; i++) {
	  _old_tblid[i] = ptIR->_tblid[i];
	  _old_nwds[i]  = ptIR->_nwds[i];
	  _old_iadr[i]  = ptIR->_iadr[i];
	}

        free(ptIR->_tblid);
        ptIR->_tblid = NULL;
        free(ptIR->_nwds);
        ptIR->_nwds = NULL;
        free(ptIR->_iadr);
        ptIR->_iadr = NULL;
	
  	ptIR->_tblid = malloc(sizeof(long) * (ptIR->_nrec + 1));
  	ptIR->_nwds = malloc(sizeof(long) * (ptIR->_nrec + 1));
  	ptIR->_iadr = malloc(sizeof(long) * (ptIR->_nrec + 1));


	for(i=0; i<ptIR->_nrec; i++) {
	  ptIR->_tblid[i] = _old_tblid[i];
	  ptIR->_nwds[i]  = _old_nwds[i];
          /*  NOTE:  when adding the 100 table, nrec increases by 1, so when adding tables, move the iadr up by 3 */
	  ptIR->_iadr[i]  = _old_iadr[i] + 3;
	}

  	ptIR->_tblid[ptIR->_nrec] = the_table_id;
  	ptIR->_nwds[ptIR->_nrec] = (long)size_table;
  	ptIR->_iadr[ptIR->_nrec] = ptIR->_iadr[ptIR->_nrec-1] + ptIR->_nwds[ptIR->_nrec-1];

  	ptIR->_nrec++;

	free(_old_tblid);
	_old_tblid = NULL;
	free(_old_nwds);
	_old_nwds = NULL;
	free(_old_iadr);
	_old_iadr = NULL;
  }
  else {
  	ptIR->_tblid = malloc(sizeof(ses_table_id) * 1);
  	ptIR->_nwds = malloc(sizeof(long) * 1);
  	ptIR->_iadr = malloc(sizeof(long) * 1);

  	ptIR->_tblid[0] = the_table_id;
  	ptIR->_nwds[0] = (long)size_table;
  	ptIR->_iadr[0] = 0 + ptIR->_nwds[0];

  	ptIR->_nrec = 1; 
  }

  
  return return_value;
}

////////////  helper functions //////////////////////////

int destruct_list_string_list(char*** the_list, int* size_lists, int size_list) {   

  //  destructs internal lists

  int return_value = 0;
  int i = 0;
  int j = 0;
  if ((the_list != (char***)NULL) && (size_lists != (int*)NULL)) {
  	for (i = 0; i < size_list; i++) {
		if (the_list[i] != (char**)NULL) {
			for (j = 0; j < size_lists[i]; j++) {
				if (the_list[i][j] != (char*)NULL) {
					free(the_list[i][j]);
					the_list[i][j] = (char*)NULL;
				}
			}			
			free(the_list[i]);
			the_list[i] = (char**)NULL;
		}
		else {
		}
	  }
  }
  else {
	return_value = 1;
  }
  return return_value;

}


int _get_table_index(struct _ses_index_record* ptIR, ses_table_id the_tid) {
  
  /*  function prototypes */

  ses_error_flag check_errors_two_arguments(struct _ses_index_record* ptIR, ses_table_id the_tid);

  /*  end function prototypes */

  ses_error_flag check = check_errors_two_arguments(ptIR, the_tid);
  if (check != SES_NO_ERROR) {
	return 0;
  }
  /*  get the table indexfor the associated table id */

  long return_value = 0;

  /*  get the table size for the id */

  int size = ptIR->_nrec;  /*  0 check made in check errors routine */

  int i=0;
  for (i=0; i<size; i++) {

    if (ptIR->_tblid[i] == the_tid){
      return_value = i;
    }
    
  }

  return return_value;
}

long _get_table_nwds(struct _ses_index_record* ptIR, ses_table_id the_tid) {
   int index = _get_table_index(ptIR, the_tid);
   //  compute ntab from the nwds

   long my_nwds = ptIR->_nwds[index];
   return my_nwds;
   
}

/*****************************ERROR HANDLERS **************************************************/

ses_error_flag check_errors_single_argument(struct _ses_index_record* the_index_record) {

  /*  error check the arguments */

  if (the_index_record == (struct _ses_index_record*)NULL) {
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_index_record->_nrec < 0) {
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  return SES_NO_ERROR;
}

ses_error_flag check_errors_two_arguments(struct _ses_index_record* the_index_record, ses_table_id the_tid) {

  /*  error check the arguments */

  if (the_index_record == (struct _ses_index_record*)NULL) {
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
 
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
    _set_latest_error(SES_INVALID_TID);
    return SES_INVALID_TID;
  }

  /*  check the table size for the id */

  long size = the_index_record->_nrec;
  if (size <= 0) {
    _set_latest_error(SES_INVALID_TID);
    return SES_INVALID_TID;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_get_address_for_table(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH) {

  /*  error check the arguments */
  
  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: _get_address_for_table passed a NULL record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: _get_address_for_table passed an invalid tid\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return 0;
  }

  if (ptIR->_nrec <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_table: nrec <= 0 in _get_address_for_table\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return 0;
  }
   return SES_NO_ERROR;
}


ses_error_flag check_errors_add_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id the_tid, long 
size) {

  /*  error check the arguments */

  if (ptIR == (struct _ses_index_record*)NULL) {
    ptIR = _construct_ses_index_record();
    if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("construct ses index record error in add table to index record \n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("            _add_table_to_index_record passed an invalid tid\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return SES_INVALID_TID;
  }

  /*  internal error checking */

  if (ptIR->_nrec < 0) {
#ifdef DEBUG_PRINT
    printf("            nrec < 0 in _add_table_to_index_record\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return SES_INVALID_TID;
  }
  return SES_NO_ERROR;
}


ses_error_flag check_errors_add_tables(struct _ses_index_record* ptIR, ses_table_id* _tblid, long* _nwds, long* _iadr, long _nrec) {
  /*  error check the arguments */

  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("in _add_tables index record passed in as null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_tblid == (ses_table_id*)NULL) {
#ifdef DEBUG_PRINT
    printf("in _add_tables _tblid passed in as null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("in _add_tables _nwds passed in as null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  
  if (_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("in _add_tables _iadr passed in as null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_nrec <= 0) {
#ifdef DEBUG_PRINT
    printf("in _add_tables _nrec <= 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }


  return SES_NO_ERROR;
}

ses_error_flag check_errors_read_index_record(struct _ses_file_handle* pSFH) {

  /*  error check the arguments */

 if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record: ses file handle NULL in _read_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH->pt2_read_index_record == NULL) {
#ifdef DEBUG_PRINT
    printf("_read_index_record:  function pointer null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_write_index_record(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {

  /*  error check the arguments */

  if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("null index record pointer passed to write index record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("null ses file handle passed to _write_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}




ses_error_flag check_errors_remove_table_from_index_record(struct _ses_index_record* ptIR, struct _ses_data_record* ptDRI, ses_table_id the_tid) {
  return SES_NO_ERROR;
}

ses_error_flag check_errors_add_100_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id tid, int size_table) {
  return SES_NO_ERROR;
}



