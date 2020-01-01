#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"
#include "string.h"

#define _create_my_index_record HEADER(_create_my_index_record)

struct _ses_material_file* _construct_ses_material_file(ses_material_id the_mid) {

  /*  construct a ses material file */

  struct _ses_material_file* ptMF = (struct _ses_material_file*)NULL;

  /*  error check the arguments */

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_material_file: invalid material id =%ld in _construct_material_file\n", the_mid);
#endif
    _set_latest_error(SES_INVALID_MID);
    return (struct _ses_material_file*)NULL;
  }

  /*  allocate memory for the return value */

  ptMF = malloc(sizeof(struct _ses_material_file)*1);
  if (ptMF == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_material_file: memory allocation error in _construct_material_file\n");
#endif

    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_material_file*)NULL;
  }

  ptMF->_the_mid = the_mid;
  ptMF->_the_index_record = (struct _ses_index_record*)NULL;
  ptMF->_number_tables = 0;
  ptMF->_the_tables2 = (struct _ses_data_record_list*)NULL;
  ptMF->_next = 0;

  ptMF->_the_index_record = _construct_ses_index_record();
  if (ptMF->_the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_material_file: memory allocation error in construct index record \n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  destruct upon error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_material_file(ptMF);
    free(ptMF);
    ptMF = (struct _ses_material_file*)NULL;

    return (struct _ses_material_file*)NULL;
  }

  ptMF->_the_tables2 = _construct_data_record_list();     // this is where the non-destructed object gets constructed in append test
  if (ptMF->_the_tables2 == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_material_file: memory allocation error in construct data record list \n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  destruct upon error */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_material_file(ptMF);
    free(ptMF);
    ptMF = (struct _ses_material_file*)NULL;

    return (struct _ses_material_file*)NULL;
  }
  ptMF->_number_tables = 0;
  ptMF->_next = (struct _ses_material_file*)NULL;

  /*  return */

  return ptMF;
}


struct _ses_material_file* _copy_ses_material_file(struct _ses_material_file* the_mf) {

  /*  copy a ses material file */

  /*  error check the arguments */

  if (the_mf == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_material_file: null material file\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_material_file*)NULL;
  }

  /*  allocate memory for the return value */

  struct _ses_material_file* return_value = _construct_ses_material_file(the_mf->_the_mid);
  if (return_value == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_material_file; construct ses material file  error \n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_material_file*)NULL;
  }

  /*  copy the data members */

  return_value->_the_mid = the_mf->_the_mid;
  if (return_value->_the_index_record != (struct _ses_index_record*)NULL) {
	_destruct_ses_index_record(return_value->_the_index_record);
	free(return_value->_the_index_record);	
	return_value->_the_index_record = (struct _ses_index_record*)NULL;
  }
  return_value->_the_index_record = _copy_index_record(the_mf->_the_index_record);
  return_value->_number_tables = the_mf->_number_tables;
  if (return_value->_the_tables2 != (struct _ses_data_record_list*)NULL) {
	_destruct_data_record_list(return_value->_the_tables2);
	free(return_value->_the_tables2);
	return_value->_the_tables2 = (struct _ses_data_record_list*)NULL;
  }
  return_value->_the_tables2 = _copy_data_record_list(the_mf->_the_tables2);
  return_value->_next = the_mf->_next;

  /*  return */

  return return_value;
}

ses_boolean                 _destruct_and_detach_material_file(struct _ses_material_file* the_file) {
  /*  destruct the ses material file */

  /*  error check the arguments */

  ses_boolean return_value = SES_TRUE;
  if (the_file == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_material_file: null pointer passed to destruct_material_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  do the destruction */

  ses_boolean didit_dir = _destruct_ses_index_record(the_file->_the_index_record);
  if (didit_dir == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_material_file: did not destruct index record in destruct material file \n");
#endif
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }
  free(the_file->_the_index_record);
  the_file->_the_index_record = (struct _ses_index_record*)NULL;

  free(the_file->_the_tables2);
  the_file->_the_tables2 = (struct _ses_data_record_list*)NULL;
  
  the_file->_next = (struct _ses_material_file*)NULL;

  /*  return */

  return return_value;
}

ses_boolean _destruct_material_file(struct _ses_material_file* the_file) {

  /*  destruct the ses material file */

  /*  error check the arguments */

  ses_boolean return_value = SES_TRUE;
  if (the_file == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_material_file: null pointer passed to destruct_material_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  do the destruction */

  ses_boolean didit_dir = _destruct_ses_index_record(the_file->_the_index_record);
  if (didit_dir == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_material_file: did not destruct index record in destruct material file \n");
#endif
    free(the_file->_the_index_record);
    the_file->_the_index_record = (struct _ses_index_record*)NULL;
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }
  free(the_file->_the_index_record);
  the_file->_the_index_record = (struct _ses_index_record*)NULL;


  ses_boolean didit_ddl = _destruct_data_record_list(the_file->_the_tables2);
  if (didit_ddl == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_material_file: did not destruct data record list in destruct material file\n");
#endif
    
    free(the_file->_the_tables2);
    the_file->_the_tables2 = (struct _ses_data_record_list*)NULL;
    _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
    return SES_FALSE;
  }
  else {
    free(the_file->_the_tables2);
    the_file->_the_tables2 = (struct _ses_data_record_list*)NULL;
  }

  
  the_file->_next = (struct _ses_material_file*)NULL;

  /*  return */

  return return_value;
}

ses_boolean  _table_exists_in_material_file(struct _ses_material_file* pMF, ses_table_id the_tid) {

  /*  return whether the object exists in the material file */

  ses_boolean return_value = SES_FALSE;

  /*  error check the arguments */

  if (pMF == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_table_exists_in_material_file: null material file passed to _table_exists_in_material file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_table_exists_in_material_file:invalid table id passed to _table_exists_in_material file\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return SES_FALSE;
  }



  /*  determine whether it exists */

  struct _ses_index_record* pIR = pMF->_the_index_record;
  if (pIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_table_exists_in_material_file: ses index record null in _table_exists_in_material file \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pIR->_nrec < 0) {
#ifdef DEBUG_PRINT
    printf("_table_exists_in_material_file: nrec < 0 in _table_exists_in_material file \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  
  

  int i=0;  
  for (i=0; i< pIR->_nrec; i++) {
    if (pIR->_tblid[i] == the_tid) {
      return_value = SES_TRUE;
    }
  }

  /*  return */

  return return_value;
}


ses_boolean _add_table(struct _ses_material_file* the_mf, 
                       struct _ses_data_record* the_data_record) {

  /*  add another table(contained in the data record) to the material file */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */


  if (the_mf == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_table: null pointer passed to _add_table\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (the_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_table: null data record passed to _add_table\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */


  return_value = _add_to_list_dr(the_mf->_the_tables2, the_data_record);
  if (return_value == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_add_table: _add_to_list_dr failed in _add_table\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FALSE;
  }


  the_mf->_number_tables++;

  return return_value;
 

}


ses_boolean _combine_two_material_files(struct _ses_material_file* this, 
                                        struct _ses_material_file* the_material_file) {

  /*  combine two material files (ending up with combined file in 'this') */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (this == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_two_material_files: null material file * passed into _combine_two_material_files\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (the_material_file  == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_combine_two_material_files: null second material file * passed into _combine_two_material_files\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }


  /*  if the two material files do NOT have the same mid, error */

  if (this->_the_mid != the_material_file->_the_mid) {
#ifdef DEBUG_PRINT
    printf("_combine_two_material_files: material id's not the same in _combine_two_material_files\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_FALSE;
  }

  /*  for each table id in the_material_file */
  /*       if the table id exists in 'this', error */
  /*       if the table id does not exist in 'this', */
  /*            add the data record for the table id to 'this'  */
  /*            add the data record information to the index record for 'this' */

  //int i=0;

  struct _ses_data_record_list* pDRL = the_material_file->_the_tables2;
  struct _ses_data_record* tmp = pDRL->_head;
#ifdef DEBUG_PRINT
  ses_material_id the_mid = 0;
  the_mid = the_material_file->_the_mid;
#endif
  ses_boolean didit_add1, didit_add2;

  while (tmp != (struct _ses_data_record*)NULL) {

      if (_table_exists_in_material_file(this, tmp->_tid) == SES_TRUE) {
#ifdef DEBUG_PRINT
        printf("_combine_two_material_files: table %ld for material %ld exists in first file, will not replace\n", tmp->_tid, the_mid);
#endif
        
	return_value = SES_FALSE;
      }
      else {

        didit_add1 = _add_to_list_dr(this->_the_tables2, tmp);
        if (didit_add1 == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("_combine_two_material_files: error adding data record to list in _combine_two_material_files\n");
#endif
	  _set_latest_error(SES_FUNCTION_FAIL_ERROR);
	  return SES_FALSE;
        }

        long size = _get_all_arrays_size(tmp);
        
        didit_add2 = _add_table_to_index_record(this->_the_index_record, tmp->_tid, size);
        if (didit_add2 == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("_combine_two_material_files: error adding table into to index record in _combine_two_material_files\n");
#endif
	  _set_latest_error(SES_FUNCTION_FAIL_ERROR);
	  return SES_FALSE;
        }
      }
      tmp = tmp->_next;

  }

  /*  return */

  pDRL = (struct _ses_data_record_list*)NULL;
  return return_value;
}

ses_boolean  _write_material_file(struct _ses_material_file* the_file, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

  /*  write a material file (index record and data record list ) to the c file handle */

  /*  function prototypes */

  struct _ses_index_record* _create_my_index_record(
                    struct _ses_data_record_list* ptDRL,
		    ses_material_id the_mid,
		    long date1, long date2, long vers);
		    

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */


  if (the_file == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_material_file: passed a null ses_material \n");
#endif

    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_material_file: ses file handle null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }


 
  /*  create the index record and then write the index record and data record list to the c file handle */

  if (the_file->_the_index_record != (struct _ses_index_record*)NULL) {

	_destruct_ses_index_record(the_file->_the_index_record);
        free(the_file->_the_index_record);
	the_file->_the_index_record = (struct _ses_index_record*)NULL;
  }

  /*  if ascii, sort the drl before creating the index record */

#define SORT_DRL
#ifdef SORT_DRL

  struct _ses_data_record* oop = the_file->_the_tables2->_head;
  struct _ses_data_record* next_head = oop->_next;
  struct _ses_data_record* previous = oop;

  struct _ses_data_record* inplace = the_file->_the_tables2->_head->_next;

  if (oop == (struct _ses_data_record*)NULL) {
	//  empy list, no need to sort 
  }
  else { 
	if (inplace == (struct _ses_data_record*)NULL) {
		//  one object in list, no need to sort 
	}
	else {
  		while ((inplace != (struct _ses_data_record*)NULL) && (oop != (struct _ses_data_record*)NULL) && (inplace->_tid <  oop->_tid)) {
			previous = inplace;
			inplace = inplace->_next;
		}
                if (inplace != the_file->_the_tables2->_head->_next) {
			previous->_next = oop;
			oop->_next = inplace;
			the_file->_the_tables2->_head = next_head;
		}
	}
  }


#endif

  long date1 = the_file->_the_tables2->_head->_date1;
  long date2 = the_file->_the_tables2->_head->_date2;
  long vers = the_file->_the_tables2->_head->_vers;
  the_file->_the_index_record = _create_my_index_record(the_file->_the_tables2,
		                the_file->_the_mid, date1, 
				date2, vers);
  if (the_file->_the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_material_file: index record construction error\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return SES_FALSE;
  }

  /*  add 100 table to the material file */
  int number_strings = 0;
  ses_string* table_strings = _get_user_defined_table_strings(&number_strings);
  if (table_strings == NULL) {

  }  
  else {

  	ses_boolean add_success = SES_FALSE;
        add_success = _add_100_table(the_file, table_strings, number_strings);
  	if (add_success == SES_FALSE) {
#ifdef DEBUG_PRINT
  	  printf("_write_material_file: Add new table failed\n");
#endif
	
	  int ij = 0;
	  for (ij = 0; ij < number_strings; ij++) {
		free(table_strings[ij]);
		table_strings[ij] = (ses_string)NULL;
	  }
	  free(table_strings);
	  table_strings = (ses_string*)NULL;
  	  _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

  	  return SES_FALSE;
  	}
        
  }
  int ik = 0;
  for (ik = 0; ik < number_strings; ik++) {
	free(table_strings[ik]);
	table_strings[ik] = (ses_string)NULL;
  }
  free(table_strings);
  table_strings = (ses_string*)NULL;


  ses_boolean didit_ir = _write_index_record(the_file->_the_index_record, pSFH);
  if (didit_ir == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_write_material_file: Index record failed to write\n");
#endif

    _set_latest_error(SES_WRITE_ERROR);

    /*  release memory on error */

    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_ses_index_record(the_file->_the_index_record);
    free(the_file->_the_index_record);
    the_file->_the_index_record = (struct _ses_index_record*)NULL;
    return SES_FALSE;
  }

  ses_boolean didit_drl = _write_data_record_list(the_file->_the_tables2, pSFH, nsig, do_valid);

  if (didit_drl == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_write_material_file: Data record list failed to write\n");
#endif
    _set_latest_error(SES_WRITE_ERROR);

    /*  release memory on error */

    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_ses_index_record(the_file->_the_index_record);
    free(the_file->_the_index_record);
    the_file->_the_index_record = (struct _ses_index_record*)NULL;
    return SES_FALSE;
  }

  /*  return */

  return return_value;
}


struct _ses_index_record* _create_my_index_record(struct _ses_data_record_list* the_list, 
	ses_material_id the_mid, long date1, long date2, long vers) {

  /* create an index record for the given data record list */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_create_my_index_record: data record list pointer null in _create_my_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_index_record*)NULL;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_create_my_index_record: invalid mid in _create_my_index_record\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return (struct _ses_index_record*)NULL;
  }

  /*  construct the index record */
  struct _ses_index_record* return_value =  _construct_ses_index_record_for_write(date1, date2, vers);
  if (return_value == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_create_my_index_record: construct index record fail in _create_my_index_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_index_record*)NULL;
  }

  /*  here, we're good to go */

  return_value->_mid = the_mid;


  long nrec = _get_number_on_list(the_list);
  ses_table_id* tblid = _get_tables_on_list(the_list);
  long* nwds = _get_nwds_on_list(the_list);


  long start = 5 + 3*nrec;

  long* iadr = _get_iadr_on_list(the_list, start);

  /*  NOTE:  when adding the 100 table, nrec increases by 1, so when adding tables, move the iadr up by 3 */

  ses_boolean didit_add = _add_tables(return_value, tblid,
			nwds, iadr, nrec);
  if (didit_add == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_create_my_index_record: add_tables returned false in _create_my_index_record\n");
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (struct _ses_index_record*)NULL;
  }


  /*  return */

  return return_value;
}


struct _ses_material_file* _read_material_file(ses_file_handle the_handle, unsigned int nsig, ses_boolean do_valid, long maddress) {

  struct _ses_material_file* return_value = (struct _ses_material_file*)NULL;

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  FILE* pFILE = pSFH->_c_file_handle;

  /* int fseek_return = 0; */
  /* fseek_return = */ fseek(pFILE, maddress*8, SEEK_SET);

  /*  error check the arguments */
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file: ses file handle null in _read_material_file\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_material_file*)NULL;
  }  

  if (FILE_LIST[the_handle]->_current_index_record != (struct _ses_index_record*)NULL) {
	_destruct_ses_index_record(FILE_LIST[the_handle]->_current_index_record);
	free(FILE_LIST[the_handle]->_current_index_record);
	FILE_LIST[the_handle]->_current_index_record = (struct _ses_index_record*)NULL;
  }

  long offset = maddress;

  FILE_LIST[the_handle]->_current_index_record = _read_index_record(pSFH, offset);

  if (FILE_LIST[the_handle]->_current_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file:  index record null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_material_file*)NULL;
  }

  ses_material_id tmp_mid = FILE_LIST[the_handle]->_current_index_record->_mid;
  return_value = _construct_ses_material_file(tmp_mid);

  if (return_value == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file: return value not constructed in _read_material_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_material_file*)NULL;
  }
  
  /*  assign the index record */

  if (return_value->_the_index_record != (struct _ses_index_record*)NULL) {
  	  _destruct_ses_index_record(return_value->_the_index_record);
          free(return_value->_the_index_record);
          return_value->_the_index_record = (struct _ses_index_record*)NULL;
  }
  return_value->_the_index_record = _copy_index_record(FILE_LIST[the_handle]->_current_index_record);

  return_value->_the_mid = return_value->_the_index_record->_mid;
  return_value->_number_tables = return_value->_the_index_record->_nrec;
  return_value->_next = 0;

  
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file: FILE* null _read_material_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  release memory on error exit */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_material_file(return_value);

    return (struct _ses_material_file*)NULL;
  }

  ses_table_id* the_tids = 0;
  the_tids = return_value->_the_index_record->_tblid;
  /* long* the_addresses = 0; */
  /* the_addresses = return_value->_the_index_record->_iadr; */
  /* long* the_nwords = 0; */
  /* the_nwords = return_value->_the_index_record->_nwds; */

  /*  read all the tables */

  if (return_value->_the_tables2 != (struct _ses_data_record_list*)NULL) {

	_destruct_data_record_list(return_value->_the_tables2);
	free(return_value->_the_tables2);
	return_value->_the_tables2 = (struct _ses_data_record_list*)NULL;
  }
  return_value->_the_tables2 = _construct_data_record_list();
  if (return_value->_the_tables2 == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file: the_tables not constructed in _read_material_file\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);

    /*  release memory on error exit */
    /* ses_boolean didit_destruct = SES_FALSE; */
    /* didit_destruct = */ _destruct_material_file(return_value);

 
    return (struct _ses_material_file*)NULL;


  }
 
  struct _ses_data_record* the_data_record = (struct _ses_data_record*)NULL;
  int i=0;

  long nr = 0;
  long nt = 0;
  long ntab = -1;
  ses_material_id the_mid = 0;
  ses_table_id the_tid = 0;
  /* ses_boolean didit_add = SES_FALSE; */
  int j = 0;
  int number_arrays = 0;

  /* ses_boolean didit_getit, didit_go; */

    struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
    long date1 = pSET->_date;
    long date2 = pSET->_date;
    long vers = pSET->_version;

  for (i=0; i < return_value->_number_tables; i++) {

    /*  go to the location for the data */

    the_mid = return_value->_the_mid;

    the_tid = the_tids[i];
    /* didit_go = */ pSFH->pt2_go_to_data_record(the_handle, the_mid, the_tid);

    /* didit_getit = */ _get_grid(the_handle, the_mid, 
			    the_tid, &nr, &nt, &ntab);

    the_data_record = _construct_ses_data_record( maddress, nr, nt, ntab, 
                                                  the_mid, the_tid, date1, date2, vers); 
    number_arrays = _get_number_arrays(the_data_record->_the_iterator);
    if (number_arrays <= 0) {
	    number_arrays = _get_standard_num_arrays(the_tid);
    }



    if (number_arrays <= 0) {
#ifdef DEBUG_PRINT
      printf("_read_material_file: number arrays <= 0 in _read_materail_file\n");
#endif
      _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
      return (struct _ses_material_file*)NULL;
    }

    the_data_record->_the_data = malloc(sizeof(ses_word)*number_arrays);
    if (the_data_record->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_material_file: memory error in _read_materiil_file\n");
#endif
      _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
      return (struct _ses_material_file*)NULL;
    }


    long* size = _get_array_sizes(the_data_record->_the_iterator);
    for (j = 0; j < number_arrays; j++) {
      the_data_record->_the_data[j] = _read_ses_word_array(pSFH, size[j], nsig, do_valid);
    }

    /* didit_add = */ _add_to_list_dr(return_value->_the_tables2, the_data_record);
    the_data_record = (struct _ses_data_record*)NULL;


  }

  return return_value;
}

ses_boolean _add_100_table(struct _ses_material_file* pMF, ses_string* table_strings, int num_strings) {

   ses_boolean return_value = SES_FALSE;

   /*  error check the arguments */

   if (pMF == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
   	printf("_add_100_table: null pointer passed in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
   }

   if ((table_strings == (ses_string*)NULL) || (num_strings <= 0)) {
#ifdef DEBUG_PRINT
        printf("_add_100_table: table strings null or num_strings <= 0\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
   }

   /*  100 table */
   /*  1 -- number strings */
   /*  2..  string describing tables */

  //  add table to index record

   struct _ses_index_record* ptIR = pMF->_the_index_record;

   if (ptIR == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
	printf("_add_100_table:  material file index record null\n");	
#endif
	_set_latest_error(SES_NULL_OBJECT_ERROR);
	return SES_FALSE;
   }

   //  compute the size of the new table and add an entry to the index record

   ses_table_id the_table_id = 100;
   long size_table = 0; 
   int j = 0;
   int L = 0;
   for (j=0; j < num_strings; j++) {
     if (L < strlen(table_strings[j]) + 1) {
	L = strlen(table_strings[j]) + 1;  /*  adding space after table_string */
     }
     L = (L > strlen(table_strings[j])) ? L : strlen(table_strings[j]);
   }
   int remainder = L%sizeof(ses_word);
   int padding = sizeof(ses_word) - remainder;

   L = L + padding;
   int words_in_string = (L) * num_strings / sizeof(ses_word);
   size_table = words_in_string + 1;  /*  one more word for the number of tables defined */


   ses_boolean didit = _add_100_table_to_index_record(ptIR, the_table_id, size_table);
   ses_material_id the_mid = pMF->_the_mid;


   if (didit == SES_TRUE) {

        struct _ses_data_record_list* pDRL = pMF->_the_tables2;
	if (pDRL == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
		printf("_add_100_table:  data record list null in material file\n");
#endif
		_set_latest_error(SES_NULL_OBJECT_ERROR);
		return SES_FALSE;
	}
	

	ses_boolean add_to_list = _add_100_table_to_data_record_list(pDRL, the_mid, table_strings, num_strings, size_table);
	if (add_to_list == SES_FALSE) {
#ifdef DEBUG_PRINT
		printf("_add_100_table:  add 100 table to data record list failed\n");
#endif
		_set_latest_error(SES_FUNCTION_FAIL_ERROR);
		return SES_FALSE;
	}


  	//  increment the number_of_tables held by this material file

   	pMF->_number_tables++;
        return_value = add_to_list;
	
    }



   return return_value;
}

