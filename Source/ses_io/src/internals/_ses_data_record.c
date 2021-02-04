


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#define check_errors_construct_ses_data_record HEADER(check_errors_construct_ses_data_record)
#define check_errors_copy_ses_data_record HEADER(check_errors_copy_ses_data_record)
#define my_copy_ses_data HEADER(my_copy_ses_data)
#define my_destruct_ses_data HEADER(my_destruct_ses_data)
#define x HEADER(x)

#undef DEBUG_PRINT

struct _ses_data_record*  _construct_ses_data_record(long maddress, long nr, long nt, long ntab, ses_material_id the_mid, ses_table_id the_tid, long date1, long date2, long vers) {

  /*  function prototypes */

  ses_error_flag check_errors_construct_ses_data_record(long maddress, long nr, long nt, ses_material_id the_mid, ses_table_id the_tid);

  /*  end function prototypes */

  ses_error_flag check = check_errors_construct_ses_data_record(maddress, nr, nt, the_mid, the_tid);
  if (check != SES_NO_ERROR) {
	return (struct _ses_data_record*)NULL;
  }


  /*  construct the current data record */

#ifdef DEBUG_PRINT
  printf ("\n_construct_ses_data_record: nr: %ld, nt: %ld, ntab: %ld, mid: %ld, tid: %d\n", nr, nt, ntab, the_mid, the_tid);
  printf ("_construct_ses_data_record: date1: %ld, date2: %ld, vers: %ld\n", date1, date2, vers);
#endif


  struct _ses_data_record* return_value =  malloc(sizeof(struct _ses_data_record)*1);
  if (return_value == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: malloc error in _construct_ses_data_record\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_data_record*)NULL;
  }
 /*  set the rest of the data members */

  return_value->_mid = the_mid;
  return_value->_tid = the_tid;
  return_value->_the_data = (ses_word_reference*)NULL;
  return_value->_next = (struct _ses_data_record*)NULL;
  return_value->_has_data = SES_FALSE;

  return_value->_date1 = date1;
  return_value->_date2 = date2;
  return_value->_vers = vers;
  
  return_value->_first = 0;

  /*  construct the data record iterator */


  return_value->_the_iterator = _construct_ses_iterator(maddress, nr, nt, ntab, the_tid);
  if (return_value->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: iterator construction error in _construct_ses_data_record \n");
#endif

    free(return_value);
    return_value = (struct _ses_data_record*)NULL;
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_data_record*)NULL;
  }

  /*  put the standard sizes, etc into the iterator  */


  /*******************************************/

  /* ses_boolean didit_fill = SES_FALSE; */
  /* didit_fill = */ _fill_iterator_size_and_label_arrays(return_value->_the_iterator, the_tid);

  /*******************************************/
  
  /*  set the rest of the data members */


  return_value->_mid = the_mid;
  return_value->_tid = the_tid;
  return_value->_the_data = (ses_word_reference*)NULL;
  return_value->_next = (struct _ses_data_record*)NULL;
  return_value->_has_data = SES_FALSE;

  /*  return */
  return return_value;

}


struct _ses_data_record* _copy_ses_data_record(struct _ses_data_record* the_dr) {

  /*  function prototypes */

  ses_word_reference* my_copy_ses_data(ses_word_reference* the_data, unsigned int the_size, long* size_arrays);
  ses_error_flag check_errors_copy_ses_data_record(struct _ses_data_record* the_dr);

  /*  end function prototypes */

  ses_error_flag check = check_errors_copy_ses_data_record(the_dr);
  if (check != SES_NO_ERROR) {
	return (struct _ses_data_record*)NULL;
  }



  struct _ses_data_record* return_value = malloc(sizeof(struct _ses_data_record)*1);

  /*  allocate return value memory */

  if (return_value == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_data_record: construction error for ses data record error in _copy_ses_data_record\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_data_record*)NULL;
  }

  /*  copy the data members */

  return_value->_mid = the_dr->_mid;
  return_value->_tid = the_dr->_tid;


  return_value->_the_iterator = _copy_ses_iterator(the_dr->_the_iterator);
  long number_arrays = _get_number_arrays(the_dr->_the_iterator);

  long* size_arrays =  _get_array_sizes(the_dr->_the_iterator);
  if (the_dr->_the_data == (ses_word_reference*)NULL) {
    return_value->_the_data = (ses_word_reference*)NULL;
    return_value->_has_data = SES_FALSE;
  }
  else {
    return_value->_the_data = my_copy_ses_data(the_dr->_the_data, number_arrays, size_arrays);
    return_value->_has_data = SES_TRUE;
  }

  return_value->_date1 = the_dr->_date1;
  return_value->_date2 = the_dr->_date2;
  return_value->_vers = the_dr->_vers;
  return_value->_first = the_dr->_first;

  return_value->_next = (struct _ses_data_record*)NULL;

  /*  return */
  
  return return_value;
}


ses_boolean _destruct_ses_data_record(struct _ses_data_record* the_data_record) {
  
  /*  destruct the ses_data_record object */

  /*  function prototypes */

  ses_boolean my_destruct_ses_data(ses_word_reference* the_data, long the_size);

  /*  end function prototypes */


  ses_boolean return_value = SES_FALSE;
  if (the_data_record == (struct _ses_data_record*)NULL) {
    return SES_TRUE;
  }

  the_data_record->_mid = 0;
  the_data_record->_tid = 0;

  /*  destruct the iterator */


  long the_size = 0;
  if (the_data_record != (struct _ses_data_record*)NULL) {
  	if (the_data_record->_the_iterator == (struct _ses_iterator*)NULL) {
  		return_value = SES_TRUE;
  	}
	else {
        	the_size = (the_data_record->_the_iterator)->_number_arrays;

                ses_boolean didit_destruct1 = _destruct_ses_iterator(the_data_record->_the_iterator);
                if (didit_destruct1 == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_destruct_ses_data_record: destruct iterator failed in destruct data record \n");
#endif
                   _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
                   return_value = SES_FALSE;
		   return return_value;
                }
                free(the_data_record->_the_iterator);
                the_data_record->_the_iterator = (struct _ses_iterator*)NULL;
  
	}
  }

  /*  destruct the data */

  if (the_data_record->_the_data != (ses_word_reference*)NULL) {
    ses_boolean didit_destruct2 = my_destruct_ses_data(the_data_record->_the_data, the_size);
    if (didit_destruct2 == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("destruct_ses_data_record:  failed in destruct data record \n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return_value = SES_FALSE;
      return return_value;
    }
    free(the_data_record->_the_data);
    the_data_record->_the_data = (ses_word_reference*)NULL;
    the_data_record->_has_data = SES_FALSE;
  }

  return_value = SES_TRUE;

  /*  unlink the "_next" value */

  /*  should I delete here?  -- no this is the data record, not the data record list */

  the_data_record->_next = (struct _ses_data_record*)NULL;  

  return return_value;
}


struct _ses_iterator*  _get_iterator(struct _ses_data_record* the_data_record) {

  /*  return the iterator */

  /*  error check the arguments */
	
  if (the_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iterator: null data record passed into _get_iterator\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_iterator*)NULL;
  }

  /*  return */

  struct _ses_iterator* return_value = the_data_record->_the_iterator;
  return return_value;

}

long  _get_all_arrays_size(struct _ses_data_record* the_record) {

  /*  return the total size of all the data */
 
  long return_value = 0;

  /*  error checking the arguments */

  if (the_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_all_arrays_size: null data record passed to _get_all_arrays_size\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  struct _ses_iterator* pIT = the_record->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_all_arrays_size: null iterator in data record in _get_all_arras_size\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  return */

  return_value = _get_sum_arrays_size(pIT);
  return return_value;

}


ses_word_reference  _get_data_dr(struct _ses_data_record* pDR, ses_label the_label) {

  /*  return any existing data in the data record  with the same label as the argument */

  ses_word_reference return_value = (ses_word_reference)NULL;

  /*  error checking the arguments */

  if (pDR == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_data_dr: null data record passed to _get_data_dr\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference)NULL;
  }

  if (the_label == (ses_label)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_data_dr: null label passed to _get_data_dr\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference)NULL;
  }

  /*  check internal structures */

  if (pDR->_has_data == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_data_dr:  no data in data record \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (ses_word_reference)NULL;

  }
  struct _ses_iterator* pIT = pDR->_the_iterator;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_data_dr: null iterator in data record passed to _get_data_dr\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference)NULL;
  }

  /*  find the index with the_label */

  int i=0;
  int index = -1;
  for (i=0; i < pIT->_number_arrays; i++) {
    if (pIT->_number_arrays == 1) {
      if (strcmp(pIT->_label_arrays[i], the_label) == 0) {
        index = i;
      }
    }
    else {
      if (strcmp(pIT->_label_arrays[i], the_label) == 0) {
        index = i;
      }
    }
  }

  /*  index error checking */

  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_data_dr: Could not find %s in iterator in _get_data_dr\n", the_label);
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return (ses_word_reference)NULL;
  }

  /*  data error checking */

  if (pDR->_the_data[index] == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_data_dr: data null in _get_data_dr\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return (ses_word_reference)NULL;
  }

  /*  retrieve the data */

  long data_size = pIT->_size_arrays[index];

  return_value = malloc(sizeof(ses_word)*data_size);
  if (return_value == (ses_word_reference)NULL){
#ifdef DEBUG_PRINT
    printf("_get_data_dr: memory allocation error in _get_data_dr\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_word_reference)NULL;
  }

  for (i=0; i<data_size; i++) {
    return_value[i] = pDR->_the_data[index][i];
  }

  /*  return */  

  return return_value;

}

ses_boolean _write_data_record(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH,  unsigned int nsig, ses_boolean do_valid) {

    ses_boolean return_value = SES_TRUE;


    if (pSFH->pt2_write_data_record == NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record:  function pointer null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_FALSE;
    }
    
    ses_error_flag didit_write_error = pSFH->pt2_write_data_record(tmp, pSFH, nsig, do_valid);
    if (didit_write_error != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
      printf("_write_data_record:  write data record function failed\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_FALSE;
    }
    return return_value;

}

/*----------------------*/

ses_word_reference* my_copy_ses_data(ses_word_reference* the_data, unsigned int the_size, long* size_arrays) {
 
  /*  copy the_data into a return set of data */

  /*  argument error checking */

  if (the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_copy_ses_data: null data pointer in _copy_ses_data \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_word_reference*)NULL;
  }

  /*  memory allocation for return value */

  ses_word_reference* return_value = malloc(sizeof(ses_word_reference)*the_size);
  if (return_value == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
    printf("my_copy_ses_data: memory allocation error in _copy_ses_data\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
     return (ses_word_reference*)NULL;
  }

  int i=0;
  int j = 0;

  /*  the_size is the number of arrays */
  for(i=0; i < the_size; i++) {

    if (the_data[i] == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
      printf("my_copy_ses_data: data array null in _copy_ses_data\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return (ses_word_reference*)NULL;
    }

      return_value[i] = malloc(sizeof(ses_word)*size_arrays[i]);
      if (return_value[i] == (ses_word_reference)NULL) {
#ifdef DEBUG_PRINT
        printf("my_copy_ses_data: memory allocation error of return_value[%d] in _copy_ses_\n", i);
#endif
	_set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
	return (ses_word_reference*)NULL;
      }


      ses_word_reference the_d = the_data[i];

      for (j=0; j < size_arrays[i]; j++) {
	return_value[i][j] = the_d[j];
      }
	  
  }

  return return_value;
}

ses_boolean my_destruct_ses_data(ses_word_reference* the_data, long  the_size) {

  /*  destruct the data */

  /*  error checking the arguments */
 
  ses_boolean return_value = SES_TRUE;

  if (the_data != (ses_word_reference*)NULL) {

    /*  destruct the data */

    unsigned int i=0;
    for (i=0; i < the_size; i++) {
      if (the_data[i] != (ses_word_reference)NULL) {
         free(the_data[i]);
         the_data[i] = (ses_word_reference)NULL;
      }
    }

  }

  /*  return */

  return return_value;

}


//************************************ error checking routines ***************************//

ses_error_flag check_errors_construct_ses_data_record(long maddress, long nr, long nt, ses_material_id the_mid, ses_table_id the_tid) {
  /*  error check the arguments */
 
  if (maddress < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: maddress < 0 in construct_ses_data_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  if (nr < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: nr < 0 in construct_ses_data_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  if (nt < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: nt < 0 in construct_ses_data_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: invalid mid in construct_ses_data_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_data_record: invalid tid = %d in construct_ses_data_record\n", (int)the_tid);
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}

ses_error_flag check_errors_copy_ses_data_record(struct _ses_data_record* the_dr) {

  /*  argument error checking */


  if (the_dr == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_data_record: null data record passed into _copy_ses_data_record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_dr->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_data_record: null iterator in copy ses data record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}





 





