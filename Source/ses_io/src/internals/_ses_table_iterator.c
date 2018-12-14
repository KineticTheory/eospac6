#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#define check_errors_construct_ses_iterator HEADER(check_errors_construct_ses_iterator)
#define check_errors_copy_ses_iterator HEADER(check_errors_copy_ses_iterator)

struct _ses_iterator* _construct_ses_iterator(long maddress, long nr, long nt, long ntab, ses_table_id the_tid) {

  /*  this function instantiates the iterator and passes it back */

  /*  function prototypes */

  ses_error_flag check_errors_construct_ses_iterator(long maddress, long nr, long nt, long ntab, ses_table_id the_tid);
  long my_adjust_num_arrays(ses_table_id tid, long ntab);

  /*  end function prototypes */

  ses_error_flag check = check_errors_construct_ses_iterator(maddress, nr, nt, ntab, the_tid);
  if (check != SES_NO_ERROR) {
	return (struct _ses_iterator*)NULL;
  }

  /*  create the return value */

  struct _ses_iterator* the_iterator = malloc(sizeof(struct _ses_iterator)*1);
  if (the_iterator == (struct _ses_iterator*)NULL) {
    /*  upon memory allocation error */
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: in _construct_iterator memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_iterator*)NULL;
  }

  //  Set the iterator data

  /*  from the set of standard data for the table, set the size of the table */

  the_iterator->_number_arrays = 0;
  long num_arrays = _get_standard_num_arrays(the_tid);

  //  adjust the number of arrays based on nr, nt, ntab

  if (ntab > 0) {
    	num_arrays = my_adjust_num_arrays(the_tid, ntab);
	
  }

  //  error check the number of arrays 

  if (num_arrays <= 0) {
    /*  num_arrays is not appropriate */
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: in _construct_iterator incorrect number arrays = %ld for table id %ld\n", num_arrays, the_tid);
#endif
   free(the_iterator);
   the_iterator = (struct _ses_iterator*)NULL;
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (struct _ses_iterator*)NULL;
  }

  //  set the number of arrays in the iterator

  the_iterator->_number_arrays = num_arrays;

  /*  set the 'next' array to the first array on the table */

  the_iterator->_current_array = 0;

  /*  get memory for the address, size, and label arrays */

  the_iterator->_address_arrays = malloc(sizeof(long)*num_arrays);
  if (the_iterator->_address_arrays == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: memory allocation error address_arrays\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_iterator(the_iterator);

    return (struct _ses_iterator*)NULL;
  }


  the_iterator->_size_arrays = malloc(sizeof(long)*num_arrays);
  if (the_iterator->_size_arrays == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: memory allocation error size_arrays\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_iterator(the_iterator);

    return (struct _ses_iterator*)NULL;
  }

  the_iterator->_label_arrays = malloc(sizeof(ses_label)*num_arrays);
  if (the_iterator->_label_arrays == (ses_label*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: memory allocation error label_arrays\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    ses_boolean didit_destruct = SES_FALSE;
    didit_destruct = _destruct_ses_iterator(the_iterator);

    return (struct _ses_iterator*)NULL;
  }

  the_iterator->_maddress = maddress;

  the_iterator->_size = 0;
  the_iterator->_nr = nr;
  the_iterator->_nt = nt;
  the_iterator->_ntab = ntab;

  int i = 0;
  for (i = 0; i < num_arrays; i++) {
	the_iterator->_size_arrays[i] = 0;
        the_iterator->_address_arrays[i] = 0;
        the_iterator->_label_arrays[i] = (ses_label)NULL;
  }

  the_iterator->_tid = the_tid;

  /*  return the iterator */

  return the_iterator;

}


struct _ses_iterator* _copy_ses_iterator(struct _ses_iterator* pIT) {

  /*  copy the iterator */

  /*  function prototypes */

  ses_error_flag check_errors_copy_ses_iterator(struct _ses_iterator* pIT);

  /*  end function prototypes */

  ses_error_flag check = check_errors_copy_ses_iterator(pIT);
  if (check != SES_NO_ERROR) {
	return (struct _ses_iterator*)NULL;
  }

  //  create return value

  struct _ses_iterator* return_value = (struct _ses_iterator*)NULL;
  return_value = _construct_ses_iterator(pIT->_maddress, pIT->_nr, pIT->_nt, pIT->_ntab, pIT->_tid);

  if (return_value == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_iterator: construction failed in _copy_ses_iterator\n");
#endif
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_iterator*)NULL;
  }

  //  copy values

  ses_boolean didit_destruct = SES_FALSE;

  return_value->_maddress = pIT->_maddress;
  return_value->_current_array = pIT->_current_array;
  return_value->_number_arrays = pIT->_number_arrays;

  if (return_value->_address_arrays != (long*)NULL) {
	free(return_value->_address_arrays);
        return_value->_address_arrays = (long*)NULL;
  }
  return_value->_address_arrays = malloc(sizeof(long)*return_value->_number_arrays);

  if (return_value->_address_arrays == (long*)NULL) {

    /*  upon memory allocation error */
#ifdef DEBUG_PRINT
    printf("_copy_ses_iterator: in _copy_iterator address_arrays memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    didit_destruct = _destruct_ses_iterator(return_value);
    free(return_value);
    return_value = (struct _ses_iterator*)NULL;
  
    return (struct _ses_iterator*)NULL;
  }

  if (return_value->_size_arrays != (long*)NULL) {
	free(return_value->_size_arrays);
        return_value->_size_arrays = (long*)NULL;
  }
  return_value->_size_arrays = malloc(sizeof(long)*return_value->_number_arrays);

  if (return_value->_size_arrays == (long*)NULL) {
    /*  upon memory allocation error */
#ifdef DEBUG_PRINT
    printf("_copy_ses_iterator: in _copy_iterator size_arrays memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    didit_destruct = _destruct_ses_iterator(return_value);
    free(return_value);
    return_value = (struct _ses_iterator*)NULL;

    return (struct _ses_iterator*)NULL;
  }

  if (return_value->_label_arrays != (ses_label*)NULL) {
        int i = 0;
	for (i = 0; i < return_value->_number_arrays; i++) {
		if (return_value->_label_arrays[i] != (ses_label)NULL) {
			free(return_value->_label_arrays[i]);
			return_value->_label_arrays[i] = (ses_label)NULL;
		}
	}
        free(return_value->_label_arrays);
	return_value->_label_arrays = (ses_label*)NULL;
  }
  return_value->_label_arrays = malloc(sizeof(ses_label)*return_value->_number_arrays);

  if (return_value->_label_arrays == (ses_label*)NULL) {
    /*  upon memory allocation error */
#ifdef DEBUG_PRINT
    printf("_copy_ses_iterator: in _copy_iterator label_arrays memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

    /*  release memory upon error */
    didit_destruct = _destruct_ses_iterator(return_value);
    free(return_value);
    return_value = (struct _ses_iterator*)NULL;

    return (struct _ses_iterator*)NULL;
  }

  //  copy data into the arrays

  int i=0;
  int size=0;
  for(i=0; i< return_value->_number_arrays; i++) {

    return_value->_address_arrays[i] = pIT->_address_arrays[i];
    return_value->_size_arrays[i] = pIT->_size_arrays[i];
    if (pIT->_label_arrays[i] != NULL) {
      size = strlen(pIT->_label_arrays[i]);
    }
    else {
      size = 0;
    }
          
    if (size <= 0) {
      return_value->_label_arrays[i] = (char*)NULL;   
    }
    else {
      return_value->_label_arrays[i] = malloc(sizeof(char)*(size + 1));
      if (return_value->_label_arrays[i] == (char*)NULL) {
#ifdef DEBUG_PRINT
        printf("_copy_ses_iterator: memory allocation error in copy ses iterator \n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);

        /*  release memory upon error */
        didit_destruct = _destruct_ses_iterator(return_value);
        free(return_value);
        return_value = (struct _ses_iterator*)NULL;

        return (struct _ses_iterator*)NULL;
      }

      strcpy(return_value->_label_arrays[i], pIT->_label_arrays[i]);
    }
  }

  return_value->_size = pIT->_size;
  return_value->_nr = pIT->_nr;
  return_value->_nt = pIT->_nt;
  return_value->_ntab = pIT->_ntab;
  return_value->_tid = pIT->_tid;


  return return_value;
}


ses_boolean  _destruct_ses_iterator(struct _ses_iterator* pIT) {

  /*  Free up the memory in the iterator */

  ses_boolean return_value = SES_FALSE;

  /*  error check the arguments */

  if (pIT == (struct _ses_iterator*)NULL) {
    /*  error, trying to free a NULL pointer */
#ifdef DEBUG_PRINT
    printf("_destruct_ses_iterator: NULL pointer passed to _destruct_iterator\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  free pointers and set to null */
  
  if (pIT->_address_arrays != (long*)NULL) {
       free(pIT->_address_arrays);
       pIT->_address_arrays = (long*)NULL;
  }

  if (pIT->_size_arrays != (long*)NULL) {

       free(pIT->_size_arrays);
       pIT->_size_arrays = (long*)NULL;
  }

  int ip=0; 
  if ((pIT->_number_arrays > 0) && (pIT->_label_arrays != (ses_label*)NULL)) {
      for (ip=0; ip < pIT->_number_arrays; ip++) {
	if (pIT->_label_arrays[ip] != (ses_label)NULL) {
           free(pIT->_label_arrays[ip]);
           pIT->_label_arrays[ip] = (ses_label)NULL;
	}
      }
  }
   
  if (pIT->_label_arrays != (ses_label*)NULL) {
        free(pIT->_label_arrays);
        pIT->_label_arrays = (ses_label*)NULL;
  }

  return_value = SES_TRUE;
  return return_value;
}

ses_boolean _is_valid_iterator(struct _ses_iterator* pIT) {

  /*  return whether the iterator is valid */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_is_valid_iterator: pIT null in _is_valid_iterator\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  return */

  return return_value;
}


long _get_number_arrays(struct _ses_iterator* ptIT) {

  /*  return the number of arrays */

  /*  error check the arguments */

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_number_arrays: null iterator pointer passed to _get_number_arrays\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }
  /*  return */

  return ptIT->_number_arrays;
}

long _get_array_size(struct _ses_iterator* ptIT, long index) {

  /*  return the size of the data at the index */

  /*  error check the arguments */

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_array_size: null iterator pointer passed to _get_number_arrays\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }  

  if (index < 0) {
#ifdef DEBUG_PRINT
    printf("_get_array_size: index passed to _get_array_size < 0\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return 0;
  }

  if (index >= ptIT->_number_arrays) {
#ifdef DEBUG_PRINT
    printf("_get_array_size: index passed to _get_array_size >=\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return 0;
  }

  /*  return */

  return ptIT->_size_arrays[index];
}

long* _get_array_sizes(struct _ses_iterator* ptIT) {

  /*  return the array sizes */

  /*  error check the arguments */

  long* return_value = (long*)NULL;

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_array_sizes: null iterator pointer passed in\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (long*)NULL;
  }

  /*  return */

  return_value = ptIT->_size_arrays;

  return return_value;
}



long  _get_sum_arrays_size(struct _ses_iterator* pIT) {

  /*  get the size of all the arrays combined in the iterator */

  /*  error check the arguments */

  long return_value = 0;
  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_sum_arrays_size: null iterator pointer pased to _get_sum_arrays_size\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  error check the internal data members */

  if (pIT->_size_arrays == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_sum_arrays_size: null _size_arrays  pased to _get_sum_arrays_size\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return 0;
  }

  /*  compute and return */

  int i = 0;
  for (i=0; i < pIT->_number_arrays; i++) {
    return_value = return_value + pIT->_size_arrays[i];
  }
  return return_value;
}

long  _get_maddress(struct _ses_iterator* ptIT) {

  /* return maddress for the iterator */

  /*  error check the arguments */

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_maddress: null iterator pointer passed to _get_maddress\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  return */

  return ptIT->_maddress;

}

long _get_array_size_with_label(struct _ses_iterator* pIT, ses_label the_label)
{

  /*  return the size of the array with the given label */

  long return_value = 0;

  /*  error check the arguments */


  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_array_size_with_label: null iterator pointer pased to _get_array_size_with_label\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  if (the_label == (ses_label)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_array_size_with_label: null label passed to _get_array_size_with_label\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /*  find the given label and return */

  int i = 0;
  for (i = 0; i < pIT->_number_arrays; i++) {
    if (strcmp(pIT->_label_arrays[i], the_label) == 0) {
      return_value = pIT->_size_arrays[i];
      break;
    }
  }

  return return_value;
}


ses_label _get_current_label(struct _ses_iterator* ptIT) {

  /*  return the label for the 'current' array in the iterator */

  /*  error check for arguments */

  /****************************************************************/

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_current_label: null iterator pointer pased to _get_current_label\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return 0;
  }

  /****************************************************************/
  /*  return */

  return ptIT->_label_arrays[ptIT->_current_array];
}


ses_boolean _set_current_label(struct _ses_iterator* ptIT, ses_label the_label) {

  /*  set the label for the 'current' array in the iterator */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (ptIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_current_label: null iterator pointer pased to _set_current_label\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (ptIT->_label_arrays[ptIT->_current_array] != (ses_label)NULL) {
	free(ptIT->_label_arrays[ptIT->_current_array]);
	ptIT->_label_arrays[ptIT->_current_array] = (ses_label)NULL;
  }

  ptIT->_label_arrays[ptIT->_current_array] = malloc(sizeof(char)*SES_MAX_LABEL_SIZE);
  if (ptIT->_label_arrays[ptIT->_current_array] == (ses_label)NULL) {
#ifdef DEBUG_PRINT
    printf("_set_current_label: memory error in  _set_current_label\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;

  }
  else {
  	strcpy(ptIT->_label_arrays[ptIT->_current_array],the_label);
  }
  
  return return_value;
}




ses_boolean _initialize_ses_iterator(struct _ses_iterator* pIT, ses_file_handle the_handle,
                                     ses_table_id the_tid) {

  /*  initialize data in the iterator with standard data for the table */

  ses_boolean return_value = SES_FALSE;

  /*  function prototypes */

  long        my_adjust_num_arrays(ses_table_id tid, long ntab);
  ses_boolean my_adjust_size_and_address_arrays(struct _ses_iterator* ptIT, ses_table_id tid, long ntab);

  /*  end function prototypes */


  /*  error check the arguments */

  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_initialize_ses_iterator: initialize iterator error, passed in a NULL pointer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  
  pIT->_current_array = 0;

  pIT->_number_arrays = _get_standard_num_arrays(the_tid);

  //  adjust the number of arrays based on ntab

  long ntab = pIT->_ntab;
  pIT->_number_arrays = my_adjust_num_arrays(the_tid, ntab);
  ses_boolean didit_adjust = my_adjust_size_and_address_arrays(pIT, the_tid, ntab);
  if (didit_adjust == SES_FALSE) {
	_set_latest_error(SES_FUNCTION_FAIL_ERROR);
	return SES_FALSE;
  }
  ses_boolean didit_fill = _fill_iterator_size_and_label_arrays(pIT, the_tid);

  if (didit_fill == SES_FALSE) {
	_set_latest_error(SES_FUNCTION_FAIL_ERROR);
	return SES_FALSE;
  }


  if (pIT->_number_arrays <= 0) {
      /*  num_arrays is not appropriate */
#ifdef DEBUG_PRINT
      printf("_initialize_ses_iterator: incorrect number arrays= %ld for table id %ld\n", pIT->_number_arrays, the_tid);
#endif
      _set_latest_error(SES_OBJECT_READY_ERROR);
      return SES_FALSE;
  }

  //  for the 100 table -- special table

  if (the_tid == 100) {

      if (pIT->_number_arrays != 2) {
#ifdef DEBUG_PRINT
	printf("_initialize_ses_iterator: index record and iterator mismatch for the 100 table \n");
#endif
	_set_latest_error(SES_OBJECT_READY_ERROR);
	return SES_FALSE;
      
      }

      /*  put the information for sizes that is in the current index record into the iterator */

      struct _ses_index_record* pIR = FILE_LIST[the_handle]->_current_index_record;

      //  create memory for the size_arrays

      if (pIT->_size_arrays == (long*)NULL) {
	pIT->_size_arrays = malloc(sizeof(long) * pIT->_number_arrays);
      }

      //  find the index for the 100 table in the current endex record

      int i = 0;
      for (i = 0; i < pIR->_nrec; i++) {
	if (pIR->_tblid[i] == 100)
	  break;
      }

      //  put data into the size arrays

      pIT->_size_arrays[0] = 1;   
      pIT->_size_arrays[1] = pIR->_nwds[i] - 1; /* NOTE: This -1 is also in _ses_standard.c::_get_standard_sizes */

  }


  return_value = SES_TRUE;

  return return_value;
}

long       my_adjust_num_arrays(ses_table_id the_tid, long ntab) {
        long return_value = _get_standard_num_arrays(the_tid);
	if (ntab > 0) {
		if (the_tid == 321) {
			return_value = 4 + ntab;
		}
		if (the_tid == 401) {
			return_value = ntab + 1;
		}
		if (the_tid == 411) {
			return_value = ntab + 3;
		}
		if (the_tid == 412) {
			return_value = ntab + 3;
		}
		if (the_tid == 431) {
			return_value = ntab + 3;
		}

	}
	return return_value;
}

ses_boolean my_adjust_size_and_address_arrays(struct _ses_iterator* pIT, ses_table_id tid, long ntab) {

	long old_arrays = pIT->_number_arrays;
	long standard_arrays = _get_standard_num_arrays(tid);
	long narrays = my_adjust_num_arrays(tid, ntab);
	if (standard_arrays != narrays) {
		//  adjust the size and address arrays in the iterator

		pIT->_size_arrays = (long*)realloc(pIT->_size_arrays, narrays * sizeof(long));
		pIT->_address_arrays = (long*)realloc(pIT->_address_arrays, narrays * sizeof(long));
		
		int i = 0;
		for (i = 0; i < old_arrays; i++) {
			if (pIT->_label_arrays[i] != (ses_label)NULL) {
				free(pIT->_label_arrays[i]);
				pIT->_label_arrays[i] = (ses_label)NULL;
			}
		}
		pIT->_label_arrays = (ses_label*)realloc(pIT->_label_arrays, narrays * sizeof(char*));
					
		//int i = 0;
		for (i=0; i < narrays; i++) {
			pIT->_size_arrays[i] = 0;
			pIT->_address_arrays[i] = 0;
			pIT->_label_arrays[i] = (ses_label)NULL;
		}
	}
	
	return SES_TRUE;
}

ses_boolean _fill_iterator_size_and_label_arrays(struct _ses_iterator* pIT, ses_table_id the_tid) {

	long standard_arrays = _get_standard_num_arrays(the_tid);
	long narrays = pIT->_number_arrays;

	long* standard_sizes = _get_standard_sizes(the_tid, pIT->_nr, pIT->_nt, pIT->_ntab);
	long* standard_addresses = _get_standard_relative_addresses( the_tid,  pIT->_nr, pIT->_nt, pIT->_ntab);
        ses_label* standard_labels =  _get_standard_labels(the_tid);
	
	if (narrays < standard_arrays) {
			int i = 0;
			for (i=0; i < narrays; i++) {
				pIT->_size_arrays[i] = standard_sizes[i];
				pIT->_address_arrays[i] = standard_addresses[i];
				if (pIT->_label_arrays[i] == (ses_label)NULL) {
					pIT->_label_arrays[i] = malloc(sizeof(char) * SES_MAX_LABEL_SIZE);
				}
				strcpy(pIT->_label_arrays[i], standard_labels[i]);
				
			}
	}
	else {
			int i = 0;
			for (i=0; i < standard_arrays; i++) {
				pIT->_size_arrays[i] = standard_sizes[i];
				pIT->_address_arrays[i] = standard_addresses[i];
				if (pIT->_label_arrays[i] == (ses_label)NULL) {
					pIT->_label_arrays[i] = malloc(sizeof(char) * SES_MAX_LABEL_SIZE);
				}
				strcpy(pIT->_label_arrays[i], standard_labels[i]);
			}
			for (i=standard_arrays; i < narrays; i++) {
				pIT->_size_arrays[i] = standard_sizes[standard_arrays-1];
				pIT->_address_arrays[i] = pIT->_address_arrays[i-1] + 8*pIT->_size_arrays[i-1];
				if (pIT->_label_arrays[i] == (ses_label)NULL) {
					pIT->_label_arrays[i] = malloc(sizeof(char) * SES_MAX_LABEL_SIZE);
				}
				strcpy(pIT->_label_arrays[i], "no label");

			}
	}

        int j = 0;
	for (j= 0; j < standard_arrays; j++) {
		free(standard_labels[j]);
		standard_labels[j] = (ses_label)NULL;
	}
	free(standard_labels);
	standard_labels = (ses_label*)NULL;
	free(standard_sizes);
	standard_sizes = (long*)NULL;
	free(standard_addresses);
	standard_addresses = (long*)NULL;

	return SES_TRUE;
}


//--------------------------------------------------------------------------------
ses_error_flag check_errors_construct_ses_iterator(long maddress, long nr, long nt, long ntab, ses_table_id the_tid) {
  /***************************************************************************/
  if (maddress < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: maddress < 0 in _construct_ses_iterator\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (nr < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: nr < 0 in _construct_ses_iterator\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (nt < 0) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: nt < 0 in _construct_ses_iterator\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_ses_iterator: invalid table id < 0 in _construct_ses_iterator\n");
#endif
    _set_latest_error(SES_INVALID_TID);
    return SES_INVALID_TID;
  }
  /**************************************************************/


  return SES_NO_ERROR;

}

ses_error_flag check_errors_copy_ses_iterator(struct _ses_iterator* pIT) {

  /*  error check the arguments */
  /**************************************************************/

  if (pIT == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_ses_iterator: null iterator pointer passed to _copy_ses_iterator\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  /**************************************************************/

  return SES_NO_ERROR;
}




