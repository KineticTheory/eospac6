

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

struct _ses_data_record_list* _construct_data_record_list() {

  /*  construct the data record list */

  /*  memory allocation for the return value */

  struct _ses_data_record_list* the_list = malloc(sizeof(struct _ses_data_record_list)*1);
  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_data_record_list: ses_memory_allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_data_record_list*)NULL;
  }

  /*  initiaze the data members */

  the_list->_head = (struct _ses_data_record*)NULL;

  /*  return */

  return the_list;
}

struct _ses_data_record_list*  _copy_data_record_list(struct _ses_data_record_list* the_list) {

  /*  copy the data record list */

  /*  error checking for the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_data_record_list: ses_data_record_list passed into _copy_data_record_list is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_data_record_list*)NULL;
  }

  /*  memory allocation for the return value */

  struct _ses_data_record_list* return_value = malloc(sizeof(struct _ses_data_record_list)*1);
  if (return_value == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_data_record_list: ses_data_record_list memory allocation error in _copy_data_record_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_data_record_list*)NULL;
  }

  /*  copy the data members */


  return_value->_head = (struct _ses_data_record*)NULL;
  struct _ses_data_record* tmp = the_list->_head;
  struct _ses_data_record* new;
  struct _ses_data_record* prev = (struct _ses_data_record*)NULL;
  
  while (tmp != (struct _ses_data_record*)NULL) {

    new = _copy_ses_data_record(tmp);
    if (new == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_copy_data_record_list: copy ses data record error in _copy_data_record_list\n");
#endif
      _set_latest_error(SES_OBJECT_COPY_ERROR);
      return (struct _ses_data_record_list*)NULL;
    }

    if (prev == (struct _ses_data_record*)NULL) {
      return_value->_head = new;
    }
    else {
      prev->_next = new;
    }
    prev = new;

    tmp = tmp->_next;
  }

  /*  return */

  return return_value;

}


ses_boolean _destruct_data_record_list(struct _ses_data_record_list* the_list) {

  /*  destruct the data record list */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_data_record_list: ses_data_record_list pointer passed in to _destruct_data_record_lsit is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }
  if (the_list->_head == (struct _ses_data_record*)NULL) {
    return SES_TRUE;
  }

  struct _ses_data_record* tmp = the_list->_head;
  struct _ses_data_record* tmp_next = (struct _ses_data_record*)NULL;
  ses_boolean didit_destruct = SES_FALSE;

  if (tmp != (struct _ses_data_record*)NULL) {
    tmp_next = tmp->_next;
  }

  while (tmp != (struct _ses_data_record*)NULL) {
 
  
    didit_destruct = _destruct_ses_data_record(tmp);
    if (didit_destruct == SES_TRUE) {
       free(tmp);
       tmp = (struct _ses_data_record*)NULL;
    }

    if (didit_destruct == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_destruct_data_record_list:  returned false in _destruct_mf_list\n");
#endif
      _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
      return SES_FALSE;
    }


    tmp = tmp_next;
    if (tmp != (struct _ses_data_record*)NULL) {

      tmp_next = (struct _ses_data_record*)(tmp->_next);

    }
    else {
      tmp_next = (struct _ses_data_record*)NULL;
    }

  }
  the_list->_head = (struct _ses_data_record*)NULL;


  return return_value;
}

long _get_total_table_size(struct _ses_data_record_list* the_list) {

  /*  return the total table size of the data in the list */

  long return_value = 0;

  /*  error check the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_total_table_size: ses_data_record_list pointer passed in to _get_total_table_size is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  count up the sizes in the data record list */

  struct _ses_data_record* tmp = the_list->_head;

  ses_number ntabs = 0;
  while (tmp != (struct _ses_data_record*)NULL) {

    if (tmp->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
      printf("_get_total_table_size: null iterator found in _get_total_table_size\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return 0;
    }

    return_value = return_value + _get_sum_arrays_size(tmp->_the_iterator);
    ntabs++;
    tmp = tmp->_next;
  }

 
  return_value = return_value;

  return return_value;
}



long _get_number_on_list(struct _ses_data_record_list* the_list) {

  /*  return the number of data records on the list */

  long return_value = 0;

  /*  error check the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_number_on_list: ses_data_record_list pointer passed in to _get_number_on_list is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  struct _ses_data_record* tmp = the_list->_head;

  while (tmp != (struct _ses_data_record*)NULL) {
    return_value++;
    tmp = tmp->_next;
  }

  /* return */

  return return_value;
}

ses_table_id* _get_tables_on_list(struct _ses_data_record_list* the_list) {

  /*  return the tables on the list */

  ses_table_id* return_value = 0;

  /*  error check the arguments */
  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_tables_on_list: ses_data_record_list pointer passed in to _get_tables_on_list is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_table_id*)NULL;
  }


  long size = _get_number_on_list(the_list);

  /*  allocate memory for the table array */

  return_value = malloc(sizeof(ses_table_id)*size);
  if (return_value == (ses_table_id*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_tables_on_list: memory allocation error in _get_tables_on_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_table_id*)NULL;
  }

  struct _ses_data_record* tmp = the_list->_head;

  int i=0;
  while (tmp != (struct _ses_data_record*)NULL) {
    return_value[i] = tmp->_tid;
    i++;
    tmp = tmp->_next;
  }

  /* return */

  return return_value;
}

long* _get_nwds_on_list(struct _ses_data_record_list* the_list) {
 
   /*  return an array with nwds */

   long* return_value = (long*)NULL;

   /*  error check the arguments */
  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_on_list: ses_data_record_list pointer passed in to _get_nwds_on_list is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_table_id*)NULL;
  }

  long size = _get_number_on_list(the_list);

  /*  allocate memory for the table array */


  return_value = malloc(sizeof(long)*size);
  if (return_value == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_on_list: memory allocation error in _get_nwds_on_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_table_id*)NULL;
  }

  struct _ses_data_record* tmp = the_list->_head;

  int i=0;
  while (tmp != (struct _ses_data_record*)NULL) {
    return_value[i] = _get_all_arrays_size(tmp);
    i++;
    tmp = tmp->_next;
  }

  /* return */

  return return_value;
}

long* _get_iadr_on_list(struct _ses_data_record_list* the_list, long start) {

  /*  return an array with iadr */

  long* return_value = (long*)NULL;

  /*  error check the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_on_list: ses_data_record_list pointer passed in to _get_iadr_on_list is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (ses_table_id*)NULL;
  }

  if (start < 0) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_on_list: start < 0 passed in to _get_iadr_on_list\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (ses_table_id*)NULL;
  }

  long size = _get_number_on_list(the_list);
  if (size < 0) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_on_list: size < 0 in _get_iadr_on_list\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return (ses_table_id*)NULL;
  }

  /*  allocate memory for the table array */

  return_value = malloc(sizeof(ses_table_id)*size);
  if (return_value == (ses_table_id*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_on_list: memory allocation error in _get_iadr_on_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (ses_table_id*)NULL;
  }

  struct _ses_data_record* tmp = the_list->_head;

  int i=0;
  long* array_sizes = _get_nwds_on_list(the_list);
  while (tmp != (struct _ses_data_record*)NULL) {
    if (i == 0) {
      return_value[i] = start;
    }
    else {
      return_value[i] = return_value[i-1]+ array_sizes[i-1];
    }
    i++;
    tmp = tmp->_next;
  }
  free(array_sizes);
  array_sizes = (long*)NULL;

  /* return */

  return return_value;
}


ses_boolean _add_to_list_dr(struct _ses_data_record_list* the_list, struct _ses_data_record* the_dr) {

  /*  add a data record to the data record list */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_to_list_dr: data record list passed into _add_to_list_dr is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (the_dr == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_to_list_dr: ses_data_record pointer passed in to _add_to_list_dr is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /* add to the list */

  /*  find tail */

  struct _ses_data_record* tail = the_list->_head;
  struct _ses_data_record* tail_next = (struct _ses_data_record*)NULL;

  if (tail == (struct _ses_data_record*)NULL) {
    tail_next = (struct _ses_data_record*)NULL;
  }
  else {
    tail_next = tail->_next;
  }



  while (tail_next != (struct _ses_data_record*)NULL) {
   tail = tail->_next;        
    if (tail->_next == (struct _ses_data_record*)NULL) {
        tail_next = (struct _ses_data_record*)NULL;
    }
    else {
        tail_next = tail->_next;
   }



 }
  struct _ses_data_record* new = the_dr;

  if (new == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_to_list_dr: memory allocation error (new) in add_to_list dr\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return SES_FALSE;
  }


  new->_next = (struct _ses_data_record*)NULL;

  if (tail == (struct _ses_data_record*)NULL ) {
    the_list->_head = new;
  }
  else {
    tail->_next = new;
    if (new != (struct _ses_data_record*)NULL) {
       new->_next = (struct _ses_data_record*)NULL;
    }
  }


  /*  return */
  
  return return_value;
}



ses_boolean _write_data_record_list(struct _ses_data_record_list* ptLIST, struct _ses_file_handle* pSFH, unsigned int  nsig, ses_boolean do_valid) {

  /*  write the data record list to the c file handle */

  ses_boolean return_value = SES_TRUE;


  /*  error check the arguments */

  if (ptLIST == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_data_record_list: ptLIST is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_data_record_list: null ses file handle\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  write out the data record list */

  /*  sort the drl */
#ifdef DEBUG_PRINT_LIST
	printf("printing list before sort\n");
	struct _ses_data_record* current = ptLIST->_head;
	while (current != (struct _ses_data_record*)NULL) {
		printf("current->_tid is %d\n", current->_tid); 
		current = current->_next;
	}
        printf("end of list\n");
#endif

#define SORT_DRL
#ifdef SORT_DRL

  struct _ses_data_record* oop = ptLIST->_head;
  struct _ses_data_record* next_head = oop->_next;
  struct _ses_data_record* previous = oop;

  struct _ses_data_record* inplace = ptLIST->_head->_next;

  if (oop == (struct _ses_data_record*)NULL) {
	/*  empy list, no need to sort */
  }
  else { 
	if (inplace == (struct _ses_data_record*)NULL) {
		/*  one object in list, no need to sort */
	}
	else {
  		while ((inplace != (struct _ses_data_record*)NULL) && (oop != (struct _ses_data_record*)NULL) && (inplace->_tid <  oop->_tid)) {
			previous = inplace;
			inplace = inplace->_next;
		}
                if (inplace != ptLIST->_head->_next) {
			previous->_next = oop;
			oop->_next = inplace;
			ptLIST->_head = next_head;
		}
	}
  }


#endif

#ifdef DEBUG_PRINT_LIST
	printf("printing list after sort\n");
	current = ptLIST->_head;
	while (current != (struct _ses_data_record*)NULL) {
		printf("current->_tid is %d\n", current->_tid); 
		current = current->_next;
	}
        printf("end of list\n");
#endif





  struct _ses_data_record* tmp = ptLIST->_head;

  ses_boolean didit_write = SES_FALSE;
  int first = 1;
  while (tmp != (struct _ses_data_record*)NULL) {

     tmp->_first = first;
     didit_write = _write_data_record(tmp, pSFH, nsig, do_valid);
     if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT 
      printf("_write_data_record_list: write to pFILE error\n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return SES_FALSE;
    }
    
    tmp = tmp->_next;
    first = 0;
  }
 
  /*  return */
  return return_value;
}

ses_boolean _add_100_table_to_data_record_list(struct _ses_data_record_list* the_list, ses_material_id the_mid, ses_string* table_strings, int num_strings, int num_words) {

  ses_boolean return_value = SES_FALSE;


  if (num_words > 0) {

  /*  error check the arguments */

   if (the_list == (struct _ses_data_record_list*)NULL) {
#ifdef DEBUG_PRINT
   	printf("_add_100_table_to_data_record_list: null pointer passed in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
   }

   if ((table_strings == (ses_string*)NULL) || (num_strings <= 0)) {
#ifdef DEBUG_PRINT
        printf("_add_100_table_to_data_record_list: table strings null or num_strings <= 0\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
   }

   /*  add the data for the 100 table to the data record list */

  long maddress = 0;

  /* the size of the data is the total size of the chars in all the strings */
  /*  the number of doubles is the number of chars / 8 */

  int size_buffer = (num_words) / num_strings;

  long nr = num_words;
  long nt = 1;
  long ntab = -1;
  ses_table_id the_tid = 100;

  long date1 = 4;
  long date2 = 5;
  long vers = 6;
  struct _ses_data_record* the_new_data_record = _construct_ses_data_record(maddress, nr, nt, ntab, the_mid, the_tid, date1, date2, vers);
  if (the_new_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
	printf("_add_100_table_to_data_record_list:  construct of ses data record failed\n");
#endif
	_set_latest_error(SES_FUNCTION_FAIL_ERROR);
	return SES_FALSE;
  }


  /*  put the data into the data record */

 
  union char_union {
    ses_word the_word;
    char the_char[8];
  } myUnion;

  ses_word** data_array = malloc(sizeof(ses_word*) * 2);

  data_array[0] = malloc(sizeof(ses_word) * 1);
  data_array[0][0] = (double)num_strings;

  int kindex = 0;

  data_array[1] = malloc(sizeof(ses_word) * (num_words));
  int k = 0;
  for (k = 0; k < num_strings; k++) {

        ses_word_reference the_buffer = malloc(sizeof(ses_word) * size_buffer);
        int k1 = 0;
        for (k1=0; k1 < size_buffer; k1++) {
	    the_buffer[k1] = 0.0;
        }
        ses_string the_comments = malloc(sizeof(char) *  (size_buffer*8));

	//  initialize the comments
        int i = 0;
       	for (i = 0; i < size_buffer*8; i++) {
		the_comments[i] = '\0';
	}
	strcpy(the_comments, table_strings[k]);
   
	int index = 0;
  	int jindex = 0;
  	for (i=0; i < size_buffer*8; i++) {
	  
  	  if (index == 8) {
  	    index = 0;
  	    the_buffer[jindex] = myUnion.the_word;
  	    jindex++;
	    if (the_comments[i] == '\0') {
	      myUnion.the_char[index] = '@';
	    }
	    else {
	      myUnion.the_char[index] = the_comments[i];
	    }
      	    index++;
	  }
	  else {
	    if (the_comments[i] == '\0') {
	      myUnion.the_char[index] = '@';
	    }
	    else {
	      myUnion.the_char[index] = the_comments[i];
	    }
            index++;
	  }
          
       }
       if ((k == (num_strings - 1)) && (jindex == (num_words-1))) {
	  myUnion.the_char[7] = '\0';
       }
       the_buffer[jindex] = myUnion.the_word;
       

       /*  copy the buffer into data_array */
       int p = 0;
       for (p = 0; p <= jindex; p++) {
	        data_array[1][kindex + p] = the_buffer[p];
       }
       

       kindex = kindex + num_words/num_strings;

       /*  free */
       free(the_comments);
       the_comments = (ses_string)NULL;
       free(the_buffer);
       the_buffer = (ses_word_reference)NULL;

       
  }

  the_new_data_record->_the_data = data_array;

  /*  add the data record to the data record list */

  ses_boolean didit_addit = _add_to_list_dr(the_list, the_new_data_record);
  if (didit_addit == SES_FALSE) {
#ifdef DEBUG_PRINT
	printf("_add_100_table_to_data_record_list:  fail on add data record to data record list\n");
#endif
        
	_set_latest_error(SES_FUNCTION_FAIL_ERROR);
	return SES_FALSE;
  }
  return_value = SES_TRUE;

  }
  else {
	return_value = SES_FALSE;
  }



  return return_value;
}




