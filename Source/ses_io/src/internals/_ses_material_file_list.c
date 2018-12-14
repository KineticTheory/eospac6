
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <stdlib.h>

#define check_errors_mfl HEADER(check_errors_mfl)
#define check_errors_does_material_exist_in_list HEADER(check_errors_does_material_exist_in_list)
#define check_errors_get_iadr_from_list HEADER(check_errors_get_iadr_from_list)
#define my_count_files HEADER(my_count_files)
#define check_errors_add_combine_list HEADER(check_errors_add_combine_list)

#undef DEBUG_SORT


/*****************************************************************/
struct _ses_material_file_list* _construct_material_file_list() {

  /*  construct a ses material file list */

  /*  allocate memory for the return */

  struct _ses_material_file_list* return_value = malloc(sizeof(struct _ses_material_file_list)*1);
  if (return_value == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_material_file_list: memory allocation error in _construct_material_file_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_material_file_list*)NULL;
  }

  /*  return */

  return_value->_head = (struct _ses_material_file*)NULL;

  return return_value;
}
/*****************************************************************/
struct _ses_material_file_list* _copy_material_file_list(struct _ses_material_file_list* the_list) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(the_list);
  if (errors != SES_NO_ERROR) {
	return (struct _ses_material_file_list*)NULL;
  }

  /*  copy the material file list */

  /*  allocate memory for the return */

  struct _ses_material_file_list* return_value = malloc(sizeof(struct _ses_material_file_list)*1);
  if (return_value == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_copy_material_file_list: memory allocation error in _copy_material_file_list \n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (struct _ses_material_file_list*)NULL;
  }

  /*  iterate and copy the list */

  return_value->_head = (struct _ses_material_file*)NULL;
  struct _ses_material_file* tmp = the_list->_head;
  struct _ses_material_file* new = (struct _ses_material_file*)NULL;
  ses_boolean didit_add = SES_FALSE;

  while (tmp != NULL) {

    new = _copy_ses_material_file(tmp);
    if (new == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
      printf("_copy_material_file_list: copy ses material file returned false in _copy_mf_list\n");
#endif
      _set_latest_error(SES_OBJECT_COPY_ERROR);

      /*  release memory on error */
      ses_boolean didit_destruct = SES_FALSE;
      didit_destruct = _destruct_material_file_list(return_value);
      free(return_value);
      return (struct _ses_material_file_list*)NULL;
    }

    didit_add = _add_to_list(return_value, new);
    tmp = tmp->_next;
  }

  /*  return */

  return return_value;

}
/*****************************************************************/

ses_boolean _destruct_material_file_list(struct _ses_material_file_list* the_list) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(the_list);
  if (errors != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  destruct the material file list */

  ses_boolean return_value = SES_TRUE;

  /*  return on empty list */
  if (the_list->_head == (struct _ses_material_file*)NULL) {
    return SES_TRUE;
  }

  /*  iterate over list, freeing and setting to NULL */

  struct _ses_material_file* tmp = the_list->_head;
  struct _ses_material_file* tmp_next = (struct _ses_material_file*)NULL;
  ses_boolean didit_destruct = SES_FALSE;
  if (tmp != (struct _ses_material_file*)NULL) {

      tmp_next = (struct _ses_material_file*)NULL;
      didit_destruct = SES_FALSE;
      tmp_next = tmp->_next;

      while (tmp != (struct _ses_material_file*)NULL) {

         didit_destruct = _destruct_material_file(tmp);
         free(tmp);
         tmp = (struct _ses_material_file*)NULL;

         if (didit_destruct == SES_FALSE) {
            _set_latest_error(SES_OBJECT_DESTRUCTION_ERROR);
            return SES_FALSE;
         }
         tmp = tmp_next;
         the_list->_head = tmp;
         if (tmp != (struct _ses_material_file*)NULL) {
            tmp_next = tmp->_next;
         }
         else {
            tmp_next = (struct _ses_material_file*)NULL;
         }
    }

  }

  the_list->_head = (struct _ses_material_file*)NULL;

  /*  return */

  return return_value;
}
/*****************************************************************/

ses_boolean _does_material_exist_in_list(struct _ses_material_file_list* the_list, ses_material_id the_mid) {

  //  function prototypes

  ses_error_flag check_errors_does_material_exist_in_list(struct _ses_material_file_list* the_list, ses_material_id the_mid);

  //  end function prototypes

  ses_error_flag errors = check_errors_does_material_exist_in_list(the_list, the_mid);
  if (errors != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  return whether the material exists in the material file list */

  ses_boolean return_value = SES_FALSE;

  /*  find the material id */

  struct _ses_material_file* tmp = (struct _ses_material_file*)NULL;
  tmp = the_list->_head;
  while (tmp != (struct _ses_material_file*)NULL) {
    if(tmp->_the_mid == the_mid) {
      return_value = SES_TRUE;
    }
    tmp = tmp->_next;
  }

  /*  return */

  return return_value;
}

/*****************************************************************/

long* _get_materials_from_list(struct _ses_material_file_list* ptMF) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(ptMF);
  if (errors != SES_NO_ERROR) {
	return (long*)NULL;
  }

  /*  return an array of material on the material file list */

  /*  allocate memory for the list */
  
  long size = _get_nfiles_from_list(ptMF);
  if (size <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_materials_from_list: number of files on material files list <= 0 \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (long*)NULL;
  }

  long* return_value = malloc(sizeof(long)*size);
  if (return_value == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_material_from_list: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (long*)NULL;
  }

  /*  copy out the material list */

  int i=0;
  struct _ses_material_file* tmp = ptMF->_head;
  while (tmp != NULL) {
    return_value[i] = tmp->_the_mid;
    i++;
    tmp = tmp->_next;
  }

  /*  return */

  return return_value;

}


/*****************************************************************/

long* _get_nwds_from_list(struct _ses_material_file_list* ptMF) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(ptMF);
  if (errors != SES_NO_ERROR) {
	return (long*)NULL;
  }

  /*  return a list of nwds from the material file list */

  /*  allocate memory for the list */
   
  long size = _get_nfiles_from_list(ptMF);
  if (size <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_from_list: number of files on material files list <= 0 \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return (long*)NULL;
  }

  long* return_value = malloc(sizeof(long)*size);
  if (return_value == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_nwds_from_list: memory allocation error\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (long*)NULL;
  }

  /*  copy out the nwds list */

  int i=0;
  struct _ses_material_file* tmp = ptMF->_head;

  int nrec;
  while (tmp != NULL) {

    nrec = _get_number_on_list(tmp->_the_tables2);
    return_value[i] = 5+3*nrec;
    i++;
    tmp = tmp->_next;
  }

  /*  return */

  return return_value; 

}

/*****************************************************************/


long* _get_iadr_from_list(struct _ses_material_file_list* ptMF, long start) {

  //  function prototypes

  ses_error_flag check_errors_get_iadr_from_list(struct _ses_material_file_list* the_list, long start);

  //  end function prototypes

  ses_error_flag errors = check_errors_get_iadr_from_list(ptMF, start);
  if (errors != SES_NO_ERROR) {
	return (long*)NULL;
  }
  /*  return a list of iadr from the material file list */

  /*  error check the arguments */

  long size = _get_nfiles_from_list(ptMF);

  /*  allocate memory for the list */
	
  long* return_value = malloc(sizeof(long)*size);
  if (return_value == (long*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_from_list: memory allocation error in _get_iadr_from_list\n");
#endif
    _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
    return (long*)NULL;
  }

  //  compute the list

  int i=0;
  struct _ses_material_file* tmp = ptMF->_head;
  struct _ses_material_file* prev;

  long number_tables;
  long size_index_record;

  while (tmp != NULL) {

    if (i == 0) {
      return_value[i] = start;
    }
    else {
      number_tables = _get_number_on_list(prev->_the_tables2);
    
      size_index_record = 5+3*number_tables;
      return_value[i] = return_value[i-1]+ size_index_record + _get_total_table_size(prev->_the_tables2);
    }
    i++;
    prev = tmp;
    tmp = tmp->_next;
  }

  /*  return */

  return return_value; 
}

/*****************************************************************/


long  _get_nfiles_from_list(struct _ses_material_file_list* ptMF) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);
  long my_count_files(struct _ses_material_file_list* ptMF);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(ptMF);
  if (errors != SES_NO_ERROR) {
	return 0;
  }

  /*  return the number of files on the list */

  long return_value = 0;

  return_value = my_count_files(ptMF);

  /*  return */
 
  return return_value;
}

/*****************************************************************/

long my_count_files(struct _ses_material_file_list* ptMF) {

  //  function prototypes

  ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list);

  //  end function prototypes

  ses_error_flag errors = check_errors_mfl(ptMF);
  if (errors != SES_NO_ERROR) {
	return 0;
  }
  /*  count the files on the list */

  long return_value = 0;

  /*  count */

  struct _ses_material_file* tmp = ptMF->_head;

  while (tmp != (struct _ses_material_file*)NULL) {
    return_value++;
    tmp = tmp->_next;
  }

  /* return */


  return return_value;
}

/*****************************************************************/

ses_boolean _add_to_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_mf) {

  //  function prototypes

  ses_error_flag check_errors_add_combine_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_mf);

  //  end function prototypes

  ses_error_flag errors = check_errors_add_combine_list(the_list, the_mf);
  if (errors != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  add the material file to the material file list */

  ses_boolean return_value = SES_TRUE;

  /*  add to the list */


  if (the_list->_head == the_mf) {
	/*  already on the list */        
	return SES_TRUE;
  }

  /* add the new thing to the HEAD */

  struct _ses_material_file* tmp = the_list->_head;  

  the_list->_head = the_mf;   /*  the new thing */
  the_mf->_next = tmp;        /*  point the next thing to the old head */


  /*  return */
  
  return return_value;
}

/*****************************************************************/

ses_boolean _combine_in_list(struct _ses_material_file_list* ptMFL, struct _ses_material_file* the_material_file) {

  //  function prototypes

  ses_error_flag check_errors_add_combine_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_mf);

  //  end function prototypes

  ses_error_flag errors = check_errors_add_combine_list(ptMFL, the_material_file);
  if (errors != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  combine the_material_file with the corresponding file
      in ptMFL */

  ses_boolean return_value = SES_FALSE;

  /*  go to the material file in the list that has the same mid as the_material_file */

  ses_material_id my_mid = the_material_file->_the_mid;
  if (_does_material_exist_in_list(ptMFL, my_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_combine_in_list:  material %ld does not exist in list\n", my_mid);
#endif
    _set_latest_error(SES_FUNCTION_FAIL_ERROR);
    return SES_FALSE;
  }

  struct _ses_material_file* tmp = ptMFL->_head;
  while (tmp != (struct _ses_material_file*)NULL) {

    if (tmp->_the_mid != my_mid) {
      tmp = tmp->_next;
    }
    else {
      
      /*  ready to combine */
	
      ses_boolean didit_combine = _combine_two_material_files(tmp, the_material_file);
      if (didit_combine == SES_FALSE) {
	_set_latest_error(SES_FUNCTION_FAIL_ERROR);
        return SES_FALSE;
      }
      else {

	/*  destruct the_material file (except for drl) */
 	_destruct_and_detach_material_file(the_material_file);
	free(the_material_file);
	the_material_file = (struct _ses_material_file*)NULL;


	return_value = SES_TRUE;


        break;
      }
    }

  }

  return return_value;
}

/*****************************************************************/


ses_boolean _write_material_file_list(struct _ses_material_file_list* ptMFL, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {


  /*  write the material file list to the output file */
  
  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */

  if (ptMFL == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_material_file_list: null material file list pointer passed to _write_material_file_list\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }


  /*  error check the internal structure */

  struct _ses_material_file* tmp = ptMFL->_head;
  if (tmp == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_write_material_file_list: _head is NULL in _material_files_to_write\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  /*  iterate and write the material files */

  ses_boolean didit_write = SES_FALSE;
  while (tmp != (struct _ses_material_file*)NULL) {

    didit_write = _write_material_file(tmp, pSFH, nsig, do_valid);
    if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_write_material_file_list: _write_material_file false in _write_material_file_list\n");
#endif
      _set_latest_error(SES_WRITE_ERROR);
      return SES_FALSE;
    }
    tmp = tmp->_next;
  }
  return return_value;
}

/*****************************************************************/

struct _ses_material_file_list* _read_material_file_list(ses_file_handle the_handle, unsigned int nsig, ses_boolean do_valid, long* _iadr) {

  /*  from the file handle, read the material file list */

  struct _ses_material_file_list* return_value = (struct _ses_material_file_list*)NULL;

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_material_file_list: null ses file handle  passed to _read_material_file_list\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return (struct _ses_material_file_list*)NULL;
  }

  return_value = _construct_material_file_list();
  if (return_value == (struct _ses_material_file_list*)NULL) {
    
    _set_latest_error(SES_OBJECT_CONSTRUCTION_ERROR);
    return (struct _ses_material_file_list*)NULL;
  }

  FILE* pFILE = pSFH->_c_file_handle;  

  struct _ses_material_file* the_material_file = (struct _ses_material_file*)NULL;
  ses_boolean didit_add = SES_FALSE;
  int i = 0;

  long nfiles = FILE_LIST[the_handle]->_directory->_nfiles;
  long start;
  while ((feof(pFILE) == 0) && i < nfiles) {

    start = _iadr[i];
    the_material_file = _read_material_file(the_handle, nsig, do_valid, start);
    if (the_material_file == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_material_file_list: material file returned null after _read_material_file in _read_material_file_list\n");
#endif
      break;
    }

    didit_add = _add_to_list(return_value, the_material_file);
    if (didit_add == SES_FALSE) {
    }

    i++;
   }


   return return_value;

}

/*****************************************************************/

ses_boolean _sort_material_file_list(struct _ses_material_file_list* the_list) {

  ses_boolean return_value = SES_TRUE;

  /*  if it's already sorted, return TRUE */

  /*  if it's not already sorted    */

#ifdef DEBUG_SORT
  void _print_material_file_list(struct _ses_material_file_list* the_list);

  printf("before _sort_material_file_list the_list is ");
  _print_material_file_list(the_list);
#endif

  
  struct _ses_material_file* current_low = the_list->_head;
  struct _ses_material_file* current_high = the_list->_head;
  struct _ses_material_file* prev_tmp = current_high;
  struct _ses_material_file* tmp = (struct _ses_material_file*)NULL;
  if (current_high != (struct _ses_material_file*)NULL) {
     tmp = current_high->_next;
  }

  while (tmp != (struct _ses_material_file*)NULL) {
       if (tmp->_the_mid >= current_high->_the_mid) {
#ifdef DEBUG_SORT
		printf("in correct order -- material %d\n", tmp->_the_mid);
#endif
		/*  tmp >= current high -- correct order */

		/*  move the high pointer */

		current_high = tmp;


       }
       else {
            /*  tmp < current high */	

	    if (tmp->_the_mid < current_low->_the_mid) {
#ifdef DEBUG_SORT
		printf("incorrect order less than current low -- material %d\n", tmp->_the_mid);
#endif

	        /*  move tmp to the head  and make it current low */
		
		current_high->_next = tmp->_next;
		tmp->_next = current_low;
		current_low = tmp;
		the_list->_head = current_low;
		
		
	    }
	    else {
#ifdef DEBUG_SORT
		printf("incorrect order geq than current low -- material %d\n", tmp->_the_mid);
#endif

		/* place it in the correct position in the tree */
		struct _ses_material_file* ptr = current_low;
		struct _ses_material_file* ptr_prev = (struct _ses_material_file*)NULL;
		while ((ptr != (struct _ses_material_file*)NULL) && (tmp->_the_mid >= ptr->_the_mid)) {
			ptr_prev = ptr;
			ptr = ptr->_next;
		}
		/*  place tmp before ptr */
		current_high->_next = tmp->_next;
		tmp->_next = ptr;
		ptr_prev->_next = tmp;
	    }

	    tmp = current_high;
       }     
	    
     
       prev_tmp = tmp;
       tmp = tmp->_next;
#ifdef DEBUG_SORT
if (tmp != NULL) {
printf("low_mid is %d high_mid is %d tmp_mid is %d\n", current_low->_the_mid, current_high->_the_mid, tmp->_the_mid);
}
else {
printf("low_mid is %d high_mid is %d tmp is NULL", current_low->_the_mid, current_high->_the_mid);
}
#endif
  }
#ifdef DEBUG_SORT
  printf("after _sort_material_file_list the_list is ");
  _print_material_file_list(the_list);
#endif


  return return_value;
}

#ifdef DEBUG_SORT
  void _print_material_file_list(struct _ses_material_file_list* the_list) {
  	struct _ses_material_file* tmp = (struct _ses_material_file*)NULL;
	tmp = the_list->_head;
	int i = 0;
	while (tmp != NULL) {
		printf("mid at %d is %d\n", i, tmp->_the_mid);
		tmp = tmp->_next;
		i++;
	}
   }
#endif

/*****************************************************************/


ses_boolean _add_100_table_to_list(struct _ses_material_file_list* the_list, ses_string* table_strings, int num_strings) {

	//  loop over all materials in the list and add the 100 table
	ses_boolean return_value = SES_TRUE;

        /*  error check the arguments */

        if (the_list == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
            printf("_add_100_table_to_list: null pointer passed in\n");
#endif
            _set_latest_error(SES_NULL_OBJECT_ERROR);
            return SES_FALSE;
        }

	if ((table_strings == (ses_string*)NULL) || (num_strings <= 0)) {
#ifdef DEBUG_PRINT
            printf("_add_100_table_to_list: table strings null or num_strings <= 0\n");
#endif
            _set_latest_error(SES_NULL_OBJECT_ERROR);
            return SES_FALSE;
	}

        /*  look over the material files and add the 100 table to each one */

        struct _ses_material_file* tmp = the_list->_head;

	ses_boolean add_success = SES_FALSE;
        while (tmp != (struct _ses_material_file*)NULL) {

            /*  PUT HERE THE ACTION */

	    add_success = _add_100_table(tmp, table_strings, num_strings);
	    return_value =  return_value && add_success;
	    
            tmp = tmp->_next;
        }
	return return_value;
}


/*****************************************************************/



/*  */
ses_boolean _create100(struct _ses_material_file_list* ptMFL) {

	
        //  add a "100" table that contains the table definitions (if there are any)
	
	//  get the user defined table strings

         ses_boolean return_value = SES_FALSE;
 
	int number_strings = 0;
	ses_string* table_strings = _get_user_defined_table_strings(&number_strings);


	if ((table_strings != (ses_string*)NULL) && (number_strings > 0)) {

		/*  create 100 table */
		/*  0 -- number_strings */
		/*  1...  string that describes the tables
		
		*/

	
		if (number_strings > 0) {
                	ses_boolean didit_addit = 
			  _add_100_table_to_list(ptMFL, 
					table_strings, number_strings);
			if (didit_addit == SES_FALSE) {
#ifdef DEBUG_PRINT
    				printf("_isit_ready_output_file: 100 table not created with valid table strings\n");
#endif
		       		_set_latest_error(SES_FUNCTION_FAIL_ERROR);
		        	return SES_FALSE;
			}
                        return_value = SES_TRUE;

        		/*  after copy, free the strings */
			int i = 0;
			for (i = 0; i < number_strings; i++) {
	
				if (table_strings[i] != (ses_string)NULL) {
					free(table_strings[i]);
					table_strings[i] = (ses_string)NULL;
				}

			
			}
			free(table_strings);
			table_strings = (ses_string*)NULL;
		}
	}

        return return_value;
  

}

/*******************************ERROR CHECKING ***********************************/

ses_error_flag check_errors_mfl(struct _ses_material_file_list* the_list) {

  if (the_list == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_ses_material_file_list error: ses_material_file_list is null\n");
#endif
   _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_does_material_exist_in_list(struct _ses_material_file_list* the_list, ses_material_id the_mid) {

  if (the_list == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_does_material_exist_in_list: ses_material_file_list pointer passed in is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_does_material_exist_in_list: invalid mid in _does_material_exist_in_list\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_INVALID_MID;
  }
  return SES_NO_ERROR;
}

ses_error_flag check_errors_get_iadr_from_list(struct _ses_material_file_list* ptMF, long start) {

  if (ptMF == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_from_list: ses_material_file_list pointer passed is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (start < 0) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_from_list: start < 0 in _does_material_exist_in_list\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

 
  long size = _get_nfiles_from_list(ptMF);
  if (size <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_iadr_from_list: number of files on material files list <= 0 in _get_iadr from list \n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_OBJECT_READY_ERROR;
  }
  return SES_NO_ERROR;

}

ses_error_flag check_errors_add_combine_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_mf) {
  /*  error check the arguments */

  if (the_list == (struct _ses_material_file_list*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_combine_list: ses_material_file_list pointer null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (the_mf == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
    printf("_add_combine_list: ses_material_file pointer is null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  return SES_NO_ERROR;
}


