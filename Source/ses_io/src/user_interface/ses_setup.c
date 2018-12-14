

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#define _get_my_open_flags HEADER(_get_my_open_flags)
#define _do_setup_for_read HEADER(_do_setup_for_read)
#define _do_setup_for_write HEADER(_do_setup_for_write)
#define _do_setup_for_change HEADER(_do_setup_for_change)
#define _do_setup_for_append HEADER(_do_setup_for_append)
#define _check_my_directory_for_material HEADER(_check_my_directory_for_material)
#define _read_my_directory HEADER(_read_my_directory)
#define _construct_my_setup HEADER(_construct_my_setup)
#define _construct_current_data_record HEADER(_construct_current_data_record)
#define _read_current_index_record HEADER(_read_current_index_record)

ses_error_flag ses_setup(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  this routine is the next step after file open */
  /*  this routine fills in (reads) information rom the file into the ses file handle */

  /*  function prototypes */

  ses_open_type  _get_my_open_flags(ses_file_handle the_handle);
  ses_boolean    _do_setup_for_read(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
  ses_boolean    _do_setup_for_write(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab);
  ses_boolean    _do_setup_for_change(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
  ses_boolean    _do_setup_for_append(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab);

  /*  end function prototypes */
 
  if (ses_is_valid(the_handle) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("ses_setup:  invalid file handle\n");
#endif
    _set_latest_error(SES_INVALID_FILE_HANDLE);
    return SES_INVALID_FILE_HANDLE;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    //printf("ses_setup:  invalid material id = %ld\n", the_mid);
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_INVALID_MID;
  }

  if (_is_valid_tid(the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    /*  NEED TO FULLY IMPLMENT THIS FURTHER DOWN */
    printf("ses_setup:  invalid standard table id -- attempting to create user defined table with tid = %ld\n", the_tid);
#endif


  }
 FILE* pFILE = 0;
 pFILE = _getPFILE(FILE_LIST[the_handle]->_the_handle);

  ses_open_type open_flags = _get_my_open_flags(the_handle);

  ses_boolean didit_setup; 
  ses_number nr, nt, ntab; 


  //  if we have a comment table, pad the string to a multiple of sizeof(ses_word)

  if ((open_flags == 'W') || (open_flags == 'A')) {

	if ((the_tid <= 200) && (the_tid >= 100)) {
		//  assume nr came in as strlen(comments) + 1

		int padded_nr = FILE_LIST[the_handle]->_the_setup->_nr + 
			sizeof(ses_word) - (FILE_LIST[the_handle]->_the_setup->_nr)%sizeof(ses_word);
		FILE_LIST[the_handle]->_the_setup->_nr = padded_nr;

	}
  }

  switch(open_flags) {

   case 'A':
        FILE_LIST[the_handle]->_the_setup->_mid = the_mid;
        FILE_LIST[the_handle]->_the_setup->_tid = the_tid;
	nr = FILE_LIST[the_handle]->_the_setup->_nr;
	nt = FILE_LIST[the_handle]->_the_setup->_nt;
	ntab = FILE_LIST[the_handle]->_the_setup->_ntab;

        if ((nr <= 0) || (nt <= 0)) {
#ifdef DEBUG_PRINT
          printf("ses_setup (for append):  grid not defined (nr or nt is <= 0)\n");
#endif
          _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
          return SES_OBJECT_OUT_OF_RANGE;
	}        

        didit_setup = _do_setup_for_append(the_handle, the_mid, the_tid, nr, nt, ntab);
	if (didit_setup == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("ses_setup (for append): setup for append error in ses setup\n");
#endif
          _set_latest_error(SES_SETUP_ERROR);
          return SES_SETUP_ERROR;
	}
        break;

   case 'R':

        didit_setup = _do_setup_for_read(the_handle, the_mid, the_tid);
	if (didit_setup == SES_FALSE) {
	  if (the_tid != 100) {
	  }
          _set_latest_error(SES_SETUP_ERROR);
          return SES_SETUP_ERROR;
	}

 
        break;

   case 'W':
        FILE_LIST[the_handle]->_the_setup->_mid = the_mid;
        FILE_LIST[the_handle]->_the_setup->_tid = the_tid;
	nr = FILE_LIST[the_handle]->_the_setup->_nr;
        nt = FILE_LIST[the_handle]->_the_setup->_nt;
	ntab = FILE_LIST[the_handle]->_the_setup->_ntab;
        if ((nr <= 0) || (nt <= 0)) {
#ifdef DEBUG_PRINT
          printf("ses_setup (for write):  grid not defined (nr or nt is <= 0)\n");
#endif
          _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
          return SES_OBJECT_OUT_OF_RANGE;
	}

        didit_setup = _do_setup_for_write(the_handle, the_mid, the_tid, nr, nt, ntab);
	if (didit_setup == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("ses_setup (for write): setup for write error in ses setup\n");
#endif
          _set_latest_error(SES_SETUP_ERROR);
          return SES_SETUP_ERROR;
	}
   
        break;

  case 'C':

        didit_setup = _do_setup_for_change(the_handle, the_mid, the_tid);
	if (didit_setup == SES_FALSE) {
#ifdef DEBUG_PRINT
          printf("ses_setup (for change): setup for change error in ses setup\n");
#endif
          _set_latest_error(SES_SETUP_ERROR);
          return SES_SETUP_ERROR;
	}
 
        break;
  default:
        break;

  }

  FILE_LIST[the_handle]->_the_setup->_setup_complete = SES_TRUE;
  _releasePFILE(FILE_LIST[the_handle]->_the_handle);

  return SES_NO_ERROR;
}

ses_boolean    _do_setup_for_append(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab) {

  ses_boolean return_value = SES_TRUE;

  /*  function prototypes */

  struct _ses_output_file*  _read_into_output_file(ses_file_handle the_handle);
  ses_boolean    _do_setup_for_write(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab);


  /*  end function prototypes */


  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_append: invalid ses file handle in ses_setup for append\n");
#endif
    return SES_FALSE;
  }


  return_value = _do_setup_for_write(the_handle, the_mid, the_tid, nr, nt, ntab);

  ses_boolean first_time = FILE_LIST[the_handle]->_first_time_through_setup;
  if (return_value == SES_TRUE) {

    if (first_time == SES_TRUE) {

      /*  if this is the first time through -- */

     
      if (FILE_LIST[the_handle]->FILE_TO_WRITE != (struct _ses_output_file*)NULL) {

	ses_boolean didit_d = SES_FALSE;
	didit_d = _destruct_output_file(FILE_LIST[the_handle]->FILE_TO_WRITE);
	free(FILE_LIST[the_handle]->FILE_TO_WRITE);
	FILE_LIST[the_handle]->FILE_TO_WRITE = (struct _ses_output_file*)NULL;

      }

      FILE_LIST[the_handle]->FILE_TO_WRITE = _read_into_output_file(the_handle);
 
      if (FILE_LIST[the_handle]->FILE_TO_WRITE == (struct _ses_output_file*)NULL) {
#ifdef DEBUG_PRINT
        printf("_do_setup_for_append: _read_into_output_file returned NULL\n");
#endif
        _set_latest_error(SES_READ_ERROR);
        return SES_FALSE;
      }
      FILE_LIST[the_handle]->_first_time_through_setup = SES_FALSE;

      /*  material should NOT exist in the directory */

      if (_check_directory_for_material(FILE_LIST[the_handle]->_directory, the_mid) == SES_TRUE) {
#ifdef DEBUG_PRINT
        printf("_do_setup_for_append:  appending material to file that already contains material -- error\n");
#endif
        _set_latest_error(SES_APPEND_ERROR);

        return SES_FALSE;
      }

    }


  }
  else {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_append:  _do_setup_for_write failed\n");
#endif
    _set_latest_error(SES_SETUP_ERROR);
    return SES_FALSE;
  }

  /*  set the file marker to the end of the file */
  return return_value;
}

ses_boolean    _do_setup_for_change(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  function prototypes */

  ses_boolean    _do_setup_for_read(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  /*  do setup for changes */

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_change: invalid ses file handle in ses_setup for change\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  /*  need to setup for read, and also create the current data record data arrays */

  return_value = _do_setup_for_read(the_handle, the_mid, the_tid);
  if (return_value == SES_FALSE) {
    return SES_SETUP_ERROR;
  }

  long number_arrays = FILE_LIST[the_handle]->_current_data_record->_the_iterator->_number_arrays;
  if (number_arrays <= 0) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_change: number arrays <= 0 in ses_setup for change\n");
#endif
    return SES_SETUP_ERROR;
  }

  if (FILE_LIST[the_handle]->_current_data_record->_the_data != (ses_word_reference*)NULL) {
        int i = 0;
	for (i = 0; i < number_arrays; i++) {
		free(FILE_LIST[the_handle]->_current_data_record->_the_data[i]);
		FILE_LIST[the_handle]->_current_data_record->_the_data[i] = (ses_word_reference)NULL;
	}
	free(FILE_LIST[the_handle]->_current_data_record->_the_data);
	FILE_LIST[the_handle]->_current_data_record->_the_data = (ses_word_reference*)NULL;
  }

  FILE_LIST[the_handle]->_current_data_record->_the_data = calloc(number_arrays, sizeof(ses_word_reference*));
  if (FILE_LIST[the_handle]->_current_data_record->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_change: malloc error for _the_data in ses_setup for change\n");
#endif
    return SES_SETUP_ERROR;
  }

  /*  initialize the arrays */

  struct _ses_index_record* ptIR = FILE_LIST[the_handle]->_current_index_record;
  long* size = _get_nwds_index_record(ptIR);

  int i=0;
  for (i=0; i < number_arrays; i++) {
    FILE_LIST[the_handle]->_current_data_record->_the_data[i] = calloc(size[i], sizeof(ses_word));
  }
  FILE_LIST[the_handle]->_current_data_record->_has_data = SES_TRUE;




  return return_value;

}

ses_boolean    _do_setup_for_read(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  do setup for read */

  /*  function prototypes */

  ses_boolean _go_to_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
  ses_boolean    _read_my_directory(ses_file_handle the_handle);
  ses_boolean    _check_my_directory_for_material(ses_file_handle the_handle, ses_material_id the_mid);
  ses_boolean    _read_current_index_record(ses_file_handle the_handle, 
	                                 ses_material_id the_mid, ses_table_id the_tid);
  ses_error_flag _construct_current_data_record(ses_file_handle the_handle, 
	                                 ses_material_id the_mid, ses_table_id the_tid);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;


  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_read: invalid ses file handle in ses_setup for read\n");
#endif
    return SES_FALSE;
  }


  /*  read the directory into the ses file handle */


  if (FILE_LIST[the_handle]->_directory == (struct _ses_directory*)NULL) {
  	ses_boolean read_dir = _read_my_directory(the_handle);
  	if (read_dir  == SES_FALSE) {
#ifdef DEBUG_PRINT
  	  printf("_do_setup_for_read: Directory does not exist -- exiting \n");
#endif
	  if (FILE_LIST[the_handle]->_directory != (struct _ses_directory*)NULL) {
		_destruct_ses_directory(FILE_LIST[the_handle]->_directory);
		free(FILE_LIST[the_handle]->_directory);
		FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;
	  }
  	  return SES_FALSE;
  	}
  }

  /*  make sure the material exists on the library */


  ses_boolean does_material_exist = _check_my_directory_for_material(the_handle, the_mid);
  if (does_material_exist == SES_FALSE) {
#ifdef DEBUG_PRINT
	printf("_do_setup_for_read: Material %ld does not exist -- exiting \n",the_mid);
#endif
	return SES_FALSE;
  }


  FILE_LIST[the_handle]->_the_setup->_mid = the_mid;
  FILE_LIST[the_handle]->_the_setup->_tid = the_tid;
    
    /*  read the current index record for the (material, table) into the ses file handle  */

  ses_boolean does_table_exist = _read_current_index_record(the_handle, the_mid, the_tid);

   if (does_table_exist == SES_FALSE) {
#ifdef DEBUG_PRINT
#ifdef PRINT_INDEX_ERROR
	printf("_do_setup_for_read:  read current idnex record error -- exiting\n");
#endif
#endif
	return SES_FALSE;
  }

  FILE_LIST[the_handle]->_the_setup->_date1 = FILE_LIST[the_handle]->_current_index_record->_date1;
  FILE_LIST[the_handle]->_the_setup->_date2 = FILE_LIST[the_handle]->_current_index_record->_date2;
  FILE_LIST[the_handle]->_the_setup->_vers = FILE_LIST[the_handle]->_current_index_record->_vers;
 
  /*  read the current data record for the (material, table) into the ses file handle    */

  ses_error_flag dr_errors = _construct_current_data_record(the_handle, the_mid, the_tid);
  if (!(dr_errors == SES_NO_ERROR)) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_read: Data record construction error -- exiting \n");
#endif
	return SES_FALSE;
  }

  /*  put the c file handle at the correct location for data read */
    

  ses_boolean didit_go = _go_to_data_record(the_handle, the_mid, the_tid);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("do_setup_for_read: go to data record failed in ses_setup\n");
#endif
    return SES_FALSE;
  }

  return return_value;

}


ses_boolean    _do_setup_for_write(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab) {

  /*  function prototypes */

  ses_error_flag _construct_my_setup(ses_file_handle the_handle, 
                     ses_material_id the_mid,  ses_table_id the_tid);

  /*  end function prototypes */

  /*  do setup for writing */

  ses_boolean return_value = SES_TRUE;
  int i=0;

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_write: invalid ses file handle in ses_setup for write\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  /*  if the current data record has stuff in it, move it to the material_file */

  if (FILE_LIST[the_handle]->_current_data_record == (struct _ses_data_record*)NULL) {

    /*  no stuff in the current data record, nothing to do */

  }
  else {

    /*  stuff in the current data record, need to write it to the FILE_TO_WRITE before
	      setup for this mid, table pair */

    ses_material_id last_mid = FILE_LIST[the_handle]->_current_data_record->_mid;
    struct _ses_material_file* pMF = _construct_ses_material_file(last_mid);
    if (pMF == (struct _ses_material_file*)NULL) {
#ifdef DEBUG_PRINT
      printf("_do_setup_for_write: malloc error with material file -- exiting\n");
#endif
      return SES_SETUP_ERROR;
    }

    /*  add stuff in current record to data record list in the material file */

    ses_boolean didit_add = _add_table(pMF, FILE_LIST[the_handle]->_current_data_record);

    if (didit_add == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_do_setup_for_write: add to list dr error in ses_setup for write\n");
#endif
      return SES_SETUP_ERROR;
    }
    

    /*  move the material file to the output file */

    ses_boolean didit_move = _add_material_file(FILE_LIST[the_handle]->FILE_TO_WRITE, pMF);
    //  do not destruct pMF, but nullify it
    pMF = (struct _ses_material_file*)NULL;
    if (didit_move == SES_FALSE) {
#ifdef DEBUG_PRINT
      printf("_do_setup_for_write: add to material file error in ses_setup for write\n");
#endif

      return SES_SETUP_ERROR;
    }


    /*  all data that remained copied out, set ready to write */

    ses_boolean didit_set = SES_FALSE;
    didit_set = _set_ready_to_write(FILE_LIST[the_handle]->FILE_TO_WRITE);

  }

  /*  construct the setup object if necessary */

  if (FILE_LIST[the_handle]->_the_setup == (struct _ses_setup*)NULL) {

    ses_error_flag construct_errors = _construct_my_setup(the_handle, the_mid, the_tid);
    if (!(construct_errors == SES_NO_ERROR)) {
#ifdef DEBUG_PRINT
      printf("_do_setup_for_write: Setup error for table %ld and material %ld -- exiting \n",the_tid, the_mid);
#endif
      return SES_SETUP_ERROR;

    }

  }


  /* if necessary, override the following:
   *               1. standard table _num_arrays for the_tid
   *               2. _current_data_record->_the_iterator->_number_arrays for the_handle
   */
  for (i=0; i<NUMBER_TABLES; i++) {
    if ((the_tid == _the_tables[i]->_the_tid)&& (_the_tables[i]->_is_user_defined == SES_FALSE)) {
      /* current_ntab only counts dependent data;
       * independent variable arrays and their extents are not counted */
      long current_ntab = 0;
      current_ntab = _the_tables[i]->_num_arrays-2*_the_tables[i]->_num_independent;
      long ntab = 0;
      ntab = FILE_LIST[the_handle]->_the_setup->_ntab - 2*_the_tables[i]->_num_independent;

      if ((ntab > 0) && (ntab > current_ntab)) {
	/* redefine the number of tables */
	long NP = _the_tables[i]->_num_arrays; /* save old value */
	int j;
	_the_tables[i]->_num_arrays = ntab + 2*_the_tables[i]->_num_independent;
        for (j=_the_tables[i]->_num_arrays; j < NP; j++) {
		free(_the_tables[j]->_label[j]);
		_the_tables[j]->_label[j] = (ses_label)NULL;
	}
	_the_tables[i]->_label = (ses_label*)realloc(_the_tables[i]->_label, sizeof(ses_label)*_the_tables[i]->_num_arrays);


	if (_the_tables[i]->_size_arrays) 

	  for (j=_the_tables[i]->_num_arrays; j < NP; j++) {
			free(_the_tables[j]->_size_arrays[j]);
			_the_tables[j]->_size_arrays[j] = (char*)NULL;	
          }

	_the_tables[i]->_size_arrays    = (char**)realloc(_the_tables[i]->_size_arrays, sizeof(char*)*_the_tables[i]->_num_arrays);


	switch (the_tid) { /* define default labels for select tables */
	case 321:
	  for (j=NP; j<_the_tables[i]->_num_arrays; j++) {
	    _the_tables[i]->_label[j] = (ses_label)malloc(sizeof(char)*SES_MAX_LABEL_SIZE);
	    sprintf(_the_tables[i]->_label[j], "mass fraction: phase %ld", j-NP+2);
	    if (j > 1) {
           	 _the_tables[i]->_size_arrays[j] = malloc(sizeof(char) * (strlen(_the_tables[i]->_size_arrays[j-1]) + 1));
	    	strcpy(_the_tables[i]->_size_arrays[j] , _the_tables[i]->_size_arrays[j-1]);
	    }
	  }
	  break;

	default:
	  /* Do nothing now. This may change if logic is needed for user-defined
	   * tables with a variable _num_arrays
	   */
	  break;
	}
      }
    }
  }




  /*  create the current data record */

  long maddress = 0;
  if (FILE_LIST[the_handle]->_current_data_record != NULL) {
	// do NOT destruct current data record, it is attached by pointer to the material file
	FILE_LIST[the_handle]->_current_data_record = (struct _ses_data_record*)NULL;
  }

  long date1 = FILE_LIST[the_handle]->_the_setup->_date1;
  long date2 = FILE_LIST[the_handle]->_the_setup->_date2;
  long vers = FILE_LIST[the_handle]->_the_setup->_vers;

#ifdef DEBUG_SETUP_WRITE
	printf("_do_setup_for_write: date1 is %ld date2 is %ld vers is %ld\n", date1, date2, vers);
#endif

  FILE_LIST[the_handle]->_current_data_record = _construct_ses_data_record(maddress, nr, nt, ntab, the_mid, the_tid, date1, date2, vers);
  if (FILE_LIST[the_handle]->_current_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_write: memory allocation error in setup for write\n");
#endif
    return SES_SETUP_ERROR;
  }


  /*  create the arrays for the data */
  
  long number_arrays = FILE_LIST[the_handle]->_current_data_record->_the_iterator->_number_arrays;
  if (number_arrays < 0) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_write: number arrays < 0 in ses_setup for write\n");
#endif
    return SES_SETUP_ERROR;
  }

  ses_word_reference* ptDATA = malloc(sizeof(ses_word_reference*)*number_arrays);
  if (ptDATA == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
    printf("_do_setup_for_write: malloc error for ptDATA in ses_setup for write\n");
#endif
    return SES_SETUP_ERROR;
  }

  

  /*  initialize the arrays */

  for (i=0; i < number_arrays; i++) {
    ptDATA[i] = NULL;
  }

  FILE_LIST[the_handle]->_current_data_record->_the_data = ptDATA;
  FILE_LIST[the_handle]->_current_data_record->_has_data = SES_FALSE;

  return return_value;
}



ses_open_type _get_my_open_flags(ses_file_handle the_handle) {

  /*  return the file open type from the file handle */

  ses_open_type return_value = '0';
  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_get_my_open_flags: invalid ses file handle\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }


  return_value = _get_open_mode(FILE_LIST[the_handle]->_the_handle);

  return return_value;
}

ses_boolean _read_my_directory(ses_file_handle the_handle) {

  /*  read the library directory from the file handle */

  ses_boolean return_value = SES_TRUE;

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_read_my_directory: invalid ses file handle in _read_my_directory:ses_setup\n");
#endif
    return SES_FALSE;
  }

  /*  get current C file handle */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_my_directory: ses file handle null\n");
#endif
    return SES_FALSE;
  }

  if (FILE_LIST[the_handle]->_directory != (struct _ses_directory*)NULL) {

    _destruct_ses_directory(FILE_LIST[the_handle]->_directory);
    free(FILE_LIST[the_handle]->_directory);
    FILE_LIST[the_handle]->_directory = (struct _ses_directory*)NULL;

  }


  FILE_LIST[the_handle]->_directory = _read_directory(pSFH);
  if (FILE_LIST[the_handle]->_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_my_directory: directory null out of _read_directory\n");
#endif
    return_value = SES_FALSE;
  }


  return return_value;

}

ses_boolean _check_my_directory_for_material(ses_file_handle the_handle, ses_material_id the_mid) {

  /*  this routine reads the directory file and returns true if the material
      is in the index */

  ses_boolean return_value = SES_TRUE;

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_check_my_directory_for_material: invalid ses file handle\n");
#endif
    return SES_FALSE;
  }

  return_value =  _check_directory_for_material(FILE_LIST[the_handle]->_directory, the_mid);
  return return_value;
}

ses_boolean _read_current_index_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  function prototype */

  ses_boolean _go_to_index_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);

  /*  end function prototype */

  ses_boolean return_value = SES_TRUE;
  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_read_current_index_record: invalid ses file handle\n");
#endif
    return SES_FALSE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_current_index_record: null ses file handle in  _read_current_index_record\n");
#endif
    return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_read_current_index_record: null c file handle in  _read_current_index_record\n");
#endif
    return SES_FALSE;
  }

  /*  we're ready to go here */

 ses_boolean didit_go = _go_to_index_record(the_handle, the_mid, the_tid);
  if (didit_go == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_read_current_index_record: go to index record failed in _read_current_index_record\n");
#endif
    return SES_FALSE;
  }

  /*  at the correct location, let's read! */

  if (FILE_LIST[the_handle]->_current_index_record != (struct _ses_index_record*)NULL) {

    _destruct_ses_index_record(FILE_LIST[the_handle]->_current_index_record);
    free(FILE_LIST[the_handle]->_current_index_record);
    FILE_LIST[the_handle]->_current_index_record = (struct _ses_index_record*)NULL;
  }



  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  long offset = _get_address_for_material(ptDIR, the_mid, pSFH);

  FILE_LIST[the_handle]->_current_index_record = _read_index_record(pSFH, offset);
  if (FILE_LIST[the_handle]->_current_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
    if (the_tid != 100) {
    	printf("_read_current_index_record: read index record failed\n");
    }
#endif
    return SES_FALSE;
  }

  /*  if tid is not in index record, return false */

  if (_table_exists(FILE_LIST[the_handle]->_current_index_record, the_tid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    if (the_tid != 100) {
#ifdef PRINT_TABLE_ERROR_MESSAGE
    	printf("_read_current_index_record: _table_exists for tid = %ld is false\n", the_tid);
#endif
    }
#endif
    return_value = SES_FALSE;
  }


  return return_value;
}


ses_error_flag _construct_my_setup(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  construct the setup object for the ses file */

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_construct_my_setup: invalid ses file handle in _construct my setup:ses_setup for write\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  if (FILE_LIST[the_handle]->_the_setup != (struct _ses_setup*)NULL) {

    _destruct_ses_setup(FILE_LIST[the_handle]->_the_setup);
  }

  FILE_LIST[the_handle]->_the_setup = _construct_ses_setup();
  if (FILE_LIST[the_handle]->_the_setup == (struct _ses_setup*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_my_setup: construct ses setup failed in _construct_my_setup\n");
#endif
    return SES_SETUP_ERROR;
  }

  long date = FILE_LIST[the_handle]->_directory->_date;
  long version = FILE_LIST[the_handle]->_directory->_version;
#ifdef DEBUG_DATE_AND_VERSION
  printf("_construct_my_setup:  setting date to %ld and version to %ld\n", date, version);
#endif
  ses_boolean didit_init = _init_ses_setup(FILE_LIST[the_handle]->_the_setup, the_mid, the_tid, date, version);
  if (didit_init == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_my_setup: setup did not inititalize in construct_my_setup\n");
#endif
    return SES_SETUP_ERROR;
  }

  return SES_NO_ERROR;
}



ses_error_flag _construct_current_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  function prototypes */

  ses_boolean _go_to_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);

  /*  end function prototypes */


  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_construct_current_data_record: invalid ses file handle in _construct_current_data_record\n");
#endif
    return SES_INVALID_FILE_HANDLE;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;


   /*  here, we're good to go */

  struct _ses_directory* pDR = FILE_LIST[the_handle]->_directory;
  if (pDR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_current_data_record: directory null in _construct_current_data_record\n");
#endif
    return SES_SETUP_ERROR;
  }


  /*  get the grid */

  long maddress = _get_address_for_material(pDR, the_mid, pSFH);

  long nr = 0;
  long nt = 0;
  long ntab = 0;
  ses_open_type the_flag = FILE_LIST[the_handle]->_the_handle->_the_open_mode;
  ses_boolean didit_getit;
  if (the_flag == 'R' || the_flag == 'C') {
    didit_getit =   _get_grid(the_handle, the_mid, the_tid, &nr, &nt, &ntab);
    if ((didit_getit == SES_FALSE) && (the_tid != 401)) {
#ifdef DEBUG_PRINT
        printf("_construct_current_data_record:  did not get grid correctly\n");
#endif
	return SES_SETUP_ERROR;
    }

  }
  else {
    int index = _get_table_index(FILE_LIST[the_handle]->_current_index_record, the_tid);
    nr = FILE_LIST[the_handle]->_current_index_record->_nr[index];
    nt = FILE_LIST[the_handle]->_current_index_record->_nt[index];
  }


  if (FILE_LIST[the_handle]->_current_data_record != (struct _ses_data_record*)NULL) {
    _destruct_ses_data_record(FILE_LIST[the_handle]->_current_data_record);
   free(FILE_LIST[the_handle]->_current_data_record);
   FILE_LIST[the_handle]->_current_data_record = (struct _ses_data_record*)NULL;
  }

  struct _ses_setup* pSET = FILE_LIST[the_handle]->_the_setup;
  long date1 = pSET->_date1;
  long date2 = pSET->_date2;
  long vers = pSET->_version;
  FILE_LIST[the_handle]->_current_data_record = _construct_ses_data_record(maddress, nr, nt, ntab, the_mid, the_tid, date1, date2, vers);

  if (FILE_LIST[the_handle]->_current_data_record == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
    printf("_construct_current_data_record: current data record came out of construct ses data record as NULL\n");
#endif
    return SES_SETUP_ERROR;
  }


  ses_boolean didit_init = _initialize_ses_iterator(FILE_LIST[the_handle]->_current_data_record->_the_iterator, the_handle,
						    the_tid);

  if (didit_init == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_construct_current_data_record: ses_interator initialize error in construct current data record \n");
#endif
    return SES_SETUP_ERROR;
  }

  return SES_NO_ERROR;
}











