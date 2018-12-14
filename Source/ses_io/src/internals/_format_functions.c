
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#undef DEBUG_GET_GRID

#define check_errors_go_to_index_record HEADER(check_errors_go_to_index_record)
#define check_errors_go_to_data_record HEADER(check_errors_go_to_data_record)
#define check_errors_get_grid HEADER(check_errors_get_grid)
#define check_errors_get_address_for_material HEADER(check_errors_get_address_for_material)
#define check_errors_get_directory_size HEADER(check_errors_get_directory_size)


////DEBUGS//////////////////
///  DEBUG_GET_GRID -- leaving in for convenience util realloc section made better -- now tables are
//     checked for by number -- could be abstracted better
////////////////////////////


/**********************************************************************/

ses_boolean     _go_to_directory(ses_file_handle the_handle) {
  return SES_TRUE;
}

/**********************************************************************/

ses_boolean _go_to_index_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {

  /*  function prototypes */

  ses_error_flag check_errors_go_to_index_record(ses_file_handle the_handle);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  ses_error_flag check = check_errors_go_to_index_record(the_handle);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  //  at this point, the handle is valid, the ses_file_handle is valid,
  //  the c file handle is valid,  and the function pointer is valid

  //  if the directory is null, read it

  struct _ses_directory* ptDIR = FILE_LIST[the_handle]->_directory;
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (ptDIR == (struct _ses_directory*)NULL) {
    ptDIR = _read_directory(pSFH);
  }

  //  go to the index record

  return_value = pSFH->pt2_go_to_index_record(the_handle, the_mid, the_tid);

  return return_value;
}

/***************************************************************/

ses_boolean _go_to_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid) {


  /*  function prototypes */

  ses_error_flag check_errors_go_to_data_record(ses_file_handle the_handle);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;

  ses_error_flag check = check_errors_go_to_data_record(the_handle);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  //  at this point, the handle is valid, the ses_file_handle is valid,
  //  the c file handle is valid,  and the function pointer is valid

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  return_value = pSFH->pt2_go_to_data_record(the_handle, the_mid, the_tid);
  
  return return_value;
}
/***************************************************************/

ses_boolean _go_to_next_array_location(struct _ses_file_handle* pSFH, long location) {

  if (pSFH == (struct _ses_file_handle*)NULL) {
     return SES_FALSE;
  }

  ses_boolean return_value = pSFH->pt2_go_to_next_array_location(pSFH, location);

  return return_value;
}

/***************************************************************/

ses_boolean _get_grid(ses_file_handle the_handle, ses_material_id the_mid,
		      ses_table_id the_tid, long* nr, long* nt, long* ntab) {

  /*  function prototypes */

  ses_error_flag check_errors_get_grid(ses_file_handle the_handle);

  /*  end function prototypes */

  ses_boolean return_value = SES_TRUE;
#ifdef DEBUG_GET_GRID
  printf("_get_grid:  entered\n");
#endif

  ses_error_flag check = check_errors_get_grid(the_handle);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  //  at this point, the handle is valid, the ses_file_handle is valid,
  //  the c file handle is valid,  and the function pointer is valid

  /*  read nr and nt directly from the file */

#ifdef DEBUG_GET_GRID
  printf("_get_grid:  past check errors\n");
#endif
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
#ifdef DEBUG_GET_GRID
  printf("_get_grid: addresses:  nr is %ld and nt is %ld\n", nr, nt);
  printf("_get_grid: *nr is %ld and *nt is %ld\n", *nr, *nt);
#endif
  return_value = pSFH->pt2_get_grid(the_handle, the_mid,
				    the_tid, nr, nt, ntab);

#ifdef DEBUG_GET_GRID
  printf("_get_grid: after pt2_get_grid, *nr is %ld and *nt is %ld and *ntab is %ld\n", *nr, *nt, *ntab);
#endif
  if ((*nr <= 0) || (*nt <= 0)) {
#ifdef DEBUG_PRINT
    printf("_get_grid:  nr and nt -- either could be 0\n");
#endif
    return SES_FALSE;
  }
#ifdef DEBUG_GET_GRID
  printf("_get_grid:  after check for 0\n");
#endif

  /* now redefine selected _the_table array elements based upon loaded data */

  int i = 0;
  for (i=0; i<NUMBER_TABLES; i++) {
    long NW = (long)0;
    long NP = (long)0;
    if ((the_tid <= 605) && (the_tid == _the_tables[i]->_the_tid) && (the_tid != 401) && (the_tid != 411) && (the_tid != 412) &&
	(the_tid != 431) && (the_tid != 432) && (the_tid != 501)
	&& !((the_tid >= 100) && (the_tid < 200)) && (the_tid != 201)) {
      /* The total number of subtables (NP) will be dynamically determined
       * using the following relation:
       *    NW = 1 + 1 + NR + NT + NR*NT*NP
       * unless it's a comments table, where we have
       *    NW = NR (NR is a sub for number of bytes )
       * where
       *    NW = total number of words in the Sesame table (from material index NWDS)
       *    NR = total number of density values in the Sesame table
       *    NT = total number of temperature values in the Sesame table
       *    NP = total number of subtables (phases) in the Sesame table
       */
      NP = _the_tables[i]->_num_arrays; /* save old value */
      NW = _get_table_size(FILE_LIST[the_handle]->_current_index_record, the_tid);
#ifdef DEBUG_GET_GRID
      printf("_get_grid: NP is %d NW is %d num_arrays is %d\n", NP, NW, _the_tables[i]->_num_arrays);
      printf("_get_grid: *nr is %ld and *nt is %ld\n", *nr, *nt);
#endif

      int j = 0;
      if (! _the_tables[i]->_is_user_defined) {

	_the_tables[i]->_num_arrays = ( NW - 2 - *nr - *nt ) / ( *nr * *nt ) + 2*_the_tables[i]->_num_independent;
#ifdef DEBUG_GET_GRID
      printf("get_grid: in ! user defined  NW is %d nr is %d nt is %d num_ind is %d num_arrays is %d\n", NW, *nr, *nt, _the_tables[i]->_num_independent, _the_tables[i]->_num_arrays);
#endif
      }

      if (NP > _the_tables[i]->_num_arrays) {
	/* remove some unneeded elements from Sesame tables that have a variable _num_arrays */

	for (j=_the_tables[i]->_num_arrays; j<NP; j++) {
	  free(_the_tables[i]->_label[j]);
	  _the_tables[i]->_label[j] = (ses_label)NULL;
	  free(_the_tables[i]->_size_arrays[j]);
	  _the_tables[i]->_size_arrays[j] = (char*)NULL;
	}
	_the_tables[i]->_label = (ses_label*)realloc(_the_tables[i]->_label, sizeof(ses_label)*_the_tables[i]->_num_arrays);
	_the_tables[i]->_size_arrays = (char**)realloc(_the_tables[i]->_size_arrays, sizeof(char*)*_the_tables[i]->_num_arrays);

      }
      //else if (NP <= _the_table[i]._num_arrays) {
      else if (NP < _the_tables[i]->_num_arrays) {
	/* add some needed elements to Sesame tables that have a variable _num_arrays */

       
	_the_tables[i]->_label = (ses_label*)realloc(_the_tables[i]->_label, sizeof(ses_label)*_the_tables[i]->_num_arrays);
        
        for(j=NP; j < _the_tables[i]->_num_arrays; j++) {
	  _the_tables[i]->_label[j] = _get_standard_label_at_index(the_tid, j);

 	}

	/* 	switch (the_tid) { */
	/* 	case 321: */
	if (_the_tables[i]->_size_arrays)
	  _the_tables[i]->_size_arrays = (char**)realloc(_the_tables[i]->_size_arrays, sizeof(char*)*_the_tables[i]->_num_arrays);
	else
	  _the_tables[i]->_size_arrays = (char**)malloc(sizeof(char*)*_the_tables[i]->_num_arrays);
	for (j=NP; j<_the_tables[i]->_num_arrays; j++) {
          free(_the_tables[i]->_label[j]);
	  _the_tables[i]->_label[j] = (ses_label)NULL;
	  _the_tables[i]->_label[j] = (ses_label)malloc(sizeof(char)*SES_MAX_LABEL_SIZE);
	  sprintf(_the_tables[i]->_label[j], "mass fraction: phase %ld", j-NP+2);
	  _the_tables[i]->_size_arrays[j] = malloc(sizeof(char) * (strlen(_the_tables[i]->_size_arrays[j-1]) + 1));
	  strcpy(_the_tables[i]->_size_arrays[j] , _the_tables[i]->_size_arrays[j-1]);
	}
	/* 	  break; */

	/* 	default: */
	/* Do nothing now. This may change if logic is needed for user-defined
	 * tables with a variable _num_arrays
	 */
	/* 	  break; */
	/* 	} */

      }
#ifdef DEBUG_GET_GRID
      printf("_get_grid: at end NP is %d NW is %d num_arrays is %d\n", NP, NW, _the_tables[i]->_num_arrays);
#endif

    }
  }

  return return_value;
}
/***************************************************************/

ses_boolean     _isit_my_format(FILE* pFILE) {

  return SES_TRUE;

}
/***********************************************************************/

long _get_address_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH) {

  /*  function prototypes */

  ses_error_flag check_errors_get_address_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  long return_value = 0;

  ses_error_flag check = check_errors_get_address_for_material(ptDIR, the_mid, pSFH);
  if (check != SES_NO_ERROR) {
	return 0;
  }

  /*  get the 'file' address for the associated material id */

  //  at this point, the directory is non null and ready,
  //  the material is valid, and the ses file handle is non null

  /*  find the address */

  return_value = pSFH->pt2_get_address_for_material(ptDIR, the_mid, pSFH);
  
  /*  return */

  return return_value;


}


/**********************************************************************/


long _get_directory_size(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {


  /*  function prototypes */

  ses_error_flag check_errors_get_directory_size(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);

  /*  end function prototypes */

  long return_value = 0;

  ses_error_flag check = check_errors_get_directory_size(ptDIR, pSFH);
  if (check != SES_NO_ERROR) {
	return SES_FALSE;
  }

  /*  return the size of the directory */

  //  at this point, the directory is non null and ready,
  //  and the ses file handle is non null

 
  /*  compute the size */

  return_value = pSFH->pt2_get_directory_size(ptDIR, pSFH);


  /*  return */

  return return_value;

}

long _get_address_for_table_format(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH) {
  return 0;
}

ses_error_flag _read_directory_format(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {
  return SES_NO_ERROR;
}

ses_error_flag _read_index_record_format(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {
  return SES_NO_ERROR;
}

ses_error_flag _read_data_record_format(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {
  return SES_NO_ERROR;
}

ses_error_flag _read_array_format(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid) {
  return SES_NO_ERROR;
}

ses_boolean _write_directory_format(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {
  return SES_NO_ERROR;
}

ses_boolean _write_index_record_format(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {
  return SES_NO_ERROR;
}

ses_error_flag _write_data_record_format(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {
  return SES_NO_ERROR;
}

ses_boolean _write_array_format(struct _ses_file_handle* ptHandle, ses_word_reference ptBuffer, long size, unsigned int nsig, ses_boolean do_valid) {
  return SES_NO_ERROR;
}

/***************************************************************************/
/***************************************************************************/
//   error checking routines

ses_error_flag check_errors_go_to_index_record(ses_file_handle the_handle) {

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record: Invalid handle passed to _go_to_index_record =  %d\n", the_handle);
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (FILE_LIST[the_handle]->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record: ses file handle null in _go_to_index_record \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  FILE* pFILE = FILE_LIST[the_handle]->_the_handle->_c_file_handle;
  if (pFILE == NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_index_record: pFILE is null in _go_to_index record\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  internal error checks */
  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH->pt2_go_to_index_record == NULL) {

#ifdef DEBUG_PRINT
    printf("_go_to_index_record: Function pointer to _go_to_index_record function is null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }

  return SES_NO_ERROR;
}

ses_error_flag check_errors_go_to_data_record(ses_file_handle the_handle) {

  if (!ses_is_valid(the_handle)) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record: Invalid handle passed to _go_to_data_record =  %d\n", the_handle);
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (FILE_LIST[the_handle]->_the_handle == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_go_to_data_record: ses file handle null in _go_to_data_record \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;

  /*  internal error checks */

  if (pSFH->pt2_go_to_data_record == NULL) {

#ifdef DEBUG_PRINT
    printf("_go_to_data_record: Function pointer to _go_to_data_record function is null\n");
#endif
    _set_latest_error(SES_READ_ERROR);
    return SES_READ_ERROR;
  }  
  return SES_NO_ERROR;
}

ses_error_flag check_errors_get_grid(ses_file_handle the_handle) {

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid: ses file handle null in _get_grid\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid: c file handle null _get_grid \n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  struct _ses_directory* pDR = FILE_LIST[the_handle]->_directory;
  if (pDR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid: directory null in _get_grid\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH->pt2_get_grid == NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid:  function pointer null\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }  
  return SES_NO_ERROR;
}

ses_error_flag check_errors_get_address_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH) {

  /* argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: null directory pointer passed in to _get_address_for_material\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: ses file handle null in _get_address_for_material\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (_is_valid_mid(the_mid) == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: invalid mid in _get_address_for_material\n");
#endif
    _set_latest_error(SES_INVALID_MID);
    return SES_NULL_OBJECT_ERROR;
  }

  /*  internal error checking */

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: Trying to _get_address_for_material from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_OBJECT_READY_ERROR;
  }


  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_address_for_material: nfiles <= 0 in _get_address_for_material\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }

  return SES_NO_ERROR;
}

ses_error_flag check_errors_get_directory_size(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

 /*  argument error checking */

  if (ptDIR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: null directory pointer\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }

  if (ptDIR->_ready == SES_FALSE) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: Trying to _get_directory_size from directory that is not ready\n");
#endif
    _set_latest_error(SES_OBJECT_READY_ERROR);
    return SES_NULL_OBJECT_ERROR;
  }
  


  if (ptDIR->_nfiles <= 0) {
#ifdef DEBUG_PRINT
    printf("_get_directory_size: nfiles <= 0 in _get_directory_size\n");
#endif
    _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
    return SES_OBJECT_OUT_OF_RANGE;
  }
  return SES_NO_ERROR;
}


