
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_ascii.h"

#undef DEBUG_PRINT

#define check_errors_WRITE_DATA_RECORD_ASCII HEADER(check_errors_WRITE_DATA_RECORD_ASCII)
#define my_write_old_ascii_header HEADER(my_write_old_ascii_header)

//--------------------------------------------------
//  debug ifdefs
//--------------------------------------------------

ses_error_flag _write_data_record_ascii(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

    /*  function prototypes  */

    ses_error_flag check_errors_WRITE_DATA_RECORD_ASCII(struct _ses_data_record* tmp);
    void my_write_old_ascii_header(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH);


    /*  end function prototypes */

    ses_error_flag return_value = SES_NO_ERROR;

    return_value = check_errors_WRITE_DATA_RECORD_ASCII(tmp);
    if (return_value != SES_NO_ERROR) {
	return return_value;
    }

    ses_boolean didit_write = SES_FALSE;

    my_write_old_ascii_header(tmp, pSFH);
    //int start_index = pSFH->_start_index;
    int start_index = 0;

    //  write the data in the next section


    
    long array_size = 0;
    long index = 0;
    long size = _get_number_arrays(tmp->_the_iterator);

#ifdef DEBUG_PRINT
    printf("_write_data_record_ascii: SIZE: %ld\n", size);
#endif

    for (index=0; index < size; index++) {   //  loop over the "arrays" in the data record

	array_size = _get_array_size(tmp->_the_iterator, index);
#ifdef DEBUG_PRINT
        printf("_write_data_record_ascii: array_size: %ld\n", array_size);
#endif

	//  check array size -- must be > 0
	if (array_size <= 0) {
#ifdef DEBUG_PRINT
          printf("_write_data_record_ascii: Invalid array size = %ld index = %ld\n", array_size, index);
#endif
          _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
	  return SES_OBJECT_OUT_OF_RANGE;
	}


        ses_table_id the_table_id;
        if (array_size > 1) {

	   //  ARRAY_SIZE > 1

	   the_table_id = tmp->_tid;
	   if (the_table_id >= 200) {

	      //  NOT 100 LEVEL TABLES
              didit_write = _write_ses_word_array_ascii(pSFH, &tmp->_the_data[index][0], array_size, nsig, do_valid);
	   }
	   else {

	      //  101 - 199 level tables 

	      didit_write = SES_FALSE;
	      didit_write = _write_ses_string_ascii(pSFH, (ses_string)(&tmp->_the_data[index][0]), array_size*8);

	   }

           if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
              printf("_write_data_record_ascii: write error\n");
#endif
              _set_latest_error(SES_WRITE_ERROR);
	      return SES_WRITE_ERROR;
	    }

        }
        else {

	  //  ARRAY SIZE IS ONE

	  the_table_id = tmp->_tid;
          didit_write = _write_double_tomax_ascii(pSFH, tmp->_the_data[index][0], 0, SES_FALSE, pSFH->_word_size);

          if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_ascii: write error\n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
	    return SES_WRITE_ERROR;
	  }



	  if (pSFH->_start_index == 4) {
       /* ses_boolean didit = 0; */
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       char f90_linefeed = '\n';
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, f90_linefeed);
		pSFH->_start_index = 0;
	  }
        }

    
 	if ((the_table_id >= 200) || (the_table_id == 100)) {

		int new_index = start_index + array_size;
		if (new_index > 4) {
			new_index = (start_index + array_size)%5;
			start_index = new_index;
		}
		else {
			start_index = new_index;
		}
        	pSFH->_start_index = new_index;
	}

    }

    //  write a '10000' at the end of the data record

    if (pSFH->_start_index != 0) {

       int i = 0;
       int j = 0;
       int num_ones = pSFH->_start_index;
       int num_zeros = 5-num_ones;
       for (i = pSFH->_start_index; i < 5; i++) {
       		/* ses_boolean didit = */ _write_char_ascii(pSFH->_c_file_handle, ' ');
#ifdef DEBUG_PRINT
           printf("_write_data_record_ascii: with word_size: %d\n",pSFH->_word_size);
#endif
           for(j = 1; j < pSFH->_word_size; j++) {
               /* didit = */ _write_char_ascii(pSFH->_c_file_handle, ' ');
           }
       }

       /* ses_boolean didit; */
       for (i = 0; i < num_ones; i++) {
          /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '1');
       }
       for (i = 0; i < num_zeros; i++) {
          /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '0');
       }
       /* didit = */ _write_char_ascii(pSFH->_c_file_handle, '\n');
       pSFH->_start_index = 0;
    }


    return return_value;

}

ses_error_flag check_errors_WRITE_DATA_RECORD_ASCII(struct _ses_data_record* tmp) {

    /*  error checks upon entrance to WRITE_DATA_RECORD_ASCII */
    if (tmp == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_ascii:  input data record null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    if (tmp->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_ascii:  table iterator null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }
    long size = _get_number_arrays(tmp->_the_iterator);
    if (size <= 0) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_ascii: size < 0\n");
#endif
      _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
      return SES_OBJECT_OUT_OF_RANGE;
    }

    if (tmp->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
       printf("_write_data_record_ascii  data is NULL\n");
#endif
       _set_latest_error(SES_NULL_OBJECT_ERROR);
       return SES_NULL_OBJECT_ERROR;
 
    }


    return SES_NO_ERROR;

}

void my_write_old_ascii_header(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH) {

    //  write the old ascii header

    long flag = (long)1;
    if (tmp->_first == 1 ) flag = (long)0;
    long mid = tmp->_mid;
    long tid = tmp->_tid;
    long length = _get_all_arrays_size(tmp);
    char the_r = 'r';

    long cdate = (long)0;
    long udate = (long)0;
    long vers = (long)0;
    cdate = tmp->_date1;
    udate = tmp->_date2;
    vers = tmp->_vers;

    //  in an old ascii file, each data record has a header;  we write it here

    //  create the header
    char the_string[100];
    if (tid < 200) {
      const char* space = " ";
      const char* spacer = "   ";
      const char* spacer2 = "                                 ";
      const char* format = "%2ld%6ld%6ld%6ld%s%c%9ld%9ld%s%3ld%s%1ld";

      /* int n = 0; */
      /* n = */ sprintf(&the_string[0], format, flag, mid, tid, length*8, spacer, the_r, cdate, udate, space, vers, spacer2, flag);
      /* n = 3; */
    }
    else {

      const char* space = " ";
      const char* spacer = "   ";
      const char* spacer2 = "                                 ";
      const char* format = "%2ld%6ld%6ld%6ld%s%c%9ld%9ld%s%3ld%s%1ld";

      /* int n = 0; */
      /* n = */ sprintf(&the_string[0], format, flag, mid, tid, length, spacer, the_r, cdate, udate, space, vers, spacer2, flag); 
      /* n = 3; */
    }
    /* ses_boolean didit_write1 = SES_FALSE; */
    /* didit_write1 = */ _write_ses_string_ascii(pSFH, the_string, 80);
    /* didit_write1 = SES_FALSE; */

}




