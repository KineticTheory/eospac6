
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "_file_list_xml.h"
#include "tags.h"
#include "../xml_utilities.h"


ses_error_flag _write_data_record_xml(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

    ses_error_flag return_value = SES_NO_ERROR;

    FILE* pFILE = pSFH->_c_file_handle;


    if (tmp == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_xml:  input data record null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    if (tmp->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_xml:  table iterator null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    long size = _get_number_arrays(tmp->_the_iterator);
    if (size <= 0) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_xml: size < 0\n");
#endif
      _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
      return SES_OBJECT_OUT_OF_RANGE;
    }
 
    char* index_tag = malloc(sizeof(char)*50);
    sprintf(index_tag, "<data_record mid = %ld tid = %ld>\n", tmp->_mid, tmp->_tid);
    _write_tag(pFILE, index_tag);
    free(index_tag);
    index_tag = (char*)NULL;


    //  write the grid tag

    long nr = 0;
    long nt =  0;
    if (tmp->_tid > 201) {
       nr = (long)(tmp->_the_data[0][0]);
       nt = (long)(tmp->_the_data[1][0]);
    }
    else {
       nr = tmp->_the_iterator->_nr;
       nt = tmp->_the_iterator->_nt;
    }

    char* index_tag2 = malloc(sizeof(char)*30);
    sprintf(index_tag2, "<grid mid = %ld tid = %ld>\n", tmp->_mid, tmp->_tid);
  
    _write_tag(pFILE, index_tag2);
    _write_tag(pFILE, "  <dim1>");
    _write_long_xml(pSFH, nr);
    _write_tag(pFILE, "  </dim1>\n");
    _write_tag(pFILE, "  <dim2>");
    _write_long_xml(pSFH, nt);
    _write_tag(pFILE, "  </dim2>\n");
    _write_tag(pFILE, "</grid>\n");


    free(index_tag2);
    index_tag2 = (char*)NULL;

    long array_size;
    long index;
    ses_boolean didit_write;
    for (index=0;index < size; index++) {


	array_size = _get_array_size(tmp->_the_iterator, index);
	if (array_size <= 0) {
#ifdef DEBUG_PRINT
          printf("_write_data_record_xml: Invalid array size = %ld index = %ld\n", array_size, index);
#endif
          _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
	  return SES_OBJECT_OUT_OF_RANGE;
	}

        if (array_size > 1) {

	 //  get the table id
	 ses_table_id the_table_id = tmp->_tid;
	 if (the_table_id >= 200) {
            didit_write = _write_ses_word_array_xml(pSFH, &tmp->_the_data[index][0], array_size, nsig, do_valid);
	 }
	 else {
	    didit_write = _write_ses_string_xml(pSFH, (ses_string)(&tmp->_the_data[index][0]), array_size*8);
	 }
         if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_xml: write ses_word_array or ses string error\n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
	    return SES_WRITE_ERROR;
	  }

        }
        else {

          if (tmp->_the_data == (ses_word_reference*)NULL) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_xml  data is NULL\n");
#endif
            _set_latest_error(SES_NULL_OBJECT_ERROR);
            return SES_NULL_OBJECT_ERROR;
 
	  }

          didit_write = _write_double_xml(pSFH, tmp->_the_data[index][0], 0, SES_FALSE);
          if (didit_write == SES_FALSE) {
#ifdef DEBUG_PRINT
            printf("_write_data_record_xml: write error on write_double_xml\n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
	    return SES_WRITE_ERROR;
	  }
        }
    }


    _write_tag(pFILE, "\n</data_record>\n");


    return return_value;

}
