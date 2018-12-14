
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>
#include "../xml_utilities.h"

#define _write_data_record_llnl_xml HEADER(_write_data_record_llnl_xml)
#define _write_xml_function_tag2 HEADER(_write_xml_function_tag2 )
#define _get_data HEADER(_get_data)
#define _write_axis_tag HEADER(_write_axis_tag)
#define _get_xml_function_file_name HEADER(_get_xml_function_file_name)
#define _get_my_function_type HEADER(_get_my_function_type)
#define _check_errors_WRITE_DATA_RECORD_LLNL_XML HEADER(_check_errors_WRITE_DATA_RECORD_LLNL_XML)
#define _get_num_functions HEADER(_get_num_functions)
#define _get_function_name HEADER(_get_function_name)
#define _write_xml_header_tag HEADER(_write_xml_header_tag)


ses_error_flag _write_data_record_llnl_xml(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid) {

    //  function prototypes

    char* _get_xml_function_file_name(ses_material_id mid, const char* function_name);
    int _get_num_functions(ses_table_id tid);
    void _write_xml_function_tag2(FILE* pFILE, struct _ses_data_record* ptDR, int function_index);
    const char* _get_function_name(ses_table_id tid, int function_index);
    const char* _get_function_type(ses_table_id tid);
    ses_error_flag _check_errors_WRITE_DATA_RECORD_LLNL_XML(struct _ses_data_record* tmp);

    //  function prototypes

    ses_error_flag return_value = SES_NO_ERROR;
    return_value = _check_errors_WRITE_DATA_RECORD_LLNL_XML(tmp);
    if (return_value != SES_NO_ERROR) {
 	return return_value;
    }



   //  open up the new file handle one level down and point the ses_file_handle to it

   int num_functions = _get_num_functions(tmp->_tid);
   int i = 0;
   const char* function_name = (char*)NULL;
   char* function_file = (char*)NULL;
   for (i = 0; i < num_functions; i++) {

   	function_name = _get_function_name(tmp->_tid, i);
   	function_file = _get_xml_function_file_name(tmp->_mid, function_name);

   	fclose(pSFH->_c_file_handle);
   	pSFH->_c_file_handle = fopen(function_file, "w");
   	strcpy(pSFH->_filename, function_file);

   	FILE* pFILE = pSFH->_c_file_handle;


   	_write_xml_function_tag2(pFILE, tmp, i+4);
        free(function_file);
        function_file = (char*)NULL;

    }

    return return_value;

}

void _write_xml_function_tag2(FILE* pFILE, struct _ses_data_record* ptDR, int function_index) {

  //  open up the new file handle one level down and point the ses_file_handle to it

  //  function prototypes

  void _write_xml_header_tag(FILE* pFILE);
  const char* _get_my_function_type(ses_table_id tid);
  const char* _get_function_name(ses_table_id tid, int function_index);
  void _write_axis_tag(FILE* pFILE, char* axis_function_name, char* size_attribute_name, int size, ses_word_reference the_data);
  ses_word_reference _get_data(struct _ses_data_record* ptDIR, int index);

  //  end function prototypes

   _write_xml_header_tag(pFILE);

   char* function_file = malloc(sizeof(char)*100);
   const char* function_name = _get_function_name(ptDR->_tid, function_index);
   sprintf(function_file, "%s_%s.xml",  function_name, _get_my_function_type(ptDR->_tid));

   const char* function_type = _get_my_function_type(ptDR->_tid);

   char* function_tag = malloc(sizeof(char) * 500);
   sprintf(function_tag, "<function name = \"%s\" type = \"%s\" >\n", 
				function_name, function_type);

   _write_tag(pFILE, function_tag);

   int nr = ptDR->_the_iterator->_nr;
   int nt = ptDR->_the_iterator->_nt;
   ses_word_reference the_density = ptDR->_the_data[2];
   ses_word_reference the_temperature = ptDR->_the_data[3];
   _write_axis_tag(pFILE, "density", "nr", nr, the_density);
   _write_axis_tag(pFILE, "temperature", "nt", nt, the_temperature);

   int i = 0;
   ses_word_reference the_data = _get_data(ptDR, function_index);
   int the_size = ptDR->_the_iterator->_size_arrays[function_index];
   for(i = 0; i < the_size; i++) {
	_write_double_pFILE_xml(pFILE, the_data[i]);
   }
   _write_tag(pFILE, "\n</function>\n");


   free(function_tag);
   function_tag = (char*)NULL;

   free(function_file);
   function_file = (char*)NULL;

}

ses_word_reference _get_data(struct _ses_data_record* ptDR, int index) {

	ses_word_reference return_value = (ses_word_reference)NULL;

	return_value = ptDR->_the_data[index];
	return return_value;
}


void _write_axis_tag(FILE* pFILE, char* axis_function_name, char* size_attribute_name, int size, ses_word_reference the_data) {

   char* size_field = malloc(sizeof(char)*10);
   sprintf(size_field, "%d", size);

   char* tag1 = malloc(sizeof(char)*100);
   sprintf(tag1, "  <axis name=\"%s\" %s = \"%s\" >\n", axis_function_name, size_attribute_name, size_field);
   _write_tag(pFILE, tag1);

   int i = 0;
   for(i = 0; i < size; i++) {
	_write_double_pFILE_xml(pFILE, the_data[i]);
   }

   _write_tag(pFILE, "\n  </axis>\n");

   free(size_field);
   size_field = (char*)NULL;

   free(tag1);
   tag1 = (char*)NULL;

}

char* _get_xml_function_file_name(ses_material_id mid, const char* function_name) {

  //  return the one level down material path name for the mid

  char* material_file = malloc(sizeof(char)*100);
  sprintf(material_file, "L%ld/%s.xml", mid, function_name);

  return material_file;

}


const char* _get_my_function_type(ses_table_id tid) {

#ifdef CHANGE_TO_STANDARD
#else

	if (tid == 301) {
		return "total";
	}
	if (tid == 101) {
		return "comments";
	}
	if (tid == 303) {
		return "303_type";
	}
#endif
	return "unknown";

}

ses_error_flag _check_errors_WRITE_DATA_RECORD_LLNL_XML(struct _ses_data_record* tmp) {

/*************************  CHECK ERRORS **********************************/

    if (tmp == (struct _ses_data_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_llnl_xml:  input data record null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    if (tmp->_the_iterator == (struct _ses_iterator*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_llnl_xml:  table iterator null\n");
#endif
      _set_latest_error(SES_NULL_OBJECT_ERROR);
      return SES_NULL_OBJECT_ERROR;
    }

    long size = _get_number_arrays(tmp->_the_iterator);
    if (size <= 0) {
#ifdef DEBUG_PRINT
      printf("_write_data_record_llnl_xml: size < 0\n");
#endif
      _set_latest_error(SES_OBJECT_OUT_OF_RANGE);
      return SES_OBJECT_OUT_OF_RANGE;
    }

/*************************  END CHECK ERRORS **********************************/

	return SES_NO_ERROR;
}

