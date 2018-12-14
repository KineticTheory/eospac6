#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <string.h>

#include "../xml_utilities.h"

#define _write_index_record_llnl_xml HEADER(_write_index_record_llnl_xml)
#define check_errors_write_index_record_llnl_xml HEADER(check_errors_write_index_record_llnl_xml)
#define _get_xml_material_file_name HEADER(_get_xml_material_file_name)
#define _write_xml_material_tag2 HEADER(_write_xml_material)
#define _write_table_llnl_xml HEADER(_write_table_llnl_xml)
#define _get_num_functions HEADER(_get_num_functions)
#define _write_xml_function_tag HEADER(_write_xml_function_tag)
#define _write_xml_reference_tag2 HEADER(_write_xml_reference_tag2)
#define _get_function_name HEADER(_get_function_name)
#define _get_function_type HEADER(_get_function_type)
#define _get_descriptive_material_name HEADER(_get_descriptive_material_name)
#define _write_xml_header_tag HEADER(_write_xml_header_tag)


ses_boolean _write_index_record_llnl_xml(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {


 //  function prototypes

 ses_error_flag check_errors_write_index_record_llnl_xml(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);
 char* _get_xml_material_file_name(ses_material_id mid);
 void _write_xml_material_tag2(FILE* pFILE, struct _ses_index_record* ptIR);

  //  end function prototypes

  ses_boolean return_value = SES_TRUE;

  ses_error_flag error_check = check_errors_write_index_record_llnl_xml(ptIR, pSFH);
  if (error_check != SES_NO_ERROR) {
	return return_value;
  }


  //  write the material level xml file

  //  open up the new file handle one level down and point the ses_file_handle to it

  char* material_file = _get_xml_material_file_name(ptIR->_mid);

  fclose(pSFH->_c_file_handle);
  pSFH->_c_file_handle = fopen(material_file, "w");
  strcpy(pSFH->_filename, material_file);

  FILE* pFILE = pSFH->_c_file_handle;

  _write_xml_material_tag2(pFILE, ptIR);

  free(material_file);
  material_file = (char*)NULL;

  return return_value;
}

ses_error_flag check_errors_write_index_record_llnl_xml(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH) {

   if (the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_index_record_llnl_xml: index_record_null NULL\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_index_record_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

  FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_write_index_record_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 

  return SES_NO_ERROR;
}


char* _get_xml_material_file_name(ses_material_id mid) {

  //  return the one level down material path name for the mid

  char* material_file = malloc(sizeof(char)*100);
  sprintf(material_file, "L%ld/material.xml", mid);

  return material_file;

}

void _write_xml_material_tag2(FILE* pFILE, struct _ses_index_record* ptIR) {

  //  function prototypes

  void _write_table_llnl_xml(struct _ses_index_record* ptIR, int i, FILE* pFILE);
  void _write_xml_header_tag(FILE* pFILE);
  char* _get_descriptive_material_name(ses_material_id mid);

  //  end function prototypes

  ses_material_id mid = ptIR->_mid;

  char* version_string = malloc(sizeof(char) * 10);
  sprintf(version_string, "%ld", mid);

  char* material_name = _get_descriptive_material_name(ptIR->_mid);

  char* material_tag = malloc(sizeof(char)*200);
  sprintf(material_tag, "<material name = \"%s\" version = \"%s\">\n", material_name, version_string);

  _write_xml_header_tag(pFILE);
  _write_tag(pFILE, material_tag);

  int i = 0;
  for (i = 0; i < ptIR->_nrec; i++) {

  	_write_table_llnl_xml(ptIR, i, pFILE);     
  }

  _write_tag(pFILE, "</material>\n");

  free(material_name);
  material_name = (char*)NULL;

  free(version_string);
  version_string = (char*)NULL;

  free(material_tag);
  material_tag = (char*)NULL;

}

void _write_table_llnl_xml(struct _ses_index_record* ptIR, int i, FILE* pFILE) {

	//  function prototypes
  
       void  _write_xml_function_tag(FILE* pFILE, struct _ses_index_record* ptIR, int i, int j);
       int _get_num_functions(ses_table_id tid);
 
	//  end function prototypes

       int j = 0;
       int num_functions = _get_num_functions(ptIR->_tblid[i]);

       for (j = 0; j < num_functions; j++) {

		_write_xml_function_tag(pFILE, ptIR, i, j);

	}
}

int _get_num_functions(ses_table_id tid) {
	
	int return_value = 0;
#ifdef CHANGE_TO_STANDARD
	return_value = _get_standard_num_functions(tid);
#else
	switch(tid) {

		case(301):
			return_value = 3;
			break;
		case(303):
			return_value = 3;
			break;
		default:
			break;
	}
#endif
	return return_value;
}


void  _write_xml_function_tag(FILE* pFILE, struct _ses_index_record* ptIR, int table_index, int function_index) {

	//  function prototypes
  
        const char* _get_function_name(ses_table_id tid, int function_index);
        const char* _get_function_type(ses_table_id tid);
	void _write_xml_reference_tag2(FILE* pFILE, char* function_path);
        void _create_xml_directory(char* path);

	//  end function prototypes

	const char* function_name = _get_function_name(ptIR->_tblid[table_index], function_index);
	const char* function_type = _get_function_type(ptIR->_tblid[table_index]);

  	char* function_file = malloc(sizeof(char)*100);
        sprintf(function_file, "L%ld/%s.xml", ptIR->_mid, function_name);

 	char* function_tag = malloc(sizeof(char) * 500);
 	sprintf(function_tag, "  <function name = \"%s\" type = \"%s\" >\n", 
		function_name, function_type);

	_write_tag(pFILE, function_tag);

        if (function_file != (char*)NULL) {

	     //  write the reference tag

	     _write_xml_reference_tag2(pFILE, function_file);

         }

	_write_tag(pFILE, "  </function>\n");

	free(function_file);
	function_file = (char*)NULL;

	free(function_tag);
	function_tag = (char*)NULL;
}

void _write_xml_reference_tag2(FILE* pFILE, char* function_file) {

	//  write the reference tag for the function

	char* reference_tag = malloc(sizeof(char)*100);
	sprintf(reference_tag, "    <reference path = \"%s\" />\n", function_file);
	_write_tag(pFILE, reference_tag);

	free(reference_tag);
	reference_tag = (char*)NULL;

}


const char* _get_function_name(ses_table_id tid, int index) {

#ifdef CHANGE_TO_STANDARD
#else

	if ((tid >= 300) && (tid < 400)) {
		if (index < 4) index = index + 4;
		if (index == 4) {
			return "pressure";
		}
		if (index == 5) {
			return "energy";
		}
		if (index == 6) {
			return "free_energy";
		}

	}
#endif
	return "unknown";
}

 

const char* _get_function_type(ses_table_id tid) {

#ifdef CHANGE_TO_STANDARD
#else
	if (tid == 301) {
		return "total";
	}
#endif
	return "unknown";

}





