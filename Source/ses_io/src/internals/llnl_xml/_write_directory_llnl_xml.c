


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "../xml_utilities.h"
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#define _write_directory_llnl_xml HEADER(_write_directory_llnl_xml)
#define _write_xml_material_tag HEADER(_write_xml_material_tag)
#define _write_xml_header_tag HEADER(_write_xml_header_tag)
#define _create_xml_directory HEADER(_create_xml_directory)
#define _write_xml_reference_tag HEADER(_write_xml_reference_tag)
#define _get_descriptive_material_name HEADER(_get_descriptive_material_name)

ses_boolean _write_directory_llnl_xml(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {

  //  write the top level xml 

  //  function prototypes

  void _write_xml_header_tag(FILE* pFILE);
  void _write_xml_material_tag(FILE* pFILE, ses_material_id mid, char* material_path, int len);

  //  end function prototypes

  ses_boolean return_value = SES_TRUE;


  FILE* pFILE = pSFH->_c_file_handle;

  _write_xml_header_tag(pFILE);
  _write_tag(pFILE, "<library name = \"sesame\">\n");


  int i = 0;
  ses_material_id mid;
  char* material_path;
  for (i = 0; i < ptDIR->_nfiles; i++) {

	material_path = ptDIR->_material_path[i];
	mid = ptDIR->_matid[i];
        _write_xml_material_tag(pFILE, mid, material_path, 20);
        
  }

  _write_tag(pFILE, "</library>\n");

  return return_value;
}

void _write_xml_material_tag(FILE* pFILE, ses_material_id mid, char* material_path, int len_material_path) {

        //  function prototypes

        void _create_xml_directory(char* path);
	char* _get_descriptive_material_name(ses_material_id mid);
        void _write_xml_reference_tag(FILE* pFILE, char* material_path);

	//  end function prototypes

        char* material_version = (char*)NULL;
	material_version = malloc(sizeof(char)*10);
	sprintf(material_version, "%ld", mid);

	char* material_name = _get_descriptive_material_name(mid);
  
        char* material_tag = malloc(sizeof(char) * 100);
	sprintf(material_tag, "  <material name=\"%s\" version = \"%s\" >\n", material_name, material_version);

  	_write_tag(pFILE, material_tag);

	
        char* mat_copy = malloc(sizeof(char)*(len_material_path+1));
	strcpy(mat_copy, material_path);
        if (material_path != (char*)NULL) {

	     //  create the path if it does not exist 

	     char* directory_name = strtok(mat_copy,"/");
	     _create_xml_directory(directory_name);

	     free(mat_copy);
	     mat_copy = (char*)NULL;

	     //  write the reference tag

	     _write_xml_reference_tag(pFILE, material_path);

         }
  	_write_tag(pFILE, "  </material>\n");

        free(material_version);
	material_version = (char*)NULL;

	free(material_name);
	material_name = (char*)NULL;
	
	free(material_tag);
	material_tag = (char*)NULL;
}

void _write_xml_header_tag(FILE* pFILE) {

	_write_tag(pFILE, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");

}


void _create_xml_directory(char* path) {

	//  if a directory does not exist named "path", create it

	int didit_work = 0;

	struct stat fileStat;
    	if(stat(path,&fileStat) < 0)    {
		//  directory path does not exist, so make it
		didit_work = mkdir(path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
	}
	else {
	}

}

void _write_xml_reference_tag(FILE* pFILE, char* material_path) {

        char* path_tag = malloc(sizeof(char) * 100);
        sprintf(path_tag,     "    <reference path=\"%s\" />\n", material_path);
	_write_tag(pFILE, path_tag);

	free(path_tag);
	path_tag = (char*)NULL;
}

char* _get_descriptive_material_name(ses_material_id mid) {

	char* return_value = (char*)NULL;
	char* material_version = (char*)NULL;
	char* material_name = (char*)NULL;
	switch(mid) {
		case(2030): 
			return_value = malloc(sizeof(char)*25);
			strcpy(return_value, "calcium");
			break;
                case(2720):
			return_value = malloc(sizeof(char)*25);
			strcpy(return_value, "silver");
			break;

		default : 
        		material_version = (char*)NULL;
			material_version = malloc(sizeof(char)*10);
			sprintf(material_version, "%ld", mid);
		
		        material_name = malloc(sizeof(char) * 25);
		        sprintf(material_name, "material %s", material_version);
			return_value = material_name;
		
			free(material_version);
			material_version = (char*)NULL;
			break;
	}	

	return return_value;

}











