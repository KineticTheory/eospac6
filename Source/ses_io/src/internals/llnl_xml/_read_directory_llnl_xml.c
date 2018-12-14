


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

//////////////////DEFINES///////////////////////
//USE_LIBXML - turned on in libxml2.a available, used with llnl xml format
////////////////////////////////////////////////


#ifdef USE_LIBXML2
#include "libxml/parser.h"
#include <libxml/xmlmemory.h>
#include "libxml/xmlstring.h"
#endif

#include "llnl_tags.h"
#include <string.h>

#define _read_directory_llnl_xml HEADER(_read_directory_llnl_xml)

ses_error_flag _read_directory_llnl_xml(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {

  /*  function prototypes */

   ses_error_flag check_errors_READ_DIRECTORY_LLNL_XML(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);
    ses_error_flag parse_library_tag(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_READ_DIRECTORY_LLNL_XML(the_directory, pSFH);
   if (return_flag != SES_NO_ERROR) {
	return return_flag;
   }

    //  read the directory for llnl xml format type 

    ses_error_flag return_value = SES_NO_ERROR;
#ifdef USE_LIBXML2
    return_value = parse_library_tag(the_directory, pSFH);
#endif
    the_directory->_has_multiple_files = SES_TRUE;

    return return_value;

}

ses_error_flag check_errors_READ_DIRECTORY_LLNL_XML(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {

   if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_directory_llnl_xml: directory NULL\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_directory_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

  FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_directory_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 

    return SES_NO_ERROR;
}


/*************************** HELPER FUNCTIONS **************************************/

#ifdef USE_LIBXML2

ses_error_flag parse_library_tag(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {

    //  parse the library tag to read the directory for llnl xml format type

    //  function prototypes

    void parse_material_tag(struct _ses_directory* ptDIR, xmlDocPtr doc, xmlNodePtr cur, int index);

    //  end function prototypes

    ses_error_flag return_value = SES_NO_ERROR;

    /*  position the file to the start of the first library tag */

    FILE* pFILE = pSFH->_c_file_handle;
    int beginning_position = ftell(pFILE);
    rewind(pFILE);

    /* doc is a tree containing the parsed document */

    char* docname = pSFH->_filename;

    xmlDoc* doc = xmlReadFile(docname, NULL, 0);
    if (doc == NULL) {
	return SES_READ_ERROR;
    }

    /*Get the root element tag -- library*/

    xmlNode* library_root = xmlDocGetRootElement(doc);
    if (xmlStrcmp(library_root->name, (const xmlChar *) "library")) {
	xmlFreeDoc(doc);
	return SES_READ_ERROR;
    }

    /*  count the material tags */

    int nfiles = 0;
    xmlNode* cur = library_root->xmlChildrenNode;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar *)"material"))){
			nfiles++;
		}
		 
		cur = cur->next;
    }

    /*  create memory for the directory structures */

    if (nfiles > 0) {
	the_directory->_matid = malloc(sizeof(ses_material_id)*nfiles);
	the_directory->_nwds = malloc(sizeof(ses_material_id)*nfiles);
	the_directory->_iadr = malloc(sizeof(ses_material_id)*nfiles);
	the_directory->_material_path = malloc(sizeof(ses_material_id)*nfiles);

    }

    /*  parse the material tags */

    cur = library_root->xmlChildrenNode;
    int index = 0;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar *)"material"))){
			parse_material_tag(the_directory, doc, cur, index);
	   		the_directory->_nwds[index] = 0;
	    		the_directory->_iadr[index] = 0;
			index++;
		}
		 
		cur = cur->next;
    }

    /*free the document */

    xmlFreeDoc(doc);
    doc = (xmlDoc*)NULL;
    xmlCleanupParser();

    /*  fill the directory data structure */

    the_directory->_nfiles = nfiles;
    the_directory->_date = 0;
    the_directory->_version = 0;
    the_directory->_ready = SES_TRUE;

    /*  return pFILE to the original position */

    fseek(pSFH->_c_file_handle, beginning_position, SEEK_SET);

    return return_value;

}

void parse_material_tag(struct _ses_directory* the_directory, xmlDocPtr doc, xmlNodePtr cur, int index) {

    /*  fill the directory data structure with information needed to go to 
        the index record for any material */
    //  the xmlNode* cur is pointed at the material tag

    //  function prototypes

    void parse_function_tag(struct _ses_directory* ptDIR, xmlDocPtr doc, xmlNodePtr cur, int index);

    //  end function prototypes

    /*  get the filename for the material */

    /*  get the matid (on the version attribute of the material tag) */

    char* version = (char*)NULL;
    version = get_attribute("version", doc, cur);
    if (version != (char*)NULL) {
    	the_directory->_matid[index] = atoi(version);
	free(version);
	version = (char*)NULL;
    }
    /*  get the material paths */

    cur = cur->xmlChildrenNode;
    char* path = (char*)NULL;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar *)"reference"))){
			path = get_attribute("path", doc, cur);
			the_directory->_material_path[index] = malloc(sizeof(char)* (strlen(path) + 1));
	    		strcpy(the_directory->_material_path[index], path);
 			free(path);
			path = (char*)NULL;
		}
		 
		cur = cur->next;
    }
}

#endif


































