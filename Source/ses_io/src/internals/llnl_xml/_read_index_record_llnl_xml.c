#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "llnl_tags.h"
#include "string_list.h"
#include "table_map.h"

//////////////////DEFINES///////////////////////
//USE_LIBXML - turned on if libxml2.a available, used with llnl xml format
////////////////////////////////////////////////


#ifdef USE_LIBXML2
#include "libxml/parser.h"
#include <libxml/xmlmemory.h>
#include "libxml/xmlstring.h"
#endif

#define _read_index_record_llnl_xml HEADER(_read_index_record_llnl_xml)
#define check_errors_READ_INDEX_RECORD_LLNL_XML HEADER(check_errors_READ_INDEX_RECORD_LLNL_XML)
#define parse_my_material_tag HEADER(parse_my_material_tag)
#define check_errors_2args HEADER(check_errors_2args)
#define parse_my_function_tag HEADER(parse_my_function_tag)
#define parse_axis_tag HEADER(parse_axis_tag)
#define back_out_tables HEADER(back_out_tables)
#define _compute_tables HEADER(_compute_tables)
#define seen_lower HEADER(seen_lower)
#define _compute_nwds HEADER(_compute_nwds)
#define _compute_array_filenames HEADER(_compute_array_filenames)
#define _compute_array_addresses HEADER(_compute_array_addresses)
#define _get_address_for_array HEADER(_get_address_for_array)
#define _get_density_address HEADER(_get_density_address)
#define _get_temperature_address HEADER(_get_temperature_address)
#define _get_function_address HEADER(_get_function_address)

ses_error_flag _read_index_record_llnl_xml(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {

  /*  function prototypes */

   ses_error_flag check_errors_READ_INDEX_RECORD_LLNL_XML(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH, long offset);
#ifdef USE_LIBXML2
  ses_error_flag parse_my_material_tag(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset);
#endif

   /*  end function prototypes */

   ses_error_flag return_flag = check_errors_READ_INDEX_RECORD_LLNL_XML(the_index_record, pSFH, offset);
   if (return_flag != SES_NO_ERROR) {
	return return_flag;
   }

  //  read the index record for llnl xml format type 


  ses_error_flag return_value = SES_READ_ERROR; 
#ifdef USE_LIBXML2
  return_value = parse_my_material_tag(the_index_record, pSFH, offset);
#else
  return_value = 0;
#endif
  the_index_record->_date1 = 0;
  the_index_record->_date2 = 0;
  the_index_record->_vers = 0;
  the_index_record->_ready = SES_TRUE;

  return return_value;

}
ses_error_flag check_errors_READ_INDEX_RECORD_LLNL_XML(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {

   if (the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_index_record_llnl_xml: index_record_null NULL\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

   if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_index_record_llnl_xml: ses file handle null\n");
#endif
      return SES_NULL_OBJECT_ERROR;
   }

  FILE* pFILE = pSFH->_c_file_handle;
   if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
      printf("_read_index_record_llnl_xml: c file handle null \n");
#endif
      return SES_NULL_OBJECT_ERROR;
   } 


  return SES_NO_ERROR;
}

/**************** HELPER FUNCTIONS ****************************************/

#ifdef USE_LIBXML2
ses_error_flag parse_my_material_tag(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {

    //  read the material file for the llnl xml format type

    //  function prototypes

    void parse_my_function_tag(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH, xmlDocPtr doc, xmlNodePtr cur, int index);
    void back_out_tables(struct _ses_index_record* ptIR);
    ses_error_flag check_errors_2args(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);

    //  end function prototypes

    ses_error_flag return_value = SES_NO_ERROR;
    return_value = check_errors_2args(the_index_record, pSFH);
    if (return_value != SES_NO_ERROR) {
	return return_value;
    }

    //  open the material file, doc is a tree containing the parsed document */

    char* docname = pSFH->_material_filename;
    xmlDoc* doc = xmlReadFile(docname, NULL, 0);
#define DEBUG_LLNL_NULL_FILE
#ifdef DEBUG_LLNL_NULL_FILE
    if (doc == (xmlDoc*)NULL) {
	return SES_READ_ERROR;
    }
#endif

    // Get the root element node -- the material

    xmlNode* material_root = xmlDocGetRootElement(doc);
    if (xmlStrcmp(material_root->name, (const xmlChar *) "material")) {
		xmlFreeDoc(doc);
		return SES_READ_ERROR;
    }

    xmlNode* cur = material_root;

    //  get the material id (an attribute of the material_root)

    char* mid_string = (char*)NULL;
    mid_string = get_attribute("version", doc, cur);
    if (mid_string != NULL) {
    	the_index_record->_mid = atoi(mid_string);
        free(mid_string);
	mid_string = (char*)NULL;
    }

    //  locate the 'functions' associated with the material and count them 

    int nfuncs = 0;
    cur = material_root->xmlChildrenNode;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar*)"function"))){
			nfuncs++;
		}
		 
		cur = cur->next;
    }

    the_index_record->_nfuncs = nfuncs;

    //  create memory for index record

    the_index_record->_nr = malloc(sizeof(ses_number)*nfuncs);
    the_index_record->_nt = malloc(sizeof(ses_number)*nfuncs);
    the_index_record->_type = malloc(sizeof(char*)*nfuncs);
    the_index_record->_function_filename = malloc(sizeof(char*)*nfuncs);
    int i = 0;
    for (i = 0; i < nfuncs; i++) {
	the_index_record->_nr[i] = 0;
	the_index_record->_nt[i] = 0;
	the_index_record->_type[i] = (char*)NULL;
	the_index_record->_function_filename[i] = (char*)NULL;
    }

    //  parse the function tags 

    cur = material_root->xmlChildrenNode;
    int index = 0;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar*)"function"))){
			parse_my_function_tag(the_index_record, pSFH, doc, cur, index);
			index++;
		}

		 
		cur = cur->next;
    }

    // identify the tables


    back_out_tables(the_index_record);

    // free the document 

    xmlFreeDoc(doc);
    doc = (xmlDoc*)NULL;
    xmlCleanupParser();

    return return_value;

}
#endif

ses_error_flag check_errors_2args(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {

    if (ptIR == (struct _ses_index_record*)NULL) {
	return SES_NULL_OBJECT_ERROR;
    }
    if (pSFH == (struct _ses_file_handle*)NULL) {
	return SES_NULL_OBJECT_ERROR;
    }
    return SES_NO_ERROR;
}


/************************************/

#ifdef USE_LIBXML2
void parse_my_function_tag(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH, xmlDocPtr doc, xmlNodePtr cur, int index) {


    //  fill the directory data structure with information needed to go to 
    //    the index record for any material 

   //  function prototypes

    void parse_axis_tag(struct _ses_index_record* ptIR, xmlDocPtr doc, xmlNodePtr cur, int index);

    ses_error_flag check_errors_2args(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);

    //  end function prototypes

    ses_error_flag return_value = SES_NO_ERROR;
    return_value = check_errors_2args(ptIR, pSFH);
    if (return_value != SES_NO_ERROR) {
	return;
    }


    //  get the type of the function

    char* type = get_attribute("type", doc, cur);
    if (type != (char*)NULL) {
        ptIR->_type[index] = malloc(sizeof(char)*(strlen(type)+1));
        strcpy(ptIR->_type[index], type);
        free(type);
	type = (char*)NULL;
    }
    else {
        ptIR->_type[index] = (char*)NULL;
    }

    //  get the filename for the function 

    cur = cur->xmlChildrenNode;

    char* path = (char*)NULL;
    while (cur != NULL) {
		if ((!xmlStrcmp(cur->name, (const xmlChar*)"reference"))){
			path = get_attribute("path", doc, cur);
			if (path != (char*)NULL) {
				ptIR->_function_filename[index] = malloc(sizeof(char)*(strlen(path)+1));
				strcpy(ptIR->_function_filename[index], path);
				free(path);
				path = (char*)NULL;				
			}
		}
		 
		cur = cur->next;
    }

    //  open the function tag file and get a new xmlDocPtr 


    xmlDoc* docpath = xmlReadFile(ptIR->_function_filename[index], NULL, 0);
    if (docpath != NULL) {

       xmlNode* function_root = xmlDocGetRootElement(docpath);
       if ((xmlStrcmp(function_root->name, (const xmlChar *) "function"))) {
		xmlFreeDoc(docpath);
		return;
        }
    	//  go to the function tag 

	xmlNode* curfunc1 = function_root;
	if ((!xmlStrcmp(curfunc1->name, (const xmlChar *)"function"))){

  		//  parse the axis tag
	        xmlNode* curfunc2 = curfunc1->xmlChildrenNode;
	        while (curfunc2 != NULL) {
			if ((!xmlStrcmp(curfunc2->name, (const xmlChar*)"axis"))){
				parse_axis_tag(ptIR, docpath, curfunc2, index);	
			}
			else {
			}
		 
			curfunc2 = curfunc2->next;
    		}


	}


    	// free the document 

	xmlFreeDoc(docpath);
        docpath = (xmlDoc*)NULL;

    }

}

void parse_axis_tag(struct _ses_index_record* ptIR, xmlDocPtr doc, xmlNodePtr cur, int index) {

    //  fill the directory data structure with information needed to go to 
    //    the index record for any material 

    //  go to the axis tag 

	        
    while (cur != NULL) {
	if ((!xmlStrcmp(cur->name, (const xmlChar*)"axis"))){
		char* nr = get_attribute("nr", doc, cur);
		char* nt = get_attribute("nt", doc, cur);
		if (nr != (char*)NULL) {
			ptIR->_nr[index] = atoi(nr);
			free(nr);
			nr = (char*)NULL;
		}
		if (nt != (char*)NULL) {
			ptIR->_nt[index] = atoi(nt);
			free(nt);
			nt = (char*)NULL;
		}


	}
		 
	cur = cur->next;
    }

}

#endif

void back_out_tables(struct _ses_index_record* ptIR) {

    //  function prototypes 
 
    int _compute_tables(struct _ses_index_record* ptIR);
    void _compute_nwds(struct _ses_index_record* ptIR);
    void _compute_array_filenames(struct _ses_index_record* ptIR);
    void _compute_array_addresses(struct _ses_index_record* ptIR);

    //  end function prototypes

    //  given the list of functions, their types, and their grid sizes,
    //  back out the list of tblid's, nwds, array_filenames, and array_iadr

    //  go through the list of functions and figure out the tables


    int ntables = _compute_tables(ptIR);

    ptIR->_nrec = ntables;

    //  compute the nwds for each table from nr, nt, and np

    _compute_nwds(ptIR);

    //  set the iadr array to 0 -- not used for llnl_xml

    ptIR->_iadr = malloc(sizeof(long) * ntables);
    int i = 0;
    for (i=0; i < ntables; i++) {
	ptIR->_iadr[i] = 0;
    }

    //  go through the list of tables and attach array_filenames to the tables

    _compute_array_filenames(ptIR);

    //  go through the list of tables and figure out the array addresses

    _compute_array_addresses(ptIR);
}

int _compute_tables(struct _ses_index_record* ptIR) {

   // function prototypes

   ses_boolean seen_lower(char** types, char* the_type, int i);

   //  end function prototypes

   //  compute ptIR->_tblid and ptIR->_nrec from the ptIR->_type array
   int ntables = 0;

   int i = 0;
   ses_table_id* my_tblid = malloc(sizeof(ses_table_id) * ptIR->_nfuncs);
   ses_boolean* seen_it = malloc(sizeof(ses_boolean) * ptIR->_nfuncs);
   for (i=0; i < ptIR->_nfuncs; i++) {
        my_tblid[i] = 0;
	seen_it[i] = SES_FALSE;
   }
   int nseen = 0;
   for (i = 0; i < ptIR->_nfuncs; i++) {
	
	if (seen_it[i] == SES_FALSE) {
		if (seen_lower(ptIR->_type, ptIR->_type[i], i) == SES_TRUE) {
			seen_it[i] = SES_TRUE;
			nseen++;
		}		
 	}
   }

   ntables = ptIR->_nfuncs - nseen;
   ptIR->_nrec = ntables;

   ptIR->_tblid = malloc(sizeof(ses_table_id) * ntables);
   ses_number* new_nr = malloc(sizeof(ses_table_id) * ntables);
   ses_number* new_nt = malloc(sizeof(ses_table_id) * ntables);

   char** new_type = malloc(sizeof(char*) * ntables);
   int index = 0;
   for (i = 0; i < ptIR->_nfuncs; i++) {
	if (seen_it[i] == SES_FALSE) {
		ptIR->_tblid[index] = _get_standard_table_id(ptIR->_type[i]);
                new_nr[index] = ptIR->_nr[i];
                new_nt[index] = ptIR->_nt[i];

		new_type[index] = malloc(sizeof(char)*(strlen(ptIR->_type[i]) + 1));
		strcpy(new_type[index], ptIR->_type[i]);
		
		index++;
	}
	free(ptIR->_type[i]);
	ptIR->_type[i] = (char*)NULL;
   }
   free(ptIR->_nr);
   ptIR->_nr = (ses_number*)NULL;

   free(ptIR->_nt);
   ptIR->_nt = (ses_number*)NULL;

   free(ptIR->_type);
   ptIR->_type = (char**)NULL;

   ptIR->_nr = new_nr;
   ptIR->_nt = new_nt;
   ptIR->_type = new_type;

   free(seen_it);
   seen_it = (ses_boolean*)NULL;
	
   free(my_tblid);
   my_tblid = (ses_table_id*)NULL;
   return ntables;

  
}

ses_boolean seen_lower(char** types, char* the_type, int index) {

   ses_boolean return_value = SES_FALSE;
   int i = 0;
   if ((types != (char**)NULL) && (the_type != (char*)NULL) && (index >= 0)) {
 	  for (i = 0; i < index; i++) {
		if (strcmp(types[i], the_type) == 0) {
			return_value = SES_TRUE;
		}
  	 }
   }
   return return_value;

}

void _compute_nwds(struct _ses_index_record* ptIR) {

   int i = 0;
   ptIR->_nwds = malloc(sizeof(long) * ptIR->_nrec);
   for (i = 0; i < ptIR->_nrec; i++) {
        ptIR->_nwds[i] = 0;
//#define CHANGE_TO_STANDARD
#ifdef CHANGE_TO_STANDARD
	long narrays = _get_standard_num_arrays(ptIR->_tblid[i]);
	long* the_sizes = _get_standard_sizes(ptIR->_tblid[i], ptIR->_nr[i], ptIR->_nt[i], -1); 
        int j = 0;
	for (j = 0; j < narrays; j++) {
		ptIR->_nwds[i] = ptIR->_nwds[i] + the_sizes[j];
	}

#else
	if (ptIR->_tblid[i] == 301) {
		ptIR->_nwds[i] = 1 + 1 + ptIR->_nr[i] + ptIR->_nt[i] + (ptIR->_nfuncs * ptIR->_nr[i] * ptIR->_nt[i]);
	}
#endif
   }
}
void _compute_array_filenames(struct _ses_index_record* ptIR) {

   //  compute array_filenames for the data records

   int i = 0;
   int j = 0;
   int narrays = 0;
   ptIR->_array_filename = malloc(sizeof(char**) * ptIR->_nrec); 	
   ptIR->_narrays = malloc(sizeof(long)*ptIR->_nrec);
   for (i = 0;  i < ptIR->_nrec; i++) {

	//  get the number of arrays for the table
	narrays = _get_standard_num_arrays(ptIR->_tblid[i]);
        ptIR->_narrays[i] = narrays;
        //  create the memory for the array_filename
	ptIR->_array_filename[i] = malloc(sizeof(char*) * narrays);
	for (j = 0; j < narrays; j++) {

#ifdef CHANGE_TO_STANDARD
	 	if ((ptIR->_tblid[i] > 100) && (ptIR->_tblid[i] <= 199)) {
		}
	 	if (ptIR->_tblid[i] == 201) {
		}
		if ((ptIR->_tblid[i] >= 301) && (ptIR->_tblid[i] <= 306)) {
			char* filename = (char*)NULL;
			if (j <= 3) {
				filename = ptIR->_function_filename[0];
			}
			else {
				filename = ptIR->_function_filename[j-4];
			}
			ptIR->_array_filename[i][j] = malloc(sizeof(char)*(strlen(filename)+1));
			strcpy(ptIR->_array_filename[i][j], filename);
		}
	 	if ((ptIR->_tblid[i] >= 401) && (ptIR->_tblid[i] <= 499)) {
		}
	 	if ((ptIR->_tblid[i] >= 501) && (ptIR->_tblid[i] <= 599)) {
		}
	 	if ((ptIR->_tblid[i] >= 601) && (ptIR->_tblid[i] <= 699)) {
		}
#else
		if (ptIR->_tblid[i] == 301) {
			char* filename = (char*)NULL;
			if (j <= 3) {
				filename = ptIR->_function_filename[0];
			}
			else {
				filename = ptIR->_function_filename[j-4];
			}
			ptIR->_array_filename[i][j] = malloc(sizeof(char)*(strlen(filename)+1));
			strcpy(ptIR->_array_filename[i][j], filename);
		}
#endif
	}
	

        
   }


}
void _compute_array_addresses(struct _ses_index_record* ptIR) {

   // function prototypes

   long _get_address_for_array(struct _ses_index_record* ptIR, char* filename, ses_table_id tid, int array_index);

   //  end function prototypes

   //  compute array_addresses for the data records

   int i = 0;
   int j = 0;
   int narrays = 0;
   ptIR->_array_iadr = malloc(sizeof(long*) * ptIR->_nrec); 	
   for (i = 0;  i < ptIR->_nrec; i++) {

	//  get the number of arrays for the table
	//narrays = _get_standard_num_arrays(ptIR->_tblid[i]);
        narrays = ptIR->_narrays[i];
        //  create the memory for the array_filename
	ptIR->_array_iadr[i] = malloc(sizeof(long) * narrays);
	for (j = 0; j < narrays; j++) {
		ptIR->_array_iadr[i][j] = _get_address_for_array(ptIR, ptIR->_array_filename[i][j], ptIR->_tblid[i], j);
	}        
   }
}

long _get_address_for_array(struct _ses_index_record* ptIR, char* filename, ses_table_id tid, int array_index) {

   // function prototypes

   long _get_density_address(char* filename);
   long _get_temperature_address(char* filename);
   long _get_function_address(char* filename);

   //  end function prototypes

   long return_value = 0;
#ifdef CHANGE_TO_STANDARD
#else
   if (tid == 301) {
	switch(array_index) {
		case(0):
			return_value = -1;
			break;
		case(1):
			return_value = -1;
			break;
		case(2):
			return_value = _get_density_address(filename);
			break;
		case(3):
			return_value = _get_temperature_address(filename);
			break;
		default:
			return_value = _get_function_address(filename);
			break;
	}	
   }
#endif
   return return_value;
}


long _get_density_address(char* filename) {
	FILE* pFILE = fopen(filename, "r");

	char* tag = _skip_tag(pFILE, "axis");
	free(tag);
	tag = (char*)NULL;

	long return_value = ftell(pFILE);
	fclose(pFILE);
	return return_value;
}

long _get_temperature_address(char* filename) {
	FILE* pFILE = fopen(filename, "r");

	char* tag = _skip_tag(pFILE, "axis");
	free(tag);
	tag = (char*)NULL;
	tag = _skip_tag(pFILE, "/axis");
	free(tag);
	tag = (char*)NULL;
	/* ses_boolean didit = SES_FALSE; */
        /* didit = */ _find_tag_data(pFILE, "axis");
	long return_value = ftell(pFILE);
	fclose(pFILE);
	return return_value;
}

long _get_function_address(char* filename) {
	FILE* pFILE = fopen(filename, "r");
	char* tag = _skip_tag(pFILE, "axis");
	free(tag);
	tag = (char*)NULL;
	tag = _skip_tag(pFILE, "/axis");
	free(tag);
	tag = (char*)NULL;
	tag = _skip_tag(pFILE, "axis");
	free(tag);
	tag = (char*)NULL;
	tag = _skip_tag(pFILE, "/axis");
	free(tag);
	tag = (char*)NULL;
	long return_value = ftell(pFILE);
	fclose(pFILE);
	return return_value;
}


  
















