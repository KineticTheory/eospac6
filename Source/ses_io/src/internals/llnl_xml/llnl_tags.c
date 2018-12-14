
#include "llnl_tags.h"
#include <string.h>

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

//////////////////DEFINES///////////////////////
//USE_LIBXML - turned on in libxml2.a available, used with llnl xml format
////////////////////////////////////////////////


#ifdef USE_LIBXML2

char* get_attribute (const char* name, xmlDocPtr doc, xmlNodePtr cur) {

  /* return the value of the attribute from the tab */

  char* return_value = (char*)NULL;
  if (name != (const char*)NULL) {
	xmlChar* attribute = xmlGetProp(cur, (const xmlChar*)name);
	if (attribute != NULL) {

		const char* almost_attribute = (char*)attribute;
		return_value = malloc(sizeof(char)*(strlen(almost_attribute)+1));
		strcpy(return_value, almost_attribute);			

		free(attribute);
		attribute = (xmlChar*)NULL;
	}
  }
  return return_value;
}

#endif

ses_boolean  _find_tag_data(FILE* pFILE, const char* name) {

 /*  position the file pointer at the tag data for the first "name" tag */

  ses_boolean return_value = SES_TRUE;

  char* the_tag = NULL;


  /*  skip through the file until the name tag is found  */

  the_tag = _skip_tag(pFILE, "");
    
  while ((!feof(pFILE)) && (strstr(the_tag, name) == NULL)) {
    free(the_tag);
    the_tag = (char*)NULL;
    the_tag = _skip_tag(pFILE, "");
  }
  free(the_tag);
  the_tag = (char*)NULL;

  
  if (feof(pFILE) == EOF) {
    return_value = SES_FALSE;
  }

  return return_value;

        
}

    

