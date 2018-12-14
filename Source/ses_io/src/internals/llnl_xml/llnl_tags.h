
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

#define _find_tag_data HEADER(_find_tag_data)
#define _skip_tag HEADER(_skip_tag)

#ifdef USE_LIBXML2
#define get_attribute HEADER(get_attribute)
char* get_attribute (const char* name, xmlDocPtr doc, xmlNodePtr cur);
#endif

ses_boolean  _find_tag_data(FILE* pFILE, const char* name);
char* _skip_tag(FILE* pFILE, char* tag_name);
 





