#include "ses_defines.h"

#define _get_standard_table_id HEADER(_get_standard_table_id)
#define get_standard_table_functions HEADER(_get_standard_table_functions)


int _get_standard_table_id(char* the_found_type);
char** get_standard_table_functions(int the_tid, int* list_size);
