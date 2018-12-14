
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table_map.h"


int _get_standard_table_id(char* the_found_type) {

	int return_value = 0;

#ifdef CHANGE_TO_STANDARD
#else
	if (the_found_type != (char*)NULL) {
		if (strcmp(the_found_type, "total") == 0) {
			return_value = 301;
		}
		if (strcmp(the_found_type, "cold") == 0) {
			return_value = 306;
		}
	}
#endif

	return return_value;
}

char** get_standard_table_functions(int the_tid, int* list_size) {

	char** return_value = (char**)NULL;
	*list_size = 0;

#ifdef CHANGE_TO_STANDARD
#else
	if (the_tid == 301) {
		*list_size = 3;
		return_value = malloc(sizeof(char*)*3);
		return_value[0] = malloc(sizeof(char*) * 16);
		strcpy(return_value[0], "total::pressure");
		return_value[1] = malloc(sizeof(char*) * 14);
		strcpy(return_value[1], "total::energy");
		return_value[2] = malloc(sizeof(char*) * 19);
		strcpy(return_value[2], "total::free_energy");

	}

	if (the_tid == 306) {
		return_value = malloc(sizeof(char*)*3);
		*list_size = 3;
		return_value[0] = malloc(sizeof(char*) * 15);
		strcpy(return_value[0], "cold::pressure");
		return_value[1] = malloc(sizeof(char*) * 13);
		strcpy(return_value[1], "cold::energy");
		return_value[2] = malloc(sizeof(char*) * 18);
		strcpy(return_value[2], "cold::free_energy");

	}
#endif
	return return_value;
}

