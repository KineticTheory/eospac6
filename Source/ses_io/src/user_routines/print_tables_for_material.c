
#include "ses_defines.h"

#include <string.h>

#define print_tables_for_material HEADER(print_tables_for_material)

/*
 void print_tables_for_material(ses_string sesame_file, ses_material_id mid) {
 
 if (sesame_file != (ses_string)NULL) {
 
 printf("\n\nprint_tables_for_material: Getting tables for material %ld on sesame file : %s\n", (long)mid, sesame_file);
 
 ses_file_handle the_handle = ses_open(sesame_file, 'R');
 
 ses_error_flag didit_setup = SES_NO_ERROR;
 didit_setup = ses_setup(the_handle, mid, 201);
 
 long size = 0;
 ses_table_id_reference the_tables = ses_get_table_ids(the_handle,mid, &size);
 if (the_tables == (ses_table_id_reference)NULL) {
 printf("print_tables_for_material: null materials list\n");
 }
 else {
 if (size > 0) {
 int i = 0;
 for (i = 0; i < size; i++) {
 printf("print_tables_for_material: Table[%d] is %ld\n", i, (long)the_tables[i]);
 }
 }
 
 }
 
	ses_error_flag didit_close = ses_close(the_handle);
 if (didit_close != SES_NO_ERROR) {l
 ses_string the_error = ses_print_error_condition(the_handle);
 printf("print_tables_for_material: error on close:  %s\n", the_error);
	}
 
 }
 else {
	printf("\n\nprint_tables_for_material: null filename passed to print_materials_on_file\n");
 }
 
 }
 
 */

ses_string print_tables_for_material(ses_string sesame_file, ses_material_id mid) {
	
	
	ses_string return_value = malloc(sizeof(char)*1000);
	strcpy(return_value, "");
	if (sesame_file != (ses_string)NULL) {
		
		ses_file_handle the_handle = ses_open(sesame_file, 'R');
		


#ifdef DEBUG_PRINT
		ses_error_flag didit_setup = SES_NO_ERROR;
		didit_setup = ses_setup(the_handle, mid, 201);
		printf("print_tables_for_material:  after ses_setup with mid = %ld, didit_setup is %i\n", mid, didit_setup);
#else
		ses_setup(the_handle, mid, 201);
#endif

		
		long size = 0;
		ses_table_id_reference the_tables = ses_get_table_ids(the_handle,mid, &size);
		if (the_tables == (ses_table_id_reference)NULL) {
			//printf("print_tables_for_material: null materials list\n");
		}
		else {
			if (size > 0) {
				int i = 0;
				ses_string the_string = malloc(sizeof(char)*100);
				for (i = 0; i < size; i++) {
					sprintf(the_string, "Table[%d] is %ld\n", i, (long)the_tables[i]);
					//printf("the_string is %s\n", the_string);
					strcat(return_value, the_string);
				}
			}
			
		}
		
		ses_error_flag didit_close = ses_close(the_handle);
		if (didit_close != SES_NO_ERROR) {
#ifdef DEBUG_PRINT
			ses_string the_error = ses_print_error_condition(the_handle);
			printf("print_tables_for_material: error on close:  %s\n", the_error);
#else
			ses_print_error_condition(the_handle);
#endif
		}
		
	}
	else {
		//printf("\n\nprint_tables_for_material: null filename passed to print_materials_on_file\n");
	}
	
	
	//printf("return_value is %s\n", return_value);
	return return_value;
}
