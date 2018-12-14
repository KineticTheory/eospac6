
#include "ses_defines.h"

#define print_materials_on_file HEADER(print_materials_on_file)


void print_materials_on_file(ses_string sesame_file) {

  if (sesame_file != (ses_string)NULL) {

  	printf("\n\nprint_materials_on_file: Getting materials on sesame file : %s\n", sesame_file);

        ses_file_handle the_handle = ses_open(sesame_file, 'R');

        long size = 0;
	ses_material_id_reference the_materials = ses_get_materials(the_handle, &size);
        if (the_materials == (ses_material_id_reference)NULL) {
            printf("print_materials_on_file: null materials list\n");
        }
        else {
		if (size > 0) {
			int i = 0;
			for (i = 0; i < size; i++) {
				printf("print_materials_on_file: Material[%d] is %ld\n", i, (long)the_materials[i]);
			}
		}
		            
        }

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		printf("print_materials_on_file: error on close:  %s\n", the_error);
	}

  }
  else {
	printf("\n\nprint_materials_on_file: null filename passed to print_materials_on_file\n");	
  }

}
