#include "ses_defines.h"

#include <string.h>

#define print_array_from_table HEADER(print_array_from_table)

void print_array_from_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid, ses_string the_label1) {

  /*  put parens around the label */

  ses_string the_label = malloc(sizeof(char) * (strlen(the_label1) + 3));
  strcpy(the_label, "\"");
  strcat(the_label, the_label1);
  strcat(the_label, "\"");

  int nr = 0;
  int nt = 0;

  ses_word_reference density = NULL;
  ses_word_reference temperature = NULL;
  ses_label density_label = NULL;
  ses_label temperature_label = NULL;

  if (sesame_file != (ses_string)NULL) {

	printf("print_array_from_table:  Material %ld Table %ld\n\n", (long)mid, (long)tid);

        ses_file_handle the_handle = ses_open(sesame_file, 'R');
	if (ses_is_valid(the_handle) == SES_TRUE) {

		ses_error_flag didit_setup = ses_setup(the_handle, mid, tid);
		if (didit_setup == SES_NO_ERROR) {

			  /*  read the file with the iterator interface */
  
			  ses_label myLabel;
			  ses_number array_size;
			  ses_word_reference ptBuffer;
			  int i=0;

			  ses_boolean first = SES_TRUE;
			  while (ses_has_next(the_handle)) {

			    myLabel = ses_get_label(the_handle);
			    array_size = ses_array_size_next(the_handle);
        
			    /*  get the data */

			    if (tid < 200) {

                      		if (tid == 100) {

					if (first == SES_TRUE) {
						first = SES_FALSE;
						ptBuffer = ses_read_next(the_handle);
 						if (ptBuffer != (ses_word_reference)NULL) {
    
			          			printf("\nprint_array_from_table: The data -- %s is: \n\n", myLabel);
			          			for (i=0;  i<array_size; i++) {
			          			  printf("%E, ", ptBuffer[i]);
			          			}
			          			printf("\n");
			          			free(ptBuffer);
			          			ptBuffer = (ses_word_reference)NULL;
			        		}
			        		else {
			          			printf("print_array_from_table: Error in reading data for 100 table\n");
			          			break;
			        		}	
					}
					else {

						ptBuffer = ses_read_next(the_handle);
						printf("print_array_from_table:  %s\n", (ses_string)ptBuffer);
			          		printf("\n");
			          		free(ptBuffer);
			          		ptBuffer = (ses_word_reference)NULL;
						
					}	

				}
				else {
					ses_string* the_string = malloc(sizeof(ses_string)*1);
                                	the_string[0] = (ses_string)NULL;
					ses_error_flag didit_readit = ses_comments(the_handle, the_string);
                                        
					if (didit_readit == SES_NO_ERROR) {
						if (strcmp(myLabel, the_label) == 0) {
							printf("\nprint_array_from_table:  The data -- %s is \n\n", myLabel);
							printf("print_array_from_table:  %s\n", *the_string);

						}
						ses_error_flag didit_skip = ses_skip(the_handle);
						if (didit_skip != SES_NO_ERROR) {
							printf("\nprint_array_from_table:  Error in iterator skip\n");
						}
					}
					else {
						printf("\nprint_array_from_table:  Error in reading comments\n");
					}
				}


			    }
			    else {
			    	ptBuffer = ses_read_next(the_handle);
			    
			        if (ptBuffer != (ses_word_reference)NULL) {

				  //printf("compare |%s| to |%s|\n", myLabel, the_label);
                                                                    
				  if (strcmp(myLabel, "\"nr (number densities)\"") == 0) {
					nr = (int)ptBuffer[0];
				  }		
                                        			
				  if (strcmp(myLabel, "\"nt (number temperatures)\"") == 0) {	
					nt = (int)ptBuffer[0];
				  }					
						
                                  if (strcmp(myLabel, "\"r - density (Mg/m3)\"") == 0) {
					density = malloc(sizeof(ses_word)*nr);	
					int i2 = 0;
					for (i2 = 0; i2 < nr; i2++) {
						density[i2] = ptBuffer[i2];
					}	
					density_label = myLabel;
			          }

                                  if (strcmp(myLabel, "\"t - temperature (K)\"") == 0) {
					temperature = malloc(sizeof(ses_word)*nt);							
					int i2 = 0;
					for (i2 = 0; i2 < nt; i2++) {
						temperature[i2] = ptBuffer[i2];
					}	
					temperature_label = myLabel;

				  }

				  if (strcmp(myLabel, the_label) == 0) {
						
				        //printf("\nprint_array_from_table: The data -- %s is: \n\n", myLabel);
					//printf("\nthe_label is %s\n", the_label);
			          	//for (i=0;  i<array_size; i++) {
			            	//	printf("%E, ", ptBuffer[i]);
			          	//}

                                        int i1 = 0;
					int j1 = 0;
					int index = 0;
					for (i1=0; i1 < nr; i1++) {
						for (j1 = 0; j1 < nt; j1++) {
							if ((i1 == 0) && (j1 == 0)) {
								printf("\n%s  %s  %s\n", density_label, temperature_label, myLabel);
							}
							printf(" %e         %e         %e\n", density[i1], temperature[j1], ptBuffer[index]);
							index++;

						}
					}
							

			          	printf("\n");
				  }
			          free(ptBuffer);
			          ptBuffer = (ses_word_reference)NULL;
			        }
			        else {
			          printf("print_array_from_table: Error in reading data\n");
			          break;
			        }


			      }
			   }
		}
		else {
			printf("print_array_from_table:  Table did not setup correctly\n");
			ses_print_error_condition(the_handle);
		}

	}

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		printf("print_array_from_table: error on close:  %s\n", the_error);
	}

  }
  else {
	printf("\n\nprint_data_on_table: null filename passed to print_materials_on_file\n");	
  }

}
