
#include "ses_defines.h"

#define print_data_on_table HEADER(print_data_on_table)
#define get_data_on_table HEADER(get_data_on_table)

#undef DEBUG_PRINT

ses_string get_data_on_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid) {

  int MAX_CHAR_LENGTH = 10000000;
  ses_string return_value = (char*)NULL;
  return_value = malloc(sizeof(char)*MAX_CHAR_LENGTH);
  char* buffer = (char*)NULL;
  buffer = malloc(sizeof(char)*MAX_CHAR_LENGTH);
  int i2 = 0;
  for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
	buffer[i2] = ' ';
  }

  if (sesame_file != (ses_string)NULL) {

	sprintf(buffer, "print_data_on_table:  Material %ld Table %ld\n\n", mid, tid);
	strcpy(return_value, buffer);
	for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
		buffer[i2] = ' ';
	}

        ses_file_handle the_handle = ses_open(sesame_file, 'R');
#ifdef DEBUG_GET_DATA
	printf("get_data_on_table:  after ses_open handle is %d\n", the_handle);
#endif
	if (ses_is_valid(the_handle) == SES_TRUE) {

		ses_error_flag didit_setup = ses_setup(the_handle, mid, tid);
#ifdef DEBUG_GET_DATA
	printf("get_data_on_table:  after ses_setup with mid = %d and %d and didit_setup is %d\n", mid, tid, didit_setup);
#endif
		if (didit_setup == SES_NO_ERROR) {

			  //  read the file with the iterator interface 
  
			  ses_label myLabel;
			  ses_number array_size;
			  //ses_error_flag read_errors;
			  ses_word_reference ptBuffer;
			  int i=0;

			  ses_boolean first = SES_TRUE;
			  while (ses_has_next(the_handle)) {

			    myLabel = ses_get_label(the_handle);
			    array_size = ses_array_size_next(the_handle);
#ifdef DEBUG_GET_DATA
	printf("get_data_on_table:  array_size is %d\n", array_size);
#endif
        
			    //  get the data 

			    if (tid < 200) {

                      		if (tid == 100) {

					if (first == SES_TRUE) {
						first = SES_FALSE;
						ptBuffer = ses_read_next(the_handle);
 						if (ptBuffer != (ses_word_reference)NULL) {
    
			          			sprintf(buffer, "\nprint_data_on_table: The data -- %s is: \n\n", myLabel);
							strcat(return_value, buffer);
							for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
								buffer[i2] = ' ';
							}

			          			for (i=0;  i<array_size; i++) {
			          			  sprintf(buffer, "%E, ", ptBuffer[i]);
							  strcat(return_value, buffer);
							  for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
								buffer[i2] = ' ';
							  }
			          			}

							sprintf(buffer, "\n");
							strcat(return_value, buffer);
							for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
							 	buffer[i2] = ' ';
							}
			          			free(ptBuffer);
			          			ptBuffer = (ses_word_reference)NULL;
			        		}
			        		else {
			          			sprintf(buffer, "print_data_on_table: Error in reading data for 100 table\n");
							strcat(return_value, buffer);
							for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
							 	buffer[i2] = ' ';
							}
			          			break;
			        		}	
					}
					else {

						ptBuffer = ses_read_next(the_handle);
						sprintf(buffer, "print_data_on_table:  %s\n", (ses_string)ptBuffer);
						strcat(return_value, buffer);
						for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
						 	buffer[i2] = ' ';
						}
			          		sprintf(buffer, "\n");
						strcat(return_value, buffer);
						for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
						 	buffer[i2] = ' ';
						}
			          		free(ptBuffer);
			          		ptBuffer = (ses_word_reference)NULL;
						
					}	

				}
				else {
					ses_string* the_string = malloc(sizeof(ses_string)*1);
                                	the_string[0] = (ses_string)NULL;
					ses_error_flag didit_readit = ses_comments(the_handle, the_string);
					if (didit_readit == SES_NO_ERROR) {
						sprintf(buffer, "\nsprint_data_on_table:  The data -- %s is \n\n", myLabel);
						strcat(return_value, buffer);
						for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
						 	buffer[i2] = ' ';
						}
						sprintf(buffer, "printf_data_on_table:  %s\n", *the_string);
						strcat(return_value, buffer);
						for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
						 	buffer[i2] = ' ';
						}
						if (the_string[0] != (ses_string)NULL) {
							free(the_string[0]);
							the_string[0] = (ses_string)NULL;
						}
						if (the_string != (ses_string*)NULL) {
							free(the_string);
							the_string = (ses_string*)NULL;
						}
						ses_error_flag didit_skip = ses_skip(the_handle);
						if (didit_skip != SES_NO_ERROR) {
							sprintf(buffer, "\nprint_data_on_table:  Error in iterator skip\n");
							strcat(return_value, buffer);
							for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
							 	buffer[i2] = ' ';
							}
						}
					}
					else {
						sprintf(buffer, "\nprint_data_on_table:  Error in reading comments\n");
						strcat(return_value, buffer);
						for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
						 	buffer[i2] = ' ';
						}
					}
				}


			    }
			    else {

#ifdef DEBUG_GET_DATA
				printf("get_data_on_table:  about to read data\n");
#endif    
			    	ptBuffer = ses_read_next(the_handle);
#ifdef DEBUG_GET_DATA
				printf("get_data_on_table:  read ptBuffer -- ptBuffer[0] is %e\n", ptBuffer[0]);
#endif    
			    
			        if (ptBuffer != (ses_word_reference)NULL) {
    
			          sprintf(buffer, "\nprint_data_on_table: The data -- %s is: \n\n", myLabel);
				  strcat(return_value, buffer);
				  for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
				 	buffer[i2] = ' ';
				  }
			          for (i=0;  i<array_size; i++) {
			            sprintf(buffer, "%E, ", ptBuffer[i]);
#ifdef DEBUG_GET_DATA
				    printf("in copy ptBuffer comes out as %s at i = %d\n", buffer, i);
#endif
 				    strcat(return_value, buffer);
#ifdef DEBUG_GET_DATA
				    //printf("after strcat return value comes out as %s at i = %d\n", return_value, i);
#endif

				    for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
				 	buffer[i2] = ' ';
				    }
			          }
#ifdef DEBUG_GET_DATA
			          printf("Got past copy ptBuffer into return_value\n");
#endif
			          free(ptBuffer);
			          ptBuffer = (ses_word_reference)NULL;
			        }
			        else {
			          sprintf(buffer, "print_data_on_table: Error in reading data\n");
 				  strcat(return_value, buffer);
				  for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
				     buffer[i2] = ' ';
				  }
			          break;
			        }


			      }
			   }

			   if (myLabel != (ses_label)NULL) {
				free(myLabel);
				myLabel = (ses_label)NULL;
			   }
		}
		else {
			sprintf(buffer, "print_data_on_table:  Table did not setup correctly\n");
			strcat(return_value, buffer);
			for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
				buffer[i2] = ' ';
			}
			ses_print_error_condition(the_handle);
		}

	}

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		sprintf(buffer, "print_data_on_table: error on close:  %s\n", the_error);
		strcat(return_value, buffer);
		for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
			buffer[i2] = ' ';
		}
	}

  }
  else {
	sprintf(buffer, "\n\nprint_data_on_table: null filename passed to print_materials_on_file\n");	
	strcat(return_value, buffer);
	for (i2=0; i2 < MAX_CHAR_LENGTH; i2++) {
		buffer[i2] = ' ';
	}
  }

#ifdef DEBUG_GET_DATA
	printf("get_data_on_table -- return string is %s\n", return_value);
#endif

  free(buffer);
  buffer = (ses_string)NULL;

  return return_value;
}

void print_data_on_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid) {
  if (sesame_file != (ses_string)NULL) {

	printf("print_data_on_table:  Material %ld Table %ld\n\n", mid, tid);

        ses_file_handle the_handle = ses_open(sesame_file, 'R');
	if (ses_is_valid(the_handle) == SES_TRUE) {

		ses_error_flag didit_setup = ses_setup(the_handle, mid, tid);
		if (didit_setup == SES_NO_ERROR) {

			  //  read the file with the iterator interface 
  
			  ses_label myLabel;
			  ses_number array_size;
			  //ses_error_flag read_errors;
			  ses_word_reference ptBuffer;
			  int i=0;

			  ses_boolean first = SES_TRUE;
			  while (ses_has_next(the_handle)) {

			    myLabel = ses_get_label(the_handle);
			    array_size = ses_array_size_next(the_handle);
        
			    //  get the data 

			    if (tid < 200) {

                      		if (tid == 100) {

					if (first == SES_TRUE) {
						first = SES_FALSE;
						ptBuffer = ses_read_next(the_handle);
 						if (ptBuffer != (ses_word_reference)NULL) {
    
			          			printf("\nprint_data_on_table: The data -- %s is: \n\n", myLabel);
			          			for (i=0;  i<array_size; i++) {
			          			  printf("%E, ", ptBuffer[i]);
			          			}
			          			printf("\n");
			          			free(ptBuffer);
			          			ptBuffer = (ses_word_reference)NULL;
			        		}
			        		else {
			          			printf("print_data_on_table: Error in reading data for 100 table\n");
			          			break;
			        		}	
					}
					else {

						ptBuffer = ses_read_next(the_handle);
						printf("print_data_on_table:  %s\n", (ses_string)ptBuffer);
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
						printf("\nprint_data_on_table:  The data -- %s is \n\n", myLabel);
						printf("printf_data_on_table:  %s\n", *the_string);
						if (the_string[0] != (ses_string)NULL) {
							free(the_string[0]);
							the_string[0] = (ses_string)NULL;
						}
						if (the_string != (ses_string*)NULL) {
							free(the_string);
							the_string = (ses_string*)NULL;
						}
						ses_error_flag didit_skip = ses_skip(the_handle);
						if (didit_skip != SES_NO_ERROR) {
							printf("\nprint_data_on_table:  Error in iterator skip\n");
						}
					}
					else {
						printf("\nprint_data_on_table:  Error in reading comments\n");
					}
				}


			    }
			    else {
    
			    	ptBuffer = ses_read_next(the_handle);
			    
			        if (ptBuffer != (ses_word_reference)NULL) {
    
			          printf("\nprint_data_on_table: The data -- %s is: \n\n", myLabel);
			          for (i=0;  i<array_size; i++) {
			            printf("%E, ", ptBuffer[i]);
			          }
			          printf("\n");
			          free(ptBuffer);
			          ptBuffer = (ses_word_reference)NULL;
			        }
			        else {
			          printf("print_data_on_table: Error in reading data\n");
			          break;
			        }


			      }
			   }

			   if (myLabel != (ses_label)NULL) {
				free(myLabel);
				myLabel = (ses_label)NULL;
			   }
		}
		else {
			printf("print_data_on_table:  Table did not setup correctly\n");
			ses_print_error_condition(the_handle);
		}

	}

	ses_error_flag didit_close = ses_close(the_handle);
        if (didit_close != SES_NO_ERROR) {
		ses_string the_error = ses_print_error_condition(the_handle);
		printf("print_data_on_table: error on close:  %s\n", the_error);
	}

  }
  else {
	printf("\n\nprint_data_on_table: null filename passed to print_materials_on_file\n");	
  }

}


