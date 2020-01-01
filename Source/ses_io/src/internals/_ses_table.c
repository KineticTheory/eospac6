#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


//////////////////DEFINES///////////////////////
//DEBUG_CONSTRUCT_STANDARD_TABLE - left in for convenience
////////////////////////////////////////////////

#include "_ses_table.h"
#include "_json_interface.h"


struct       _standard_table* _copy_standard_table(struct _standard_table* the_table) {
	
  struct _standard_table* return_value = malloc(1 * sizeof(struct _standard_table));


  if (the_table != (struct _standard_table*)NULL) {

    return_value->_the_tid = the_table->_the_tid;
    return_value->_num_arrays = the_table->_num_arrays;
    return_value->_num_independent = the_table->_num_independent;
    return_value->_nr = the_table->_nr;
    return_value->_nt = the_table->_nt;

    if (the_table->_description != NULL) {
      return_value->_description =  malloc(strlen(the_table->_description) + 1);
      strcpy(return_value->_description ,the_table->_description);
    }
    else {	
      return_value->_description = malloc(sizeof(char)*15);
      strcpy(return_value->_description, "no description");
    }
	
    return_value->_is_user_defined = the_table->_is_user_defined;

    return_value->_size_arrays = malloc(sizeof(char*) * the_table->_num_arrays);
    int i = 0;
    for (i = 0; i < the_table->_num_arrays; i++) {
      if (the_table->_size_arrays[i] != (ses_string)NULL) {
         return_value->_size_arrays[i] = malloc(sizeof(char) * (strlen(the_table->_size_arrays[i]) + 1));
         strcpy(return_value->_size_arrays[i] , the_table->_size_arrays[i]);
      }
      else {
	 return_value->_size_arrays[i] = (ses_string)NULL;
      }
    }

    return_value->_label = malloc(sizeof(ses_label) * the_table->_num_arrays);
    for (i = 0; i < the_table->_num_arrays; i++) {
      if (the_table->_label[i] != NULL) {
        return_value->_label[i] = (ses_label)malloc(sizeof(char)*(strlen(the_table->_label[i])+1));
        strcpy(return_value->_label[i], the_table->_label[i]);
      }
      else {
	return_value->_label[i] = (ses_string)NULL;
      }	
    }
  }

  return_value->_the_parse_node = copy_parse_node(the_table->_the_parse_node);

  return return_value;


}


struct _standard_table* _construct_standard_table(ses_string the_string1) {

	//  take a json string that describes the table and construct it

	struct _standard_table* return_value = (struct _standard_table*)NULL;

	//  parse the string

	struct json_node* the_parse_node1 = (struct json_node*)NULL;   
	the_parse_node1 = initialize_parser();
	/* int parser_return = 0; */
	/* parser_return = */ parse_string(the_string1, the_parse_node1);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
	print_json_node(the_parse_node1, 0);
#endif

	//  get the key, value pairs
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
  printf("Entered _construct_standard_table singular the_string1 is %s\n", the_string1);
#endif

	char** the_keys = (char**)NULL;
	char** the_types =(char**)NULL;
	char** the_values = (char**)NULL;

	int num_keys = get_key_value_pairs(&the_keys, &the_types, &the_values, the_parse_node1);

	//  take each key-value pair and translate that into the data needed for a ses_io standard table

	int i = 0;
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
	for (i = 0; i < num_keys; i++) {
		printf("key:%s| type: %s| value: %s|\n", the_keys[i], the_types[i], the_values[i]);
	}
#endif

	return_value = my_construct_standard_table();

	if (return_value != (struct _standard_table*)NULL) {

        return_value->_the_parse_node = the_parse_node1;
        return_value->_num_arrays = (long)0;

        long number_arrays = (long)0;
        long num_independent = (long)0;
	for (i = 0; i < num_keys; i++) {
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
                printf("the_keys[i] is %s the_types[i] is %s\n", the_keys[i], the_types[i]);
#endif
		if (strcmp(the_keys[i], "tid") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of tid as int is %d\n", atoi(stringer));
#endif
				return_value->_the_tid = atoi(stringer);
				
			}
		}
		if (strcmp(the_keys[i], "nr") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of nr as int is %d\n", atoi(stringer));
#endif
				return_value->_nr = atoi(stringer);
				
			}
		}
		if (strcmp(the_keys[i], "nt") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("the_values[i] is %s\n", the_values[i]);
				printf("stringer is %s\n", stringer);

				printf("Value of nt as int is %d\n", atoi(stringer));
#endif
				return_value->_nt = atoi(stringer);
				
			}
		}

		if (strcmp(the_keys[i], "num_arrays") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of num_arrays as int is %d\n", atoi(stringer));
#endif
				number_arrays = atoi(stringer);
				return_value->_num_arrays = (long)number_arrays;
			}
		}
		if (strcmp(the_keys[i], "number_arrays") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of number_arrays as int is %d\n", atoi(stringer));
#endif
				number_arrays = atoi(stringer);
				return_value->_num_arrays = (long)number_arrays;
			}
		}
		if (strcmp(the_keys[i], "description") == 0) {
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
			printf("Values of description as string is %s\n", the_values[i]);
#endif
			return_value->_description = malloc(sizeof(char) * strlen(the_values[i])+1);
			strcpy(return_value->_description, the_values[i]);
		}

		if (strcmp(the_keys[i], "labels") == 0) {
			if (strcmp(the_types[i], "ARRAY") == 0) {
				return_value->_label = malloc(sizeof(char*)* number_arrays);
				
				int i2;
				for (i2 = 0; i2 < number_arrays; i2++) {
					
					char* the_expression2 = get_value_for_key("labels", i2, the_parse_node1);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
			                 printf("Values of labels at index %d as string is %s\n", i2, the_expression2);
#endif
					return_value->_label[i2] = malloc(sizeof(char*)* (strlen(the_expression2)+1));
					strcpy(return_value->_label[i2], the_expression2);

					free(the_expression2);
					the_expression2 = (char*)NULL;
				}
			}
		}

		if (strcmp(the_keys[i], "sizes") == 0) {
			if (strcmp(the_types[i], "ARRAY") == 0) {
				return_value->_size_arrays = malloc(sizeof(char*)* number_arrays);
				
				int i2;
				for (i2 = 0; i2 < number_arrays; i2++) {
					
					char* the_expression2 = get_value_for_key("sizes", i2, the_parse_node1);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
                                        printf("i2 is %d strlen(the_expression2) is %d and expression is %s\n", i2, strlen(the_expression2), the_expression2);
#endif
					return_value->_size_arrays[i2] = calloc(strlen(the_expression2)+1, sizeof(char));
					strncpy(return_value->_size_arrays[i2], the_expression2, strlen(the_expression2)+1);

					free(the_expression2);
					the_expression2 = (char*)NULL;
				}
			}
		}
		if (strcmp(the_keys[i], "num_independent") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of num_independent as int is %d\n", atoi(stringer));
#endif
				num_independent = atoi(stringer);
				return_value->_num_independent = (long)num_independent;
			}
		}
		if (strcmp(the_keys[i], "number_independent") == 0) {
			if (strcmp(the_types[i], "NUMBER") == 0) {
				char* stringer = get_first_non_white(the_values[i]);
#ifdef DEBUG_CONSTRUCT_STANDARD_TABLE
				printf("Value of number_independent as int is %d\n", atoi(stringer));
#endif
				num_independent = atoi(stringer);
				return_value->_num_independent = (long)num_independent;
			}
		}
		}
	}
	if (return_value->_the_tid >= 699) {
		return_value->_is_user_defined = SES_TRUE;
	}


	//  free the strings

	int i3;
	for (i3 = 0; i3 < num_keys; i3++) {
		free(the_keys[i3]);
		free(the_types[i3]);
		free(the_values[i3]);
	}
	free(the_keys);
	free(the_types);
	free(the_values);

	//  end up with an array of standard tables -- utilize the same standard table functions in sesio

	return return_value;

}



struct _standard_table*  my_construct_standard_table() {


	//  blank constructor for a standard table

	struct _standard_table*	return_value = calloc(1, sizeof(struct _standard_table));
	if (return_value != (struct _standard_table*)NULL) {
		return_value->_the_parse_node = (struct json_node*)NULL;
		return_value->_the_tid = (ses_table_id)0;
		return_value->_num_arrays = (long)0;
                return_value->_num_independent = 0;
		return_value->_description = (char*)NULL;
		return_value->_is_user_defined = SES_FALSE;
        	return_value->_size_arrays = (ses_string*)NULL;
		return_value->_label = (ses_string*)NULL;
	}
	return return_value;
}

long       _get_standard_num_arrays_for_table(struct _standard_table* the_table) {

	//  get the number of arrays

        long return_value = 0;
	if (the_table != (struct _standard_table*)NULL) {
		return_value = the_table->_num_arrays;
	}
	return return_value;
}

char**      _get_standard_sizes_for_table_as_char(struct _standard_table* the_table, long nr, long nt, long ntab){

	char** return_value = (char**)NULL;
	if (the_table != (struct _standard_table*)NULL) {
		if (the_table->_the_parse_node != (struct json_node*)NULL) {

			char** vars = (char**)NULL;
			vars = malloc(sizeof(char*) * 3);
			vars[0] = "nr";
			vars[1] = "nt";
			vars[2] = "ntab";
			int*   values = malloc(sizeof(int)*3);
        		values[0] = (int)nr;
			values[1] = (int)nt;
			values[2] = (int)ntab;
	
			//int the_int = 0;
			int i = 0;
			char* the_expression2 = (char*)NULL;
			return_value = malloc(sizeof(char*) * the_table->_num_arrays);
			for (i = 0; i < the_table->_num_arrays; i++) {
				the_expression2 = get_value_for_key("sizes", i, the_table->_the_parse_node);
				return_value[i] = malloc(sizeof(char) * (strlen(the_expression2) + 1));
				strcpy(return_value[i], the_expression2); 
				free(the_expression2);
				the_expression2 = (char*)NULL;
			}
	
			free(values);
			values = (int*)NULL;
			free(vars);
			vars = (char**)NULL;

		}
		else {
			return_value = malloc(sizeof(char*) * the_table->_num_arrays);
			int i = 0;
			for (i = 0; i < the_table->_num_arrays; i++) {
				return_value[i] = malloc(sizeof(char) * (1 + 1));
				strcpy(return_value[i], "0"); 
			}
		}
	}
	return return_value;

}

long*      _get_standard_sizes_for_table(struct _standard_table* the_table, long nr, long nt, long ntab) {

	long* return_value = (long*)NULL;
	if (the_table != (struct _standard_table*)NULL) {
		if (the_table->_the_parse_node != (struct json_node*)NULL) {

			char** vars = (char**)NULL;
			vars = malloc(sizeof(char*) * 2);
			vars[0] = "nr";
			vars[1] = "nt";
			int*   values = malloc(sizeof(int)*2);
        		values[0] = (int)nr;
			values[1] = (int)nt;
	
			int the_int = 0;
			int i = 0;
			char* the_expression2 = (char*)NULL;
			return_value = malloc(sizeof(long) * the_table->_num_arrays);
			for (i = 0; i < the_table->_num_arrays; i++) {
				the_expression2 = get_value_for_key("sizes", i, the_table->_the_parse_node);
				if (the_expression2 != NULL) {
					if (strchr(the_expression2, '(') != (char*)NULL) {
						the_int = evaluate_expression("sizes", i, vars, values, 2, the_table->_the_parse_node);
					}
					else {
						the_int = atoi(the_expression2);
					}
				}

                                return_value[i] = (long)the_int;
				free(the_expression2);
				the_expression2 = (char*)NULL;
			}
	
			free(values);
			values = (int*)NULL;
			free(vars);
			vars = (char**)NULL;

		}
		else {
			return_value = malloc(sizeof(long) * the_table->_num_arrays);
			int i = 0;
			for (i = 0; i < the_table->_num_arrays; i++) {
				return_value[i] = 0;
			}
		}
	}
	return return_value;
}

ses_boolean _destruct_standard_table(struct _standard_table* the_table) {


  /*  destruct the standard tables */

  ses_boolean return_value = SES_TRUE;

  /*  error check the arguments */


  if (the_table == (struct _standard_table*)NULL) {
#ifdef DEBUG_PRINT
    printf("_destruct_table: null table array passed to _destruct_table\n");
#endif
    _set_latest_error(SES_NULL_OBJECT_ERROR);
    return SES_FALSE;
  }

  int i = 0;

  if (the_table->_description != (char*)NULL) {
  	free(the_table->_description);
  	the_table->_description = (ses_string)NULL;
  }
  for (i = 0; i < the_table->_num_arrays; i++) {

    if (the_table->_label[i] != (ses_label)NULL) {
      free(the_table->_label[i]);
      the_table->_label[i] = (ses_label)NULL;
    }

    if (the_table->_size_arrays[i] != (char*)NULL) {
      free(the_table->_size_arrays[i]);
      the_table->_size_arrays[i] = (ses_string)NULL;
    }
  }
  free(the_table->_label);
  the_table->_label = (ses_label*)NULL;

  free(the_table->_size_arrays);
  the_table->_size_arrays = (ses_string*)NULL;

  if (the_table->_the_parse_node != (struct json_node*)NULL) {
     free_node(the_table->_the_parse_node);
     free(the_table->_the_parse_node);
     the_table->_the_parse_node = (struct json_node*)NULL;
  }

  return return_value;
}


void _print_standard_table(struct _standard_table* the_table) {

	printf("PRINTING STANDARD TABLE\n");
	if (the_table != (struct _standard_table*)NULL) {
		printf("_the_tid is %ld\n", the_table->_the_tid);
		if (the_table->_description != (char*)NULL) {
			printf("_the_description is %s\n", the_table->_description);
		}
		printf("_num_arrays is %ld\n", the_table->_num_arrays);
		printf("_num_independent is %ld\n", the_table->_num_independent);

		if (the_table->_num_arrays > 0) {
			int i = 0;
			for(i = 0; i < the_table->_num_arrays; i++) {
				printf("_size_arrays[%d] = %s\n", 
					i, the_table->_size_arrays[i]);
			}
			for(i = 0; i < the_table->_num_arrays; i++) {
				printf("_label[%d] = %s\n", 
					i, the_table->_label[i]);
			}
		}

	}
	printf("\n\n");
}


long*      _get_standard_relative_addresses_for_table(struct _standard_table* the_table, long nr, long nt, long ntab) {

  long* address_arrays = (long*)NULL;

  int num_arrays = the_table->_num_arrays;

  address_arrays = malloc(sizeof(long)*num_arrays);
  if (address_arrays == (long*)NULL) {

    return (long*)NULL;
  }

  long*  my_sizes = _get_standard_sizes_for_table(the_table, nr, nt, ntab);


  int j = 0;
  address_arrays[0] = 1;
  for (j = 1; j < num_arrays; j++) {
	address_arrays[j] = (address_arrays[j-1] + my_sizes[j-1]);
  }
  for (j = 0; j < num_arrays; j++) {
	address_arrays[j] = 8 * (address_arrays[j] -1);
  }


  free(my_sizes);
  my_sizes = (long*)NULL;


  return address_arrays;



}

ses_label*  _get_standard_labels_for_table(struct _standard_table* the_table) {

  ses_label* label_array = (ses_label*)NULL;

  if (the_table != (struct _standard_table*)NULL) {

  	if (the_table->_num_arrays >= 1) {
  	  label_array = malloc(sizeof(ses_label)*the_table->_num_arrays);
	  int i = 0;
          for (i=0; i < the_table->_num_arrays; i++) {
		label_array[i] = (ses_label)NULL;
	  }
  	}

  	if (label_array == (ses_label*)NULL) {
  	  return (ses_label*)NULL;
  	}

  	/* return */

	
  	int j = 0;
  	int size = SES_MAX_LABEL_SIZE;
	
  	for (j = 0; j < the_table->_num_arrays; j++) {

  	  label_array[j] = malloc(sizeof(char) * size);

  	  if (the_table->_label[j] != (ses_label)NULL) {
  	      if (j == 0) {
		strcpy(label_array[0], the_table->_label[0]);
	      }
	      else {
		strcpy(label_array[j] , the_table->_label[j]);
	      }
	    
	  }
	  else {
		strcpy(label_array[j], "no label");
	  }
       }
  }

  return label_array;

}
