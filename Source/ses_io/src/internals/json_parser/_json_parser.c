

#include "_json_interface.h"
#include "_json_parser.h"
#include "_json_print.h"
#include "_my_string_functions.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define PARSE_SUCCESS HEADER(PARSE_SUCCESS)
#define PARSE_FAILURE HEADER(PARSE_FAILURE)

const int PARSE_SUCCESS = 0;
const int PARSE_FAILURE = 1;

//#define DEBUG_PARSE_JSON

//  user interface -- defined in json_interface.h //

struct json_node* initialize_parser() {

	//  create a json_node

	struct json_node* new_node = (struct json_node*)NULL;
        free_parser(new_node);
	new_node = malloc(sizeof(struct json_node)*1);
	return new_node;
}

int parse_string(char* the_string, struct json_node* the_node) {

	//  parse the string from the top

	int return_value = 0;
	char* my_return = parse_json(the_string, (char*)NULL, OBJECT, the_node);
	if (my_return != (char*)NULL) {
		return_value = 1;
		free(my_return);
		my_return = (char*)NULL;
	}
	return return_value;

}

void  print_parse_tree(struct json_node* the_node) {

	//  print the parse tree from the top

	print_global_data(the_node);

}

void free_parser(struct json_node* the_node) {

	//  free memory for the internal data structures

	if (the_node != (struct json_node*)NULL) {
		free_node(the_node);
		free(the_node);
		the_node = (struct json_node*)NULL;
	}

}

int get_key_value_pairs(char*** the_keys, char*** types, char*** values, struct json_node* the_node) {

	int number_keys = get_keys(the_keys, the_node);

	int MAX_KEYS = number_keys;

	char** the_types = calloc(MAX_KEYS, sizeof(char*));
	char** the_values = calloc(MAX_KEYS, sizeof(char*));

	int i = 0; 
	for (i = 0; i < number_keys; i++) {
		struct json_node* local_node = walk_tree(the_node, (*the_keys)[i]);
		if (local_node != (struct json_node*)NULL) {
			char* local_type = get_type(local_node->the_type);
			if (local_type != (char*)NULL) {
				the_types[i] = calloc(strlen(local_type)+1, sizeof(char));
				strcpy(the_types[i], local_type);
				if (local_node->the_value != (char*)NULL) {	
					char* local_value = local_node->the_value;
					the_values[i] = calloc(strlen(local_value)+1, sizeof(char));
					strcpy(the_values[i], local_value);		
				}
				else {
					the_values[i] = calloc(2, sizeof(char));
					strcpy(the_values[i] , "0");
				}
			}
		}
	}

	(*types) = the_types;
	(*values) = the_values;

	return number_keys;
}


int get_keys(char*** my_keys, struct json_node* the_node){

	int return_value = 0;

	struct json_node* current_node = the_node;

	int MAX_KEYS = 4000;
	char** the_keys = calloc(MAX_KEYS, sizeof(char*));
	
	int current = 0;
	struct json_node* node2 = (struct json_node*)NULL;
	while  (current_node != (struct json_node*)NULL) {
		if (current_node->the_type != KEY) {

			//  NOT KEY
			if (current_node->the_type == MEMBERS) {

				node2 = current_node;
				current_node = current_node->the_node1;
			}
			else {
				current_node = current_node->the_node1;
			}
				

		}
		else {

			//  KEY

			//  add key, value, type to output list

			char* the_key = current_node->the_value;
			the_keys[current] = calloc(strlen(the_key)+1, sizeof(char));
			strcpy(the_keys[current], get_first_non_white(the_key));
			current++;
			current_node = node2->the_node2;
			
		}
	}

	*my_keys = the_keys;
	return_value = current;

	return return_value;

}


char* get_type_for_key(char* key, struct json_node* the_in_node) {


	char* return_value = (char*)NULL;

	struct json_node* the_node = walk_tree(the_in_node, key);


	if (the_node != (struct json_node*)NULL) {
		if ((the_node->the_node1) != (struct json_node*)NULL) {
			return_value = get_type(the_node->the_node1->the_type);
		}
	}
	return return_value;

}



char* get_value_for_key(char* key , int index, struct json_node* the_in_node) {


	char* return_value = (char*)NULL;
	struct json_node* the_node = walk_tree(the_in_node, key);
        if (the_node != (struct json_node*)NULL) {
		if (the_node->the_node1 == (struct json_node*)NULL) {
			return_value = malloc((strlen(the_node->the_value) + 1)*sizeof(char));
			strcpy(return_value, the_node->the_value);
		}
		

        	the_node = the_node->the_node1;  //  may point to array
		if (the_node != (struct json_node*)NULL) {
			enum node_type type = the_node->the_type;
        		if ((index >= 0) && (type == ELEMENT)) {
				int i = 0;
				for (i = 0; i < index; i++) {
					if (the_node != (struct json_node*)NULL) {
						the_node = the_node->the_node2;   //  element node
					}
				}
				if (the_node != (struct json_node*)NULL) {
					if (the_node->the_node1 != (struct json_node*)NULL) {
						if (the_node->the_node1->the_value != (char*)NULL) {
							return_value = calloc(strlen(the_node->the_node1->the_value)+1, sizeof(char));
							strcpy(return_value, the_node->the_node1->the_value);
						}
						else {
							return_value = (char*)NULL;
						}
					}
					else {
						return_value = calloc(strlen(the_node->the_value)+1, sizeof(char));
						strcpy(return_value , the_node->the_value);
					}
				}
			}
			else {	
				if (the_node != (struct json_node*)NULL) {
					return_value = calloc(strlen(the_node->the_value)+1, sizeof(char));
					strcpy(return_value , the_node->the_value);
				}
			}
		}
	}

	return return_value;
}

int evaluate_expression(char* the_key, int index, char** vars, int* values, int num_values, struct json_node* the_in_node) {

	int return_value = 0;

	struct json_node* the_node = walk_tree(the_in_node, the_key);
	struct json_node* new_node = (struct json_node*)NULL;
	if (the_node != (struct json_node*)NULL) {
		if (the_node->the_type == ARRAY) {
			new_node = get_node_for_index(the_node, index);
			if (new_node != (struct json_node*)NULL) {
				the_node = new_node;
			}
		}
		if (the_node->the_node1 != (struct json_node*)NULL) {
        		the_node = the_node->the_node1;
		}

		struct json_expression** resolved = malloc(sizeof(struct json_expression*) * num_values);

		int i = 0; 
		for (i = 0; i < num_values; i++) {
			if (i == 0) {
				resolved[0]  = resolve_value(the_node->the_expression, vars[i], values[i]);
			}
			else {
				resolved[i]  = resolve_value(resolved[i-1], vars[i], values[i]);
			}
		}
		//  all variables resolved, get the expression value
		
		return_value = get_value(resolved[num_values-1]);

		/*  free the array */

		for (i = 0; i < num_values; i++) {
			free_expression(resolved[i]);
			free(resolved[i]);
			resolved[i] = (struct json_expression*)NULL;
		}

		free(resolved);
		resolved = (struct json_expression**)NULL;
	}

	
	return return_value;
}

struct json_node* walk_tree(struct json_node* the_node, char* the_key) {

	//  find the node with the specified key

	struct json_node* return_value = (struct json_node*)NULL;

	if (the_node != (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                printf("walk_tree:  the_key is %s and the_type is %d\n", the_key, the_node->the_type);
		printf("walk_true:: the_node->the_value is %s\n", the_node->the_value);
#endif
		if (the_node->the_type != KEY) {
#ifdef DEBUG_WALK_TREE
                        printf("walk_tree:  type is not KEY\n");
#endif
			if (the_node->the_node1 != (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                                printf("walk_tree:  node1 is not null\n");
#endif

				struct json_node* return_value1 = walk_tree(the_node->the_node1, the_key);
				if (return_value1 == (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                                        printf("walk_tree:  return_value1 is null\n");
#endif

					if (the_node->the_node2 != (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                                                printf("walk_tree:  node2 is not null\n");
#endif

						struct json_node* return_value2 = walk_tree(the_node->the_node2, the_key);
						if (return_value2 != (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                                                        printf("walk_tree:  return_value2 is not null\n");
#endif
							return_value = return_value2;
						}
                                                
					}
				}
				else {
#ifdef DEBUG_WALK_TREE
                                        printf("walk_tree:  return_value1 is notnull\n");
#endif
					return_value = return_value1;
				}
			}

		}
		else {
#ifdef DEBUG_WALK_TREE
                        printf("walk_tree:  type is KEY\n");
#endif
			if (the_node->the_value != (char*)NULL) {

#ifdef DEBUG_WALK_TREE1
                        	printf("walk_tree:  the_value not null is %s and the_key is %s\n", the_node->the_value, the_key);
#endif
				char* check_substring = strstr(the_node->the_value, the_key);
				if ((check_substring != (char*)NULL) && (strlen(the_node->the_value) == strlen(the_key))) {
#ifdef DEBUG_WALK_TREE1
                               		 printf("walk_tree:  check_substring is %s\n", check_substring);
#endif

					if (the_node->associated_node != (struct json_node*)NULL) {
#ifdef DEBUG_WALK_TREE
                               			 printf("walk_tree:  the_node->associated_node not null\n");
#endif
						return_value = the_node->associated_node->the_node1;
					}
				}
				
			}
		}
	}
#ifdef DEBUG_WALK_TREE
	if (return_value != (struct json_node*)NULL) {
		if (return_value->the_value != (char*)NULL) {
	        	printf("walk_treee:  returning %s\n", return_value->the_value);
		}
		else {
			printf("walk_tree:  returning NULL value\n");
		}
	}
	else {
		printf("walk_tree:  returning NULL\n");
	}
	
#endif
	return return_value;
}



//  defined in json_parser.h //

struct json_node* copy_parse_node(struct json_node* the_node) {
	
	struct json_node* return_value = (struct json_node*)NULL;

	if (the_node != (struct json_node*)NULL) {

		return_value = malloc(sizeof(struct json_node)*1);
		return_value->the_type = the_node->the_type;
		if (the_node->the_value != (char*)NULL) {
			return_value->the_value = malloc(sizeof(char)*(strlen(the_node->the_value)+1));
                        strcpy(return_value->the_value, the_node->the_value);
		}
		else {
			return_value->the_value = (char*)NULL;
		}
		return_value->the_node1 = copy_parse_node(the_node->the_node1);
		return_value->the_node2 = copy_parse_node(the_node->the_node2);

		if (the_node->the_expression != (struct json_expression*)NULL) {
			return_value->the_expression = copy_expression(the_node->the_expression);
		}
		else {
			return_value->the_expression = (struct json_expression*)NULL;
		}

		return_value->associated_node = copy_parse_node(the_node->associated_node);
		return_value->is_copied = 1;
	}
	

        return return_value;
}

void free_node(struct json_node* the_node) {

	//  free the node's contents

	if (the_node != (struct json_node*)NULL) {

		if (the_node->the_expression != (struct json_expression*)NULL) {
			free_expression_node(the_node->the_expression);
			free(the_node->the_expression);
			the_node->the_expression = (struct json_expression*)NULL;
		}

		if (the_node->the_value != (char*)NULL) {
			free(the_node->the_value);
			the_node->the_value = (char*)NULL;
		}

		if (the_node->the_node1 != (struct json_node*)NULL) {
			free_node(the_node->the_node1);
			free(the_node->the_node1);
			the_node->the_node1 = (struct json_node*)NULL;
		}
		if (the_node->the_node2 != (struct json_node*)NULL) {
			free_node(the_node->the_node2);
			free(the_node->the_node2);
			the_node->the_node2 = (struct json_node*)NULL;
		}

		//  DO NOT free the associated nodes, they are attached to other nodes

		if (the_node->is_copied == 1) {
			free_node(the_node->associated_node);
			free(the_node->associated_node);
		}
		the_node->associated_node = (struct json_node*)NULL;
	}
}

void free_expression_node(struct json_expression* the_expression) {

	//  free the expression node's contents

	if (the_expression != (struct json_expression*)NULL) {

		if (the_expression->the_value != (char*)NULL) {
			free(the_expression->the_value);
			the_expression->the_value = (char*)NULL;
		}

		if (the_expression->left != (struct json_expression*)NULL) {
			free_expression_node(the_expression->left);	
			free(the_expression->left);
			the_expression->left = (struct json_expression*)NULL;
		}
		if (the_expression->right != (struct json_expression*)NULL) {
			free_expression_node(the_expression->right);
			free(the_expression->right);
			the_expression->right = (struct json_expression*)NULL;
		}
	
	}
}



char* parse_json(char* the_string, char* prefix, enum node_type object_type, struct json_node* the_node) {


	//  PARSE a json string , return value is the remaining string to parse
	//  this routine creates a data structure that holds the PARSEd string 

	
	char* return_value = (char*)NULL;
#ifdef DEBUG_PARSE_JSON
        printf("\n\nParsing object type %d = |%s|\n", object_type, the_string);
#endif

	//////////////////////////////////////////////////////////

	//  if the string is null return with failure
	if (the_string == (char*)NULL) {
		the_node->the_type = object_type;
		the_node->the_value = (char*)NULL;
		the_node->the_node1= (struct json_node*)NULL;
		the_node->the_node2 = (struct json_node*)NULL;
		the_node->associated_node = (struct json_node*)NULL;
		the_node->is_copied = 0;
		return (char*)NULL;
	}

	//  if you're here, the string is not null

	//  construct the data members for the input node

	//  the_type
	
	the_node->the_type = object_type;

	//  the_value

	char* the_string1 = (char*)NULL;
	if (the_string != (char*)NULL) {
		//  create the value 

	        int strlength = 0;
		the_string1 = strtrim(the_string);
		if (the_string1 != (char*)NULL) {
	                strlength = (int)(strlen(the_string1) + 1); 
			if ((object_type == KEY) && (prefix != (char*)NULL)) {
				strlength = strlength + strlen(prefix) + 2;  // 2 for "::"
			}

			the_node->the_value = malloc(sizeof(char) * strlength);
			strcpy(the_node->the_value, ""); 
		
			if ((object_type == KEY) && (prefix != (char*)NULL)) {
				strcpy(the_node->the_value, prefix);
				strcat(the_node->the_value, "::");
			}
			strcat(the_node->the_value, the_string1); 
			free(the_string1);
			the_string1 = (char*)NULL;


		}
		else {
			the_node->the_value = malloc(sizeof(char)*1);
			the_node->the_value[0] = '\0';
		}
	}
	else {
		the_node->the_value = malloc(sizeof(char)*1);
		the_node->the_value[0] = '\0';
	}

	//  is_copied

	the_node->is_copied = 0;

	//  the_node1

        the_node->the_node1= (struct json_node*)NULL;

	//  the_node2

	the_node->the_node2 = (struct json_node*)NULL;

	//  the_expression

	the_node->the_expression = (struct json_expression*)NULL;

	//  associated node

	the_node->associated_node = (struct json_node*)NULL;

	//  parse the string and create the node tree

	char* sub1 = (char*)NULL;
	char* sub2 = (char*)NULL;
  char* without_quotes = (char*)NULL;
  
	enum node_type the_node_type = STRING;

	switch (object_type) {

		case(OBJECT):

			//  object is MEMBERS*
			sub1 = get_substring_for_object_type(the_string, OBJECT);  //  peel off the surrounding braces
			sub2 = (char*)NULL;
#ifdef DEBUG_PARSE_JSON
                        printf("OBJECT sub1 is %s\nsub2 is %s\n", sub1, sub2);
#endif

			if (sub1 != (char*)NULL) {

				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, MEMBERS, the_node->the_node1);
				free(sub1);	
				sub1 = (char*)NULL;
			}

			break;

		case(MEMBERS):

			//  members is PAIR, MEMBERS*
			sub1 = get_substring_for_object_type(the_string, PAIR);      //  stuff before delimeter that is not object
			sub2 = get_substring_for_object_type(the_string, MEMBERS);   
			
			
#ifdef DEBUG_PARSE_JSON
                        printf("MEMBERS:PAIR sub1 is %s sub2 is %s\n", sub1, sub2);
#endif
			

			if (sub1 != (char*)NULL) {
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, PAIR, the_node->the_node1);
				free(sub1);	
				sub1 = (char*)NULL;
			}
			
			
						
#ifdef DEBUG_PARSE_JSON
                        printf("MEMBERS:MEMBERS sub1 is %s sub2 is %s\n", sub1, sub2);
#endif


			if (sub2 != (char*)NULL) {
				the_node->the_node2 = malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub2, prefix, MEMBERS, the_node->the_node2);
				//free(sub2);  //  same string as sub1, already freed
				sub2 = (char*)NULL;
			}
			
			break;
		case(PAIR):

			//  pair is KEY KVALUE
			sub1 = get_substring_for_object_type(the_string, KEY);   //  part of the string before the : not in quotes

			if (sub1 != (char*)NULL) {
				sub2 = the_string + sizeof(char)*(strlen(sub1) + 1);
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, KEY, the_node->the_node1);
				if (sub1 != the_string) {
					free(sub1);	
					sub1 = (char*)NULL;
				}
			}

#ifdef DEBUG_PARSE_JSON
                        printf("PAIR:KVALUE sub1 is %s sub2 is %s\n", sub1, sub2); 
#endif

			if (sub2 != (char*)NULL) {

				the_node->the_node2 = malloc(sizeof(struct json_node)*1);

				//  hook the associated nodes up

				(the_node->the_node1)->associated_node = (the_node->the_node2);
				(the_node->the_node2)->associated_node = (the_node->the_node1);

				//  continue

				return_value = parse_json(sub2, prefix, KVALUE, the_node->the_node2);

				sub2 = (char*)NULL;

			}
			
			break;

		case(KEY):

			sub1 = the_string;
			sub2 = (char*)NULL;
#ifdef DEBUG_PARSE_JSON
                        printf("KEY sub1 is %s\n", sub1); 
#endif

			break;

		case(KVALUE):

			the_node_type = get_node_type(the_string);

			sub1 = malloc(sizeof(char)*(strlen(the_string)+1));
			strcpy(sub1, the_string);

#ifdef DEBUG_PARSE_JSON
                        printf("KVALUE sub1 is %s node_type is %d\n", sub1, the_node_type); 
#endif
			if (sub1 != (char*)NULL) {
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, the_node_type, the_node->the_node1);
				free(sub1);
				sub1 = (char*)NULL;
			}
			
			break;
		case(VALUE):
#ifdef DEBUG_PARSE_JSON
		        printf("VALUE the_string is %s\n", the_string);
#endif
			the_node_type = get_node_type(the_string);
			sub1 = get_substring_for_object_type(the_string, the_node_type);
#ifdef DEBUG_PARSE_JSON
                        printf("VALUE sub1 is %s the_node_type is %d\n", sub1, the_node_type); 
#endif

			if (sub1 != (char*)NULL) {
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, the_node_type, the_node->the_node1);
				free(sub1);
				sub1 = (char*)NULL;
			}

			break;

		case(ARRAYVALUE):

			the_node_type = get_node_type(the_string);
			sub1 = get_substring_for_object_type(the_string, the_node_type);
#ifdef DEBUG_PARSE_JSON
                        printf("ARRAYVALUE node_type is %d sub1 is %s\n", the_node_type, sub1); 
#endif
			if (sub1 != (char*)NULL) {
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				return_value = parse_json(sub1, prefix, the_node_type, the_node->the_node1);
				free(sub1);
				sub1 = (char*)NULL;
			}

			break;

		case(STRING):

			sub1 = get_substring_for_object_type(the_string, STRING);
#ifdef DEBUG_PARSE_JSON
      printf("STRING sub1 is %s\n", sub1);
#endif
			without_quotes = strtrim(remove_quotes(sub1));
			
#ifdef DEBUG_PARSE_JSON
      printf("STRING without_quotes is %s\n", without_quotes);
#endif
			strcpy(the_node->the_value, without_quotes);

			if (sub1 != (char*)NULL) {
				free(sub1);
				sub1 = (char*)NULL;
			}
	
			if (without_quotes != (char*)NULL) {
				free(without_quotes);
				without_quotes = (char*)NULL;
			}

			break;

		case(NUMBER):

			sub1 = get_substring_for_object_type(the_string, NUMBER);

#ifdef DEBUG_PARSE_JSON
      printf("NUMBER the_string is |%s| sub1 is |%s|\n", the_string, sub1);
#endif
			if (sub1 != (char*)NULL) {
				free(sub1);
				sub1 = (char*)NULL;
			}

			break;
		case(ARRAY):

			sub1 = get_substring_for_object_type(the_string, ARRAY);
#ifdef DEBUG_PARSE_JSON
      printf("ARRAY sub1 is %s\n", sub1);
#endif
			the_node->the_node1= malloc(sizeof(struct json_node)*1);
			return_value = parse_json(sub1, prefix, ELEMENT, the_node->the_node1);
			if (sub1 != (char*)NULL) {
				free(sub1);
				sub1 = (char*)NULL;
			}
			
			
			break;

		case(ELEMENT):

			//  ELEMENT is ARRAYVALUE ELEMENT*

			sub1 = get_substring_for_object_type(the_string, ARRAYVALUE);
			if (sub1 != (char*)NULL) {
			
#ifdef DEBUG_PARSE_JSON
        printf("ELEMENT sub1 is %s\n", sub1);
#endif
				
				the_node->the_node1= malloc(sizeof(struct json_node)*1);
				if (the_node->the_value != (char*)NULL) {
					free(the_node->the_value);
					the_node->the_value = malloc(sizeof(char)*(strlen(sub1)+1));
					strcpy(the_node->the_value, sub1);
				}		
				return_value = parse_json(sub1, prefix, VALUE, the_node->the_node1);
#ifdef DEBUG_PARSE_JSON
				printf("ELEMENT the_node->the_node1->the_value is %s\n", the_node->the_node1->the_value);
#endif

				free(sub1);
				sub1 = (char*)NULL;
			}	

			sub2 = get_substring_for_object_type(the_string, ELEMENT);
			if (sub2 != (char*)NULL) {
				the_node->the_node2 = malloc(sizeof(struct json_node)*1);
#ifdef DEBUG_PARSE_JSON
        printf("ELEMENT sub2 is %s\n", sub2);
#endif
				return_value = parse_json(sub2, prefix, ELEMENT, the_node->the_node2);
				free(sub2);
				sub2 = (char*)NULL;
			}

			break;
		case(BOOLEAN):

			sub1 = the_string;
#ifdef DEBUG_PARSE_JSON
      printf("BOOLEAN sub1 is %s\n", sub1);
#endif


			break;
		case(MYNULL):

			sub1 = the_string;
#ifdef DEBUG_PARSE_JSON
      printf("MYNULL sub1 is %s\n", sub1);
#endif

			break;

		case(EXPRESSION) :

			sub1 = the_string;

#ifdef DEBUG_PARSE_JSON
                        printf("EXPRESSION sub1 is %s\n", sub1); 
#endif

			//  create the expression node

			the_node->the_expression = create_expression_node(sub1);
#ifdef DEBUG_PARSE_JSON
      printf("the_node->the_expression is %s\n", the_node->the_expression->the_value);
      printf("the_node->the_expression->the_operator is %c\n", the_node->the_expression->the_operator);
#endif


			break;
		case(NOT_VALID):

			sub1 = the_string;
#ifdef DEBUG_PARSE_JSON
      printf("NOT_VALID sub1 is %s\n", sub1);
#endif


			break;
		default:
			break;
	}
	//////////////////////////////////////////////////////////

#ifdef DEBUG_PARSE_JSON
  printf("return_value is %s\n", return_value);
#endif
	return return_value;
}


struct json_expression* create_expression_node(char* the_string) {


	struct json_expression* return_value = (struct json_expression*)NULL;
        int found_operator = 0;

	if (the_string != (char*)NULL) {

		return_value = calloc(1 ,sizeof(struct json_expression));
		int i = 0;
		return_value->the_value = malloc((strlen(the_string)+1) * sizeof(char));
		strcpy(return_value->the_value, the_string);
		for (i = 0; i < strlen(the_string); i++) {    
			if ((the_string[i] == '+') || (the_string[i] == '*') || (the_string[i] == '-') || (the_string[i] == '/') ) {

				return_value->the_operator = the_string[i];

				int length1 = i;
				char* left = calloc(length1 + 1, sizeof(char) );
				strcpy(left, "");
				strncpy(left, the_string, i);
				left[length1] = '\0';
				int i = 0;
				for (i =0; i < length1; i++) {
					if (left[i] == '(') {
						left[i] = ' ';
					}
				}

				int length2 = strlen(the_string) - i  - 1;
				char* right = calloc(length2 + 1 , sizeof(char));
				strcpy(right, "");
				strncpy(right, the_string + i + 1, length2);
				right[length2] = '\0';

				for (i =0; i < length1; i++) {
					if (right[i] == ')') {
						right[i] = '\0';
					}
				}

				return_value->left = create_expression_node(left);
				return_value->right = create_expression_node(right);

				found_operator = 1;

				free(left);
				left = (char*)NULL;
				free(right);
				right = (char*)NULL;
			
			}
		}

		if (found_operator == 0) {
			return_value->the_operator = ' ';
			strcpy(return_value->the_value, the_string);
			return_value->left = (struct json_expression*)NULL;		
			return_value->right = (struct json_expression*)NULL;		
		}
	}

	
	return return_value;
}


struct json_expression* copy_expression(struct json_expression* the_expression) {



	struct json_expression* return_value = (struct json_expression*)NULL;
	if (the_expression != (struct json_expression*)NULL) {

		return_value = malloc(1 * sizeof(struct json_expression));

		return_value->the_operator = the_expression->the_operator;
		if (the_expression->the_value != (char*)NULL) {
			return_value->the_value = malloc( (strlen(the_expression->the_value)+1) * sizeof(char) );
			strcpy(return_value->the_value, the_expression->the_value);
		}
		else {
			return_value->the_value = (char*)NULL;
		}

		return_value->left = copy_expression(the_expression->left);
		return_value->right = copy_expression(the_expression->right);
	}

        return return_value;
}



void free_expression(struct json_expression* the_expression) {

	if (the_expression != (struct json_expression*)NULL) {

		the_expression->the_operator = ' ';
		if (the_expression->the_value != (char*)NULL) {
			free(the_expression->the_value);
			the_expression->the_value = (char*)NULL;
		}

		if (the_expression->left != (struct json_expression*)NULL) {
			free_expression(the_expression->left);
			free(the_expression->left);
			the_expression->left = (struct json_expression*)NULL;
		}

		if (the_expression->right != (struct json_expression*)NULL) {
			free_expression(the_expression->right);
			free(the_expression->right);
			the_expression->right = (struct json_expression*)NULL;
		}
	}

}


struct json_expression* resolve_value(struct json_expression* the_expression, char* the_variable, int the_value) {


	struct json_expression* return_value = (struct json_expression*)NULL;
	char* returned_value = (char*)NULL;

	if (the_expression != (struct json_expression*)NULL) {

	   switch(the_expression->the_operator) {
		case(' '):
		        returned_value = convert_to_value(the_expression->the_value, the_variable, the_value);
			return_value = create_expression_node(returned_value);
			if (return_value == (struct json_expression*)NULL) {
				return_value = copy_expression(the_expression);
			}	
			free(returned_value);
			returned_value = (char*)NULL;
			break;
		default:
			return_value = create_expression_node(the_expression->the_value);
			struct json_expression* l1 = resolve_value(the_expression->left, the_variable, the_value);
			struct json_expression* r1 = resolve_value(the_expression->right, the_variable, the_value);

			free_expression(return_value->left);
			free(return_value->left);
			return_value->left = l1;

			free_expression(return_value->right);
			free(return_value->right);
			return_value->right = r1;
			break;
	   }
	}
        else {
	}

	return return_value;
}


int get_value(struct json_expression* the_expression) {

	int return_value = 0;
	int left, right;
	if (the_expression != (struct json_expression*)NULL) {
	  switch(the_expression->the_operator) {
		case('+'):
			left = get_value(the_expression->left);
			right = get_value(the_expression->right);
			return_value = left + right;
			break;
		case('-'):
			left = get_value(the_expression->left);
			right = get_value(the_expression->right);
			return_value = left - right;
			break;
		case('*'):
			left = get_value(the_expression->left);
			right = get_value(the_expression->right);
			return_value = left * right;
			break;
		case('/'):
			left = get_value(the_expression->left);
			right = get_value(the_expression->right);
			return_value = left / right;
			break;
		
		case(' '):
			return_value = atoi(the_expression->the_value);
			break;
	  }
	}
	return return_value;
}



struct json_node* get_node_for_index(struct json_node* the_node , int index) {

	struct json_node* return_value = (struct json_node*)NULL;
	enum node_type type = the_node->the_type;

	if (index == 0) {
		
		if (the_node->the_node1 != (struct json_node*)NULL) {
			return_value = the_node->the_node1->the_node1;
		}
		else {
			return_value = the_node;
		}
	}
	if (index > 0) {
		if (type == ARRAY) {
			the_node = the_node->the_node1;
			return_value = get_node_for_index(the_node, index);
		}
		if (type == ELEMENT) {
			if (the_node->the_node2 != (struct json_node*)NULL) {
				the_node = the_node->the_node2;
			}
			else {
				the_node = the_node->the_node1;
			}
			return_value = get_node_for_index(the_node, index-1);

		}
		if (type == VALUE) {
			the_node = the_node->the_node1;
			return_value = the_node;
        	}
	}
	return return_value;
}

char* convert_to_value(char* the_string, char* var, int the_value) {


	char* return_value = (char*)NULL;
	char* string_to_convert = the_string;
	char* string1 = get_first_non_white(string_to_convert);
	char* var1 = get_first_non_white(var);


        if ((string1 != (char*)NULL) && (var1 != (char*)NULL)) {	
		if (strcmp(string1, var1) == 0) {
			return_value = malloc(sizeof(char)*10);
			sprintf(return_value, "%d" , the_value);
		}
	}

	
	return return_value;
}


