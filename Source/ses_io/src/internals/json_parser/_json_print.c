   

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "_json_parser.h"
#include "_json_print.h"


void print_global_data(struct json_node* the_data) {

	printf("\n\n\nprint_global_data\n\n");
	print_json_node(the_data, 0);

}



void print_json_node(struct json_node* the_node, int indent_level) {


	char* indent_string = (char*)NULL;
        indent_string = make_indent_string(indent_level);

	if (the_node != (struct json_node*)NULL) {
		char* type_string = (char*)NULL;
		type_string = make_type_string(the_node->the_type);
		char* item_type = (char*)NULL;
		item_type = get_type(the_node->the_type);
		printf("%s %s: %s of type %s\n", indent_string, type_string, the_node->the_value, item_type);  //  valgrind squawks here	
		
		
		if (the_node->the_node1 != (struct json_node*)NULL) {
			print_json_node(the_node->the_node1, indent_level + 1);
		}
		if (the_node->the_node2 != (struct json_node*)NULL) {
			print_json_node(the_node->the_node2, indent_level + 1);
		}

		if (type_string != NULL) {
			free(type_string);
			type_string = (char*)NULL;
		}
	}
	if (indent_string != NULL) {
		free(indent_string);
		indent_string = (char*)NULL;
	}
}

char* make_indent_string(int indent_level) {

	char* return_value = malloc(sizeof(char)*(indent_level + 1));
	int i = 0;
        for (i = 0; i < indent_level; i++) {
		return_value[i] = ' ';
	}
        return_value[indent_level] = '\0';
	return return_value;
}

char* make_type_string(enum node_type the_type) {

	char* return_value = (char*)NULL;
	return_value = malloc(sizeof(char)*15);
	
	switch(the_type) {
		case(OBJECT):
			strcpy(return_value, "OBJECT");
			break;
		case(MEMBERS):
			strcpy(return_value, "MEMBERS");
			break;
		case(PAIR):
			strcpy(return_value, "PAIR");
			break;
		case(KEY):
			strcpy(return_value, "KEY");
			break;
		case(KVALUE):
			strcpy(return_value, "KVALUE");
			break;
		case(VALUE):
			strcpy(return_value, "VALUE");
			break;
		case(STRING):
			strcpy(return_value, "STRING");
			break;
		case(NUMBER):
			strcpy(return_value, "NUMBER");
			break;
		case(ARRAY):
			strcpy(return_value, "ARRAY");
			break;
		case(BOOLEAN):
			strcpy(return_value, "BOOLEAN");
			break;
		case(MYNULL):
			strcpy(return_value, "NULL");
			break;
		case(ELEMENT):
			strcpy(return_value, "ELEMENT");
			break;
		case(EXPRESSION):
			strcpy(return_value, "EXPRESSION");
			break;
		case(NOT_VALID):
			strcpy(return_value, "NOT_VALID");
			break;
		default:
			free(return_value);
			return_value = (char*)NULL;
			break;
	}
	return return_value;
}




