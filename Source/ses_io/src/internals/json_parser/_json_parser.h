
#include "_header.h"

#ifndef _JSON_PARSER_H
#define _JSON_PARSER_H

#ifndef HEADER
#define HEADER(n) json ## _ ## n
#endif

#define copy_parse_node HEADER(copy_parse_node)
#define free_node HEADER(free_node)
#define free_expression_node HEADER(free_expression_node)

#define parse_json HEADER(parse_json)
#define create_expression_node HEADER(create_expression_node)
#define copy_expression HEADER(copy_expression)
#define free_expression HEADER(free_expression)

#define get_substring_for_object_type HEADER(get_substring_for_object_type)
#define get_substring_for_object HEADER(get_substring_for_object)
#define get_substring_for_pair_members HEADER(get_substring_for_pair_members)
#define get_substring_for_key_value HEADER(get_substring_for_key_value)
#define get_substring_for_element HEADER(get_substring_for_element)
#define get_substring_for_array HEADER(get_substring_for_array)
#define get_substring_for_expression HEADER(get_substring_for_expression)
#define get_type HEADER(get_type)
#define get_node_type HEADER(get_node_type)

#define get_node_type HEADER(get_node_type)
#define resolve_value HEADER(resolve_value)
#define get_value HEADER(get_value)
#define get_node_for_index HEADER(get_node_for_index)
#define convert_to_value HEADER(convert_to_value)

#define detect_object_divider HEADER(detect_object_divider)



enum node_type { OBJECT, MEMBERS, PAIR, KEY,  KVALUE, VALUE, ARRAYVALUE, STRING, NUMBER, ARRAY, ELEMENT, BOOLEAN, MYNULL, EXPRESSION, NOT_VALID };

struct json_expression {

        char the_operator;
	char* the_value;
	struct json_expression* left;
	struct json_expression* right;

};

struct json_node {
	
	enum node_type the_type;
	char* the_value;
	struct json_node* the_node1;
	struct json_node* the_node2;

	struct json_expression* the_expression;

	struct json_node* associated_node;
	int is_copied;
	
};




//  internal functions

struct json_node* copy_parse_node(struct json_node* the_node);
void free_node(struct json_node* the_node);
void free_expression_node(struct json_expression* the_expression);

char* parse_json(char* the_string, char* prefix, enum node_type object_type, struct json_node* the_node);
struct json_expression* create_expression_node(char* the_string);
struct json_expression* copy_expression(struct json_expression*);
void free_expression(struct json_expression*);

//  parse_strings functions

char* get_substring_for_object_type(char* the_string, enum node_type object_type);
enum node_type get_node_type(char* the_string);
char* get_substring_for_object(char* the_string);
char* get_substring_for_pair_members(char* the_string, char direction, char delimter);
char* get_substring_for_key_value(char* the_string, char direction, char delimeter);
char* get_substring_for_element(char* the_string);
char* get_substring_for_array(char* the_string);
char* get_substring_for_expression(char* the_string);
char* get_type(enum node_type object_type);
enum node_type get_node_type(char* the_string);


//  helper routines

struct json_expression* resolve_value(struct json_expression* the_expression, char* the_variable, int the_value);
int get_value(struct json_expression* the_expression);
struct json_node* get_node_for_index(struct json_node* the_node , int index);
char* convert_to_value(char* the_string, char* var, int the_value);

#endif
