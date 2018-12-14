
#include "_header.h"

#ifndef HEADER
#define HEADER(n) json ## _ ## n
#endif

#define initialize_parser HEADER(initialize_parser)
#define parse_string HEADER(parse_string)
#define print_parse_tree HEADER(print_parse_tree)
#define free_parser HEADER(free_parser)

#define get_key_value_pairs HEADER(get_key_value_pairs)
#define get_keys HEADER(get_keys)
#define get_type_for_key HEADER(get_type_for_key)
#define get_value_for_key HEADER(get_value_for_key)

#define evaluate_expression HEADER(evaluate_expression)

#define walk_tree HEADER(walk_tree)


//  user interface functions

struct json_node* initialize_parser(void);
int   parse_string(char* the_string, struct json_node* the_node);
void  print_parse_tree(struct json_node* the_node);
void  free_parser(struct json_node* the_node);

//  functions that pass back values

int get_key_value_pairs(char*** the_keys, char*** types, char*** values, struct json_node* the_node);
int get_keys(char*** the_keys, struct json_node* the_node);
char* get_type_for_key(char* key, struct json_node* the_node);
char* get_value_for_key(char* key, int index, struct json_node* the_node);

int evaluate_expression(char* the_key, int index, char** vars, int* values, int num_values, struct json_node* the_node);

//  functions for looking at values

struct json_node* walk_tree(struct json_node* the_node, char* the_key);

