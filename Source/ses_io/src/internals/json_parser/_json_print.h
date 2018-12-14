
#include "_header.h"

#ifndef HEADER
#define HEADER(n) json ## _ ## n
#endif

//  print functions

#define print_global_data HEADER(print_global_data)
#define print_json_node HEADER(print_json_node)
#define make_indent_string HEADER(make_indent_string)
#define make_type_string HEADER(make_type_string)

void print_global_data(struct json_node* the_data);
void print_json_node(struct json_node* the_node, int indent_level);
char* make_indent_string(int indent_level);
char* make_type_string(enum node_type the_type);
