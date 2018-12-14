
#include "ses_defines.h"

#define add_element HEADER(add_element)
#define print_list HEADER(print_list)
#define is_sublist_contained HEADER(is_sublist_contained)
#define contains HEADER(contains)

int add_element(char** the_list, int size_list, char* the_element);
void print_list(char** mylist, int size_list);

int is_sublist_contained(char** container, int lenc, char** sublist, int lens);
int contains(char** container, int lenc, char* element);





