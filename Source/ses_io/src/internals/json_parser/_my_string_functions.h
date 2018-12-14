
#include "_header.h"

#ifndef HEADER
#define HEADER(n) json ## _ ## n
#endif

#define get_interior HEADER(get_interior)
#define find_first_delimeter HEADER(find_first_delimeter)
#define find_delimeter HEADER(find_delimeter)
#define get_first_non_white HEADER(get_first_non_white)
#define strtrim HEADER(strtrim)
#define remove_quotes HEADER(remove_quotes)


char* get_interior(char* the_string, char LEFT, char RIGHT);
char* find_first_delimeter(char* the_string, char delimeter);
char* find_delimeter(char* the_string, char delimeter, char type);
char* get_first_non_white(char* the_string);
char* strtrim(char* the_string);  //  strip off white space at the end
char* remove_quotes(char* the_string);

