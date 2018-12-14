
#include "ses_globals.h"

/*  routines for parsing a string in the json format 

{ tid: 801, number_arrays: 2, sizes:[32, 54], labels:["label1", "label2"] } */

#define _get_table_string HEADER(_get_table_string)
#define _internal_parse HEADER(_internal_parse)
#define _internal_parse_keys HEADER(_internal_parse_keys)
#define _internal_parse_values HEADER(_internal_parse_values)
#define _convert_to_long HEADER(_convert_to_long)
#define _convert_to_long_array HEADER(_convert_to_long_array)
#define _convert_to_string_array HEADER(_convert_to_string_array)
#define _string_compare HEADER(_string_compare)
#define _parse_block HEADER(_parse_block)
#define _remove_white_space HEADER(_remove_white_space)
#define _compute_addresses HEADER(_compute_addresses)
#define _remove_leading_white_space HEADER(_remove_leading_white_space)
#define _remove_trailing_white_space HEADER(_remove_trailing_white_space)


ses_string              _get_table_string(struct _standard_table the_table);

/*  internal parsing routines */



int 			_internal_parse(ses_string the_string, ses_string** keys, ses_string** values);
ses_string* 	        _internal_parse_keys(ses_string* almost, int length);
ses_string* 	        _internal_parse_values(ses_string* almost, int length);

long                    _convert_to_long(ses_string the_string);
long*                   _convert_to_long_array(ses_string the_string, int* num);
ses_string*             _convert_to_string_array(ses_string the_string);
ses_boolean             _string_compare(ses_string string1, ses_string string2);
ses_string*             _parse_block(ses_string the_string, char left_block, char right_block, char delimeter, int* size_return);
ses_boolean             _remove_white_space(ses_string the_string);
long*                   _compute_addresses(long* size_arrays, int num_arrays);
ses_boolean             _remove_leading_white_space(ses_string the_string);
ses_boolean             _remove_trailing_white_space(ses_string the_string);




