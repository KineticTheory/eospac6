
#include "_json_parser.h"
#include "_my_string_functions.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


char detect_object_divider(char* the_string) {
	char return_value = ',';
	if (strstr(the_string, ";") != (char*)NULL) {
		return_value = ';';
	}
	return return_value;
}

char* get_substring_for_object_type(char* the_string, enum node_type object_type) {

	//  return a substring pulled from "the_string" for the given object type

	char KEY_DELIMETER = ':';
	char OBJECT_DIVIDER = detect_object_divider(the_string);

	char* return_value = (char*)NULL;
	char* return_value1 = (char*)NULL;

	if (the_string != (char*)NULL) {


		if (object_type == OBJECT) {
			return_value = get_substring_for_object(the_string);
		}
		if (object_type == MEMBERS) {
			return_value = find_first_delimeter(the_string, OBJECT_DIVIDER);
		}
		if (object_type == PAIR) {
			if (the_string != (char*)NULL) {
				int len1 = strlen(the_string);
				return_value1 = find_first_delimeter(the_string, OBJECT_DIVIDER);
				if (return_value1 != (char*)NULL) {
					int len2 = strlen(return_value1);
					int mylen = len1 - len2 - 1;
					return_value = malloc(sizeof(char)*(mylen+1));
					strncpy(return_value, the_string, mylen);			
					return_value[mylen]='\0';
				}
				else {
					return_value = malloc(sizeof(char)*(len1+1));
					strcpy(return_value, the_string);			
		
					
				}
			}
			

		}
		if (object_type == KEY) {
			return_value = get_substring_for_key_value(the_string, 'b', KEY_DELIMETER);
		}
		if (object_type == VALUE) {
			return_value = get_substring_for_key_value(the_string, 'a', KEY_DELIMETER);
		}
		if (object_type == KVALUE) {
			return_value = get_substring_for_key_value(the_string, 'a', KEY_DELIMETER);
		}
		if (object_type == ARRAYVALUE) {
			if (the_string != (char*)NULL) {
				int len1 = strlen(the_string);
				return_value1 = find_first_delimeter(the_string, OBJECT_DIVIDER);
				if (return_value1 != (char*)NULL) {
					int len2 = strlen(return_value1);
					int mylen = len1 - len2 - 1;
					return_value = malloc(sizeof(char)*(mylen+1));
					strncpy(return_value, the_string, mylen);			
					return_value[mylen]='\0';
				}
				else {
					return_value = malloc(sizeof(char)*(len1+1));
					strcpy(return_value, the_string);			
		
					
				}
			}
			

			
		}
		if (object_type == EXPRESSION) {
			return_value = get_substring_for_expression(the_string);
		}
		if (object_type == STRING) {
			return_value = malloc(sizeof(char)*(strlen(the_string)+1));  //valgrind squawks on strlen

			char* no_quotes_string = remove_quotes(the_string);
			strcpy(return_value, no_quotes_string);
		}
		if (object_type == NUMBER) {
			return_value = malloc(sizeof(char)*(strlen(the_string)+1));//valgrind squawks on strlen
			strcpy(return_value, the_string);
		}
		if (object_type == ARRAY){
			return_value = get_substring_for_array(the_string);
		}
		if (object_type == ELEMENT){
			if (the_string != (char*)NULL) {
				return_value1 = find_first_delimeter(the_string, OBJECT_DIVIDER);
				if (return_value1 != (char*)NULL) {
					int len2 = strlen(return_value1);
					return_value = malloc(sizeof(char)*(len2+1));
					strcpy(return_value, return_value1);			
				}
				else {
					return_value = (char*)NULL;
		
					
				}
			}
			
		}
		if (object_type == BOOLEAN) {
			return_value = malloc(sizeof(char)*(strlen(the_string)+1)); //valgrind squawks on strlen
			strcpy(return_value, the_string);
		}
		if (object_type == MYNULL) {
			return_value = malloc(sizeof(char)*(strlen(the_string)+1));//valgrind squawks on strlen
			strcpy(return_value, the_string);
		}
		

	}

	return return_value;
}

char* get_substring_for_object(char* the_string) {

	//  the first thing that is non blank in the string is '{'
        //  inbetween, you hit at least one key:value pairs with comma's as the delimeter between them
	//  the last thing that is non blank in the string is '}'

	char* return_value = (char*)NULL;
	return_value = get_interior(the_string, '{', '}');   //  return the stuff between the { }

	if (return_value == (char*)NULL) {
		return_value = malloc(sizeof(char)*(strlen(the_string)+1));
		strcpy(return_value, the_string);
	}

	return return_value;
}

char* get_substring_for_pair_members(char* the_string, char direction, char delimeter) {

	//  the substring for members is the stuff before/after the first delimeter
	//  when that delimeter is NOT surrounded by [] or {} or ""

	//  if the delimeter does not exist -- direction 'a' returns NULL, direction 'b' returns the_string

	//  return value is a NEW string that is created HERE, or NULL

	char* return_value = (char*)NULL;    //  make return value separate string
	char* string_at_first_delimeter = find_first_delimeter(the_string, delimeter);
	if (string_at_first_delimeter != (char*)NULL) {
		if (direction == 'b') {
			int length = (string_at_first_delimeter  - the_string)/sizeof(char);
			/* int strlength = 0; */
			/* strlength = strlen(the_string); */
			if (length <= 1) {
				return_value = calloc((strlen(the_string) + 1), sizeof(char));
				strcpy(return_value, the_string);
			}
			else {
                                
				return_value = calloc((length), sizeof(char));  //  back up to before delimeter
				return_value[length-1] = '\0';
				if (length > 1) {
					strncpy(return_value, the_string, length-1);
				}
			}
		
		}
		if (direction == 'a') {
			return_value = malloc(sizeof(char)*(strlen(string_at_first_delimeter)+1));
			strcpy(return_value, "");  //  or valgrind squawks
			strcpy(return_value, string_at_first_delimeter + 1);
		}
		
	}
	else {
		if (direction == 'a') {
			return_value = (char*)NULL;
		}
		if (direction == 'b') {
			return_value = malloc(sizeof(char)*(strlen(the_string) + 1));
			strcpy(return_value, "");  //  or valgrind squawks
			strcpy(return_value, the_string);

		}
	}


	return return_value;
}



char* get_substring_for_key_value(char* the_string, char direction, char delimeter) {

	//  the substring for value is the stuff (before or after) the first delimeter

	//  return value is a NEW string that is created HERE, or NULL

	char* return_value = (char*)NULL;
	char* almost_return_value = (char*)NULL;  //  initialize or valgrind squawks 

	if (the_string != (char*)NULL) {
		if (the_string[0] != '\"') {
			almost_return_value = strchr(the_string, delimeter);
		}
		else {
			almost_return_value = strchr(&the_string[1], '\"');
		}
		
		if (almost_return_value != (char*)NULL) {
			if (direction == 'b') {
				//  pointer arithmetic
				int length = (almost_return_value - the_string)/sizeof(char);
				if (length < 0) {
					length = strlen(the_string);   //  delimeter in the_string[0]
				}

				return_value = malloc(sizeof(char)*(length + 1));
				strcpy(return_value, "");  //  or else valgrind squawks
				strncpy(return_value, the_string, length);
				return_value[length] = '\0';  //  strncpy needs terminator
		
			}
			if (direction == 'a') {
				//  valgrind squawks on strlen, use of almost_return_value (but that's nuts)
				if ((almost_return_value != (char*)NULL) && (strlen(almost_return_value) > 1)) {
					return_value = malloc(sizeof(char)*(strlen(&almost_return_value[1]) + 1));
					strcpy(return_value , &almost_return_value[1]);  
				}
			}
		}
	}

	return return_value;
}


char* get_substring_for_expression(char* the_string) {

	//  the first thing that is non blank in the string is '('
        //  inbetween, you hit any character
	//  the last thing that is non blank in the string is ')'

	char* return_value = get_interior(the_string, '(', ')');
	return return_value;

}


char* get_substring_for_array(char* the_string) {

	//  the first thing that is non blank in the string is '['
        //  inbetween, you hit at least one key:value pairs with comma's as the delimeter between them
	//  the last thing that is non blank in the string is ']'

	return get_interior(the_string, '[', ']');

}


char* get_substring_for_element(char* the_string) {

	//  the substring for an element is the stuff after the first comma
	
	char* return_value = (char*)NULL;

	char MEMBER_DELIMETER = ',';
	char* almost_return_value = strchr(the_string, MEMBER_DELIMETER);
	if (almost_return_value != (char*)NULL) {
		return_value = malloc(sizeof(char)*(1 + strlen(&almost_return_value[1])));
		strcpy(return_value , &almost_return_value[1]);
	}

	return return_value;

}


char* get_type(enum node_type object_type) {

		//  given a node_type, return a string describing that type

		char* return_value = (char*)NULL;

		if (object_type == OBJECT) {
			return_value = "OBJECT";
		}
		if (object_type == MEMBERS) {
			return_value = "MEMBERS";
		}
		if (object_type == PAIR) {
			return_value = "PAIR";
		}
		if (object_type == KEY) {
			return_value = "KEY";
		}
		if (object_type == VALUE) {
			return_value = "VALUE";
		}
		if (object_type == KVALUE) {
			return_value = "KVALUE";
		}
		if (object_type == ARRAYVALUE) {
			return_value = "ARRAYVALUE";
		}
		if (object_type == STRING) {
			return_value = "STRING";
		}
		if (object_type == NUMBER) {
			return_value = "NUMBER";
		}
		if (object_type == ARRAY){
			return_value = "ARRAY";
		}
		if (object_type == ELEMENT){
			return_value = "ELEMENT";
		}
		if (object_type == BOOLEAN) {
			return_value = "BOOLEAN";
		}
		if (object_type == MYNULL) {
			return_value = "MYNULL";
		}
		if (object_type == EXPRESSION) {
			return_value = "EXPRESSION";
		}
		return return_value;

}


enum node_type get_node_type(char* the_string) {

	//  given a STRING, this routine guesses the node_type

	//  a value is a STRING or a NUMBER or an OBJECT or ARRAY or BOOLEAN or MYNULL or EXPRESSIOn
	//  a NUMBER starts with a digit
	//  an OBJECT starts with a {
	//  an ARRAY starts with a [
	//  a BOOLEAN is either true or false
	//  a NULL is the string null
	//  an EXPRESSION starts with a (
	//  NOT_VALID is the default



	enum node_type return_value = NOT_VALID;

	if (the_string != NULL) {

	   char* pch1 = get_first_non_white(the_string);

	   if (pch1 != (char*)NULL) {

		if (pch1[0] != '\"') {

		  if (strncmp(pch1, "null", 4) == 0){
			return_value = MYNULL;
			if (strlen(pch1) > 4) {
				if (pch1[4] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }
		  
		  if (strncmp(pch1, "NULL", 4) == 0){
			return_value = MYNULL;
			if (strlen(pch1) > 4) {
				if (pch1[4] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }
		  if (strncmp(pch1, "true", 4) == 0){
			return_value = BOOLEAN;
			if (strlen(pch1) > 4) {
				if (pch1[4] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }
		  if (strncmp(pch1, "TRUE", 4) == 0){
			return_value = BOOLEAN;
			if (strlen(pch1) > 4) {
				if (pch1[4] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }

		  if (strncmp(pch1, "false", 5) == 0){
			return_value = BOOLEAN;
			if (strlen(pch1) > 5) {
				if (pch1[5] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }
		  if (strncmp(pch1, "FALSE", 5) == 0){
			return_value = BOOLEAN;
			if (strlen(pch1) > 5) {
				if (pch1[5] != ' ') {
					return_value = NOT_VALID;
				}
			}
		  }
		  if (pch1[0] == '['){
			return_value = ARRAY;
		  }
		  if (pch1[0] == '{'){
			return_value = OBJECT;

		  }
		  if (isdigit(pch1[0]) != 0){
			return_value = NUMBER;
		  }

		  if (pch1[0] == '-') {
			return_value = NUMBER;
		  }

		  if (pch1[0] == '(') {
			return_value = EXPRESSION;
		  }
		  if ((return_value == NOT_VALID) && (isascii(pch1[0]) > 0)) {
			return_value = STRING;
		  }




		}
		else {
		  return_value = STRING;
		}
	   }
	}

	return return_value;
}




