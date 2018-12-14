
#include "_my_string_functions.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


char* remove_quotes(char* the_string) {

	//  if the string has both leading and trailing quotes, remove them

	char* return_value = the_string;

	/*  if string is delimeted in ""'s, remove them */
	int i = 0;
	int found_starting_quote = 0;
	int squote = -1;
	int found_ending_quote = 0;
	int equote = -1;
	for (i = 0; i < strlen(the_string); i++) {
#ifdef DEBUG_REMOVE_QUOTES
		printf("the_string[i] is %c\n", the_string[i]);
#endif
		if ((found_starting_quote == 0) && (the_string[i] == '\"')) {
			found_starting_quote = 1;
			squote = i;
		}
	}
	for (i = strlen(the_string) - 1; i >= 0; i--) {
		if ((found_ending_quote == 0) && (the_string[i] == '\"')) {
			found_ending_quote = 1;
			equote = i;
		}
	}
#ifdef DEBUG_REMOVE_QUOTES
	printf("fsq %d squote %d feq %d equote %d\n", found_starting_quote, squote, found_ending_quote, equote);
#endif

	if ((found_starting_quote == 1) && (found_ending_quote == 1)) {
		the_string[squote] = ' ';
		the_string[equote] = '\0';
	}


	return return_value;

	
}


char* get_interior(char* the_string, char LEFT, char RIGHT) {

	//  return the stuff between the starting LEFT and ending RIGHT delimeters 

	//  return value is a NEW string that is created HERE, or NULL

	char* return_value = (char*)NULL;

	char LEFT_DELIMETER = LEFT;
	char RIGHT_DELIMETER = RIGHT;
	int length = strlen(the_string) + 1;
	if (length > 1) {

		//  check the string for a starting LEFT
		//  check the string for an ending RIGHT

		char* pch1 = strchr(the_string, LEFT_DELIMETER);
		int start = (pch1 - the_string)/sizeof(char);
		char* pch2 = strrchr(the_string, RIGHT_DELIMETER);
		int in_length = ((pch2-1) - pch1)/(sizeof(char));

		if ((pch1 != (char*)NULL) && (pch2 != (char*)NULL)) {
			return_value = calloc((in_length + 1), sizeof(char));			
			strncpy(return_value, &the_string[start+1], in_length);
			return_value[in_length] = '\0';   //  strncpy needs terminator
		}
			
	}

	return return_value;
}



char* find_first_delimeter(char* the_string, char delimeter) {

	//  this routine finds the first given delimeter that is not in brackets [ ] or braces { }  or ""
        //  and passes back the string after that delimeter

	char* return_value = (char*)NULL;

	int len = strlen(the_string);
	int i = 0;
	int number_open_brackets = 0;
	int number_open_braces = 0;
	int number_open_quotes = 0;
	int saw_start_quote = 0;
	while (i < len) {

		switch (the_string[i]) {
			case '[' : 
				number_open_brackets++;
				break;
			case ']' :
				number_open_brackets--;
				break;
			case '{' :
				number_open_braces++;
				break;
			case '}' :
				number_open_braces--;
				break;
			case '"' :
				if (saw_start_quote == 0) {
					saw_start_quote = 1;
					number_open_quotes++;
				}
				else {
					saw_start_quote = 0;
					number_open_quotes--;
				}
				break;

		}
		if ((number_open_brackets == 0) && (number_open_braces == 0) && (number_open_quotes == 0)) {
			if (the_string[i] == delimeter) {
				return_value = &(the_string[i+1]);
				break;		
			}
		}
		i++;
	}

	return return_value;



}



char* find_delimeter(char* the_string, char delimeter, char type) {

	//  pass back a string that starts with the first delimeter that is not of type B - Bracket b - brace 
        //  B - [ ] bracket
        //  b - { } brace

	char* return_value = (char*)NULL;

	char* first_start_bracket;
	char* first_end_bracket;
	char* almost_return_value = strchr(the_string, delimeter);
	char* string_end = the_string + strlen(the_string)*sizeof(char);
	char start_char = '[';
	char end_char = ']';
	if (type != 'B') {
		start_char = '{';
		end_char = '}';
	}

	while (almost_return_value != string_end) {
		first_start_bracket = strchr(almost_return_value, start_char);
		first_end_bracket = strchr(almost_return_value, end_char);
		
		if ((first_start_bracket != (char*)NULL) && (first_end_bracket != (char*)NULL) &&
		    (almost_return_value != (char*)NULL)) {
			//  pointer arithmetic
			if (almost_return_value < first_start_bracket) {
				return_value = almost_return_value;
			}
			if ((almost_return_value > first_start_bracket) && 
			    (almost_return_value < first_end_bracket)) {
				//  your delimeter is in the brackets
				almost_return_value = first_end_bracket + sizeof(char);			

			}
			else {
				return_value = almost_return_value;
				almost_return_value = string_end;
			}

		}
		else {
			return_value = almost_return_value;
			almost_return_value = string_end;
		}
		first_start_bracket = 0;
		first_end_bracket = 0;
	}

	return return_value;
}


char* get_first_non_white(char* the_string) {

        //  return a pointer to the first non white space character in a string
	//  if non blank returns null
	//  NOTE:  currently using ' ' as the only white space character HMA 

	char* return_value = (char*)NULL;
	int i = 0;
	if (the_string != (char*)NULL) {
		int length = strlen(the_string);
		for (i = 0; i < length; i++) {    // valgrind squawks on strlen
			if (the_string[i] != ' ') {
				return_value = &(the_string[i]);
				break;
				
			}
			
		}
	}
	return return_value;
}

char* strtrim(char* the_string) {

        //  trim the string off the back

	char* return_value = (char*)NULL;

        if (the_string != (char*)NULL) {

		//  trim the string on the back
		char* xstring = get_first_non_white(the_string);
		int length = 0;
		if (xstring == (char*)NULL) {
			length = 0;
		}
		else {
			length = strlen(xstring);
		}
		int i = 0;
		int actual_length = length;
		for (i = length-1; i >= 0; i--) {
			if (xstring[i] != ' ') {
				break;
			}
			actual_length--;
		}


		if (actual_length > 0) {
			return_value = calloc((actual_length + 1), sizeof(char));	
			strncpy(return_value, xstring, actual_length);
			return_value[actual_length] = '\0';
		}
	}
	return return_value;
}



