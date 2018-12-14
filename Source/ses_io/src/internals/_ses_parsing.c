
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

//////////////////DEFINES///////////////////////
//PARSE_QUOTED_SUBSTRINGS - left in for convenience
////////////////////////////////////////////////



/*  routines for parsing a string in the json format  { name_value pairs separated by delimter ';' }
    values can be long, string, long_array, string_array
    an array of longs is [ longs separated by delimter ',' ]
    an array of strings is [ strings separated by delimter ',' ]

    this routine does NOT parse the whole json format, just looks for what we have defined for the table

{ tid: 801; description: "801 table def"; number_independent: 0; number_arrays: 2; sizes:[26, 54]; labels:["label1", "label2"] } */


ses_string _get_table_string(struct _standard_table the_table) {

	ses_string return_value = (ses_string)NULL;

	/*the table string looks like:
		{ tid: 801; description: "801 table def"; number_independent: 0; number_arrays: 2; nr:3; nt:4; sizes:[26, 54]; labels:["label1", "label2"] } */



	if (the_table._is_user_defined == SES_TRUE) {

		ses_table_id tid = the_table._the_tid;
		ses_string description = the_table._description;
		long num_ind = the_table._num_independent;
		long num_arr = the_table._num_arrays;
                char**sizes = the_table._size_arrays;
		ses_label* labels = the_table._label;
		long nr = 0;
		nr = the_table._nr;
                long nt = 0;
		nt = the_table._nt;

		int number_chunks = 8;
		ses_string* the_string = malloc(sizeof(ses_string) * number_chunks);

		int tid_len = 5 + 30 + 1;  //  30 spaces for the tid
		the_string[0] = calloc(tid_len, sizeof(char));          //  sizeof the_string[0] is at max 36
		the_string[0][0] = '\0';
		int n = sprintf(the_string[0], "tid: %ld", tid);

		int descrip_len = 14 + strlen(description) + 1;         //  sizeof the_string[1] is at max 15 + strlen(description)
		the_string[1] = calloc(descrip_len, sizeof(char));
		the_string[1][0] = '\0';
		n = sprintf(the_string[1], "description : %s", description);
	
		int ind_len = 21 + 10 + 1;  //  10 spaces for num_ind    
		the_string[2] = calloc(ind_len, sizeof(char));          //  sizeof the_string[2] is at max 32
		the_string[2][0] = '\0';
		n = sprintf(the_string[2], "number_ind : %ld", num_ind);

		int num_arr_len = 16 + 10 + 1;  //  10 spaces for num_arr     
		the_string[3] = calloc(num_arr_len, sizeof(char));         //  sizeof the_string[3] is at max 27
		the_string[3][0] = '\0';
		n = sprintf(the_string[3], "num_arrays : %ld", num_arr);

                /*  add nr and nt */

		int nr_len = 5 + 10 + 1;  //  25 spaces for nr
		the_string[4] = calloc(nr_len, sizeof(char));               // sizeof the_string[4] is at max  16
		the_string[4][0] = '\0';
                n = sprintf(the_string[4], "nr : %ld", nr);


		int nt_len = 5 + 10 + 1;  //  25 spaces for nt
		the_string[5] = calloc(nt_len, sizeof(char));               //  sizeof the_string[5] is at max 16
		the_string[5][0] = '\0';

                n = sprintf(the_string[5], "nt : %ld", nt);

                /*  end chunk for nr and nt */

		int computed_size = 0;
		int i = 0;
		for (i = 0; i < 6; i++) {
			computed_size = computed_size + strlen(the_string[i]);
		}
		computed_size = computed_size + 1;

		int number_string_size = (30 + 4 + 1);  //  30 spaces for the size, 4 for the " %s ," 1 for the '\0'
		computed_size = num_arr * number_string_size +  4 + 1;  //  "[ " and " ]" and '\0'
		ses_string size_string = calloc(computed_size, sizeof(char));
		strcpy(size_string, "[ ");
		for (i = 0; i < num_arr; i++) {
			ses_string number_string = calloc(number_string_size, sizeof(char));
			if (i < num_arr - 1) {
				sprintf(number_string, " %s ,", sizes[i]);
			}
			else {
				sprintf(number_string, " %s ", sizes[i]);
			}
			size_string = strcat(size_string, number_string);
			free(number_string);
			number_string = (ses_string)NULL;			
		}
		size_string = strcat(size_string, " ]");

                computed_size = 0;
		for (i = 0; i < num_arr; i++) {
		  computed_size = computed_size + (strlen(labels[i]) + 3);  //  labels + " , "
		}
		computed_size = computed_size + 4 + 1;  //  sum (labels + " , ") + "[ " + " ]" + '\0

		ses_string label_string = calloc(computed_size, sizeof(char));
		strcpy(label_string, "[ ");
		for (i = 0; i < num_arr; i++) {
			if (i < num_arr - 1) {
				label_string = strcat(label_string, labels[i]);
				label_string = strcat(label_string, " , ");
			}
			else {
				label_string = strcat(label_string, labels[i]);
			}
		}
		label_string = strcat(label_string, " ]");

                the_string[6] = calloc((strlen(size_string)+8+1), sizeof(char));
		n = sprintf(the_string[6], "sizes : %s", size_string);
		the_string[7] = calloc((strlen(label_string)+9+1), sizeof(char));
		n = sprintf(the_string[7], "labels : %s", label_string);
		
		/* make one long string from the rest */

		int total_size = 0;
		for (i = 0; i < 8; i++) {
			total_size = total_size + strlen(the_string[i]) + 4;
		}
		total_size = total_size + 4 + 1;
		return_value = calloc(total_size, sizeof(char));
		n = sprintf(return_value, " { %s  , %s , %s , %s , %s , %s , %s , %s } ", the_string[0], the_string[1], the_string[2], the_string[3], the_string[4], the_string[5], the_string[6], the_string[7]);


                /* free */
                free(size_string);
		size_string = (ses_string)NULL;
		free(label_string);
		label_string = (ses_string)NULL;
		for (i = 0; i < number_chunks; i++) {
			free(the_string[i]);
			the_string[i] = (ses_string)NULL;
		}
		free(the_string);
		the_string = (ses_string*)NULL;
	}
	
	return return_value;
}


int _internal_parse(ses_string the_string, ses_string** keys, ses_string** values) {

	int return_value = 0;



	if ((keys == (ses_string**)NULL) || (values == (ses_string**)NULL)) {
#ifdef DEBUG_PRINT
		printf("_internal_parse:  keys or values null\n");
#endif
		return 0;
	}
        
        ses_string* almost_strings =  _parse_block(the_string, '{', '}', ';', &return_value);


        

	if (return_value > 0) {

		keys[0] = _internal_parse_keys(almost_strings, return_value);
		values[0] = _internal_parse_values(almost_strings, return_value);

	}
	else {
		return_value = 0;
	}
        
	int i = 0;
	for (i = 0; i < return_value; i++) {
		free(almost_strings[i]);
		almost_strings[i] = (ses_string)NULL;
	}
	free(almost_strings);
	almost_strings = (ses_string*)NULL;





	return return_value;
}

ses_string* _internal_parse_keys(ses_string* almost, int length) {

	ses_string* return_value = (ses_string*)NULL;	


	ses_string the_key = (ses_string)NULL;

	if ((almost != (ses_string*)NULL) && (length > 0)) {

		return_value = malloc(sizeof(ses_string) * length);
		ses_string current_string = (ses_string)NULL;

		int k = 0;
		for (k = 0; k < length; k++) {

			current_string = almost[k];
			the_key = malloc(sizeof(char)*8*(strlen(current_string) + 1));
			ses_boolean saw_colon = SES_FALSE;

			int j = 0;
			for (j = 0; j < strlen(current_string); j++) {
				if (current_string[j] == ':') {
					the_key[j] = '\0';
					saw_colon = SES_TRUE;
				}
				if (saw_colon == SES_FALSE) {
					the_key[j] = current_string[j];
				}
				
			}
			return_value[k] = malloc(sizeof(char)*(strlen(the_key) + 1));
			strcpy(return_value[k], the_key);
			free(the_key);
			the_key = (ses_string)NULL;
		}
	}
	return return_value;
}
ses_string* _internal_parse_values(ses_string* almost, int length) {
	
	ses_string* return_value = (ses_string*)NULL;	


	ses_string the_value = (ses_string)NULL;

	if ((almost != (ses_string*)NULL) && (length > 0))  {

		return_value = malloc(sizeof(ses_string) * length);
		ses_string current_string = (ses_string)NULL;

		int k = 0;
		for (k = 0; k < length; k++) {

			current_string = almost[k];

			the_value = malloc(sizeof(char)*(strlen(current_string) + 1));
			ses_boolean saw_colon = SES_FALSE;
			int start_value = 0;

			int j = 0;
			for (j = 0; j < strlen(current_string); j++) {
				if (saw_colon == SES_TRUE) {
					the_value[j - start_value - 1] = current_string[j];
				}
				if (current_string[j] == ':') {
					saw_colon = SES_TRUE;
					start_value = j;
				}
				
			}
			the_value[j-start_value - 1] = '\0';
			return_value[k] = malloc(sizeof(char)*(strlen(the_value) + 1));
			strcpy(return_value[k], the_value);

			free(the_value);
			the_value = (ses_string)NULL;
		}
	}

	return return_value;
}




long _convert_to_long(ses_string the_string) {

	long return_value = 0;



	return_value = atol(the_string);


	return return_value;
}

long* _convert_to_long_array(ses_string the_string, int* num) {

	long* return_value = (long*)NULL;



	//  get the substrings corresponding to the longs

	int num_strings = 0;
	ses_string* the_substring = _parse_block(the_string, '[', ']', ',', &num_strings);

	if (the_substring != (ses_string*)NULL) {

		//  create memory for the return array

		return_value = malloc(sizeof(long) * num_strings);

		//  convert the strings to longs and free memory

		int i = 0; 
		for (i=0; i<num_strings; i++) {
			return_value[i] = _convert_to_long(the_substring[i]);
			free(the_substring[i]);
		}

		//  free memory

		free(the_substring);

	}
	*num = num_strings;

	//  return the long array	
	
	return return_value;
}

ses_label* _convert_to_string_array(ses_string the_string) {

	ses_label* return_value = (ses_label*)NULL;

	//  get the substrings corresponding to the longs

	int num_strings = 0;
	ses_string* the_substring = _parse_block(the_string, '[', ']', ',', &num_strings);

	if ((num_strings > 0) && (the_substring != (ses_string*)NULL)) {

		//  create memory for the return array

		return_value = malloc(sizeof(ses_label) * num_strings);

		//  copy the strings

		int i = 0; 
		for (i=0; i<num_strings; i++) {
			return_value[i] = malloc(sizeof(char)*8*(strlen(the_substring[i]) + 1));
			strcpy(return_value[i],  the_substring[i]);
			free(the_substring[i]);
		}

		//  free memory

		free(the_substring);

	}
	else {
#ifdef DEBUG_PRINT
		printf("_convert_to_string_array: The substring returned was null, or nstrings is %d\n", num_strings);
#endif
	}

	//  return the string array
	
	return return_value;

}
/* Parse the input text into an unescaped cstring, and populate item. */
static const unsigned char _ses_firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
/*static const char *_ses_parse_string(const char *str)
{
  const char *ptr=str+1;char *ptr2;char *out;int len=0;unsigned uc,uc2;
  if (*str!='\"') {return 0;}	// not a string! 
	
  while (*ptr!='\"' && *ptr && ++len) if (*ptr++ == '\\') ptr++;	// Skip escaped quotes. 
	
  out=(char*)malloc(sizeof(char)*(len+1));	// This is how long we need for the string, roughly. 
  if (!out) return 0;
	
  ptr=str+1;ptr2=out;
  while (*ptr!='\"' && *ptr)
  {
    if (*ptr!='\\') *ptr2++=*ptr++;
    else
    {
      ptr++;
      switch (*ptr)
      {
      case 'b': *ptr2++='\b';	break;
      case 'f': *ptr2++='\f';	break;
      case 'n': *ptr2++='\n';	break;
      case 'r': *ptr2++='\r';	break;
      case 't': *ptr2++='\t';	break;
      case 'u':	 // transcode utf16 to utf8. 
	sscanf(ptr+1,"%4x",&uc);ptr+=4;	// get the unicode char. 

	if ((uc>=0xDC00 && uc<=0xDFFF) || uc==0)	break;	// check for invalid.

	if (uc>=0xD800 && uc<=0xDBFF)	// UTF16 surrogate pairs.
	{
	  if (ptr[1]!='\\' || ptr[2]!='u')	break;	// missing second-half of surrogate.
	  sscanf(ptr+3,"%4x",&uc2);ptr+=6;
	  if (uc2<0xDC00 || uc2>0xDFFF)		break;	// invalid second-half of surrogate.
	  uc=0x10000 | ((uc&0x3FF)<<10) | (uc2&0x3FF);
	}

	len=4;if (uc<0x80) len=1;else if (uc<0x800) len=2;else if (uc<0x10000) len=3; ptr2+=len;
					
	switch (len) {
	case 4: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
	case 3: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
	case 2: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
	case 1: *--ptr2 =(uc | _ses_firstByteMark[len]);
	}
	ptr2+=len;
	break;
      default:  *ptr2++=*ptr; break;
      }
      ptr++;
    }
  }
  *ptr2=0;
  if (*ptr=='\"') ptr++;
  return out;
}
*/
ses_string* _parse_block(ses_string the_string, char left_block, char right_block, char delimeter, int* size_return) {

        ses_string* return_value = (ses_string*)NULL;


	int number_delims = 0;

	int MAX_SUBSTRING_SIZE = strlen(the_string) + 1;  //  the max size that a substring could be
        if (the_string != (ses_string)NULL) {

		int length = strlen(the_string);
		ses_boolean saw_start_block = SES_FALSE;
		ses_boolean saw_end_block = SES_FALSE;
		int MAX_NUMBER_STRINGS = 10000;

		if (length > 0) {

			return_value = malloc(sizeof(ses_string) * MAX_NUMBER_STRINGS);
			ses_string substring = (ses_string)NULL;
			int substring_index = 0;
			int substring_start_index = 0;

			int i = 0;
			int j = 1;
			for (i = 0; i < length; i++) {

#ifdef PARSE_QUOTED_SUBSTRINGS
			  if ((the_string[i] == '"')) {
			    /* parse a quoted string */
			    int len0 = strlen(substring);
			    int len1 = len0;
			    strcat(substring,_ses_parse_string(&(the_string[i])));
			    len1 = strlen(substring);
			    i += len1 - len0 + 1;

			    /*  move the substring */
			    return_value[substring_index] = malloc(sizeof(char) * (strlen(substring) + 1));
			    strcpy(return_value[substring_index], substring);
			    substring_index++;
			  }
#endif
			  if (the_string[i] == left_block) {
			    saw_start_block = SES_TRUE;
			    substring = malloc(sizeof(char) * MAX_SUBSTRING_SIZE);
			    substring_start_index = i + 1;
			  }
			  if ((the_string[i] != left_block) &&
			      (saw_start_block == SES_TRUE) &&
			      (the_string[i] != delimeter)) {
			    substring[i - substring_start_index] = the_string[i];
			  }
			  if ((saw_start_block == SES_TRUE) &&
			      (saw_end_block == SES_FALSE) &&
			      ((the_string[i] == delimeter) ||
			       (the_string[i] == right_block))) {
			    substring[i - substring_start_index] = '\0';

			    /*  move the substring */
			    return_value[substring_index] = malloc(sizeof(char) * (strlen(substring) + 1));
			    strcpy(return_value[substring_index], substring);
			    substring_index++;

			    if (the_string[i] == delimeter) {

			      free(substring);
			      substring = (ses_string)NULL;
						
			      substring = malloc(sizeof(char) * MAX_SUBSTRING_SIZE);
			      substring_start_index = i+1;
					
			      number_delims++;
			      if (number_delims > j*MAX_NUMBER_STRINGS) {
#ifdef DEBUG_PRINT
				printf("return_value(%ld) : %ld ; number_delims(%d) > MAX_NUMBER_STRINGS(%d) = %d\n",
				       (long)return_value, sizeof(ses_string) * number_delims, number_delims, MAX_NUMBER_STRINGS,
				       number_delims > MAX_NUMBER_STRINGS);
#endif
				j++;
				ses_string* ptr = (ses_string*)realloc(return_value, sizeof(ses_string) * j * MAX_NUMBER_STRINGS);
				assert(ptr != NULL);
				assert(return_value != NULL);
			      }
			    }
			    else {
			      saw_end_block = SES_TRUE;
			    }

					
			  }
			}
                        free(substring);
		        substring = (ses_string)NULL;

		}
  
		if (saw_start_block == SES_TRUE) {
			*size_return = number_delims + 1;
		}
		if (saw_end_block == SES_FALSE) {
			*size_return = 0;
		}

	}
	return return_value;
}

ses_boolean _string_compare(ses_string string1, ses_string string2) {
	
	ses_boolean return_value = SES_FALSE;

	int the_return = strcmp(string1, string2);
	if (the_return == 0) {
		return_value = SES_TRUE;
	}


	return return_value;
}

ses_boolean  _remove_trailing_white_space(ses_string the_string) {

	ses_boolean return_value = SES_TRUE;

	if (the_string != (ses_string)NULL) {

		int length = strlen(the_string);
		if (length > 0) {

			int end = 0;
			ses_boolean saw_end = SES_FALSE;

			int i = 0;
			for (i = length - 1; i >= 0; i--) {
				if (saw_end == SES_FALSE) {

					if (isspace(the_string[i]) == SES_FALSE) {
						saw_end = SES_TRUE;
						end = i;
					}
				}
			}

			/*  move the NULL to end */

			the_string[end+1] = '\0';
			for (i = end+2; i < length; i++) {
				the_string[i] = ' ';
			}
		}
	}


	return return_value;
}


ses_boolean  _remove_leading_white_space(ses_string the_string) {

	/*  remove leading white space from a string */

	ses_boolean return_value = SES_TRUE;


	if (the_string != (ses_string)NULL) {

		int length = strlen(the_string);
		if (length > 0) {
			int start = 0;
			ses_boolean saw_char = SES_FALSE;
			int i = 0;
			for (i = 0; i < length; i++) {
				if (saw_char == SES_FALSE) {
					if (isspace(the_string[i]) == SES_FALSE) {
						saw_char = SES_TRUE;
						start= i;
					}
				}
			}

			if (start > 0) {
				for (i = 0; i < length - start; i++) {
					the_string[i] = the_string[i + start];		
				}
				the_string[length - start] = '\0';
				for (i = length - start + 1; i < length; i++) {
					the_string[i] = ' ';
				}
			}
		
		}
	}


	return return_value;
}

long*   _compute_addresses(long* size_arrays, int num_arrays) {

	long* return_value = (long*)NULL;


	if (size_arrays == (long*)NULL) {
#ifdef DEBUG_PRINT
		printf("_compute_addresses:  size_arrays null\n");
#endif
		return return_value;
	}
	
	return_value = malloc(sizeof(long) * num_arrays);


	int i = 0; 
	for (i = 0; i < num_arrays; i++) {
    		if (i == 0) {
      			return_value[0] = (long)1;
		}
		else {
			
			return_value[i] = return_value[i-1] + size_arrays[i-1];
		}
	}

	return return_value;
}



