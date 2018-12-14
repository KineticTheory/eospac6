
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "string_list.h"

int add_element(char** the_list, int size_list, char* the_element) {

  int return_value = 1;  /*  false */
  int i = 0;
  for (i = 0; i < size_list; i++) {
	if (the_list[i] == (char*)NULL) {
		the_list[i] = malloc(sizeof(char) * (strlen(the_element)+1));
		strcpy(the_list[i], the_element);
		i = size_list;
		return_value = 0;
	}
  }
  return return_value;
}


void print_list(char** mylist, int size_list) {

  int i = 0;
  for (i = 0; i < size_list; i++) {
	
	if (mylist[i] != (char*)NULL) {
	  printf("List element %d:  | %s |\n", i, mylist[i]);
	}
  }

}


int is_sublist_contained(char** container, int len_container, char** looking_for, int len_looking) {

	//  are all the strings you're looking_for contained in the container?

	//  function prototypes

	int contains(char** container, int lenc, char* element);

	//  end function prototypes

	int return_value = 0;     /* return true */
	
	if ((container != (char**)NULL) && (looking_for != (char**)NULL)) {
		int i = 0;
		for (i = 0; i < len_looking; i++) {
			if (looking_for[i] != (char*)NULL) {
				if (contains(container, len_container, looking_for[i]) == 1) {
					return_value = 1;
				}
			}
        	}
	}

	return return_value;
}


int contains(char** container, int lenc, char* element) {

	int return_value = 1;
        if ((container != (char**)NULL) && (lenc > 0) && (element != (char*)NULL)) {
		int i = 0;
		for (i = 0; i < lenc; i++) {
			if (container[i] != (char*)NULL) {
				if (strcmp(container[i], element) == 0) {
					return_value = 0;
				}
			}
			else {
			}
        	}
	}
	return return_value;

}


