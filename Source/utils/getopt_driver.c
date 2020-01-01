/*!
 * \file
 * \ingroup utils
 */
#include "getopt.h"
#include "getopt_driver_help.h"

/*!
 * \brief Simple MAIN to demonstrate getoptions usage
 *
 * \author David A. Pimentel
 *         $Date: 2010/02/19 13:52:45 MST $
 *
 * \par STDERR
 *       All information and error messages are returned
 *
 * \par  USAGE
 *         <cmd> '<option_string>' [<options>] [<arguments>]
 */
int main(int argc, char **argv) {
  int i, err=0;
  if (argc < 2) {
    printf("%s", help_str);
    return(-1);
  }
  err = getoptions(argc-2, argv+2, argv[1]);
  if(! err) {
    printf("\nNon-option arguments:\n");
    for(i = 0; i<argsc; i++) {
      printf("args[%d]: %s\n", i, args[i]);
    }
    printf("\n");
    return(err);
  }
  return(err);
}
