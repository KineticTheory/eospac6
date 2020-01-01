/*!
 * \file
 * \ingroup utils
 */
#ifndef GETOPT_H
#define GETOPT_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>

#define ERR(s, c)   if(opterr) {		\
    (void) fputs(argv[0], stderr);		\
    (void) fputs(s, stderr);			\
    (void) fputc(c, stderr);			\
    (void) fputc('\n', stderr);			\
  }

/*This is a hack around to prevent conflicts with the clib versions of the quantities*/
#define opterr __opterr__
#define optind __optind__
#define optopt __optopt__
#define optarg __optarg__
#define passthru __passthru__

static int   opterr = 1;   /* flag:error message on unrecognzed options */
static int   optind = 0;   /* last touched cmdline argument */
static int   optopt;       /* last returned option */
static char  *optarg;      /* argument to optopt */
static int   passthru = 0; /* pass through remaining arguments to caller? */
static int   optCnt = 0;   /* count of option-related cmdline arguments specified */

static int   init=0;       /* reset getopt */

typedef struct {
  int flag;
  char *arg;
} option_t;
#define _OPTSIZE 256
static option_t option[_OPTSIZE];

static char **args = NULL; /* non-option cmdline arguments specified */
static int argsc = 0;      /* count of non-option cmdline arguments specified (i.e., extent of args[]) */

int debug(int lvl, char *file, int line, char *msg) {
  if (option['v'].flag >= lvl) {
    printf("debug%d :: %s (%d) : %s\n", lvl, file, line, msg);
    return 1;
  }
  return 0;
}

/*!
 * \brief This function is the public domain version of getopt, the command
 *        line argument processor.
 *
 * \par STDERR
 *       All information and error messages are returned
 *
 * \param[in] *argc       - int : number of variable arguments;
 *
 * \param[in] **argv      - char** : pointer to array of cmdline arguments;
 *
 * \param[in] *opts       - char* : string of defined options, which may contain characters 0-255;
 *                                  A ':' character specifies a required option argument for the
 *                                  preceding character in *opts.
 *
 * \arg args              - char** : global array of non-option command line arguments
 *
 * \arg argsc             - int : global extent of args
 *
 * \arg options           - int[_OPTSIZE] : global array of option flags
 *
 */

/* int    argc is the number of arguments on cmdline
 * char   **argv is the pointer to array of cmdline arguments
 * char   *opts is the string of all valid options
 * each char case must be given; options taking an arg are followed by ':'
 */

static int getopt_internal(int argc, char **argv, char *opts) {
  static int sp = 1;
  register int c;
  register char *cp;
  if(sp == 1) {
    /* check for end of options */
    if(optind >= argc ||
       argv[optind][0] == '\0') {
      return(-1);
    }
    else if(!strcmp(argv[optind], "--")) {
      optind++;
      return(-1);
    }
  }
  optopt = c = argv[optind][sp];
  if( (argv[optind][0] == '-'
       && ! isdigit(argv[optind][sp]) && argv[optind][sp] != '.'
       && (cp=strchr(opts, c)) == NULL)) {
    /* if arg sentinel as option or other invalid option,
       handle the error and return '?' */
    if(argv[optind][++sp] == '\0') {
      optind++;
      sp = 1;
    }
    return('?');
  }
  else if (argv[optind][0] != '-'
	   || (isdigit(argv[optind][sp]) || argv[optind][sp] == '.')) {
    return(-2); /* identify non-option argument */
  }
  if(*++cp == ':') {
    /* if option is given an argument...  */
    if(argv[optind][sp+1] != '\0')
      /* and the OptArg is in that CmdLineArg, return it... */
      optarg = &argv[optind++][sp+1];
    else if(++optind >= argc) {
      /* but if the OptArg isn't there and the next CmdLineArg
	 isn't either, handle the error... */
      ERR(": option requires an argument -- ", (char)c);
      sp = 1;
      return('?');
    } else
      /* but if there is another CmdLineArg there, return that */
      optarg = argv[optind++];
    /* and set up for the next CmdLineArg */
    sp = 1;
  } else {
    /* no arg for this opt, so null arg and set up for next option */
    if(argv[optind][++sp] == '\0') {
      sp = 1;
      optind++;
    }
    optarg = NULL;
  }
  return(c);
}

/*!
 * \brief This function will parse all options and other arguments.
 *
 * \param[in] *argc       - int : number of variable arguments;
 *
 * \param[in] **argv      - char** : pointer to array of cmdline arguments;
 *
 * \param[in] *opts       - char* : string of defined options, which may contain characters 0-255;
 *                                  A ':' character specifies a required option argument for the
 *                                  preceding character in *opts.
 *
 */
static int getoptions(int argc, char **argv, char *opts) {
  int i;
  char msg1[2048];
  char msg2[1024];

  if (! init) /* initialize option array elements */
    for(i = 0; i<_OPTSIZE; i++) {
      option[i].flag = 0;
      option[i].arg = NULL;
    }

  i = 0;
  while(optind<argc) {
    int idx = -1;
    if (! passthru) { /* NOTE: passthru is static and initialized to 0 */
      idx = getopt_internal(argc, argv, opts);
      if (idx == -1) passthru = 1; /* found '--' or '\0'; remaining argv copied to args */
    }
    if (idx >= 0 && idx < _OPTSIZE && idx != '?') { /* set option and any associated argument */
      option[idx].flag++;
      if (optarg) {
	if (! option[i].arg)
	  free(option[i].arg);
	option[idx].arg = (char*) malloc((strlen(optarg)+1)*sizeof(char));
	option[idx].arg = strcpy(option[idx].arg, optarg);
	optCnt++;
      }

      if (option['v'].flag) { /* display option and any associated argument */
	sprintf(msg1, "-%c: option[%d]=%d", idx, idx, option[idx].flag);
	if (optarg)
	  sprintf(msg2," => %s",optarg);
	else
	  sprintf(msg2,"%s", "");
	debug(1, (char*)__FILE__, (int)__LINE__, strcat(msg1,msg2));
      }
      optCnt++;
    }
    else if(idx == '?' && ! passthru) { /* an error was returned */
      return(idx);
    }
    else if(*(argv+(optind))) { /* populate **args with non-options */
      if(! args) args = (char**) malloc((++argsc)*sizeof(char*));
      else args = realloc(args,(++argsc)*sizeof(char*));
      args[argsc-1] = (char*) malloc((strlen(*(argv+(optind)))+1)*sizeof(char));
      strcpy(args[argsc-1],argv[optind++]);
    }
    i++;
  }
  return(0);
}
#endif
