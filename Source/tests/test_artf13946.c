/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 * \ingroup C tests quick
 * \brief Ensure sesameFilesDir.txt parsing errors do not recur.
 *        See SourceForge© Issue #artf13946 for more details:
 *        https://tf.lanl.gov/sf/go/artf13946
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eos_Interface.h"
#include "TEST_FUNCTIONS.h"

#define EOS_FREE(p) {if(p != NULL) free(p); p=NULL;}

int main ()
{

  int i;
  FILE *fp = NULL;
  char *indexFileName = "sesameFilesDir.txt";
  EOS_CHAR *file_str = NULL;

  char *customIndex =
    "# Distributed Sesame file list\n"
    "  # blank line with trailing comment\n"
    "\n"
    "# this is a very long line(1).this is a very long line(2).this is a very long line(3).this is a very long line(4).this is a very long line(5).this is a very long line(6).this is a very long line(7).this is a very long line(8).this is a very long line(9).this is a very long line(10).this is a very long line(11).this is a very long line(12).this is a very long line(13).this is a very long line(14).this is a very long line(15).this is a very long line(16).this is a very long line(17).this is a very long line(18).this is a very long line(19).this is a very long line(20).this is a very long line(21).this is a very long line(22).this is a very long line(23).this is a very long line(24).this is a very long line(25).this is a very long line(26).this is a very long line(27).this is a very long line(28).this is a very long line(29).this is a very long line(30).this is a very long line(31).this is a very long line(32).this is a very long line(33).this is a very long line(34).this is a very long line(35).this is a very long line(36).this is a very long line(37).this is a very long line(38).this is a very long line(39).this is a very long line(40).this is a very long line(41).this is a very long line(42).this is a very long line(43).this is a very long line(44).this is a very long line(45).this is a very long line(46).this is a very long line(47).this is a very long line(48).this is a very long line(49).this is a very long line(50).this is a very long line(51).this is a very long line(52).this is a very long line(53).this is a very long line(54).this is a very long line(55).this is a very long line(56).this is a very long line(57).this is a very long line(58).this is a very long line(59).this is a very long line(60).this is a very long line(61).this is a very long line(62).this is a very long line(63).this is a very long line(64).this is a very long line(65).this is a very long line(66).this is a very long line(67).this is a very long line(68).this is a very long line(69).this is a very long line(70).this is a very long line(71).this is a very long line(72).this is a very long line(73).this is a very long line(74).this is a very long line(75).this is a very long line(76).this is a very long line(77).this is a very long line(78).this is a very long line(79).this is a very long line(80).this is a very long line(81).this is a very long line(82).this is a very long line(83).this is a very long line(84).this is a very long line(85).this is a very long line(86).this is a very long line(87).this is a very long line(88).this is a very long line(89).this is a very long line(90).this is a very long line(91).this is a very long line(92).this is a very long line(93).this is a very long line(94).this is a very long line(95).this is a very long line(96).this is a very long line(97).this is a very long line(98).this is a very long line(99).this is a very long line(100).this is a very long line(101).this is a very long line(102).this is a very long line(103).this is a very long line(104).this is a very long line(105).this is a very long line(106).this is a very long line(107).this is a very long line(108).this is a very long line(109).this is a very long line(110).this is a very long line(111).this is a very long line(112).this is a very long line(113).this is a very long line(114).this is a very long line(115).this is a very long line(116).this is a very long line(117).this is a very long line(118).this is a very long line(119).this is a very long line(120).this is a very long line(121).this is a very long line(122).this is a very long line(123).this is a very long line(124).this is a very long line(125).this is a very long line(126).this is a very long line(127).this is a very long line(128).this is a very long line(129).this is a very long line(130).this is a very long line(131).this is a very long line(132).this is a very long line(133).this is a very long line(134).this is a very long line(135).this is a very long line(136).\n"
    "\n"
    "this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/this/is/a/very/long/relative/path/name/used/to/demonstrate/that/it/will/ignored/by/the/search/algorithm/in/EOSPAC/version/six/sesame\n"
    "\n";

  extern EOS_CHAR **sesameFiles;
  extern EOS_INTEGER sesameFilesL;

  int err = EOS_OK;

  /* Reset the file list */
  if (sesameFiles) {
    for (i = 0; i < sesameFilesL; i++) {
      EOS_FREE(sesameFiles[i]);
    }
    EOS_FREE(sesameFiles);
    sesameFilesL = 0;
  }

  /*  Read sesameFilesDir.txt into memory */
  fp = fopen (indexFileName, "r");  /* open indexFileName */
  file_str = (EOS_CHAR*) malloc(1 * sizeof (EOS_CHAR));
  file_str = strcpy(file_str, "");
  i = 0;
  if (fp) {
    EOS_CHAR s[1024];
    while (fgets(s, 1024, fp)) {
      /* this reads the entire file into file_str[] */
      i = strlen(file_str) + strlen(s) + 1;
      file_str = realloc(file_str, (i * sizeof (EOS_CHAR)));
      strcat(file_str, s);
    }
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -1;
    goto CLEANUP;
  }

  /* Write custom sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s\n%s\n  END\n", customIndex, file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -2;
    goto CLEANUP;
  }

  /*  Write custom sesameFilesDir.txt to stdout */
  printf ("\n----- BEGIN CUSTOM sesameFilesDir.txt -----\n");
  fp = fopen (indexFileName, "r");  /* open indexFileName */
  if (fp) {
    EOS_CHAR s[1024];
    while (fgets(s, 1024, fp)) {
      printf("%s", s);
    }
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -1;
    goto CLEANUP;
  }
  printf ("\n------ END CUSTOM sesameFilesDir.txt ------\n");

  printf ("\nCustom file list:\n");
  err = print_FileList(-1, "<...>/");
  if (! err)
    goto CLEANUP;

  /*  Write file_str to sesameFilesDir.txt */
  fp = fopen (indexFileName, "w");  /* open indexFileName */
  if (fp) {
    fprintf(fp, "%s", file_str);
    fclose (fp);              /* close indexFileName */
  }
  else {
    err = -3;
    goto CLEANUP;
  }

 CLEANUP:
  EOS_FREE (file_str);
  for (i = 0; i < sesameFilesL; i++)
    EOS_FREE (sesameFiles[i]);
  EOS_FREE (sesameFiles);
  return(err);

}
