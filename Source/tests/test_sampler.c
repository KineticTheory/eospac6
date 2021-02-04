/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/

/*! \file
 *  \ingroup C tests quick unit
 *  \brief This is a unit test of the eos_Utils::_eos_CompressArray
 *         function. Apparently actual data provides a better challenge
 *         for the algorithm's robustness, so selected pressure tables
 *         were used for sampling.
 *
 *  See SourceForge issue 
 *  <a href="https://tf.lanl.gov/sf/go/artf35695">artf35695</a>
 *  for associated details.
 *
 * Uses the following routines:
 * _eos_CompressArray
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include "../src/eos_types_internal.h"
#include "../src/eos_Utils.h"
#include "test_sampler.h"

int main ()
{
  EOS_INTEGER N, i, j;
  EOS_CHAR c = ' ';
  FILE *tableFile;
  static EOS_CHAR *fname = "TablesLoaded.dat";

  /* get array sizes for data arrays */
  get_data_sz(data_sz);

  for (j = 0; j < NTESTS; j++) {

    N = data_sz[j];

    _eos_CompressArray (data[j], &N, 78, NULL, NULL);

    data_sz[j] = N;

  }

  printf ("set key outside\n");
  printf ("set logscale xy\n");
  printf ("set terminal x11 1 title 'positive data'\n");
  printf ("plot");
  
  c = ' ';
  for (j = 0; j < NTESTS; j++) {
    printf("%c 'test_sampler.%s' i %i:%i t 'TEST #%i for %i, N=%i' w lp",c,fname,j,j,j+1,table[j],data_sz[j]);
    c = ',';
  }
  printf("\n");

  printf ("set nologscale\n");
  printf ("set xrange [1:10]\n");
  printf ("set yrange [-7:0]\n");
  printf ("set terminal x11 2 title 'negative data'\n");
  printf ("plot");
  
  c = ' ';
  for (j = 0; j < NTESTS; j++) {
    printf("%c 'test_sampler.%s' i %i:%i t 'TEST #%i for %i, N=%i' w lp",c,fname,j,j,j+1,table[j],data_sz[j]);
    c = ',';
  }
  printf("\n");


  /* Create dummy TablesLoaded.dat file */
  tableFile = fopen (fname, "w");
  if (!tableFile)
    return 1;

  for (j = 0; j < NTESTS; j++) {
    N = data_sz[j];
    for (i = 0; i < N; i++) {
      fprintf (tableFile, "%i %.15e\n", i+1, (data[j])[i]);
    }
    fprintf (tableFile, "\n\n");
  }

  fclose (tableFile);

  return 0;
}
