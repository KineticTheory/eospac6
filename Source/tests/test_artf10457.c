/*********************************************************************
 * Test Program
 * ---------------------------------------------------------
 * Filetype: (SOURCE)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/

/*! \file
 *  \ingroup tests quick unit
 *  \brief This is a unit test of the eos_Utils::_eos_QuickSort
 *  to ensure the EOS_RECURSION_LIMIT is not exceeded. See
 *  SourceForge© Issue #artf10457 for more details:
 *  https://tf.lanl.gov/sf/go/artf10457
 *
 * Uses the following routine:
 * _eos_QuickSort
 *
 * \note
 * MATIDS TO TEST: none
 */

#include <stdio.h>
#include <stdlib.h>
#include "../src/eos_types_internal.h"
#include "../src/eos_Utils.h"

EOS_INTEGER BinarySearch(EOS_REAL *a, EOS_INTEGER N, EOS_REAL value) {
  EOS_INTEGER mid, low = 0;
  EOS_INTEGER high = N;
  
  while (low < high) {
    mid = (low + high)/2;
    if (a[mid] < value) {
      low = mid + 1;
    }
    else {
      /* can't be high = mid-1: here a[mid] >= value,
	 so high can't be < mid if A[mid] == value */
      high = mid;
    }
  }
  /* high == low, using high or low depends on taste */
  if ((low < N) && (a[low] == value))
    return low; // found
  else
    return -1; // not found
}

int main ()
{
  int num;
  EOS_INTEGER err, N, N0, i, j, k, index,
    count_missing = 0, count_misplaced = 0, max_recursion_level;
  FILE *pFile;
  EOS_REAL *a=NULL, *b=NULL;
  EOS_CHAR *errorMessage = NULL;
  EOS_BOOLEAN found=EOS_FALSE;

  /* Assign N unique, unsorted, numbers to a[] for sorting.
   * These numbers are taken from a multiphase tin EOS, which
   * exhibited a need for a larger EOS_RECURSION_LIMIT. */
  pFile = fopen ("test_artf10457.inp", "r");
  if (!pFile) {
    printf("Cannot open file: test_artf10457.inp\n");
    return 3;
  }
  fscanf (pFile,   "%d", &num); /* get number of elements to read */
  N = num;
  printf("%-d values\n", N);
  a = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
  if (!a) {
    printf("malloc(a) failed\n");
    return 2;
  }
  for (i=0;i<N;i++) {
    fscanf(pFile, "%le", &a[i]); /* get number of elements to read */
    /* printf("a[%d] = %23.15e\n", i, a[i]); */
    /* printf("%-.15e\n", a[i]); */
  }
  fclose(pFile);

  /* allocate b array */
  b = (EOS_REAL*) malloc(N * sizeof(EOS_REAL));
  if (!b) {
    printf("malloc(b) failed\n");
    return 2;
  }

  err = EOS_OK;
  printf ("BEFORE SORT:\n");
  for (i = 0; i < N; i++) {
    b[i] = a[i];
    /* printf ("a[%i] = b[%i] = %.15e\n", i, i, a[i]); */
  }

  max_recursion_level = _eos_QuickSort (N, a, 0, &err, &errorMessage);
  printf ("\nAFTER SORT: (err=%i)\n", err);
  if (err != EOS_OK) {
    printf ("_eos_QuickSort ERROR %i: %s\n", err, errorMessage);
    return err;
  }

  if (0) { /* just dump sorted a[] */

    for (i = 0; i < N; i++)
      printf ("a[%i] = %.15e\n", i, a[i]);

  }
  else { /* perform detailed comparison of sorted a[] and unsorted b[] */

    N0 = N;
    for (i = 0; i < N0; i++) {
      for (j = 0; j < N; j++) {
	index = BinarySearch(a, N, b[i]);
	/* if (a[j] == b[i]) { */
	if (index > -1) {
	  /* found b[i] in sorted a[] */
	  found=EOS_TRUE;
	  break;
	}
      }
      if (found) {
	printf ("a[%i] = b[%i] = %.15e\n", index, i, b[i]);
      }
      else {
	printf ("b[%i] = %.15e *** NOT IN a[] ***\n", i, b[i]);
	count_missing++;
      }
      if (i > 0 && a[i] < a[i - 1]) {
	count_misplaced++;
	printf ("  *** (a[%i]=%.15e) < (a[%i]=%.15e)\n", i, a[i], (i - 1),
		a[i - 1]);
      }
      if (0 && found) { /* match found; remove a[j] to accelerate next search */
	for (k = j; k < N-1; k++)
	  a[k] = a[k+1];
	N--;
	found=EOS_FALSE;
      }
    }

    printf ("\nmax_recursion_level = %i\n", max_recursion_level);

    if (count_missing > 0) {
      printf ("\n*** %i of a[] NOT IN b[] ***\n\n", count_missing);
      return 1;
    }

    if (count_misplaced > 0) {
      printf ("\n*** %i of a[] out of order ***\n\n", count_misplaced);
      return 2;
    }

    if (count_misplaced + count_missing == 0)
      printf ("\nPASSED: sorted a[] is valid\n");

  }

  free(a);
  free(b);

  return 0;
}
