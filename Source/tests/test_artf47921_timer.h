/*********************************************************************
 * test_artf47921 series-specific common declarations
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 *
 * Copyright -- see file named COPYRIGHTNOTICE
 *
 ********************************************************************/
/*
 * EOS_REAL    wctime    - wall clock time (seconds) elapsed since time state was reset
 * EOS_REAL    cputime   - cpu clock time (seconds) elapsed since time state was reset
 * EOS_REAL    cpucycles - cpu clock time (seconds) elapsed since time state was reset
 * EOS_INTEGER err       - error flag
 * EOS_INTEGER num_X     - number of x values use to calculate N
 * EOS_INTEGER num_Y     - number of y values use to calculate N
 * EOS_INTEGER N         - num_X * num_Y
 */

#ifndef test_artf47921_timer_h

#define test_artf47921_timer_h

typedef struct {
    EOS_REAL wctime;
    EOS_REAL cputime;
    EOS_REAL cpucycles;
    EOS_INTEGER err;
    EOS_INTEGER num_X;
    EOS_INTEGER num_Y;
    EOS_INTEGER N;
} TIMER_RESULTS_t;

TIMER_RESULTS_t *TIMER_RESULTS_SETUP;
TIMER_RESULTS_t *TIMER_RESULTS_INTRP;

EOS_BOOLEAN true = EOS_TRUE, false = EOS_FALSE;
EOS_INTEGER nInsert = 1;

#endif
