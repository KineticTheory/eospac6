
/* 
    This file contains all functions that READ and WRITE 'little' objects (double, long, char, ses_material_id, ses_table_id, ses_word_array)
*/

/* Here is a hack to define isinf for Solaris 10, which seems to be missing this necessary function. DAP */

#if defined (__SVR4) && defined (__sun)
#ifndef isinf
#include <ieeefp.h>
#define isinf(x) (!finite((x)) && (x)==(x))
#endif
#endif

#define _read_double HEADER(_read_double)
#define _write_double HEADER(_write_double)
#define _read_long HEADER(_read_long)
#define _write_long HEADER(_write_long)
#define _read_ses_word_array HEADER(_read_ses_word_array)
#define _write_ses_word_array HEADER(_write_ses_word_array)
#define _read_char HEADER(_read_char)
#define _write_char HEADER(_write_char)
#define my_get_mantissa HEADER(my_get_mantissa)
#define my_trunc_to_sig_dig HEADER(my_trunc_to_sig_dig)
#define my_flip_bytes HEADER(my_flip_bytes)
#define my_truncate_for_significant_digits HEADER(my_truncate_for_significant_digits)
#define my_do_validation_double HEADER(my_do_validation_double)


double             _read_double(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_double(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation);

long               _read_long(struct _ses_file_handle* PSFH);
ses_boolean        _write_long(struct _ses_file_handle* pSFH, long the_long);

ses_word_reference _read_ses_word_array(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_ses_word_array(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation);

char               _read_char(FILE* pFILE);
ses_boolean        _write_char(FILE* pFILE, char the_char);


double             my_get_mantissa(double x, int* exponent) ;
double             my_trunc_to_sig_dig(double x, unsigned int nsig, int* returned_sig) ;
double             my_flip_bytes(double the_double);
double             my_truncate_for_significant_digits(double the_double, unsigned int nsig);
ses_boolean        my_do_validation_double(double the_double);





