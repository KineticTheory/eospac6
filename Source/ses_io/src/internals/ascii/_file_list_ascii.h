
/* 
    This file contains all functions that READ and WRITE 'little' objects in ascii(double, long, char, ses_material_id, ses_table_id, ses_word_array)
*/

#define _read_double_ascii HEADER(_read_double_ascii)
#define _read_double_tomax_ascii HEADER(_read_double_tomax)
#define _write_double_ascii HEADER(_write_double_ascii)
#define _write_double_tomax_ascii HEADER(_write_double_tomax_ascii)
#define _read_long_tomax_ascii HEADER(_read_long_tomax_ascii)
#define _read_long_tomax_pFILE_ascii HEADER(_read_long_tomax_pFILE_ascii)
#define _read_long_ascii HEADER(_read_long_ascii)
#define _write_long_ascii HEADER(_write_long_ascii)
#define _read_long_pFILE_ascii HEADER(_read_long_pFILE_ascii)
#define _write_long_pFILE_ascii HEADER(_write_long_pFILE_ascii)
#define _read_char_ascii HEADER(_read_char_ascii)
#define _write_char_ascii HEADER(_write_char_ascii)
#define _read_ses_word_array_ascii HEADER(_read_ses_word_array_ascii)
#define _write_ses_word_array_ascii HEADER(_write_ses_word_array_ascii)
#define _write_ses_string_ascii HEADER(_write_ses_string_ascii)
#define _write_eof_ascii HEADER(_write_eof_ascii)

double             _read_double_ascii(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation);
double             _read_double_tomax_ascii(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation, int max_length);

ses_boolean        _write_double_ascii(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_double_tomax_ascii(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation, int max_length);

long               _read_long_tomax_ascii(struct _ses_file_handle* PSFH, unsigned int fixed_size);
long               _read_long_ascii(struct _ses_file_handle* PSFH);
ses_boolean        _write_long_ascii(struct _ses_file_handle* pSFH, long the_long);


long               _read_long_tomax_pFILE_ascii(FILE* pFILE, unsigned int fixed_size, ses_boolean needs_flip);
long               _read_long_pFILE_ascii(FILE* pFILE, ses_boolean needs_flip);
ses_boolean        _write_long_pFILE_ascii(FILE* pFILE, ses_boolean needs_flip);

char               _read_char_ascii(FILE* pFILE);
ses_boolean        _write_char_ascii(FILE* pFILE, char the_char);

ses_word_reference _read_ses_word_array_ascii(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_ses_word_array_ascii(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_ses_string_ascii(struct _ses_file_handle* pSFH, ses_string the_string, long size_array);

ses_error_flag     _write_eof_ascii(struct _ses_file_handle* pSFH);



//ses_boolean        _go_to_next_array_location_ascii(struct _ses_file_handle* pSFH, long location);

