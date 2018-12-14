
/* 
    This file contains all functions that READ and WRITE 'little' objects in binary(double, long, char, ses_material_id, ses_table_id, ses_word_array)

*/

#define _read_char_binary HEADER(_read_char_binary)
#define _write_char_binary HEADER(_write_char_binary)
#define _read_long_binary HEADER(_read_long_binary)
#define _write_long_binary HEADER(_write_long_binary)
#define _read_double_binary HEADER(_read_double_binary)
#define _write_double_binary HEADER(_write_double_binary)
#define _read_long_pFILE_binary HEADER(_read_long_pFILE_binary)
#define _write_long_pFILE_binary HEADER(_write_long_pFILE_binary)
#define _read_ses_word_array_binary HEADER(_read_ses_word_array_binary)
#define _write_ses_word_array_binary HEADER(_write_ses_word_array_binary)
#define _write_ses_string_binary HEADER(_write_ses_string_binary)


char 		   _read_char_binary(FILE* pFILE );
ses_boolean 	    _write_char_binary(FILE* pFILE, char the_char);

long               _read_long_binary(struct _ses_file_handle* PSFH);
ses_boolean        _write_long_binary(struct _ses_file_handle* pSFH, long the_long);

double             _read_double_binary(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_double_binary(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation);

long               _read_long_pFILE_binary(FILE* pFILE, ses_boolean needs_flip);
ses_boolean        _write_long_pFILE_binary(FILE* pFILE, ses_boolean needs_flip);

ses_word_reference _read_ses_word_array_binary(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_ses_word_array_binary(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation);

ses_boolean        _write_ses_string_binary(struct _ses_file_handle* pSFH, ses_string the_string, long size_array);


