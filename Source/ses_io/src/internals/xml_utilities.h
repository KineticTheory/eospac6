

//  xml tag functions

#define _read_long_tag HEADER(_read_long_tag)
#define _read_long_list_tag HEADER(_read_long_list_tag)
#define _write_tag HEADER(_write_tag)
#define _skip_tag HEADER(_skip_tag)
#define _read_char_pFILE_xml HEADER(_read_char_pFILE_xml)
#define _write_char_pFILE_xml HEADER(_write_char_pFILE_xml)
#define _read_char_xml HEADER(_read_char_xml)
#define _write_char_xml HEADER(_write_char_xml)
#define _read_long_pFILE_xml HEADER(_read_long_pFILE_xml)
#define _write_long_pFILE_xml HEADER(_write_long_pFILE_xml)
#define _read_long_xml HEADER(_read_long_xml)
#define _write_long_xml HEADER(_write_long_xml)
#define _read_double_pFILE_xml HEADER(_read_double_pFILE_xml)
#define _write_double_pFILE_xml HEADER(_write_double_pFILE_xml)
#define _read_double_xml HEADER(_read_double_xml)
#define _write_double_xml HEADER(_write_double_xml)
#define _read_word_pFILE_xml HEADER(_read_word_pFILE_xml)
#define _write_word_pFILE_xml HEADER(_write_word_pFILE_xml)
#define _read_word_list_pFILE_xml HEADER(_read_word_list_pFILE_xml)
#define _write_word_list_pFILE_xml HEADER(_write_word_list_pFILE_xml)
#define _read_word_list_xml HEADER(_read_word_list_xml)
#define _write_word_list_xml HEADER(_write_word_list_xml)
#define _read_ses_word_array_xml HEADER(_read_ses_word_array_xml)
#define _write_ses_word_array_xml HEADER(_write_ses_word_array_xml)
#define _read_ses_string_xml HEADER(_read_ses_string_xml)
#define _write_ses_string_xml HEADER(_write_ses_string_xml)

long               _read_long_tag(FILE* pFILE, char* tag_name);
long*              _read_long_list_tag(FILE* pFILE, char* tag_name, int num);

ses_boolean        _write_tag(FILE* pFILE, char* the_tag);
char*              _skip_tag(FILE* pFILE, char* the_tag);


//  file list functions

char               _read_char_pFILE_xml(FILE* pFILE);
ses_boolean        _write_char_pFILE_xml(FILE* pFILE, char the_char);

char               _read_char_xml(struct _ses_file_handle* pSFH);
ses_boolean        _write_char_xml(struct _ses_file_handle* PSFH, char the_char);

long               _read_long_pFILE_xml(FILE* pFILE);
ses_boolean        _write_long_pFILE_xml(FILE* pFILE, long the_long);

long               _read_long_xml(struct _ses_file_handle* pSFH);
ses_boolean        _write_long_xml(struct _ses_file_handle* pSFH, long the_long);

double             _read_double_pFILE_xml(FILE* pFILE);
ses_boolean        _write_double_pFILE_xml(FILE* pFILE, double the_double);

double             _read_double_xml(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_double_xml(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation);

ses_word           _read_word_pFILE_xml(FILE* pFILE);
ses_boolean        _write_word_pFILE_xml(FILE* pFILE, ses_word the_word);

ses_word_reference _read_word_list_pFILE_xml(FILE* pFILE, int num);
ses_boolean        _write_word_list_pFILE_xml(FILE* pFILE, ses_word the_array[], int size_array);

ses_word_reference _read_word_list_xml(struct _ses_file_handle* pSFH, int num);
ses_boolean        _write_word_list_xml(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array);

ses_word_reference _read_ses_word_array_xml(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean        _write_ses_word_array_xml(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation);

ses_string         _read_ses_string_xml(struct _ses_file_handle* pSFH);
ses_boolean        _write_ses_string_xml(struct _ses_file_handle* pSFH, ses_string the_string, int size_array);
