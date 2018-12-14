
#define _find_grid_tag HEADER(_find_grid_tag)
#define _find_index_record_tag HEADER(_find_index_record_tag)
#define _find_data_record_tag HEADER(_find_data_record_tag)


//long _read_long_tag(FILE* pFILE, char* tag_name);
//long* _read_long_list_tag(FILE* pFILE, char* tag_name, long num);
//ses_word_reference _read_word_list_text(FILE* pFILE, long num);
//long _read_long_text_pFILE(FILE* pFILE);

ses_boolean _find_grid_tag(FILE* pFILE, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean _find_index_record_tag(FILE* pFILE, ses_material_id the_mid);
ses_boolean _find_data_record_tag(FILE* pFILE, ses_material_id the_mid, ses_table_id the_tid);

//void _write_tag(FILE* pFILE, char* the_tag);
//void _write_long_text(struct _ses_file_handle* pSFH, long num);
//void _write_long_text_pFILE(FILE* pFILE, long num);

//char* _skip_tag(FILE* pFILE, char* tag_name);

