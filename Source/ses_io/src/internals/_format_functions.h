
/* 
    This file contains all higher functions necessary to define a new sesame format type and to read and write it 
    (except those in _file_list, which contains functions for lower level objects (int, char, word, etc) )
   
    Not all formats will need or have each function
*/

#define _go_to_directory HEADER(_go_to_directory)
#define _go_to_index_record HEADER(_go_to_index_record)
#define _go_to_data_record HEADER(_go_to_data_record)
#define _go_to_next_array_location HEADER(_go_to_next_array_location)
#define _get_grid HEADER(_get_grid)
#define _isit_my_format HEADER(_isit_my_format)
#define _get_address_for_material HEADER(_get_address_for_material)
#define _get_directory_size HEADER(_get_directory_size)
#define _get_address_for_table_format HEADER(_get_address_for_table_format)
#define _read_directory_format HEADER(_read_directory_format)
#define _read_index_record_format HEADER(_read_index_record_format)
#define _read_data_record_format HEADER(_read_data_record_format)
#define _read_array_format HEADER(_read_array_format)
#define _write_directory_format HEADER(_write_directory_format)
#define _write_index_record_format HEADER(_write_index_record_format)
#define _write_data_record_format HEADER(_write_data_record_format)
#define _write_array_format HEADER(_write_array_format)


/*  functions that go to the object */

ses_boolean     _go_to_directory(ses_file_handle the_handle);
ses_boolean     _go_to_index_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean     _go_to_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean     _go_to_next_array_location(struct _ses_file_handle* pSFH, long location);

/*  getters */

ses_boolean     _get_grid(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, long* nr, long* nt, long* ntab);
ses_boolean     _isit_my_format(FILE* pFILE);

/*  directory functions */

long _get_address_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);
long _get_directory_size(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);

/*  index record functions */

long _get_address_for_table_format(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);

/*  readers */

ses_error_flag _read_directory_format(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);
ses_error_flag _read_index_record_format(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset);
ses_error_flag _read_data_record_format(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_array_format(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid);

/*  writers */

ses_boolean _write_directory_format(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
ses_boolean _write_index_record_format(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);
ses_error_flag _write_data_record_format(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_boolean _write_array_format(struct _ses_file_handle* ptHandle, ses_word_reference ptBuffer, long size, unsigned int nsig, ses_boolean do_valid);

