
/*  files for the binary format */

//------------------------------------
//debug ifdefs in progress
#define ADD_NTAB_TO_GET_GRID
//------------------------------------

#define _isit_my_format_binary HEADER(_isit_my_format_binary)
#define _get_address_for_material_binary HEADER(_get_address_for_material_binary)
#define _get_address_for_table_binary HEADER(_get_address_for_table_binary)
#define _get_directory_size_binary HEADER(_get_directory_size_binary)
#define _get_grid_binary HEADER(_get_grid_binary)
#define _go_to_data_record_binary HEADER(_go_to_data_record_binary)
#define _go_to_index_record_binary HEADER(_go_to_index_record_binary)
#define _go_to_next_array_location_binary HEADER(_go_to_next_array_location_binary)
#define _read_array_binary HEADER(_read_array_binary)
#define _read_data_record_binary HEADER(_read_data_record_binary)
#define _read_directory_binary HEADER(_read_directory_binary)
#define _read_index_record_binary HEADER(_read_index_record_binary)
#define _write_array_binary HEADER(_write_array_binary)
#define _write_data_record_binary HEADER(_write_data_record_binary)
#define _write_directory_binary HEADER(_write_directory_binary)
#define _write_index_record_binary HEADER(_write_index_record_binary)

#include "_file_list_binary.h"

ses_boolean     _isit_my_format_binary(FILE* pFILE);
long            _get_address_for_material_binary(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);
long           _get_address_for_table_binary(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);
long           _get_directory_size_binary(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
#ifdef ADD_NTAB_TO_GET_GRID
ses_boolean    _get_grid_binary(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt, long* ntab);
#else
ses_boolean    _get_grid_binary(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt);
#endif
ses_boolean _go_to_data_record_binary(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean _go_to_index_record_binary(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean  _go_to_next_array_location_binary(struct _ses_file_handle* pSFH, long location);
ses_error_flag _read_array_binary(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_data_record_binary(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_directory_binary(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);
ses_error_flag _read_index_record_binary(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset);
ses_boolean _write_array_binary(struct _ses_file_handle* ptHandle, ses_word_reference ptBuffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _write_data_record_binary(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_boolean _write_directory_binary(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
ses_boolean _write_index_record_binary(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);


