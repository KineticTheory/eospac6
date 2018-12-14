
/*  files for the xml format */

#define _isit_my_format_xml HEADER(_isit_my_format_xml)
#define _get_address_for_material_xml HEADER(_get_address_for_material_xml)
#define _get_address_for_table_xml HEADER(_get_address_for_table_xml)
#define _get_directory_size_xml HEADER(_get_directory_size_xml)
#define _get_grid_xml HEADER(_get_grid_xml)
#define _go_to_data_record_xml HEADER(_go_to_data_record_xml)
#define _go_to_index_record_xml HEADER(_go_to_index_record_xml)
#define _go_to_next_array_location_xml HEADER(_go_to_next_array_location_xml)
#define _read_array_xml HEADER(_read_array_xml)
#define _read_data_record_xml HEADER(_read_data_record_xml)
#define _read_directory_xml HEADER(_read_directory_xml)
#define _read_index_record_xml HEADER(_read_index_record_xml)
#define _write_array_xml HEADER(_write_array_xml)
#define _write_data_record_xml HEADER(_write_data_record_xml)
#define _write_directory_xml HEADER(_write_directory_xml)
#define _write_index_record_xml HEADER(_write_index_record_xml)

#include "_file_list_xml.h"

ses_boolean     _isit_my_format_xml(FILE* pFILE);
long            _get_address_for_material_xml(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);
long           _get_address_for_table_xml(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);
long           _get_directory_size_xml(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
ses_boolean    _get_grid_xml(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt, long* ntab);
ses_boolean      _go_to_data_record_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean  _go_to_index_record_xml(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);

ses_boolean  _go_to_next_array_location_xml(struct _ses_file_handle* pSFH, long location);
ses_error_flag _read_array_xml(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_data_record_xml(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_directory_xml(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);
ses_error_flag _read_index_record_xml(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset);
ses_boolean _write_array_xml(struct _ses_file_handle* ptHandle, ses_word_reference ptBuffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _write_data_record_xml(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_boolean _write_directory_xml(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
ses_boolean _write_index_record_xml(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);


