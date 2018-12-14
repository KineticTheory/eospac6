
/*  files for the template format */

#ifdef DEFINE_TEMPLATE

long           _get_address_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid, struct _ses_file_handle* pSFH);
long           _get_address_for_table(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);
long           _get_directory_size(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
ses_boolean    _get_grid(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt);
ses_boolean _go_to_data_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_boolean _go_to_index_record(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
ses_error_flag _read_array(struct _ses_file_handle* pSFH, ses_word_reference the_buffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_data_record(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _read_directory(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH);
ses_error_flag _read_index_record(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH);
ses_boolean _write_array(struct _ses_file_handle* ptHandle, ses_word_reference ptBuffer, long size, unsigned int nsig, ses_boolean do_valid);
ses_error_flag _write_data_record(struct _ses_data_record* tmp, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
ses_boolean _write_directory(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH) {
ses_boolean _write_index_record(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH) {

#endif
