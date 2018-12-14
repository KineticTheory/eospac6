
/*  _ses_standard.c  */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_standard_tables HEADER(_construct_standard_tables)
#define _destruct_standard_tables HEADER(_destruct_standard_tables)
#define _get_standard_num_arrays HEADER(_get_standard_num_arrays)
#define _get_standard_relative_addresses HEADER(_get_standard_relative_addresses)
#define _get_standard_sizes HEADER(_get_standard_sizes)
#define _get_standard_sizes_as_chars HEADER(_get_standard_sizes_as_chars)
#define _get_standard_labels HEADER(_get_standard_labels)
#define _get_standard_label_at_index HEADER(_get_standard_label_at_index)
#define _is_valid_mid HEADER(_is_valid_mid)
#define _is_valid_tid HEADER(_is_valid_tid)
#define _is_valid_open_flag HEADER(_is_valid_open_flag)
#define _is_valid_file_type HEADER(_is_valid_file_type)
#define _get_more_table_definitions HEADER(_get_more_table_definitions)
#define _add_table_definition HEADER(_add_table_definition)
#define _is_standard_table_defined HEADER(_is_standard_table_defined)
#define _read_and_define_tables_from_file HEADER(_read_and_define_tables_from_file)
#define _add_user_table HEADER(_add_user_table)
#define _get_user_defined_table_strings HEADER(_get_user_defined_table_strings)
#define my_read_tables HEADER(my_read_tables)
#define my_add_standard_table HEADER(my_add_standard_table)
#define my_read_number_tables HEADER(my_read_number_tables)
#define my_get_index HEADER(my_get_index)


ses_boolean _construct_standard_tables(void);
ses_boolean _destruct_standard_tables(void);

/*  GETTERS */


long       _get_standard_num_arrays(ses_table_id the_tid);
long*      _get_standard_relative_addresses(ses_table_id the_tid, long nr, long nt, long ntab);
long*      _get_standard_sizes(ses_table_id the_tid, long nr, long nt, long ntab);
char**     _get_standard_sizes_as_chars(ses_table_id the_tid, long nr, long nt, long ntab);
ses_label* _get_standard_labels(ses_table_id the_tid);

ses_label  _get_standard_label_at_index(ses_table_id the_tid, int index);


ses_boolean _is_valid_mid(ses_material_id the_mid);
ses_boolean _is_valid_tid(ses_table_id the_tid);
ses_boolean _is_valid_open_flag(ses_open_type open_flags);
ses_boolean _is_valid_file_type(ses_file_type the_type);


ses_boolean _get_more_table_definitions(int index);
ses_boolean _add_table_definition(ses_string the_data);
ses_boolean _is_standard_table_defined(ses_table_id the_tid);

ses_boolean _read_and_define_tables_from_file(void);


/*  FOR USER DEFINED TABLES */

ses_error_flag _add_user_table(ses_table_id the_tid);
ses_string* _get_user_defined_table_strings(int* number_strings);

/*  INTERNAL FUNCTIONS */

ses_boolean my_read_tables(void);
ses_error_flag  my_add_standard_table(struct _standard_table* the_table);
int my_read_number_tables(void);
int my_get_index(ses_table_id the_tid);




