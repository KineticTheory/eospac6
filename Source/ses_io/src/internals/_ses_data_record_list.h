
/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_data_record_list HEADER(_construct_data_record_list)
#define _copy_data_record_list HEADER(_copy_data_record_list)
#define _destruct_data_record_list HEADER(_destruct_data_record_list)
#define _get_total_table_size HEADER(_get_total_table_size)
#define _get_number_on_list HEADER(_get_number_on_list)
#define _get_tables_on_list HEADER(_get_tables_on_list)
#define _get_nwds_on_list HEADER(_get_nwds_on_list)
#define _get_iadr_on_list HEADER(_get_iadr_on_list)
#define _add_to_list_dr HEADER(_add_to_list_dr)
#define _add_100_table_to_data_record_list HEADER(_add_100_table_to_data_record_list)
#define _write_data_record_list HEADER(_write_data_record_list)


struct _ses_data_record_list*  _construct_data_record_list(void);
struct _ses_data_record_list*  _copy_data_record_list(struct _ses_data_record_list*);

ses_boolean _destruct_data_record_list(struct _ses_data_record_list* the_list);

/*  GETTERS */

long _get_total_table_size(struct _ses_data_record_list* the_list);
long _get_number_on_list(struct _ses_data_record_list* the_list);
ses_table_id* _get_tables_on_list(struct _ses_data_record_list* the_list);
long* _get_nwds_on_list(struct _ses_data_record_list* the_list);
long* _get_iadr_on_list(struct _ses_data_record_list* the_list, long start);

/* SETTERS */

ses_boolean _add_to_list_dr(struct _ses_data_record_list* the_list, struct _ses_data_record* the_mf);
ses_boolean _add_100_table_to_data_record_list(struct _ses_data_record_list* the_list, ses_material_id the_mid, ses_string* table_strings, int num_strings, int num_words);

/*  WRITE */

ses_boolean _write_data_record_list(struct _ses_data_record_list* ptLIST, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);


