/*  _ses_index_record.c */

#define _construct_ses_index_record HEADER(_construct_ses_index_record)
#define _construct_ses_index_record_for_write HEADER(_construct_ses_index_record_for_write)
#define _copy_index_record HEADER(_copy_index_record)
#define _destruct_ses_index_record HEADER(_destruct_ses_index_record)
#define _isit_ready_index_record HEADER(_isit_ready_index_record)
#define _get_tblid HEADER(_get_tblid)
#define _get_nwds_index_record HEADER(_get_nwds_index_record)
#define _get_iadr_index_record HEADER(_get_iadr_index_record)
#define _get_address_for_table HEADER(_get_address_for_table)
#define _get_table_size HEADER(_get_table_size)
#define _table_exists HEADER(_table_exists)
#define _get_table_index HEADER(_get_table_index)
#define _add_table_to_index_record HEADER(_add_table_to_index_record)
#define _remove_table_from_index_record HEADER(_remove_table_from_index_record)
#define _add_tables HEADER(_add_tables)
#define _add_100_table_to_index_record HEADER(_add_100_table_to_index_record)
#define _read_index_record HEADER(_read_index_record)
#define _write_index_record HEADER(_write_index_record)
#define _ses_print_index_record HEADER(_print_index_record)


/*  CONSTRUCTORS AND DESTRUCTORS */

struct _ses_index_record* _construct_ses_index_record(void);
struct _ses_index_record* _construct_ses_index_record_for_write(long date1, long date2, long vers);
struct _ses_index_record* _copy_index_record(struct _ses_index_record* the_ir);

ses_boolean _destruct_ses_index_record(struct _ses_index_record* the_index_record);

/*  GETTERS */

ses_boolean _isit_ready_index_record(struct _ses_index_record* the_index_record);

long* _get_tblid(struct _ses_index_record* the_index_record);
long* _get_nwds_index_record(struct _ses_index_record* the_index_record);
long* _get_iadr_index_record(struct _ses_index_record* the_index_record);

long _get_address_for_table(struct _ses_index_record* ptIR, ses_table_id the_tid, struct _ses_file_handle* pSFH);

long _get_table_size(struct _ses_index_record* ptIR, ses_table_id the_tid);

ses_boolean _table_exists(struct _ses_index_record* ptIR, ses_table_id the_tid);

int _get_table_index(struct _ses_index_record* ptIR, ses_table_id the_tid);
long _get_table_nwds(struct _ses_index_record* ptIR, ses_table_id the_tid);

/*  SETTERS */

ses_boolean _add_table_to_index_record(struct _ses_index_record* ptIR,
   ses_table_id the_tid, long size);

ses_boolean _remove_table_from_index_record(struct _ses_index_record* ptIR, struct _ses_data_record* ptDR, 
					    ses_table_id the_tid);

ses_boolean _add_tables(struct _ses_index_record* the_directory, ses_table_id* _tblid,
			long* _nwds, long* _iadr, long _nrec);
ses_boolean _add_100_table_to_index_record(struct _ses_index_record* ptIR, ses_table_id the_table_id, int size_table);
/*  READERS AND WRITERS */

struct _ses_index_record* _read_index_record(struct _ses_file_handle* pSFH, long offset);
ses_boolean _write_index_record(struct _ses_index_record* ptIR, struct _ses_file_handle* pSFH);

ses_string _ses_print_index_record(struct _ses_index_record* pIR, ses_table_id tid);


