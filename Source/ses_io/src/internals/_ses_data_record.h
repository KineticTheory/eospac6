/*  _ses_data_record.c */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_ses_data_record HEADER(_construct_ses_data_record)
#define _copy_ses_data_record HEADER(_copy_ses_data_record)
#define _destruct_ses_data_record HEADER(_destruct_ses_data_record)
#define _get_iterator HEADER(_get_iterator)
#define _get_all_arrays_size HEADER(_get_all_arrays_size)
#define _get_data_dr HEADER(_get_data_dr)
#define _write_data_record HEADER(_write_data_record)
#define _print_index_record HEADER(_print_index_record)

struct _ses_data_record*   _construct_ses_data_record(long maddress, long nr, long nt, long ntab,  
                                          ses_material_id the_mid, ses_table_id the_tid, long date1, 
					  long date2, long vers);
struct _ses_data_record*   _copy_ses_data_record(struct _ses_data_record* the_dr);

ses_boolean                _destruct_ses_data_record(struct _ses_data_record* the_data_record);

/*  GETTERS */

struct _ses_iterator*      _get_iterator(struct _ses_data_record* the_data_record);
long                       _get_all_arrays_size(struct _ses_data_record* the_record);
ses_word_reference         _get_data_dr(struct _ses_data_record* pDR, ses_label the_label);

/*  WRITERS */

ses_boolean _write_data_record(struct _ses_data_record* pDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);


