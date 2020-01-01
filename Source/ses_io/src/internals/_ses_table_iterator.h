

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_ses_iterator HEADER(_construct_ses_iterator)
#define _copy_ses_iterator HEADER(_copy_ses_iterator)
#define _destruct_ses_iterator HEADER(_destruct_ses_iterator)
#define _is_valid_iterator HEADER(_is_valid_iterator)
#define _get_number_arrays HEADER(_get_number_arrays)
#define _get_array_size HEADER(_get_array_size)
#define _get_array_sizes HEADER(_get_array_sizes)
#define _get_sum_arrays_size HEADER(_get_sum_arrays_size)
#define _get_maddress HEADER(_get_maddress)
#define _get_array_size_with_label HEADER(_get_array_size_with_label)
#define _get_current_label HEADER(_get_current_label)
#define _set_current_label HEADER(_set_current_label)
#define _initialize_ses_iterator HEADER(_initialize_ses_iterator)
#define _fill_iterator_size_and_label_arrays HEADER(_fill_iterator_size_and_label_arrays)

#define my_adjust_num_arrays HEADER(my_adjust_num_arrays)
#define my_adjust_size_and_address_arrays HEADER(my_adjust_size_and_address_arrays)

struct _ses_iterator* _construct_ses_iterator(long maddress, long nr, long nt, long ntab, ses_table_id the_tid);
struct _ses_iterator* _copy_ses_iterator(struct _ses_iterator* pIT);
ses_boolean           _destruct_ses_iterator(struct _ses_iterator* pIT);

/*  GETTERS */

ses_boolean           _is_valid_iterator(struct _ses_iterator* pIT);


long  _get_number_arrays(struct _ses_iterator* ptIT);
long  _get_array_size(struct _ses_iterator* ptIT, long index);
long* _get_array_sizes(struct _ses_iterator* ptIT);

long  _get_sum_arrays_size(struct _ses_iterator* pIT);

long  _get_maddress(struct _ses_iterator* pIT);
long  _get_array_size_with_label(struct _ses_iterator* pIT, ses_label the_label);


ses_label _get_current_label(struct _ses_iterator* ptIT);

/*  SETTERS */

ses_boolean _set_current_label(struct _ses_iterator* ptIT, ses_label the_label);
ses_boolean _initialize_ses_iterator(struct _ses_iterator* pIT, ses_file_handle the_handle,
                                               ses_table_id the_tid);
ses_boolean _fill_iterator_size_and_label_arrays(struct _ses_iterator* pIT, ses_table_id the_tid);


