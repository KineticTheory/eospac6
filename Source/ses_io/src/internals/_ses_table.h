

#define _construct_standard_table HEADER(_construct_standard_table)
#define _destruct_standard_table HEADER(_destruct_standard_table)
#define _copy_standard_table HEADER(_copy_standard_table)
#define _print_standard_table HEADER(_print_standard_table)
#define _get_standard_num_arrays_for_table HEADER(_get_standard_num_arrays_for_table)
#define _get_standard_relative_addresses_for_table HEADER(_get_standard_relative_addresses_for_table)
#define _get_standard_sizes_for_table HEADER(_get_standard_sizes_for_table)
#define _get_standard_sizes_for_table_as_char HEADER(_get_standard_sizes_for_table_as_char )
#define _get_standard_labels_for_table HEADER(_get_standard_labels_for_table)
#define my_construct_standard_table HEADER(my_construct_standard_table)

//#define CHANGE_TO_STANDARD
#ifdef CHANGE_TO_STANDARD
#define _get_standard_num_functions_for_table HEADER(_get_standard_num_functions_for_table)
#endif

struct       _standard_table* _construct_standard_table(ses_string the_string);
ses_boolean  _destruct_standard_table(struct _standard_table* the_table);
struct       _standard_table* _copy_standard_table(struct _standard_table* the_table);

void         _print_standard_table(struct _standard_table* the_table);

long        _get_standard_num_arrays_for_table(struct _standard_table* the_table);
#ifdef CHANGE_TO_STANDARD
long	    _get_standard_num_functions_for_table(struct _standard_table* the_table);
#endif
long*       _get_standard_relative_addresses_for_table(struct _standard_table* the_table, long nr, long nt, long ntab);
long*       _get_standard_sizes_for_table(struct _standard_table* the_table,  long nr, long nt, long ntab);
char**      _get_standard_sizes_for_table_as_char(struct _standard_table* the_table, long nr, long nt, long ntab);
ses_label*  _get_standard_labels_for_table(struct _standard_table* the_table);

/*  local functions */

struct       _standard_table*  my_construct_standard_table(void);


