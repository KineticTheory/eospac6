/*  _globals   */

#define _construct_globals HEADER(_construct_globals)
#define _set_latest_error HEADER(_set_latest_error)
#define _globals_contains HEADER(_globals_contains)
#define _get_handle_from_globals HEADER(_get_handle_from_globals)
#define _get_next_empty_slot_from_globals HEADER(_get_next_empty_slot_from_globals)
#define _destruct_globals HEADER(_destruct_globals)
#define _destruct_file_list HEADER(_destruct_file_list)
#define _set_current_format HEADER(_set_current_format)

ses_boolean      _construct_globals(ses_open_type open_flags);
int              _set_latest_error(ses_error_flag the_flag);

ses_boolean      _globals_contains(ses_string filename);
ses_file_handle  _get_handle_from_globals(ses_string filename);
int              _get_next_empty_slot_from_globals(void);

ses_boolean      _destruct_globals(void);
ses_boolean      _destruct_file_list(void);

ses_file_type    _get_current_format(void);
void             _set_current_format(ses_file_type the_type);


