/* _ses_file_handle.c */

#define _construct_ses_file_handle HEADER(_construct_ses_file_handle)
#define _copy_ses_file_handle HEADER(_copy_ses_file_handle)
#define _destruct_ses_file_handle HEADER(_destruct_ses_file_handle)
#define _get_c_file_handle HEADER(_get_c_file_handle)
#define _get_open_mode HEADER(_get_open_mode)
#define _get_filename HEADER(_get_filename)
#define _set_format_type HEADER(_set_format_type)
#define _getPFILE HEADER(_getPFILE)
#define _releasePFILE HEADER(_releasePFILE)

/*  CONSTRUCTORS AND DESTRUCTORS */
struct _ses_file_handle*  _construct_ses_file_handle(FILE* the_c_file_handle, 
                                                     ses_open_type the_open_mode, 
                                                     ses_file_type the_type, 
                                                     ses_boolean is_little, 
                                                     ses_string filename);
struct _ses_file_handle*  _copy_ses_file_handle(struct _ses_file_handle* pSFH);

ses_boolean    _destruct_ses_file_handle(struct _ses_file_handle* the_handle);

/*  GETTERS */

FILE*          _get_c_file_handle(struct _ses_file_handle* the_handle);
ses_open_type  _get_open_mode(struct _ses_file_handle* the_handle);
ses_string     _get_filename(struct _ses_file_handle* the_handle);


void _set_format_type(struct _ses_file_handle* the_handle, ses_file_type the_type);

FILE* _getPFILE(struct _ses_file_handle* the_handle);


void _releasePFILE(struct _ses_file_handle* the_handle);




