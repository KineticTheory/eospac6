/* _ses_file.c */

#include <stdio.h>

#define _construct_ses_file HEADER(_construct_ses_file)
#define _construct_static_ses_file HEADER(_construct_static_ses_file)
#define _construct_dynamic_ses_file HEADER(_construct_dynamic_ses_file)
#define _copy_ses_file HEADER(_copy_ses_file)
#define _copy_ses_file2 HEADER(_copy_ses_file2)
#define _destruct_ses_file HEADER(_destruct_ses_file)
#define _get_directory HEADER(_get_directory)
#define _get_current_index_record HEADER(_get_current_index_record)
#define _get_current_data_record HEADER(_get_current_data_record)


/*  CONSTRUCTORS AND DESTRUCTORS */

struct _ses_file*         _construct_ses_file(FILE* pFILE, 
                                              ses_open_type open_flags, 
                                              ses_file_type the_type, 
                                              ses_boolean isit_little,
                                              ses_string filename);
struct _ses_file          _construct_static_ses_file(FILE* pFILE, 
                                              ses_open_type open_flags, 
                                              ses_file_type the_type, 
                                              ses_boolean isit_little,
                                              ses_string filename);
struct _ses_file*          _construct_dynamic_ses_file(FILE* pFILE, 
                                              ses_open_type open_flags, 
                                              ses_file_type the_type, 
                                              ses_boolean isit_little,
                                              ses_string filename);
struct _ses_file*         _copy_ses_file(struct _ses_file* pSF);

struct _ses_file          _copy_ses_file2(struct _ses_file pSF);


ses_boolean               _destruct_ses_file(struct _ses_file* the_library);

/*  GETTERS */

struct _ses_directory*    _get_directory(struct _ses_file* the_library);
struct _ses_index_record* _get_current_index_record(struct _ses_file* the_library);
struct _ses_data_record*  _get_current_data_record(struct _ses_file* the_library);
