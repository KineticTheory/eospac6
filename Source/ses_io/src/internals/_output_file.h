/*  _output_file.c */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_output_file HEADER(_construct_output_file)
#define _copy_output_file HEADER(_copy_output_file)
#define _combine_into_one HEADER(_combine_into_one)
#define _destruct_output_file HEADER(_destruct_output_file)
#define _destruct_and_detach_output_file HEADER(_destruct_and_detach_output_file)
#define _get_size_of_directory HEADER(_get_size_of_directory)
#define _isit_ready_output_file HEADER(_isit_ready_output_file)
#define _does_material_exist_in_output_file HEADER(_does_material_exist_in_output_file)
#define _set_directory HEADER(_set_directory)
#define _add_material_file HEADER(_add_material_file)
#define _set_ready_to_write HEADER(_set_ready_to_write)
#define _read_into_output_file HEADER(_read_into_output_file)
#define _ses_write HEADER(_ses_write)

struct _ses_output_file*  _construct_output_file(char _output_file_type);
struct _ses_output_file*  _copy_output_file(struct _ses_output_file* pOF);
struct _ses_output_file*  _combine_into_one(ses_file_handle handle1, 
                                            ses_file_handle handle2);

ses_boolean               _destruct_output_file(struct _ses_output_file* the_file);
ses_boolean               _destruct_and_detach_output_file(struct _ses_output_file* the_file);

/*  GETTERS */

long                      _get_size_of_directory(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH);
ses_boolean               _isit_ready_output_file(struct _ses_output_file* the_output_file);
ses_boolean               _does_material_exist_in_output_file(struct _ses_output_file* the_output_file,
                               ses_material_id the_mid);
/*  SETTERS */

ses_boolean               _set_directory(struct _ses_output_file* the_output_file,
                                         struct _ses_directory* the_directory);
ses_boolean               _add_material_file(struct _ses_output_file* the_output_file, 
                                             struct _ses_material_file* the_material_file);
ses_boolean               _set_ready_to_write(struct _ses_output_file* the_output_file);

/*  READ  */

struct _ses_output_file*  _read_into_output_file(ses_file_handle the_handle);

/*  WRITE */

ses_boolean               _ses_write(struct _ses_output_file* the_file, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation, ses_boolean material_sort, long the_date, long the_version);


