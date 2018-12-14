  
#define _construct_material_file_list HEADER(_construct_material_file_list)
#define _copy_material_file_list HEADER(_copy_material_file_list)
#define _destruct_material_file_list HEADER(_destruct_material_file_list)
#define _does_material_exist_in_list HEADER(_does_material_exist_in_list)
#define _get_materials_from_list HEADER(_get_materials_from_list)
#define _get_nwds_from_list HEADER(_get_nwds_from_list)
#define _get_iadr_from_list HEADER(_get_iadr_from_list)
#define _get_nfiles_from_list HEADER(_get_nfiles_from_list)
#define _add_to_list HEADER(_add_to_list)
#define _combine_in_list HEADER(_combine_in_list)
#define _add_100_table_to_list HEADER(_add_100_table_to_list)
#define _create100 HEADER(_create100)
#define _sort_material_file_list HEADER(_sort_material_file_list)
#define _read_material_file_list HEADER(_read_material_file_list)
#define _write_material_file_list HEADER(_write_material_file_list)

/*  CONSTRUCTORS AND DESTRUCTORS */

struct _ses_material_file_list* _construct_material_file_list(void);
struct _ses_material_file_list* _copy_material_file_list(struct _ses_material_file_list* the_list);

ses_boolean _destruct_material_file_list(struct _ses_material_file_list* the_list);

/*  GETTERS */

ses_boolean _does_material_exist_in_list(struct _ses_material_file_list* the_list, ses_material_id the_mid);

long* _get_materials_from_list(struct _ses_material_file_list* ptMF);
long* _get_nwds_from_list(struct _ses_material_file_list* ptMF);
long* _get_iadr_from_list(struct _ses_material_file_list* ptMF, long start);

long  _get_nfiles_from_list(struct _ses_material_file_list* ptMF);

/*  SETTERS */

ses_boolean _add_to_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_mf);
ses_boolean _combine_in_list(struct _ses_material_file_list* the_list, struct _ses_material_file* the_material_file);
ses_boolean _add_100_table_to_list(struct _ses_material_file_list* the_list, ses_string* table_strings, int num_strings);
ses_boolean _create100(struct _ses_material_file_list* ptMFL);

/*  SORTING */
  
ses_boolean _sort_material_file_list(struct _ses_material_file_list* the_list);

/*  READERS */

struct _ses_material_file_list* _read_material_file_list(ses_file_handle the_handle, unsigned int nsig, ses_boolean do_validation, long* iadr);

/*  WRITERS */

ses_boolean  _write_material_file_list(struct _ses_material_file_list* ptMFL,
                                       struct _ses_file_handle* pSFH, 
                                       unsigned int nsig, ses_boolean do_validation);
