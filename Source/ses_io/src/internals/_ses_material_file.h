/*  _ses_material_file.c */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_ses_material_file HEADER(_construct_ses_material_file)
#define _copy_ses_material_file HEADER(_copy_ses_material_file)
#define _destruct_material_file HEADER(_destruct_material_file)
#define _destruct_and_detach_material_file HEADER(_destruct_and_detach_material_file)
#define _table_exists_in_material_file HEADER(_table_exists_in_material_file)
#define _add_table HEADER(_add_table)
#define _combine_two_material_files HEADER(_combine_two_material_files)
#define _add_100_table HEADER(_add_100_table)
#define _read_material_file HEADER(_read_material_file)
#define _write_material_file HEADER(_write_material_file)

struct _ses_material_file*  _construct_ses_material_file(
                                    ses_material_id the_mid);
struct _ses_material_file* _copy_ses_material_file(struct _ses_material_file* the_mf);

ses_boolean                 _destruct_material_file(
                                    struct _ses_material_file* the_file);
ses_boolean                 _destruct_and_detach_material_file(struct _ses_material_file* the_file);

/*  GETTERS */

ses_boolean _table_exists_in_material_file(struct _ses_material_file* this, ses_table_id the_tid);

/*  SETTERS */

ses_boolean _add_table(struct _ses_material_file* the_mf, 
                       struct _ses_data_record* the_data_record);
ses_boolean _combine_two_material_files(struct _ses_material_file* this, 
                                        struct _ses_material_file* the_material_file);
ses_boolean _add_100_table(struct _ses_material_file* pMF, ses_string* table_strings, int num_strings);

/*  READERS */

struct _ses_material_file* _read_material_file(ses_file_handle the_handle, unsigned int nsig, ses_boolean do_validation, long maddress);

/*  WRITERS */

ses_boolean  _write_material_file(struct _ses_material_file* the_file, 
                                  struct _ses_file_handle* pSFH, unsigned int nsig, 
                                  ses_boolean do_valid);





