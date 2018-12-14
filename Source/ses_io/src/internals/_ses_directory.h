/*  _ses_directory.c */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_ses_directory HEADER(_construct_ses_directory)
#define _construct_ses_directory_from_material_file_list HEADER(_construct_ses_directory_from_material_file_list)
#define _copy_ses_directory HEADER(_copy_ses_directory)
#define _destruct_ses_directory HEADER(_destruct_ses_directory)
#define _isit_ready_directory HEADER(_isit_ready_directory)
#define _get_matid HEADER(_get_matid)
#define _get_nwds_directory HEADER(_get_nwds_directory)
#define _get_iadr_directory HEADER(_get_iadr_directory)
#define _check_directory_for_material HEADER(_check_directory_for_material)
#define _add_materials HEADER(_add_materials)
#define _add_new_material HEADER(_add_new_material)
#define _write_directory HEADER(_write_directory)
#define _read_directory HEADER(_read_directory)
#define _get_material_index HEADER(_get_material_index)

struct _ses_directory* _construct_ses_directory(void);


struct _ses_directory* _construct_ses_directory_from_material_file_list(
                                      struct _ses_material_file_list* pMFL, struct _ses_file_handle* pSFH);
struct _ses_directory* _copy_ses_directory(struct _ses_directory* pDIR);

ses_boolean _destruct_ses_directory(struct _ses_directory* the_directory);

/*  GETTERS */

ses_boolean _isit_ready_directory(struct _ses_directory* the_directory);
long* _get_matid(struct _ses_directory* the_directory);
long* _get_nwds_directory(struct _ses_directory* the_directory);
long* _get_iadr_directory(struct _ses_directory* the_directory);

ses_boolean _check_directory_for_material(struct _ses_directory* ptDIR, ses_material_id the_mid);


/*  SETTERS */

ses_boolean _add_materials(struct _ses_directory* the_directory, long* _matid,
                           long* _nwds, long* _iadr, long _nfiles);
ses_boolean _add_new_material(struct _ses_directory* the_directory, ses_material_id the_mid);


/*  READ AND WRITE */

ses_boolean _write_directory(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
struct _ses_directory*  _read_directory(struct _ses_file_handle* pSFH);


int _get_material_index(struct _ses_directory* the_directory, ses_material_id the_mid);

