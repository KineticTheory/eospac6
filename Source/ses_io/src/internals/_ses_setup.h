/*  _ses_setup_functions.c */

/*  CONSTRUCTORS AND DESTRUCTORS */

#define _construct_ses_setup HEADER(_construct_ses_setup)
#define _copy_ses_setup HEADER(_copy_ses_setup)
#define _destruct_ses_setup HEADER(_destruct_ses_setup)
#define _isit_setup HEADER(_isit_setup)
#define _get_do_validation HEADER(_get_do_validation)
#define _get_significant_digits HEADER(_get_significant_digits)
#define _init_ses_setup HEADER(_init_ses_setup)
#define _set_do_validation HEADER(_set_do_validation)
#define _set_significant_digits HEADER(_set_significant_digits)
#define _set_array_order HEADER(_set_array_order)
#define _set_material_order HEADER(_set_material_order)

struct _ses_setup* _construct_ses_setup(void);
struct _ses_setup* _copy_ses_setup(struct _ses_setup* pSET);

ses_boolean        _destruct_ses_setup(struct _ses_setup* the_setup);

/*  GETTERS */

ses_boolean        _isit_setup(struct _ses_setup* the_setup);

ses_boolean        _get_do_validation(struct _ses_setup* the_setup);
ses_number         _get_significant_digits(struct _ses_setup* the_setup);


/*  SETTERS */

ses_boolean        _init_ses_setup(struct _ses_setup* the_setup, ses_material_id the_mid, ses_table_id the_tid, long date, long version);
 
ses_boolean        _set_do_validation(struct _ses_setup* the_setup);
ses_boolean        _set_significant_digits(struct _ses_setup* the_setup, ses_number digits);
ses_boolean        _set_array_order(struct _ses_setup* the_setup, ses_array_order the_order);
ses_boolean        _set_material_order(struct _ses_setup* the_setup);




