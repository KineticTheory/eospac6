 
#include "stdio.h"

#include "ses_defines.h"

#ifndef _SES_GLOBALS_H
#define _SES_GLOBALS_H

struct _ses_directory {

  long  _nfiles;
  long  _date;
  long  _version;
  long* _matid;
  long* _nwds;
  long* _iadr;
  ses_boolean _ready;

  char**                 _material_path;
  ses_boolean            _has_multiple_files;

};

struct _ses_index_record {

  ses_material_id            _mid;
  long                       _date1;
  long                       _date2;
  long                       _vers;
  long                       _nrec;
  ses_table_id*              _tblid;
  long*                      _nwds;
  long*                      _iadr;
  ses_boolean                _ready;

  ses_number*                 _nr;
  ses_number*                 _nt;
  char**                      _function_filename;
  char**                      _type;
  int                         _nfuncs;

  char***                     _array_filename;
  long*                       _narrays;
  long**                      _array_iadr;                

  
};

struct _ses_data_record {

  ses_material_id            _mid;
  ses_table_id               _tid;
  struct _ses_iterator*      _the_iterator;
  ses_word_reference*        _the_data;
  struct _ses_data_record*   _next;
  ses_boolean                _has_data;
  long                       _date1;
  long                       _date2;
  long                       _vers;
  int                        _first;
};


struct _ses_file_handle {

  FILE*                      _c_file_handle;
  ses_boolean                _is_valid;
  int                        _current_location;
  ses_open_type              _the_open_mode;
  ses_string                 _filename;
  char                       _filetype;
  ses_boolean                _is_little_endian;
  ses_boolean                _machine_is_little_endian;
  ses_boolean                _needs_flip;
  ses_boolean                _is_a_copy;
  int                        _start_index;
  int                        _word_size;
  char*                      _material_filename;
  char*                      _array_filename;
  int                        _array_address;
  int                        _iteration_index;


  char (*pt2_read_char)(FILE* pFILE );
  ses_error_flag (*pt2_read_directory)(struct _ses_directory*, struct _ses_file_handle*);
  ses_error_flag (*pt2_read_index_record)(struct _ses_index_record*, struct _ses_file_handle*, long offset);
  ses_error_flag (*pt2_read_array)(struct _ses_file_handle*, ses_word_reference, long size, unsigned int nsig, ses_boolean do_valid );
  ses_error_flag (*pt2_read_data_record)(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
 
  ses_error_flag (*pt2_go_to_index_record)(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
  ses_error_flag (*pt2_go_to_data_record)(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);

  ses_boolean    (*pt2_get_grid)(ses_file_handle the_handle, ses_material_id the_mid, 
			      ses_table_id the_tid, long* nr, long* nt, long* ntab);
 
  ses_error_flag (*pt2_write_directory)(struct _ses_directory*, struct _ses_file_handle*);
  ses_error_flag (*pt2_write_index_record)(struct _ses_index_record*, struct _ses_file_handle*);
  ses_error_flag (*pt2_write_array)(struct _ses_file_handle*, ses_word_reference, long size, unsigned int nsig, ses_boolean do_valid );
  ses_error_flag (*pt2_write_data_record)(struct _ses_data_record* ptDR, struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_valid);
  
  long            (*pt2_read_long)(struct _ses_file_handle* pSFH);
  ses_boolean     (*pt2_go_to_next_array_location)(struct _ses_file_handle* pSFH, long location);
  ses_boolean     (*pt2_write_long)(struct _ses_file_handle* pSFH, long the_long);
  double          (*pt2_read_double)(struct _ses_file_handle* pSFH, unsigned int nsig, ses_boolean do_validation);
  ses_boolean     (*pt2_write_double)(struct _ses_file_handle* pSFH, double the_double, unsigned int nsig, ses_boolean do_validation);

ses_word_reference  (*pt2_read_ses_word_array)(struct _ses_file_handle* pSFH, long size_array, unsigned int nsig, ses_boolean do_validation);
ses_boolean         (*pt2_write_ses_word_array)(struct _ses_file_handle* pSFH, ses_word the_array[], long size_array, unsigned int nsig, ses_boolean do_validation);


  long (*pt2_get_directory_size)(struct _ses_directory* ptDIR, struct _ses_file_handle* pSFH);
  long (*pt2_get_address_for_material)(struct _ses_directory* ptDIR, ses_material_id mid, struct _ses_file_handle* pSFH);
  long (*pt2_get_address_for_table)(struct _ses_index_record* ptIR, ses_table_id tid, struct _ses_file_handle* pSFH);
  
  

};





struct _ses_setup {

  ses_material_id            _mid;
  ses_table_id               _tid;
  ses_number                 _significant_digits;
  ses_boolean                _do_validation;

  ses_array_order            _array_order;
  ses_boolean                _order_materials;

  ses_boolean                _setup_complete;

  ses_number                 _nr;
  ses_number                 _nt;
  ses_number                 _ntab; /* number of dependent variable arrays */

  ses_boolean                _is_user_defined_table;

  ses_boolean                _date_changed;
  long                       _date;

  ses_boolean                _version_changed;
  long                       _version;

  long                       _date1;
  long                       _date2;
  long                       _vers;

};

struct _ses_iterator {

  long                       _current_array;
  long                       _number_arrays;
  long*                      _address_arrays;
  long*                      _size_arrays;
  ses_label*                 _label_arrays;
  long                       _maddress;
  long                       _size;
  long                       _nr;
  long                       _nt;
  long                       _ntab;
  ses_table_id               _tid;
   
};



struct _ses_data_record_list {
  struct _ses_data_record* _head;
};



struct _ses_material_file {

  ses_material_id                   _the_mid;
  struct _ses_index_record*         _the_index_record;
  long                              _number_tables;
  struct _ses_data_record_list*     _the_tables2;
  struct _ses_material_file*        _next;
};


struct _ses_file {
  struct _ses_file_handle*    _the_handle;
  struct _ses_setup*          _the_setup;

  struct _ses_directory*      _directory;

  struct _ses_index_record*   _current_index_record;
  struct _ses_data_record*    _current_data_record;

  struct _ses_output_file*    FILE_TO_WRITE;
  ses_boolean                _first_time_through_setup;

  long           _output_date;
  long           _output_version;

  ses_boolean                _is_constructed;

};



struct _ses_material_file_list {
  struct _ses_material_file* _head;
};

struct _ses_output_file {
  ses_boolean                     _ready_to_write;
  struct _ses_directory*          _directory_to_write;
  struct _ses_material_file_list* _material_files_to_write;
  char 				  _filetype;
  ses_boolean                     _seen_100;
};


struct _standard_table {

	  ses_table_id     _the_tid;
	  long            _num_arrays;
          long            _num_independent; // number of independent variable arrays 
	  ses_label        _description;
          ses_boolean     _is_user_defined;
	  ses_string*     _size_arrays;
          long            _nr; // only used for user-defined tables 
          long            _nt; // only used for user-defined tables 
	  ses_string*     _label;

	  struct          json_node* _the_parse_node;


};

#define _registered_formats HEADER(_registered_formats)
#define _current_format HEADER(_current_format)
#define _default_format HEADER(_default_format)
#define _number_registered_formats HEADER(_number_registered_formats)
#define pt2_registered_formats HEADER(pt2_registered_formats)
#define pt2_isit_my_format HEADER(pt2_isit_my_format)

#define _the_tables HEADER(_the_tables)
#define NUMBER_TABLES HEADER(NUMBER_TABLES)
#define FILE_LIST HEADER(FILE_LIST)
#define _next_empty_file HEADER(_next_empty_file)
#define _latest_error HEADER(_latest_error)


#ifdef _SES_INTERNAL_

struct         _standard_table** _the_tables;
int            NUMBER_TABLES;
struct         _ses_file*       FILE_LIST[2000];
int            _next_empty_file;
ses_error_flag _latest_error;
ses_file_type*  _registered_formats;
ses_file_type   _current_format;
ses_file_type   _default_format;
int             _number_registered_formats;
ses_boolean     (**pt2_registered_functions)(FILE* pFILE);
ses_boolean     (*pt2_isit_my_format)(FILE* pFILE);

#else  /* ! defined _SES_INTERNAL_ */

extern struct         _standard_table** _the_tables;
extern int                       NUMBER_TABLES;
extern struct _ses_file*          FILE_LIST[2000];
extern int                       _next_empty_file;
extern ses_error_flag            _latest_error;
extern ses_file_type*  _registered_formats;
extern ses_file_type   _current_format;
extern ses_file_type   _default_format;
extern int             _number_registered_formats;
extern ses_boolean     (**pt2_registered_functions)(FILE* pFILE);
extern ses_boolean     (*pt2_isit_my_format)(FILE* pFILE);


#endif /* ifdef _SES_INTERNAL_ */

#endif

