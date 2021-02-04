#ifndef _SES_DEFINES_H
#define _SES_DEFINES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRY_OPTIONAL
#ifdef TRY_OPTIONAL
#include <stdarg.h>
#endif

//  In order to have a sesio code base inside eospac as well as a separate
//  sesio library, redefine the function names with a header so that symbol
//  table confusion can be avoided if by some off chance the user combines
//  the eospac library and the sesio library in one executable


#ifdef HEADER_EOS
#define HEADER(n) eos ## _ ## n
#else
#define HEADER(n) sesio ## _ ## n
#endif

//  user interface

#define ses_access_directory HEADER(ses_access_directory)
#define ses_access_table_index HEADER(ses_access_table_index)
#define ses_set_table_index HEADER(ses_set_table_index)
#define ses_array_size_next HEADER(ses_array_size_next)
#define ses_change_next HEADER(ses_change_next)
#define ses_delete_next HEADER(ses_delete_next)
#define ses_has_next HEADER(ses_has_next)
#define ses_read_next HEADER(ses_read_next)
#define ses_skip HEADER(ses_skip)
#define ses_write_next HEADER(ses_write_next)
#define ses_comments HEADER(ses_comments)
#define ses_get_comments HEADER(ses_get_comments)
#define ses_read_1D HEADER(ses_read_1D)
#define ses_read_2D HEADER(ses_read_2D)
#define ses_read_3D HEADER(ses_read_3D)
#define ses_read_named_array HEADER(ses_read_named_array)
#define ses_read_number HEADER(ses_read_number)
#define ses_read_pairs HEADER(ses_read_pairs)
#define ses_read_word HEADER(ses_read_word)
#define ses_write_1D HEADER(ses_write_1D)
#define ses_write_2D HEADER(ses_write_2D)
#define ses_write_3D HEADER(ses_write_3D)
#define ses_write_comments HEADER(ses_write_comments)
#define ses_write_next HEADER(ses_write_next)
#define ses_write_number HEADER(ses_write_number)
#define ses_write_pairs HEADER(ses_write_pairs)
#define ses_write_word HEADER(ses_write_word)
#define ses_close HEADER(ses_close)
#define ses_open HEADER(ses_open)
#define ses_setup HEADER(ses_setup)
#define ses_write_setup HEADER(ses_write_setup)
#define ses_combine HEADER(ses_combine)
#define ses_delete_table HEADER(ses_delete_table)
#define ses_define_table HEADER(ses_define_table)
#define ses_date HEADER(ses_date)
#define ses_format HEADER(ses_format)
#define ses_get_date HEADER(ses_get_date)
#define ses_get_grid HEADER(ses_get_grid)
#define ses_get_label HEADER(ses_get_label)
#define ses_get_ascii_word_type HEADER(ses_get_ascii_word_type)
#define ses_set_ascii_word_type HEADER(ses_set_ascii_word_type)
#define ses_get_materials HEADER(ses_get_materials)
#define ses_get_table_ids HEADER(ses_get_table_ids)
#define ses_get_table_sizes HEADER(ses_get_table_sizes)
#define ses_is_valid HEADER(ses_is_valid)
#define ses_version HEADER(ses_version)
#define ses_set_array_order HEADER(ses_set_array_order)
#define ses_set_date HEADER(ses_set_date)
#define ses_set_endianness HEADER(ses_set_endianness)
#define ses_set_format HEADER(ses_set_format)
#define ses_set_grid HEADER(ses_set_grid)
#define ses_set_label HEADER(ses_set_label)
#define ses_set_material_order HEADER(ses_set_material_order)
#define ses_set_significant_digits HEADER(ses_set_significant_digits)
#define ses_set_validate HEADER(ses_set_validate)
#define ses_set_version HEADER(ses_set_version)
#define ses_indicates_error HEADER(ses_indicates_error)
#define ses_print_error_condition HEADER(ses_print_error_condition)
#define ses_print_error_message HEADER(ses_print_error_message)
#define ses_exit HEADER(ses_exit)

//  user routines

#define print_array_from_table HEADER(print_array_from_table)
#define print_data_on_table HEADER(print_data_on_table)
#define get_data_on_table HEADER(get_data_on_table)
#define print_materials_on_file HEADER(print_materials_on_file)
#define print_table HEADER(print_table)
#define print_tables_for_material HEADER(print_tables_for_material)
#define print_index_record HEADER(print_index_record)

//  misc

#define ses_get_available_formats HEADER(ses_get_available_formats)

/*  sesame io library defines --
 */

typedef int          ses_file_handle;
typedef char*        ses_string;
typedef char         ses_open_type;
typedef int          ses_error_flag;
typedef long         ses_material_id;
typedef long*        ses_material_id_reference;
typedef long         ses_table_id;
typedef long*        ses_table_id_reference;
typedef char         ses_file_format_type;       // Default: Binary
typedef char         ses_ascii_word_type;        // M - medium (default), S - small
typedef int          ses_ascii_word_size;        // 22 - medium (default), 15 - small
typedef long*        ses_table_sizes_reference;
typedef char*        ses_label;
typedef long         ses_number;
typedef long*        ses_number_reference;
typedef int          ses_boolean;
typedef double       ses_word;
typedef double*      ses_word_reference;
typedef char         ses_file_type;
typedef int          ses_array_order;




enum {
    SES_NO_ERROR = 0,
    SES_ARRAY_SIZE_ERROR,
    SES_MEMORY_ALLOCATION_ERROR,
    SES_OBJECT_CONSTRUCTION_ERROR,
    SES_NULL_OBJECT_ERROR,
    SES_OBJECT_COPY_ERROR,
    SES_OBJECT_DESTRUCTION_ERROR,
    SES_FUNCTION_FAIL_ERROR,
    SES_INVALID_FILE_HANDLE,
    SES_INVALID_MID,
    SES_OBJECT_READY_ERROR,         // 10
    SES_OPEN_ERROR,
    SES_INVALID_TID,
    SES_OBJECT_OUT_OF_RANGE,
    SES_SETUP_ERROR,
    SES_CLOSE_ERROR,
    SES_FILE_READY_ERROR,
    SES_FILE_WRITE_ERROR,
    SES_READ_ERROR,
    SES_WRITE_ERROR,
    SES_CHANGE_ERROR,               // 20
    SES_COMBINE_ERROR,
    SES_COMMENTS_ERROR,
    SES_DELETE_ERROR,
    SES_NOT_IMPLEMENTED,
    SES_INVALID_OPEN_TYPE,
    SES_DOUBLE_SIZE_ERROR,
    SES_TEST_ERROR,
    SES_APPEND_ERROR,
    SES_NO_DATA_ERROR,
    SES_INVALID_FILE_FORMAT_TYPE,  // 30
    SES_INVALID_ASCII_WORD_TYPE,
    SES_INVALID_NUM_PARAMETERS,
    SES_INVALID_SESAME_ASCII
    
};


static const ses_file_handle SES_NULL_HANDLE = 0;

static const ses_number SES_NUMBER_ARRAY_SIZE_ERROR = 0;

static const ses_number SES_MAX_STRING_SIZE = 270;
static const ses_number SMALL_STRING_SIZE = 1024;
static const ses_number SES_MAX_LABEL_SIZE = 8*1024;

static const ses_label SES_NULL_LABEL = 0;

static const ses_string NULL_STRING = 0;

static const ses_boolean SES_FALSE = 0;
static const ses_boolean SES_TRUE = 1;

static const ses_array_order SES_COLUMN_MAJOR = 0;
static const ses_array_order SES_ROW_MAJOR = 1;

static const ses_table_id_reference SES_NULL_TABLES = 0;
static const ses_material_id_reference SES_NULL_MATERIALS = 0;

static const ses_file_format_type BINARY_TYPE  = 'B'; // LANL sesameBinary
static const ses_file_format_type ASCII_TYPE   = 'A'; // LANL sesame Ascii
static const ses_file_format_type XML_TYPE     = 'X'; // LANL sesame XML
static const ses_file_format_type LLNL_TYPE    = 'L'; // LLNL sesame XML
static const ses_file_format_type QUERY_4_TYPE = 'Q'; // Figuring out the type
static const ses_file_format_type UNKNOWN_TYPE = 'N'; // Unknown

static const ses_ascii_word_type ASCII_MEDIUM_WORD_TYPE   = 'M'; // ASCII Default
static const ses_ascii_word_type ASCII_SMALL_WORD_TYPE    = 'S';
static const ses_ascii_word_type ASCII_UNKNOWN_WORD_TYPE  = 'U';

static const ses_ascii_word_size ASCII_MEDIUM_WORD_SIZE   = 22;
static const ses_ascii_word_size ASCII_SMALL_WORD_SIZE    = 15;
static const ses_ascii_word_size ASCII_UNSUPPORTED_WORD_SIZE  = 0;



/*  function definitions for the user interface */
#ifdef __cplusplus
extern "C"
{
#endif
    
    /*  functions for direct access to the directory and table indices */
    
    long                      ses_access_directory (ses_file_handle the_handle,
                                                    ses_material_id_reference* return_matid,
                                                    long** nwds,
                                                    long** iadr,
                                                    long* date, long* version);
    long                      ses_access_table_index(ses_file_handle the_handle,
                                                     ses_table_id_reference* return_tblid, long** nwds, long** iadr,
                                                     long* date1, long* date2, long* version);
    ses_boolean 		  ses_set_table_index(ses_file_handle the_handle,
                                              long date1, long date2, long version);
    
    
    /*  functions for the iterator interface  */
    
    ses_number                ses_array_size_next(ses_file_handle the_handle);
    ses_error_flag            ses_change_next(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim);
    ses_error_flag            ses_delete_next(ses_file_handle the_handle);
    ses_boolean               ses_has_next(ses_file_handle the_handle);
    ses_word_reference        ses_read_next(ses_file_handle the_handle);
    ses_error_flag            ses_skip(ses_file_handle the_handle);
    ses_error_flag            ses_write_next(ses_file_handle the_handle, ses_word_reference the_buffer,
                                             ses_number dim, ses_label the_label);
    
    /*  functions for the array iterface */
    
    
    /*  read array interface */
    ses_error_flag            ses_comments(ses_file_handle the_handle, ses_string* the_string);
    ses_error_flag            ses_get_comments(ses_file_handle the_handle, ses_string* the_string);
    ses_error_flag            ses_read_1D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim);
    ses_error_flag            ses_read_2D(ses_file_handle the_handle, ses_word_reference  the_buffer,
                                          ses_number dim1, ses_number dim2);
    ses_error_flag            ses_read_3D(ses_file_handle the_handle, ses_word_reference the_buffer,
                                          ses_number dim1, ses_number dim2, ses_number dim3);
    ses_error_flag            ses_read_named_array(ses_file_handle the_handle, ses_label the_label, ses_word_reference buf1);
    ses_error_flag            ses_read_number(ses_file_handle the_handle, ses_number_reference the_buffer);
    ses_error_flag            ses_read_pairs(ses_file_handle the_handle, ses_word_reference buf1,
                                             ses_word_reference buf2, ses_number dim);
    ses_error_flag            ses_read_word(ses_file_handle the_handle, ses_word_reference the_buffer);
    
    
    /*  write array interface */
    ses_error_flag            ses_write_1D(ses_file_handle the_handle, ses_word_reference the_buffer, ses_number dim);
    ses_error_flag            ses_write_2D(ses_file_handle the_handle, ses_word_reference the_buffer,
                                           ses_number dim1, ses_number dim2);
    ses_error_flag            ses_write_3D(ses_file_handle the_handle, ses_word_reference the_buffer,
                                           ses_number dim1, ses_number dim2, ses_number dim3);
    ses_error_flag            ses_write_comments(ses_file_handle the_handle, ses_string the_comments, ses_number dim);
    ses_error_flag            ses_write_next(ses_file_handle the_handle, ses_word_reference the_buffer,
                                             ses_number dim, ses_label the_label);
    ses_error_flag            ses_write_number(ses_file_handle the_handle, ses_number the_buffer);
    ses_error_flag            ses_write_pairs(ses_file_handle the_handle, ses_word_reference buf1,
                                              ses_word_reference buf2, ses_number dim);
    ses_error_flag            ses_write_word(ses_file_handle the_handle, ses_word the_buffer);
    
    /*  open, setup, and close, and exit */
    
    ses_error_flag            ses_close(ses_file_handle the_handle);
    ses_boolean               ses_exit(void);
    ses_file_handle           ses_open(ses_string filename, ses_open_type open_flags);
    ses_error_flag            ses_setup(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid);
    
    ses_error_flag            ses_write_setup(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, ses_number nr, ses_number nt, ses_number ntab);
    
    /*  file management routines */
    
    ses_error_flag            ses_combine(ses_file_handle  file1, ses_file_handle file2, ses_string new_filename);
    ses_error_flag            ses_delete_table(ses_file_handle the_handle);
    
    /*  user defined table routines */
    
    ses_error_flag            ses_define_table(ses_table_id tid, ses_label description, long nr, long nt, long num_independent,
                                               long num_arrays, char** size_arrays, ses_label* labels);
    
    /*  getter routines */
    
    ses_string                ses_date(ses_file_handle the_handle);
    ses_string                ses_format(ses_file_handle the_handle);
    long                      ses_get_date(ses_file_handle the_handle);
    ses_boolean               ses_get_grid(ses_file_handle the_handle, ses_material_id the_mid, ses_table_id the_tid, long* nr, long* nt, long* ntab);
    
    ses_label                 ses_get_label(ses_file_handle the_handle);
    
    /*ses_material_id           ses_get_material_id(ses_string material_name);*/  /*removed from user interface 4/2/2013 */
    
    ses_material_id_reference ses_get_materials(ses_file_handle the_handle, long* size);
    ses_table_id_reference    ses_get_table_ids(ses_file_handle the_handle, ses_material_id mid, long* size);
    ses_table_sizes_reference ses_get_table_sizes(ses_file_handle the_handle, ses_material_id mid, long* size);
    ses_boolean               ses_is_valid (ses_file_handle the_handle);
    ses_string                ses_version(ses_file_handle the_handle);
    long                      ses_get_version(ses_file_handle the_handle);
    ses_ascii_word_type       ses_get_ascii_word_type(ses_file_handle the_handle);
    ses_boolean               ses_set_ascii_word_type(ses_file_handle the_handle, ses_ascii_word_type);
    /*  setter routines */
    
    ses_error_flag            ses_set_array_order(ses_file_handle the_handle, ses_array_order the_order);
    ses_error_flag            ses_set_date(ses_file_handle the_handle, long the_date);

#ifdef TRY_OPTIONAL
    ses_error_flag            ses_set_format(ses_number num_args, ...);
#else
    ses_error_flag            ses_set_format(ses_file_handle the_handle, ses_file_type the_file_type, ses_ascii_word_type the_ascii_word_type);
#endif

    
    ses_error_flag            ses_set_grid(ses_file_handle the_handle, ses_number nr, ses_number nt, ses_number ntab);
    ses_error_flag            ses_set_label(ses_file_handle the_handle, ses_label the_label);
    ses_error_flag            ses_set_material_order(ses_file_handle the_handle);
    ses_error_flag            ses_set_significant_digits(ses_file_handle the_handle, ses_number number_digits);
    ses_error_flag            ses_set_validate(ses_file_handle the_handle);
    ses_error_flag            ses_set_version(ses_file_handle the_handle, long the_version);
    
    ses_error_flag             ses_set_endianness(ses_file_handle the_handle, char the_endianness);
    
    /*  error checking routines */
    
    ses_boolean               ses_indicates_error(ses_error_flag the_ses_error_flag);
    ses_string                ses_print_error_condition(ses_file_handle the_handle);
    ses_string                ses_print_error_message(ses_error_flag the_error);
    
    /*  memory clean up routines */
    
    ses_boolean ses_exit(void);
    
    
    /*  user routines */
    
    void        print_array_from_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid, ses_string the_label1);
    void        print_data_on_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid);
    ses_string  get_data_on_table(ses_string sesame_file, ses_material_id mid, ses_table_id tid);
    void        print_materials_on_file(ses_string sesame_file);
    ses_string  print_table(ses_material_id the_mid);
    ses_string  print_tables_for_material(ses_string sesame_file, ses_material_id mid);
    ses_string  print_index_record(ses_file_handle tH, ses_material_id mid, ses_table_id tid);
    
    /*  miscellaneous */
    
    ses_string  ses_get_available_formats(void);
    
    
    
#ifdef __cplusplus
}
#endif

#endif


