

#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_internals.h"
#include "ses_externs.h"

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>

#include <sys/time.h>
#include <sys/resource.h>

#undef DEBUG_SES_OPEN
#undef DEBUG_SEEK_CUR

//////  defines /////////////////////
//  BIG_END - 1 for testing byte order
//  LITTLE_END - 0 for testing byte order
/////////////////////////////////////

#undef DEBUG_PRINT

#define BIG_END 1
#define LITTLE_END 0

#define my_construct_file HEADER(my_construct_file)
#define my_determine_file_type HEADER(my_determine_file_type)
#define my_determine_endianness HEADER(my_determine_endiannness)
#define my_is_small HEADER(my_is_small)
#define my_TestByteOrder HEADER(my_TestByteOrder)
#define my_determine_word_size_ascii HEADER(my_determine_word_size_ascii)

#include "../internals/binary/_file_list_binary.h"

ses_file_handle ses_open(ses_string filename, ses_open_type open_flags)
{
    
    /*  open the named file with the given open_flags and
     return a ses_file_handle */
    
    /*  function prototypes */
    
#ifdef DEBUG_PRINT
    printf("ses_open: Call my_construct_file\n");
#endif
    ses_file_handle my_construct_file(ses_string filename,
                                      ses_open_type open_flags);
    
#ifdef DEBUG_SES_OPEN
    printf("Entered sesio\n");
#endif
    
    /*  end function prototypes */
    
    if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
        printf("ses_open: filename null in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return 0;
    }
    
    if (_is_valid_open_flag(open_flags) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("ses_open: open type invalid in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return 0;
    }
    
    /*  construct globals upon first usage
     -- assumes this will always be the first routine called */
    
    if (_next_empty_file == 0) {
#ifdef DEBUG_SES_OPEN
        printf("ses_open:  calling _construct_globals\n");
#endif
        _construct_globals(open_flags);
    }
    
    /*  open the sesame file and get a C file handle */
    
    ses_file_handle return_value = (ses_file_handle)(0);
    if (_globals_contains(filename) == SES_FALSE) {
        
        // if file not already open
#ifdef DEBUG_SES_OPEN
        printf("ses_open: calling my_construct_file, filename: %s, open_flags: %c\n", filename, open_flags);
#endif
        // Returns the file handle...
        return_value = my_construct_file(filename, open_flags);        
#ifdef DEBUG_SES_OPEN
        printf("ses_open:  after my_construct_file returning with return_value = %d\n", return_value);
#endif
        
    }
    else {
        
        //  if file already open
#ifdef DEBUG_SES_OPEN
        printf("ses_open:  calling _get_handle_from_globals\n");
#endif
        
        return_value = _get_handle_from_globals(filename);
#ifdef DEBUG_SES_OPEN
        struct _ses_file_handle* pSFH = FILE_LIST[return_value]->_the_handle;
        printf("ses_open:  after _get_handle returning with return_value = %d and open_flags in %cand we wanted %c\n", return_value, _get_open_mode(pSFH), open_flags);
#endif
        
    }
    
    if (return_value < 1) {
#ifdef DEBUG_PRINT
        printf("ses_open: table handle error (< 1) in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return 0;
    }
    
#ifdef DEBUG_SES_OPEN
    struct _ses_file_handle* pSFH = FILE_LIST[return_value]->_the_handle;
    printf("ses_open:  returning with return_value = %d and open_flags in %c\n", return_value, _get_open_mode(pSFH));
#endif
    
    _number_open_handles++;
    
    return return_value;
    
}

ses_file_handle my_construct_file(ses_string filename, ses_open_type open_flags)
{
    
    /*  open the named file */
    /*  returns: NULL if file handle invalid */
    
    /*  function prototypes  */
    
    ses_file_type       my_determine_file_type(ses_string filename, ses_open_type open_flags);
    ses_ascii_word_size my_determine_word_size_ascii(FILE* pFILE, ses_string filename);
    ses_boolean   my_determine_endianness(FILE* pFILE);
    int           my_TestByteOrder();
    
    /*  end function prototypes */
    
    ses_file_handle return_value = (ses_file_handle)0;
    
    if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
        printf("my_construct_file: filename null in my_construct_file\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return (ses_file_handle)0;
    }
    if (_is_valid_open_flag(open_flags) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("my_construct_file: open type invalid in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return (ses_file_handle)0;
    }
    
    
    /*  get the file type */
    
    ses_boolean file_exists = SES_FALSE;
    /* ses_boolean format_exists = SES_FALSE; */
    
    ses_file_type the_type = QUERY_4_TYPE;
    if (open_flags == 'R' || open_flags == 'C' || open_flags == 'A' ) {
        the_type = my_determine_file_type(filename, open_flags);
        
        if (the_type != UNKNOWN_TYPE) {
            file_exists = SES_TRUE;
            if (the_type == QUERY_4_TYPE) {
                if (open_flags != 'A') {
                    /* format_exists = SES_FALSE; */
                }
                else {
                    /* format_exists = SES_TRUE; */
                    the_type = BINARY_TYPE;  /*  appending to non-existent file, open as current format */
                }
            }
        }
        else {
            file_exists = SES_FALSE;
            the_type = _current_format;
#ifdef DEBUG_PRINT
            printf("my_construct_file:  Could not ascertain file type, set to: %d\n",the_type);
#endif
        }
    }
    else {
        the_type = _current_format;
    }
    
    
    if (the_type == QUERY_4_TYPE) {
#ifdef DEBUG_PRINT
        printf("my_construct_file:  Could not ascertain file type \n");
#endif
        return -1;
    }
    
    FILE* pFILE = NULL;
    
    /*  open the file */
    
    
    switch(open_flags) {
        case 'R':
        
          pFILE = fopen(filename, "r");  //  Note file not exist returns 0
          break;
        
        case 'A':
        
#ifdef DEBUG_PRINT
            printf("my_construct_file:  APPEND: opening the file with A, filename: %s \n", filename);
#endif
          pFILE = fopen(filename, "r+");
          break;
        
        case 'W':
        
          if (file_exists == SES_FALSE) {
            pFILE = fopen(filename, "w");
          }
          else {
            pFILE = 0;
#ifdef DEBUG_PRINT
            printf("my_construct_file:  trying to open for write an existing file \n");
#endif
            _set_latest_error(SES_WRITE_ERROR);
            return 0;
          }
          break;
        
        case 'C':
        
          pFILE = fopen(filename, "r+");
          break;
        
        default:
          _set_latest_error(SES_OPEN_ERROR);
          return (ses_file_handle)0;
          break;
        
    }
    
    
    /*  put the file handle on the file handle list */
    
    /*  check to see if the C file handle limit has been reached */
    
    struct rlimit rlim;
    getrlimit(RLIMIT_NOFILE, &rlim);
    int process_open_files = rlim.rlim_cur;
#ifdef DEBUG_PRINT
    printf("my_construct_file:  process_open_files %d, _next_empty_file: %d, filename: %s \n", process_open_files, _next_empty_file,filename);
#endif

    // FILE_LIST max size is 2000, and we start at 1... updated:7/8/20
    if (process_open_files >= 2000){
      process_open_files = 1999;
    }
    if (pFILE == NULL || _next_empty_file >= process_open_files) {
        if (pFILE == NULL) {
#ifdef DEBUG_PRINT
            printf("my_construct_file:  FILE %s NOT FOUND\n", filename);
#endif
        }
        else {
#ifdef DEBUG_PRINT
            printf("my_construct_file: C file handle null or max file limit reached in ses_open\n");
#endif
        }
        
        _set_latest_error(SES_OPEN_ERROR);
        return (ses_file_handle)0;
    }
    else {
        
        /*  file was successfully opened */
        
        /*  construct the _ses_file* object with all the necessary information */
        
        /*  get the endianness */
        
        
        ses_boolean isit_little = SES_TRUE;
        if (open_flags == 'R' || open_flags == 'C' || open_flags == 'A') {
            isit_little = my_determine_endianness(pFILE);
        }
        else {
            isit_little = SES_TRUE;
        }
        
        int index = _get_next_empty_slot_from_globals();
        if (index >= 1) {
            
            /*  construct the file */
            
            FILE_LIST[index] = _construct_dynamic_ses_file(pFILE,
                                                           open_flags, the_type, isit_little, filename);
            
            
            /*  set the file handle information */
            
            ses_boolean isit_big = my_TestByteOrder();
            if (isit_big == SES_TRUE) {
                FILE_LIST[index]->_the_handle->_machine_is_little_endian = SES_FALSE;
            }
            else {
                FILE_LIST[index]->_the_handle->_machine_is_little_endian = SES_TRUE;
            }
            
            
            if (isit_little == my_TestByteOrder()) {
                FILE_LIST[index]->_the_handle->_needs_flip = SES_TRUE;
            }
            else {
                FILE_LIST[index]->_the_handle->_needs_flip = SES_FALSE;
            }
            
            if ((open_flags != 'W') &&
                (open_flags != 'A') && (FILE_LIST[index]->_directory == (struct _ses_directory*)NULL)) {
                FILE_LIST[index]->_directory = _read_directory(FILE_LIST[index]->_the_handle);
                
                
            }
            
            /* If we have an ASCII file figure out the word size: Medium or Small
             */
            if ( the_type == ASCII_TYPE ) {
                
                // If the files is ascii, figure out the word size.
                // Check the size of the data: if small or medium
                ses_ascii_word_size word_size = ASCII_MEDIUM_WORD_SIZE;
                
                word_size = my_determine_word_size_ascii(pFILE, filename);
#ifdef DEBUG_PRINT
                printf("my_construct_file: Determined word size: %d\n", word_size);
#endif
                
                if ( (word_size == ASCII_UNSUPPORTED_WORD_SIZE) ){
                    return( SES_INVALID_SESAME_ASCII );
                }
#ifdef DEBUG_PRINT
                printf("my_construct_file: SET WORD SIZE!!!\n");
#endif
                FILE_LIST[index]->_the_handle->_word_size = word_size;
            }
            
            
            /* At this point, the data structures for the particular file
             have been filled correctly
             
             Adding code to:  setup to 100 table
             ifit did the setup correctly, read table definitions
             add those table definitions to the standard tables
             */
            
            /*  index is the ses file handle before it's been passed back to the user */
            ses_boolean done_correctly = _get_more_table_definitions((ses_file_handle)index);
            
            if (done_correctly == SES_FALSE) {
                return_value = SES_FALSE;
            }
            
            
            /*  pass the sesame file handle back to the user */
            return_value = (ses_file_handle)index;
        }
        
        
    }
    
//    if (open_flags == 'A' || open_flags == 'C') {
        // Seek to the end:
//        fseek(pFILE, (long)0, SEEK_END);
//    }
    
    return return_value;
    
}

ses_file_type my_determine_file_type(ses_string filename, ses_open_type the_flag)
{
    /* determine the file type by opening it and checking it */
    
    ses_file_type return_value = UNKNOWN_TYPE;
    if (filename == (ses_string)NULL) {
#ifdef DEBUG_PRINT
        printf("my_determine_file_type: filename null in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return return_value;
    }
    if (_is_valid_open_flag(the_flag) == SES_FALSE) {
#ifdef DEBUG_PRINT
        printf("my_determine_file_type: open type invalid in ses_open\n");
#endif
        _set_latest_error(SES_OPEN_ERROR);
        return return_value;
    }
    
    
    return_value = QUERY_4_TYPE;
    FILE* pFILE = (FILE*)NULL;
    pFILE = fopen(filename, "r");  /*  Note file not exist returns 0 */
    
    
    if (pFILE == (FILE*)NULL) {
        /*  file does not exist, set the default file type to 'N' */
        return UNKNOWN_TYPE;
    }
    
    /*  go through the list of registered formats and find the correct format */
    
    ses_boolean found_format = SES_FALSE;
    
    //  look at the current format first
    found_format = pt2_isit_my_format(pFILE);
    
    int i = 0;
    if (found_format == SES_FALSE) {
        for (i = 0; i < _number_registered_formats; i++) {
#ifdef DEBUG_PRINT
            printf("looking at format %c, _current_format %c, found_format: %d \n", _registered_formats[i], _current_format, found_format);
#endif
            // already looked at current format, so ignore it
            if (_current_format != _registered_formats[i]) {
                
                _set_current_format(_registered_formats[i]);
                found_format = pt2_isit_my_format(pFILE);
#ifdef DEBUG_PRINT
                printf("found format is  %d SES_TRUE is %d \n", found_format, SES_TRUE);
#endif
                
                if (found_format == SES_TRUE) {
                    _current_format = _registered_formats[i];
                    return_value = _current_format;
                    i = _number_registered_formats;
                }
            }
        }
    }
    else {
        return_value = _current_format;
    }
    
    /* int close_return = 0; */
    /* close_return = */ fclose(pFILE);
    
    pFILE = (FILE*)NULL;
    
    return return_value;
}

ses_boolean my_determine_endianness(FILE* pFILE)
{
    
    /*  determine the byte order of the machine upon which we are running */
    
    /*  SES_TRUE - little, SES_FALSE - big */
    /*  the first thing on any sesame file should be a small integer number (the number of materials) */
    
    /*  function prototypes */
    
    ses_boolean my_is_small(long next);
    int my_TestByteOrder();
    
    /*  end function protytypes */
    
    ses_boolean return_value = SES_TRUE;
    
    int machine_endianness = my_TestByteOrder();
    if (pFILE == 0) {
        /*  file does not exist, set the default file type to the machine type*/
        
        if (machine_endianness == LITTLE_END) {
            return SES_TRUE;
        }
        else {
            return SES_FALSE;
        }
    }
    
    ses_boolean needs_flip = SES_FALSE;
    
    long next = _read_long_pFILE_binary(pFILE, needs_flip);
    if (next == 0) {
        /*  needs a flip */
        rewind(pFILE);
        needs_flip = SES_TRUE;
        next = _read_long_pFILE_binary(pFILE, needs_flip);
    }
    
    /*  if next is small integer, endianness is same as machine */
    
    if (machine_endianness == LITTLE_END) {
        if (needs_flip == SES_FALSE) {
            return_value = SES_TRUE;  /*  returning little endian */
        }
        else {
            return_value = SES_FALSE;
        }
    }
    else {   /*  machine is big endian */
        if (needs_flip == SES_FALSE) {
            return_value = SES_FALSE;
        }
        else {
            return_value = SES_TRUE;
        }
    }
    
    return return_value;
}

ses_boolean my_is_small(long next)
{
    
    /*  determine if the passed in long is 'small' (0 < next <= 10000) */
    
    ses_boolean return_value = SES_TRUE;
    
    if ((next >= 10000) || (next < 0)) {
        return_value = SES_FALSE;
    }
    
    return return_value;
    
}

int my_TestByteOrder()
{
    
    /*  test the byte order - return endianness */
    
    short int word = 0x0001;
    char* byte = (char *) &word;
    if (byte[0] == 1) {
        return LITTLE_END;
    }
    else {
        return BIG_END;
    }
}

ses_ascii_word_size my_determine_word_size_ascii(FILE* pFILE, ses_string filename)
{
    
    /*  This is only called, if the file format is ascii.
     Read data from the current file handle to get ASCII size: 22(medium) or 15(small)
     */
    ses_boolean not_found = SES_TRUE;
    ses_boolean no_data   = SES_FALSE;
    ses_ascii_word_size return_value = ASCII_UNSUPPORTED_WORD_SIZE;
    long  size;	       	// Get the size of the file
    char *e_ptr;        // Pointer to the first 'E' in the material's data
    int   index  = 0;   // Index of "E" in material's data
    FILE* sameFILE;
    
    
    // Constant numerical values:
    static const int fortran_line_len = 80; // Length of an old Fortran punch card
    static const int medium_E_index   = 18; // Where E is expected in Medium data: " 5.400000000000000E+00"
    static const int small_E_index    = 11; // Where E is expected in Small  data: " 4.06000000E+01"
    
    int  file_id      = 0;      // file ID - not used but necessary for debugging & place to read into.
    int  mat_id       = 0;      // Material ID - not used but necessary for debugging & place to read into.
    int  table_id     = 0;      // Table ID for the following information.
    int  num_words    = 0;      // Number of words (bytes for comments) in the data.
    
    // For table_id 101-199: the num_words is bytes of characters for comments
    int dataLength =  fortran_line_len + 1; // add one for linefeed.
    char data[dataLength];
        
    /*  file does not exist, set the default word size to unknown */
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("my_determine_word_size_ascii: file doesn't exist.\n");
#endif
        return ASCII_UNSUPPORTED_WORD_SIZE;
    }
    
    // We need to open the same file with another pointer to read it.
    sameFILE = fopen( filename, "r");
    
    //  NOTE:  IN VALGRIND, the value of rv comes back as -1 instead of 0 like it does in make test
    
    // See if there is anything in the file:
    fseek(sameFILE, (long)0, SEEK_END);
    
    size = ftell(sameFILE);

    if (size <= 0){
#ifdef DEBUG_PRINT
        printf("my_determine_word_size_ascii: file is empty, size: %ld\n", size);
#endif
        return ASCII_MEDIUM_WORD_SIZE;
    }
    
    rewind(sameFILE);
  
#ifdef DEBUG_PRINT
    unsigned long curFilePos = 0;
#endif
  
    // Read until we have something other than a 10x series of table_id:
    while (not_found){
        file_id   = _read_long_tomax_pFILE_ascii(sameFILE, 2, SES_FALSE);
        mat_id    = _read_long_tomax_pFILE_ascii(sameFILE, 6, SES_FALSE);
        table_id  = _read_long_tomax_pFILE_ascii(sameFILE, 6, SES_FALSE);
        num_words = _read_long_tomax_pFILE_ascii(sameFILE, 6, SES_FALSE);
      
        // Do some nonesense so that file_id & mat_id are not skipped by compilers...
        dataLength = file_id + mat_id;
      
        // Read to the end of line. 80 chars
        if (fgets(data, fortran_line_len, sameFILE) == NULL)
            not_found = SES_FALSE;
      
#ifdef DEBUG_PRINT
        curFilePos = ftell(sameFILE);
        printf("my_determine_word_size_ascii: FILE POSITION: %ld\n", curFilePos);
        printf("\t file_id: %d, mat_id: %d, table_id: %d, num_words: %d\n",
               file_id, mat_id, table_id, num_words);
        printf("\t date: %s\n", data);
#endif
        if ( table_id > 199 ){
            not_found = SES_FALSE;
          
            e_ptr = strchr(data, 'E');
            if (e_ptr == NULL){
                if (feof( sameFILE )) {
                    not_found = SES_FALSE;
                    no_data = SES_TRUE;
                }
#ifdef DEBUG_PRINT
                printf("\t No data @ %s, EOF\n", data);
#endif
            }
        }
        else if (table_id >= 101 && table_id <= 199){
            // Skip over all of the comments.
            // Figure out how many lf/cr:
            int linefeeds = 0;
            if ( num_words % fortran_line_len > 0){
                linefeeds = 1;
            }
            linefeeds += (num_words/fortran_line_len);

#ifdef DEBUG_PRINT
            printf("Number of linefeeds: %d, number of lines: %d, remainder: %d\n", linefeeds, (num_words/fortran_line_len), (num_words%fortran_line_len));
            curFilePos = ftell (sameFILE);
#endif

            fseek( sameFILE, num_words+linefeeds, SEEK_CUR );
#ifdef DEBUG_PRINT
            //fgets(data, fortran_line_len, sameFILE);
            //printf("Position was: %ld, Seeked: %d\nDATA: %s\n", curFilePos, (num_words+linefeeds), data);
            //fseek( sameFILE, -(fortran_line_len), SEEK_CUR );
#endif

        }
        else { // 100 tables...
            if (num_words == 0) num_words = 1;
#ifdef DEBUG_PRINT
            printf("Number of words: %d\n", num_words);
            curFilePos = ftell (sameFILE);
#endif

            fseek( sameFILE, num_words, SEEK_CUR );
        }
    }
#ifdef DEBUG_PRINT
    printf("\nmy_determine_word_size_ascii:\n");
    printf("\t mat_id: %d, table_id: %d, num_words: %d\n", mat_id, table_id, num_words);
#endif

    // If there is only comment files, we should allow the user to read what they can.
    // So allow the open, provide a valid file handle and set the word size to a
    // mythical length of MEDIUM...
    // If this causes troubles, back this out and give the user an error.
    if (no_data){
        fclose(sameFILE);
        return (ASCII_MEDIUM_WORD_SIZE);
    }

    // Grab one piece of data to see what the size is:
    fgets(data, (ASCII_MEDIUM_WORD_SIZE +1), sameFILE);
#ifdef DEBUG_PRINT
    printf("my_determine_word_size_ascii\t FIRST data: \"%s\"\n", data);
#endif
    
    // If the file is not of the proper format or has values larger then medium,
    // e_ptr is gonna come back with a value of 0.
    e_ptr = strchr(data, 'E');
#ifdef DEBUG_PRINT
    printf("my_determine_word_size_ascii:\t e_ptr: %s\n", e_ptr);
#endif
   
    if (e_ptr != NULL){
        // get the offset to figure out the data size:
        index = (int)(e_ptr - data);
#ifdef DEBUG_PRINT
        printf("my_determine_word_size_ascii:\t Index %d, small_E_index: %d\n", index, small_E_index);
#endif
        
        if ( index == medium_E_index ){
            return_value = ASCII_MEDIUM_WORD_SIZE;
        }
        else if ( index == small_E_index ){
            return_value = ASCII_SMALL_WORD_SIZE;
        }
        else{
            return_value = ASCII_UNSUPPORTED_WORD_SIZE;
        }
    }
    else{
        return_value = ASCII_UNSUPPORTED_WORD_SIZE;
    }
    
    // Close the file you opened to read data...
    fclose(sameFILE);
    
    return return_value;
    
}

