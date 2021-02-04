


#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#include "_file_list_ascii.h"

#undef DEBUG_PRINT

ses_error_flag _read_directory_ascii(struct _ses_directory* the_directory, struct _ses_file_handle* pSFH) {
    
    ses_error_flag return_value = SES_NO_ERROR;
    
    /*******************************************************************/
    /* argument error checking */
    if (the_directory == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii: null directory pointer passed in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return 0;
    }
    
    if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii:  null ses file handle in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
    }
    
    FILE* pFILE = pSFH->_c_file_handle;
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii: Null FILE* \n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return return_value;
    }
    
    /*******************************************************************/
    
    
    /*  return to the start of the file  */
#ifdef DEBUG_PRINT
    printf("_read_directory_ascii: rewind file\n");
#endif
    
    rewind(pFILE);
#ifdef DEBUG_PRINT
    printf("_read_directory_ascii: rewind file DONE\n");
#endif
   
    
    //  this routine needs to extract the number of materials, nwds in each material file, and iadr for each material from the file
    
    //  go through the file and count the number of FILE_MARKS
    
    /* int current_location = 0; */
    /* current_location = ftell(pSFH->_c_file_handle); */
    rewind(pSFH->_c_file_handle);
#ifdef DEBUG_PRINT
    printf("_read_directory_ascii: rewind second time\n");
#endif
    
    int current_line_location = ftell(pSFH->_c_file_handle);
   
#ifdef DEBUG_PRINT
    printf("_read_directory_ascii: current_line_location: %d \n", current_line_location);
    printf("_read_directory_ascii: after ftell\n");
#endif

    int nfiles = 0;
    long iadr[999];
    long matid[999];
    long nwds[999];
    /* int return_status = 0; */
    /* long dummy = 0; */
    long date, version;
    if ( pFILE != NULL )
    {
        char line [ 128 ]; /* or other suitable maximum line size */
        while ( fgets ( line, sizeof line, pSFH->_c_file_handle) != NULL ) /* read a line */
        {
#ifdef DEBUG_PRINT
            printf("_read_directory_ascii: line: %s, length: %ld\n", line, sizeof line);
#endif
            // The following is a poor woman's parser. These are indexes that the line should be a zero or
            // blank when describing a beginning record of a material.
            if ((line[0] == ' ') && (line[1] == '0') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && (line[77] == ' ') && (line[78] == ' ') && ((line[79] == '1') || (line[79] == '0'))) {
                iadr[nfiles] = current_line_location;
                if (nfiles > 0) {
                    nwds[nfiles] = iadr[nfiles] - iadr[nfiles - 1];
                }
                if (nfiles == 0) {
                    nwds[nfiles] = iadr[nfiles];
#ifdef DEBUG_PRINT
                    printf("_read_directory_ascii: nwds[%i]: %ld\n", nfiles, nwds[nfiles]);
#endif
                }
                int after_line = ftell(pFILE);
                if (current_line_location > 0) {
                    /* return_status = */ fseek(pSFH->_c_file_handle, current_line_location, SEEK_SET);
                }
                else {
                    rewind(pSFH->_c_file_handle);
                }
                // Values that are not kept at this time:
                int  file_num     = 0;      // Start of header marker: 0 first header, 2 last header,
                                            // 1 all headers in between.
                int  mat_id       = 0;      // Material ID
                int  table_id     = 0;      // Table ID for the following information.
                int  num_words    = 0;      // Number of words (bytes for comments) in the data.
                char r_value      = 'v';    // Has never been used
                     date  = 0;             // Creation Date
                int  update_date  = 0;      // Updated the data date
                     version      = 0;      // Version of the data
                int  end_file_num = 0;      // End of header marker: 0 first header, 2 last header, 1 all headers in between.
                
                fscanf( pSFH->_c_file_handle, "%1d%6d%6d%6d   %c%9ld%9d%4ld%34d", &file_num, &mat_id, &table_id, &num_words,
                       &r_value, &date, &update_date, &version, &end_file_num );

                matid[nfiles] = (long)mat_id;
#ifdef DEBUG_PRINT
                printf("_read_directory_ascii: matid[%i]: %ld\n", nfiles, matid[nfiles]);
#endif
                /* return_status = */ fseek(pSFH->_c_file_handle, after_line, SEEK_SET);
                nfiles++;
            }
            current_line_location = ftell(pSFH->_c_file_handle);
            
            
        }
    }
    
#ifdef DEBUG_PRINT
    printf("_read_directory_ascii: nfiles: %i\n", nfiles);
#endif
    
    
    /*  fill arrays */
    
    the_directory->_matid = malloc(sizeof(long)*nfiles);
    if (the_directory->_matid == (long*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii: memory allocation error _matid in _read_directory_ascii\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    
    
    int i = 0;
    for(i = 0; i < nfiles; i++) {
        the_directory->_matid[i] = matid[i];
    }
    
    
    the_directory->_nwds = malloc(sizeof(long)*nfiles);
    if (the_directory->_nwds == (long*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii: memory allocation error _nwds in _read_directory_ascii\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    
    for(i = 0; i < nfiles; i++) {
        the_directory->_nwds[i] = nwds[i];
    }
    
    
    the_directory->_iadr = malloc(sizeof(long)*nfiles);
    if (the_directory->_iadr == (long*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_directory_ascii: memory allocation error _iadr in _read_directory_ascii\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    
    for(i = 0; i < nfiles; i++) {
        the_directory->_iadr[i] = iadr[i];
    }
    
    the_directory->_nfiles = nfiles;
    
    the_directory->_date    = date;
    the_directory->_version = version;
    
    the_directory->_has_multiple_files = SES_FALSE;
    the_directory->_ready              = SES_TRUE;
    
    /*  return */
    return return_value;
    
}







































