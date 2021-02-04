#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#undef DEBUG_PRINT

ses_error_flag _read_index_record_ascii(struct _ses_index_record* the_index_record, struct _ses_file_handle* pSFH, long offset) {
    
    ses_error_flag return_value = SES_NO_ERROR;
    
    /****************************************************************************/
    /* argument error checking */
    if (the_index_record == (struct _ses_index_record*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii: null directory pointer passed in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return 0;
    }
    
    if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii:  null ses file handle in\n");
#endif
        _set_latest_error(SES_NULL_OBJECT_ERROR);
        return SES_FALSE;
    }
    
    /****************************************************************************/
    
    
    //  this routine needs to extract the number of tables for a material, nwds in each table, and iadr for each table in the record
    
    int current_location = ftell(pSFH->_c_file_handle);
    
    int i = 0;
    long nwds[999];
    long iadr[999];
    long tblid[999];
    int  myflag       = 0;      // Start of header marker: 0 first header, 2 last header,
    // 1 all headers in between.
    int  mat_id       = 0;      // Material ID
    int  prev_mat_id  = 0;      // To check the mat_id
    int  table_id     = 0;      // Table ID for the following information.
    int  num_words    = 0;      // Number of words (bytes for comments) in the data.
    char r_value      = 'v';    // Has never been used
    int  create_date  = 0;      // Creation Date
    int  update_date  = 0;      // Updated the data date
    int  version      = 0;      // Version of the data
   
    for (i = 0; i < 999; i++) {
        nwds[i] =0;
        iadr[i] = 0;
        tblid[i] = 0;
    }
    
    long nrec = 0;
    int current_line_location = 0;
    iadr[nrec] = current_location;
    
    
    FILE* pFILE = pSFH->_c_file_handle;    
    
    int fixed_size = 6;
    myflag      = _read_long_tomax_ascii(pSFH, 2);
    mat_id      = _read_long_tomax_ascii(pSFH, fixed_size);
    table_id    = _read_long_tomax_ascii(pSFH, fixed_size);
    num_words   = _read_long_tomax_ascii(pSFH, fixed_size);
    r_value     = _read_long_tomax_ascii(pSFH, 4);
    create_date = _read_long_tomax_ascii(pSFH, 9);
    update_date = _read_long_tomax_ascii(pSFH, 9);
    version     = _read_long_tomax_ascii(pSFH, 4);
    
    nwds[nrec]  = num_words;
    tblid[nrec] = table_id;
    prev_mat_id = mat_id;
#ifdef DEBUG_PRINT
    printf("_read_index_record_ascii 1: my_flag: %d, mat_id: %d, table_id: %d, num_words: %d\n", myflag, mat_id, table_id, num_words);
#endif
    myflag = 1;
    nrec++;

    
    char line [ 128 ]; /* or other suitable maximum line size */
    while (( fgets ( line, sizeof line, pSFH->_c_file_handle) != NULL ) && (myflag == 1)) /* read a line */
    {
        
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii:  myflag: %d line: %s\n", myflag, line);
#endif
        
        if ( ( (line[1] == '0') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && 
                                   (line[77] == ' ') && (line[78] == ' ') &&
               (line[79] == '0') )                                                                 ||
             ( (line[1] == '1') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') &&
                                   (line[77] == ' ') && (line[78] == ' ') &&
               (line[79] == '1') ) ) {
            
            
            iadr[nrec] = current_line_location;
            if (nrec > 0) {
                nwds[nrec] = iadr[nrec] - iadr[nrec - 1];
            }
            if (nrec == 0) {
                nwds[nrec] = iadr[nrec];
            }
            int after_line = ftell(pFILE);
            if (current_line_location > 0) {
                /* return_status = */ fseek(pSFH->_c_file_handle, current_line_location, SEEK_SET);
            }
            else {
                rewind(pSFH->_c_file_handle);
            }
            prev_mat_id = mat_id;
                 
            if (myflag == 1) {
                myflag    =  _read_long_tomax_ascii(pSFH, 2);
                mat_id    =  _read_long_tomax_ascii(pSFH, fixed_size);
                table_id  =  _read_long_tomax_ascii(pSFH, fixed_size);
                num_words =  _read_long_tomax_ascii(pSFH, fixed_size);;
                r_value     = _read_long_tomax_ascii(pSFH, 4);
                create_date = _read_long_tomax_ascii(pSFH, 9);
                update_date = _read_long_tomax_ascii(pSFH, 9);
                version     = _read_long_tomax_ascii(pSFH, 4);
                
#ifdef DEBUG_PRINT
                printf("_read_index_record_ascii 2:  myflag: %d, mat_id: %d, prev_mat_id: %d, table_id: %d, num_words: %d\n", myflag, mat_id, prev_mat_id, table_id, num_words);
                printf("_read_index_record_ascii 3: create: %d, update: %d, version: %d\n", create_date, update_date, version);
#endif
                // Need to check if we are in the same material.
                // If not, need to add this to a different mat_id.
                if (mat_id == prev_mat_id){
                    nwds[nrec]  = num_words;
                    tblid[nrec] = table_id;
                
                    iadr[nrec] = current_line_location;
                    nrec++;
                
                    /* return_status = */ fseek(pSFH->_c_file_handle, after_line, SEEK_SET);
                }
            }
            
        }
        
        current_line_location = ftell(pSFH->_c_file_handle);
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii 4:  current_line_location: %d\n", current_line_location);
#endif
    }
    
    the_index_record->_mid = mat_id;
    the_index_record->_date1 = create_date;
    the_index_record->_date2 = update_date;
    the_index_record->_vers = version;
    nrec = nrec + 1;
    the_index_record->_nrec = nrec - 1;

#ifdef DEBUG_PRINT
    printf("_read_index_record_ascii 3:  mat_id: %d, table_id: %d, create_date: %d, nrec: %ld the_index_record->_nrec: %ld\n", mat_id, table_id, create_date, nrec, the_index_record->_nrec);
#endif
    
    fseek(pSFH->_c_file_handle, current_location, SEEK_SET);
    
    
    the_index_record->_tblid = malloc(sizeof(ses_table_id)*nrec);
    if (the_index_record->_tblid == NULL) {
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii: memory allocation error in read index record _tblid\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    
    the_index_record->_nwds = malloc(sizeof(long)*nrec);
    if (the_index_record->_nwds == NULL) {
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii: memory allocation error in read index record _nwds\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    
    the_index_record->_iadr = malloc(sizeof(long)*nrec);
    if (the_index_record->_iadr == NULL) {
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii: memory allocation error in read index record _iadr\n");
#endif
        _set_latest_error(SES_MEMORY_ALLOCATION_ERROR);
        return SES_MEMORY_ALLOCATION_ERROR;
    }
    for(i = 0; i < nrec; i++) {
        the_index_record->_tblid[i] = tblid[i];
        the_index_record->_nwds[i] = nwds[i];
        the_index_record->_iadr[i] = iadr[i];

        // To get rid of compiler warnings...
        r_value++;
#ifdef DEBUG_PRINT
        printf("_read_index_record_ascii: tblid[%d] = %ld\n", i, tblid[i]);
        printf("_read_index_record_ascii: nwds[%d] = %ld\n", i, nwds[i]);
        printf("_read_index_record_ascii: iadr[%d] = %ld\n", i, iadr[i]);
#endif
    }
    
    
#ifdef DEBUG_PRINT
    printf("_read_index_record_ascii:  return_value: %d, mat_id: %d, table_id: %d\n", return_value, mat_id, table_id);
#endif
    
    
    /*  return */
    
    return return_value;
}




