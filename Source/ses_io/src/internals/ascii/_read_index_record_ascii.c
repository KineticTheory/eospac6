#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include "stdio.h"
#include "string.h"

#define FIX_ASCII_INDEX_READ


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

  for (i = 0; i < 999; i++) {
	nwds[i] =0;
	iadr[i] = 0;
	tblid[i] = 0;
  }
  
  long nrec = 0;
  long flag = 1;
  long myflag = 1;
  long the_r = 0;
  long matid;
  int current_line_location = 0;

  long date1, date2, version;

  FILE* pFILE = pSFH->_c_file_handle;
  int return_status = 0;
  flag = _read_long_ascii(pSFH);	
  matid = _read_long_ascii(pSFH);
  tblid[nrec] = _read_long_ascii(pSFH);

  nwds[nrec] = _read_long_ascii(pSFH);
  the_r = _read_long_ascii(pSFH);
  iadr[nrec] = current_location;
  date1 = _read_long_ascii(pSFH);
  date2 = _read_long_ascii(pSFH);
  version = _read_long_ascii(pSFH);

  nrec++;
  //if ( pFILE != NULL )
  //{
      char line [ 128 ]; /* or other suitable maximum line size */
      //for (i = 0; i < 128; i++) {
	//line[i] = ' ';
      //}
//printf ("before while loop\n");
      while (( fgets ( line, sizeof line, pSFH->_c_file_handle) != NULL ) && (myflag == 1)) /* read a line */
      {

//printf("line is %s line[79] is %c\n", line, line[79]);
         
#ifdef FIX_ASCII_INDEX_READ

        if ((line[1] == '0') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && (line[77] == ' ') && (line[78] == ' ') && ((line[79] == '0') || (line[79] == '1'))) {
            	if (nrec > 0) {
			myflag = 0;
		}
		else {
			iadr[nrec] = current_line_location;
			if (nrec > 0) {
				nwds[nrec] = iadr[nrec] - iadr[nrec - 1];
			}
			if (nrec == 0) {
				nwds[nrec] = iadr[nrec];
			}
			int after_line = ftell(pFILE);
			if (current_line_location > 0) {
				return_status = fseek(pSFH->_c_file_handle, current_line_location, SEEK_SET);
			}
			else {
				rewind(pSFH->_c_file_handle);
			}
			if (myflag == 1) {	
	  		  flag = _read_long_ascii(pSFH);	
			  matid = _read_long_ascii(pSFH);
	                  tblid[nrec] = _read_long_ascii(pSFH);
	                  nwds[nrec] = _read_long_ascii(pSFH);
	                  the_r = _read_long_ascii(pSFH);
	                  iadr[nrec] = current_line_location;
			  nrec++;
		  
		
			  return_status = fseek(pSFH->_c_file_handle, after_line, SEEK_SET);
			}
		

		}
	    }

	if ( (line[1] == '1') && (line[2] == ' ') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && (line[77] == ' ') && (line[78] == ' ') && ((line[79] == '0') || (line[79] == '1'))) {


		iadr[nrec] = current_line_location;
		if (nrec > 0) {
			nwds[nrec] = iadr[nrec] - iadr[nrec - 1];
		}
		if (nrec == 0) {
			nwds[nrec] = iadr[nrec];
		}
		int after_line = ftell(pFILE);
		if (current_line_location > 0) {
			return_status = fseek(pSFH->_c_file_handle, current_line_location, SEEK_SET);
		}
		else {
			rewind(pSFH->_c_file_handle);
		}
		if (myflag == 1) {	
  		  flag = _read_long_ascii(pSFH);	
		  matid = _read_long_ascii(pSFH);
                  tblid[nrec] = _read_long_ascii(pSFH);
                  nwds[nrec] = _read_long_ascii(pSFH);
                  the_r = _read_long_ascii(pSFH);
                  iadr[nrec] = current_line_location;
		  nrec++;
		  
		
		  return_status = fseek(pSFH->_c_file_handle, after_line, SEEK_SET);
		}
		
	    } 


#else

            if (((line[1] == '1')||(line[1] == '0')) && (line[2] == ' ') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && (line[77] == ' ') && (line[78] == ' ') && ((line[79] == '0') || (line[79] == '1'))) {


		iadr[nrec] = current_line_location;
		if (nrec > 0) {
			nwds[nrec] = iadr[nrec] - iadr[nrec - 1];
		}
		if (nrec == 0) {
			nwds[nrec] = iadr[nrec];
		}
		int after_line = ftell(pFILE);
		if (current_line_location > 0) {
			return_status = fseek(pSFH->_c_file_handle, current_line_location, SEEK_SET);
		}
		else {
			rewind(pSFH->_c_file_handle);
		}
		if (myflag == 1) {	
  		  flag = _read_long_ascii(pSFH);	
		  matid = _read_long_ascii(pSFH);
                  tblid[nrec] = _read_long_ascii(pSFH);
                  nwds[nrec] = _read_long_ascii(pSFH);
                  the_r = _read_long_ascii(pSFH);
                  iadr[nrec] = current_line_location;
		  nrec++;
		  
		
		  return_status = fseek(pSFH->_c_file_handle, after_line, SEEK_SET);
		}
		
	    } 
            if ((line[1] == '0') && (line[74] == ' ') && (line[75] == ' ') && (line[76] == ' ') && (line[77] == ' ') && (line[78] == ' ') && ((line[79] == '0') || (line[79] == '1'))) {
	        if (nrec >= 0) {
            		if (nrec > 0) {
				myflag = 0;
			}
		}
	    }
#endif
            current_line_location = ftell(pSFH->_c_file_handle);

         
          
      } 
  //}  


  the_index_record->_mid = matid;
  the_index_record->_date1 = date1;
  the_index_record->_date2 = date2;
  the_index_record->_vers = version;
  nrec = nrec + 1;
  the_index_record->_nrec = nrec - 1;


  

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
  }
  for(i = 0; i < nrec; i++) {
	the_index_record->_nwds[i] = nwds[i];
  }
  for(i = 0; i < nrec; i++) {
	the_index_record->_iadr[i] = iadr[i];
  }

  


  /*  return */

  return return_value;
}




