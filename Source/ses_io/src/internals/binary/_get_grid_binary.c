
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>



#include "_file_list_binary.h"

ses_boolean _get_grid_binary(ses_file_handle the_handle, ses_material_id the_mid, 
		      ses_table_id the_tid, long* nr, long* nt, long* ntab) {

  /* define prototypes */

  int my_get_index(ses_table_id the_tid);

#ifdef DEBUG_GET_GRID_BINARY
  printf("_get_grid_binary:  entered\n");
#endif

  ses_boolean return_value = SES_TRUE;
  /* int i; */
 
  /*  read nr and nt directly from the file */

  struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
  if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid_binary: ses file handle null in _get_grid\n");
#endif
    return SES_FALSE;
  }

  FILE* pFILE = pSFH->_c_file_handle;
  if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid_binary: c file handle null _get_grid \n");
#endif
    return SES_FALSE;
  }

  struct _ses_directory* pDR = FILE_LIST[the_handle]->_directory;
  if (pDR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
    printf("_get_grid_binary: directory null in _get_grid\n");
#endif
    return SES_FALSE;
  }


  /*  get the grid */

  long my_nr = 0;
  long my_nt = 0;
  long my_ntab = -1;
  my_nr = 0;
  my_nt = 0;

  ses_boolean needs_flip = FILE_LIST[the_handle]->_the_handle->_needs_flip;
  long current_position = ftell(pFILE);

#ifdef DEBUG_GET_GRID_BINARY
  printf("_get_grid_binary:  the_tid is %d\n", the_tid);
#endif

  struct _ses_index_record* current_index_record = FILE_LIST[the_handle]->_current_index_record;
  switch(the_tid) {

    /* ses_boolean didit_go; */


  case 100:
  case 101:
  case 102:
  case 103: 
  case 104:
  case 105:
  case 106:
  case 107:
  case 108:
  case 109:
  case 110:
  case 111:
  case 112:
  case 113:
  case 114:
  case 115:
  case 116:
  case 117:
  case 118:
  case 119:
  case 120:
  case 121:
  case 122:
  case 123:
  case 124:
  case 125:
  case 126:
  case 127:
  case 128:
  case 129:
  case 130:
  case 131:
  case 132:
  case 133:
  case 134:
  case 135:
  case 136:
  case 137:
  case 138:
  case 139:
  case 140:
  case 141:
  case 142:
  case 143:
  case 144:
  case 145:
  case 146:
  case 147:
  case 148:
  case 149:
  case 150:
  case 151:
  case 152:
  case 153:
  case 154:
  case 155:
  case 156:
  case 157:
  case 158:
  case 159:
  case 160:
  case 161:
  case 162:
  case 163:
  case 164:
  case 165:
  case 166:
  case 167:
  case 168:
  case 169:
  case 170:
  case 171:
  case 172:
  case 173:
  case 174:
  case 175:
  case 176:
  case 177:
  case 178:
  case 179:
  case 180:
  case 181:
  case 182:
  case 183:
  case 184:
  case 185:
  case 186:
  case 187:
  case 188:
  case 189:
  case 190:
  case 191:
  case 192:
  case 193:
  case 194:
  case 195:
  case 196:
  case 197:
  case 198:
  case 199:
    
    my_nr = _get_table_size(FILE_LIST[the_handle]->_current_index_record, the_tid);
    my_nt = 1;
    break;
    
  case 201:
    my_nr = 5;
    my_nt = 1;
    break;

  case 301: 
  case 303:
  case 304:
  case 305:
  case 311:
  case 321:
  case 502:
  case 503:
  case 504:
  case 505:
  case 601:
  case 602:
  case 603:
  case 604:
  case 605:

    /* didit_go = */ _go_to_data_record_binary(the_handle, the_mid, the_tid);
#ifdef DEBUG_GET_GRID_BINARY
    printf("_get_grid_binary -- calling read-long.... to get my_nr and my_nt\n");
#endif
    my_nr = _read_long_pFILE_binary(pFILE, needs_flip);
    my_nt = _read_long_pFILE_binary(pFILE, needs_flip);
#ifdef DEBUG_GET_GRID_BINARY
  printf("_get_grid_binary table 301:  my_nr is %d and my_nr is %d\n", my_nr, my_nt);
#endif
    //  find ntab for the table
    if ((the_tid == 321) && (current_index_record != (struct _ses_index_record*)NULL)) {
	long table_nwds = _get_table_nwds(current_index_record, the_tid);
	long my_tables = (table_nwds - 2 - my_nt - my_nr)/(my_nt*my_nr);
	//printf("Before Adjusting 321:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	if (my_tables != 1) {
		my_ntab = my_tables;
		//printf("Adjusting 321:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	}

    }
    
    break;
    
  case 306:
    /* didit_go = */ _go_to_data_record_binary(the_handle, the_mid, the_tid);
    my_nr = _read_long_pFILE_binary(pFILE, needs_flip);
    my_nt = 1;
    
    break;

  case 401:
    /* didit_go = */ _go_to_data_record_binary(the_handle, the_mid, the_tid);
    my_nr = 1;
    my_nt = _read_long_pFILE_binary(pFILE, needs_flip);
    //  find ntab for the table
    if (current_index_record != (struct _ses_index_record*)NULL) {
	long table_nwds = _get_table_nwds(current_index_record, the_tid);
	long my_tables = (table_nwds - 1)/my_nt;
	//printf("Before Adjusting 401:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	if (my_tables != 7) {
		my_ntab = my_tables;
		//printf("Adjusting 401:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	}

    }
    break;

  case 411:
  case 412:
  case 431:
  case 501:
    /* didit_go = */ _go_to_data_record_binary(the_handle, the_mid, the_tid);
    my_nr = _read_long_pFILE_binary(pFILE, needs_flip);
    my_nt = 1;

    //  find ntab for the table
    if ((the_tid == 411) && (current_index_record != (struct _ses_index_record*)NULL)) {
	long table_nwds = _get_table_nwds(current_index_record, the_tid);
	long my_tables = (table_nwds - 2 - my_nt)/my_nr;
	//printf("Before Adjusting 411:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	if (my_tables != 5) {
		my_ntab = my_tables;
		//printf("Adjusting 411:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	}

    }
    if ((the_tid == 412) && (current_index_record != (struct _ses_index_record*)NULL)) {
	long table_nwds = _get_table_nwds(current_index_record, the_tid);
	long my_tables = (table_nwds - 2 - my_nt)/my_nr;
	//printf("Before Adjusting 412:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	if (my_tables != 5) {
		my_ntab = my_tables;
		//printf("Adjusting 412:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	}

    }
    if ((the_tid == 431) && (current_index_record != (struct _ses_index_record*)NULL)) {
	long table_nwds = _get_table_nwds(current_index_record, the_tid);
	long my_tables = (table_nwds - 3)/my_nr;
	//printf("Before Adjusting 431:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	if (my_tables != 2) {
		my_ntab = my_tables;
		//printf("Adjusting 431:  On nr is %d nt is %d ntab is %d table_nwds is %d my_tables is %d\n", 
				//my_nr, my_nt, my_ntab, table_nwds, my_tables);
	}

    }
  
    break;
  
  
 
   
  default:
    /* i = */ my_get_index(the_tid);
    /*  guess that nr and nt are right at the start, get them there */
    /* didit_go = */ _go_to_data_record_binary(the_handle, the_mid, the_tid);
    my_nr = _read_long_pFILE_binary(pFILE, needs_flip);
    my_nt = _read_long_pFILE_binary(pFILE, needs_flip);
    if (my_nr == 0)my_nr = 1;
    if (my_nt == 0)my_nt = 1;

    break;

  }

  int fseek_return = fseek(pFILE, current_position, SEEK_SET);
  if (fseek_return != 0) {
#ifdef DEBUG_PRINT
    printf("_get_grid_binary: error in fseek in _get_grid \n");
#endif
    return SES_FALSE;
  }

#ifdef DEBUG_GET_GRID_BINARY
  printf("_get_grid_binary:  addresses *nr is %ld and *nt is %ld\n", nr, nt);
  printf("_get_grid_binary:  *nr is %ld and *nt is %ld\n", *nr, *nt);
  printf("_get_grid_binary:  my_nr is %d and my_nt is %d\n", my_nr, my_nt);
#endif

  if ((my_nr > 0) && (my_nt > 0)) {
  	*nr = my_nr;
  	*nt = my_nt;
        *ntab = my_ntab;
  }
  else {
	return_value = SES_FALSE;
  }

#ifdef DEBUG_GET_GRID_BINARY
  printf("_get_grid_binary:  addresses *nr is %ld and *nt is %ld\n", nr, nt);
  printf("_get_grid_binary:  *nr is %ld and *nt is %ld\n", *nr, *nt);
#endif

  return return_value;
}


