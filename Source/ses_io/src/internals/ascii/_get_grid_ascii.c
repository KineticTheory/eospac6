
#include "ses_defines.h"
#include "ses_globals.h"
#include "ses_externs.h"
#include "ses_internals.h"

#include <stdio.h>
#include <string.h>

#include "_file_list_ascii.h"

#undef DEBUG_PRINT
ses_boolean _get_grid_ascii(ses_file_handle the_handle, ses_material_id the_mid,
                            ses_table_id the_tid, long* nr, long* nt, long* ntab) {
    
    ses_boolean return_value = SES_TRUE;
    /* int i; */
    int word_size = 0;
    
        
    /*  read nr and nt directly from the file */
    
    /******************************************************************/
    
    struct _ses_file_handle* pSFH = FILE_LIST[the_handle]->_the_handle;
    if (pSFH == (struct _ses_file_handle*)NULL) {
#ifdef DEBUG_PRINT
        printf("_get_grid_ascii: ses file handle null in _get_grid\n");
#endif
        return SES_SETUP_ERROR;
    }
    
    FILE* pFILE = pSFH->_c_file_handle;
    if (pFILE == (FILE*)NULL) {
#ifdef DEBUG_PRINT
        printf("_get_grid_ascii: c file handle null _get_grid \n");
#endif
        return SES_SETUP_ERROR;
    }
    
    struct _ses_directory* pDR = FILE_LIST[the_handle]->_directory;
    if (pDR == (struct _ses_directory*)NULL) {
#ifdef DEBUG_PRINT
        printf("_get_grid_ascii: directory null in _get_grid\n");
#endif
        return SES_SETUP_ERROR;
    }
    
    word_size = pSFH->_word_size;
#ifdef DEBUG_PRINT
    printf("_get_grid_ascii: word_size = %d\n", word_size);
#endif

    /******************************************************************/
    
    
    /*  get the grid */
    
    long my_nr = 0;
    long my_nt = 0;
    long my_ntab = -1;
    my_nr = 0;
    my_nt = 0;
    
    ses_boolean needs_flip = FILE_LIST[the_handle]->_the_handle->_needs_flip;
    
    long current_position = ftell(pFILE);
    struct _ses_index_record* current_index_record = FILE_LIST[the_handle]->_current_index_record;
    switch(the_tid) {
            
            /* ses_boolean didit_go; */
        case 100:
            
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
            long number_tables = _read_long_pFILE_ascii(pFILE, needs_flip);
            
            my_nr = number_tables * (SES_MAX_STRING_SIZE) + 1;
            my_nt = 1;
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 100. nr: %ld, nt = 1\n", my_nr);
#endif
            break;
            
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
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 101-199. nr: %ld, nt = 1\n", my_nr);
#endif
            break;
            
        case 201:
            my_nr = 5;
            my_nt = 1;
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 201. nr: %ld, nt = 1\n", my_nr);
#endif
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
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: 40\n");
#endif
            
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: 50\n");
#endif
            my_nr = _read_long_pFILE_ascii(pFILE, needs_flip);
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: 60, my_nr = %ld\n", my_nr);
#endif
            //my_nt = _read_long_pFILE_ascii(pFILE, needs_flip);
            my_nt = _read_long_tomax_pFILE_ascii(pFILE, word_size, needs_flip);
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 301-605. nr: %ld, nt = %ld\n", my_nr, my_nt);
#endif
            
            break;
            
        case 306:
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
            my_nr = _read_long_pFILE_ascii(pFILE, needs_flip);
            my_nt = 1;
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 306 nr: %ld, nt = %ld\n", my_nr, my_nt);
#endif
            
            break;
            
        case 401:
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
            //my_nt = _read_long_pFILE_ascii(pFILE, needs_flip);
            my_nt = _read_long_tomax_pFILE_ascii(pFILE, word_size, needs_flip);
            my_nr = 1;
            //  find ntab for the table
            if (current_index_record != (struct _ses_index_record*)NULL) {
                long table_nwds = _get_table_nwds(current_index_record, the_tid);
                long my_tables = (table_nwds - 1)/my_nt;
                if (my_tables != 7) {
                    my_ntab = my_tables;
                }
                
            }
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 401. nr: %ld, nt = %ld\n", my_nr, my_nt);
#endif
            break;
            
        case 411:
        case 412:
        case 431:
        case 501:
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
            my_nr = _read_long_pFILE_ascii(pFILE, needs_flip);
            my_nt = 1;
            //  find ntab for the table
            if ((the_tid == 411) && (current_index_record != (struct _ses_index_record*)NULL)) {
                long table_nwds = _get_table_nwds(current_index_record, the_tid);
                long my_tables = (table_nwds - 2 - my_nt)/my_nr;
                if (my_tables != 5) {
                    my_ntab = my_tables;
                }
                
            }
            if ((the_tid == 412) && (current_index_record != (struct _ses_index_record*)NULL)) {
                long table_nwds = _get_table_nwds(current_index_record, the_tid);
                long my_tables = (table_nwds - 2 - my_nt)/my_nr;
                if (my_tables != 5) {
                    my_ntab = my_tables;
                }
                
            }
            if ((the_tid == 431) && (current_index_record != (struct _ses_index_record*)NULL)) {
                long table_nwds = _get_table_nwds(current_index_record, the_tid);
                long my_tables = (table_nwds - 3)/my_nr;
                if (my_tables != 2) {
                    my_ntab = my_tables;
                }
                
            }
            
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: case 411~501. nr: %ld, nt = %ld\n", my_nr, my_nt);
#endif
            break;
            
            
            
            
        default:
            /* i = */ my_get_index(the_tid);
            
            /*  guess that nr and nt are right at the start, get them there */
            /* didit_go = */ _go_to_data_record_ascii(the_handle, the_mid, the_tid);
            my_nr = _read_long_pFILE_ascii(pFILE, needs_flip);
            //my_nt = _read_long_pFILE_ascii(pFILE, needs_flip);
            my_nt= _read_long_tomax_pFILE_ascii(pFILE, word_size, needs_flip);
            
#ifdef DEBUG_PRINT
            printf("_get_grid_ascii: DEFAULT. nr: %ld, nt = %ld\n", my_nr, my_nt);
#endif
            
            break;
            
    }
    
    int fseek_return = fseek(pFILE, current_position, SEEK_SET);
    if (fseek_return != 0) {
#ifdef DEBUG_PRINT
        printf("_get_grid: error in fseek in _get_grid \n");
#endif
        return SES_SETUP_ERROR;
    }
    
    *nr = my_nr;
    *nt = my_nt;
    *ntab = my_ntab;
    return return_value;
}
