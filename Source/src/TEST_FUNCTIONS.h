/*********************************************************************
 * Unit Test API Functions
 * ---------------------------------------------------------
 * Filetype: (HEADER)
 * 
 * Copyright -- see file named COPYRIGHTNOTICE
 * 
 ********************************************************************/
#ifndef TEST_FUNCTIONS_HEADERFILE
#define TEST_FUNCTIONS_HEADERFILE
#include "eos_universal_types.h"
#include "stdlib.h"

#ifdef __cplusplus
# define _EXTERN_C_HEAD_ extern "C" {
# define _EXTERN_C_TAIL_ }
#else
# define _EXTERN_C_HEAD_
# define _EXTERN_C_TAIL_
#endif

#define ERROR_TO_TEXT(i) ((i==EOS_xHi_yHi) ? "EOS_xHi_yHi" : \
                          (i==EOS_xHi_yOk) ? "EOS_xHi_yOk" : \
                          (i==EOS_xHi_yLo) ? "EOS_xHi_yLo" : \
                          (i==EOS_xOk_yLo) ? "EOS_xOk_yLo" : \
                          (i==EOS_xLo_yLo) ? "EOS_xLo_yLo" : \
                          (i==EOS_xLo_yOk) ? "EOS_xLo_yOk" : \
                          (i==EOS_xLo_yHi) ? "EOS_xLo_yHi" : \
                          (i==EOS_xOk_yHi) ? "EOS_xOk_yHi" : \
                          (i==EOS_UNDEFINED) ? "EOS_UNDEFINED" : \
                          (i==EOS_CONVERGENCE_FAILED) ? "EOS_CONVERGENCE_FAILED" : \
                          (i==EOS_CANT_INVERT_DATA) ? "EOS_CANT_INVERT_DATA" : \
                          (i==EOS_OK     ) ? "EOS_OK" : \
                          "INVALID EXTRAP ERROR" \
                         )

_EXTERN_C_HEAD_

void get_data(void);
int insensitive_strcmp(const char * cs,const char * ct);
EOS_CHAR* get_tableType_str(EOS_INTEGER t);
EOS_INTEGER get_tableType_FromStr(EOS_CHAR *s);
EOS_CHAR* get_tableType_description(EOS_INTEGER t);
EOS_INTEGER get_no_tables(void);
EOS_INTEGER get_no_handles(void);
EOS_INTEGER get_no_alloc(void);
EOS_INTEGER get_no_generalOptions(void);
EOS_INTEGER get_no_tableOptions(void);
EOS_BOOLEAN get_generalOptions_bval(EOS_INTEGER th, EOS_INTEGER i);
EOS_INTEGER get_generalOptions_ival(EOS_INTEGER th, EOS_INTEGER i);
EOS_REAL get_generalOptions_rval(EOS_INTEGER th, EOS_INTEGER i);
EOS_INTEGER get_generalOption_type(EOS_INTEGER i);
EOS_CHAR* get_generalOption_flag(EOS_INTEGER i);
EOS_BOOLEAN get_tableOptions_bval(EOS_INTEGER th, EOS_INTEGER i);
EOS_INTEGER get_tableOptions_ival(EOS_INTEGER th, EOS_INTEGER i);
EOS_REAL get_tableOptions_rval(EOS_INTEGER th, EOS_INTEGER i);
EOS_INTEGER get_tableOption_type(EOS_INTEGER th, EOS_INTEGER i);
EOS_CHAR* get_tableOption_flag(EOS_INTEGER th, EOS_INTEGER i);
EOS_BOOLEAN get_interpolationOptions_bval(EOS_INTEGER th, EOS_INTEGER optionFlag);
EOS_INTEGER get_table_type(EOS_INTEGER th);
EOS_INTEGER get_matID(EOS_INTEGER th);
EOS_INTEGER get_creationDate(EOS_INTEGER th);
EOS_INTEGER get_modificationDate(EOS_INTEGER th);
EOS_INTEGER get_matIdListFromFile(EOS_INTEGER** list, EOS_CHAR *fn);
EOS_INTEGER get_matIdList(EOS_INTEGER** list);
EOS_INTEGER get_matidFileName (EOS_INTEGER mat, EOS_CHAR **fn);
EOS_CHAR* get_tableHandleFileName (EOS_INTEGER th);
EOS_INTEGER print_FileList (EOS_INTEGER limit, EOS_CHAR *filter);
EOS_INTEGER get_matidTableInfo (EOS_INTEGER mat, EOS_INTEGER **tableList, EOS_INTEGER **tableSize);
void* get_dataObjects_ptr(void);
void* get_tableHandlesMap_ptr(void);
EOS_INTEGER get_tableHandlesMap_val(EOS_INTEGER i);
void* get_tableTypes_ptr(void);
EOS_INTEGER get_tableTypes_val(EOS_INTEGER i);
void* get_errorCodes_ptr(void);
EOS_INTEGER get_errorCodes_val(EOS_INTEGER i);
void* get_isHandlePublic_ptr(void);
void get_RecordType1_XYF(EOS_INTEGER tableHandle, EOS_REAL **X, EOS_REAL **Y, EOS_REAL ***F, EOS_INTEGER *nX, EOS_INTEGER *nY);
void get_UpperLowerBndsRecordType1_XY(EOS_INTEGER tableHandle, EOS_REAL *xMin, EOS_REAL *xMax, EOS_REAL *yMin, EOS_REAL *yMax );
void generate_RandomPoints(EOS_REAL *xList, EOS_REAL *yList, EOS_INTEGER numPts, EOS_REAL xMin, EOS_REAL xMax, EOS_REAL yMin, EOS_REAL yMax, EOS_BOOLEAN dosort);
void generate_Log10DistributedPoints(EOS_REAL *xList, EOS_REAL *yList, EOS_INTEGER numPts, EOS_REAL xMin, EOS_REAL xMax, EOS_REAL yMin, EOS_REAL yMax);
EOS_INTEGER __eos_GetExpandedGrid (EOS_INTEGER nAdd, EOS_INTEGER *nX, EOS_REAL **X);
EOS_INTEGER get_dataTypeCategory(EOS_INTEGER t);
EOS_INTEGER get_dataTypeSubCategory(EOS_INTEGER t);
EOS_INTEGER get_dataTypeSubTable(EOS_INTEGER t);
EOS_INTEGER get_dataTypeReference1(EOS_INTEGER t);
EOS_INTEGER get_dataTypeReference2(EOS_INTEGER t);
EOS_INTEGER get_dataTypeDepVar(EOS_INTEGER t);
EOS_INTEGER get_dataTypeIndepVar1(EOS_INTEGER t);
EOS_INTEGER get_dataTypeIndepVar2(EOS_INTEGER t);
EOS_INTEGER get_VarListIndex(EOS_INTEGER t);
EOS_CHAR* get_VarStr(EOS_INTEGER t, EOS_INTEGER flag);
void generate_random_gridpts(void);
int fcmp(EOS_REAL u, EOS_REAL v, EOS_REAL rel_diff, EOS_REAL abs_diff);
void print_nr_and_nt(EOS_INTEGER th);
EOS_CHAR* get_DataTypeDescription(EOS_INTEGER t);
EOS_INTEGER get_DataType(EOS_INTEGER table, EOS_INTEGER subtable, EOS_INTEGER *type);
EOS_INTEGER get_eosDataType(EOS_INTEGER i);
EOS_INTEGER get_eosTableNum(EOS_INTEGER i);
EOS_INTEGER get_eosSubTableNum(EOS_INTEGER i);
EOS_INTEGER get_fileContent(EOS_CHAR *fn, EOS_CHAR **str);
void mt_init(void);
double mt_random(void);
void latinCube(double *vdata, const double *a, const double *b, int nparam, int nlev/* , unsigned short xsubi[3] */);
void display_gEosDataMap_tableHandlesMap_details(void);
int getSamplesLatinHyperCube(int N, EOS_REAL v_lower, EOS_REAL v_upper, EOS_REAL *v);
void test_if_globals_are_free(void);

_EXTERN_C_TAIL_

#endif
