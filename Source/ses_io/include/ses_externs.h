
#ifndef _SES_EXTERNS_H
#define _SES_EXTERNS_H


#ifndef _SES_INTERNAL_

extern struct _standard_table*    _the_table;
extern int                       NUMBER_TABLES;
extern struct _ses_file*          FILE_LIST[2000];
extern int                       _next_empty_file;
extern ses_error_flag            _latest_error;

#endif /* ifdef _SES_INTERNAL_ (defined or not in internals/_globals.c) */

#include "_binary.h"
#include "_ascii.h"
#include "_xml.h"
#include "_llnl_xml.h"

#endif


