


#ifdef DEBUG_32BIT
#define LONG             integer(kind=4)
#define LONG_POINTER     integer(kind=4), pointer
#define LONG_ARRAY_POINTER  integer(kind=4), dimension(:), pointer
#else
#define LONG             integer(kind=8)
#define LONG_POINTER     integer(kind=8), pointer
#define LONG_ARRAY_POINTER  integer(kind=8), dimension(:), pointer
#endif
#define MY_CHAR           character(len=1)

#define SES_LEN_LABEL 60

#define MAKE_LARGE_LARGER
#ifdef MAKE_LARGE_LARGER
#define SES_LEN_LARGE 2000
#else
#define SES_LEN_LARGE 270
#endif

#define SES_LABEL_STRING      character(len=SES_LEN_LABEL)
#define SES_LARGE_STRING      character(len=SES_LEN_LARGE)

#define SMALL_CHAR_POINTER     SES_LABEL_STRING, pointer
#define LARGE_CHAR_POINTER     SES_LARGE_STRING, pointer
#define INT              integer(kind=4)
#define DOUBLE           real(kind=8)
#define DOUBLE_POINTER   real(kind=8),pointer
#define DOUBLE_ARRAY_POINTER  real(kind=8), dimension(:), pointer

#define SES_FILE_HANDLE INT
#define SES_STRING LARGE_CHAR_POINTER
#define SES_OPEN_TYPE MY_CHAR
#define SES_ERROR_FLAG INT
#define SES_MATERIAL_ID LONG
#define SES_MATERIAL_ID_REFERENCE LONG_ARRAY_POINTER
#define SES_TABLE_ID LONG
#define SES_TABLE_ID_REFERENCE LONG_POINTER
#define SES_TABLE_SIZES_REFERENCE LONG_POINTER
#define SES_LABEL SMALL_CHAR_POINTER
#define SES_NUMBER LONG
#define SES_NUMBER_REFERENCE LONG_POINTER
#define SES_BOOLEAN INT
#define SES_WORD DOUBLE
#define SES_WORD_REFERENCE DOUBLE_ARRAY_POINTER
#define SES_FILE_TYPE MY_CHAR
#define SES_ARRAY_ORDER MY_CHAR

#define SES_ASCII_WORD_TYPE MY_CHAR






