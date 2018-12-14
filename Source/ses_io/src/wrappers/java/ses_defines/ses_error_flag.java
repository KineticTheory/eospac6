
package ses_defines;

public class ses_error_flag {

        public static final int SES_NO_ERROR = 0;
        public static final int SES_ARRAY_SIZE_ERROR = 1;
        public static final int SES_MEMORY_ALLOCATION_ERROR = 2;
        public static final int SES_OBJECT_CONSTRUCTION_ERROR = 3;
        public static final int SES_NULL_OBJECT_ERROR = 4;
        public static final int SES_OBJECT_COPY_ERROR = 5;
        public static final int SES_OBJECT_DESTRUCTION_ERROR = 6;
        public static final int SES_FUNCTION_FAIL_ERROR = 7;
        public static final int SES_INVALID_FILE_HANDLE = 8;
        public static final int SES_INVALID_MID = 9;
        public static final int SES_OBJECT_READY_ERROR = 10;
        public static final int SES_OPEN_ERROR = 11;
        public static final int SES_INVALID_TID = 12;
        public static final int SES_OBJECT_OUT_OF_RANGE = 13;
        public static final int SES_SETUP_ERROR = 14;
        public static final int SES_CLOSE_ERROR = 15;
        public static final int SES_FILE_READY_ERROR = 16;
        public static final int SES_FILE_WRITE_ERROR = 17;
        public static final int SES_READ_ERROR = 18;
        public static final int SES_CHANGE_ERROR = 19;
        public static final int SES_COMBINE_ERROR = 20;
        public static final int SES_COMMENTS_ERROR = 21;
        public static final int SES_DELETE_ERROR = 22;
        public static final int SES_NOT_IMPLEMENTED = 23;
        public static final int SES_INVALID_OPEN_TYPE = 24;
        public static final int SES_DOUBLE_SIZE_ERROR = 25;
        public static final int SES_TEST_ERROR = 26;
        public static final int SES_APPEND_ERROR_ERROR = 27;
        public static final int SES_NO_DATA_ERROR = 28;


	public int the_value;
        public ses_error_flag() {
		the_value = 0;
	}
  	public ses_error_flag(int initial_value) {
		the_value = initial_value;
	}
}
