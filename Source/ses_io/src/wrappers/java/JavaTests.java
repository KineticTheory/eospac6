// The JavaTests.Java file

import ses_defines.*;
import MySesIO.*;

public class JavaTests
{

    public static SesIO my_ses_io;
    // The main program
    public static void main(String[] args)
    {

        System.out.println("Entered main for JavaTests");
        System.out.println("WRAP TEST:  SesArraySizeNext");
        test_ses_array_size_next();
        System.out.println("WRAP TEST:  SesChangeNext");
        test_ses_change_next();
        System.out.println("WRAP TEST:  SesClose");
        test_ses_close();
        System.out.println("WRAP TEST:  SesCombine");
        test_ses_combine();
        System.out.println("WRAP TEST:  SesDate");
        test_ses_date();
        System.out.println("WRAP TEST:  SesDefineTable");
        test_ses_define_table();
        System.out.println("WRAP TEST:  SesDeleteNext");
        test_ses_delete_next();
        System.out.println("WRAP TEST:  SesDeleteTable");
        test_ses_delete_table();
	System.out.println("WRAP TEST: SesGetLabel");
        test_ses_get_label();
        System.out.println("WRAP TEST:  SesGetMaterialID");
        test_ses_get_material_id();
        System.out.println("WRAP TEST: SesGetMaterials"); 
        test_ses_get_materials();
        System.out.println("WRAP TEST:  SesGetTableIDS");
        test_ses_get_table_ids();
        System.out.println("WRAP TEST:  SesHasNext");
        test_ses_has_next();
        System.out.println("WRAP TEST:  SesIndicatesError");
        test_ses_indicates_error();
        System.out.println("WRAP TEST:  SesIsValid");
        test_ses_is_valid();
        System.out.println("WRAP_TEST:  SesOpen");
        test_ses_open();
        System.out.println("WRAP_TEST: SesPrintErrorCondition");
        test_ses_print_error_condition();
        System.out.println("WRAP_TEST:  SesRead1D");
        test_ses_read_1d();
	System.out.println("WRAP_TEST:  SesRead2D");
        test_ses_read_2d();
        System.out.println("WRAP_TEST:  SesRead3D");
        test_ses_read_3d();
        System.out.println("WRAP_TEST:  SesReadNamedArray");
        test_ses_read_named_array();
        System.out.println("WRAP_TEST:  SesReadNext");
        test_ses_read_next();
        System.out.println("WRAP_TEST:  SesReadNumber");
        test_ses_read_number();
        System.out.println("WRAP_TEST:  SesReadWord");
        test_ses_read_word();
        System.out.println("WRAP_TEST:  SesSetArrayOrder");
        test_ses_set_array_order();
        System.out.println("WRAP_TEST:  SesSetDate");
        test_ses_set_date();
        System.out.println("WRAP_TEST:  SesSetGrid");
        test_ses_set_grid();
        System.out.println("WRAP_TEST:  SetSetLabel");
        test_ses_set_label();
        System.out.println("WRAP_TEST:  SesSetMaterialOrder");
        test_ses_set_material_order();
        System.out.println("WRAP_TEST:  SesSetSignificantDigits");
        test_ses_set_significant_digits();
        System.out.println("WRAP_TEST:  SesSetValidate");
        test_ses_set_validate();
        System.out.println("WRAP_TEST:  SesSetVersion");
        test_ses_set_version();
        System.out.println("WRAP_TEST:  SesSetup");
        test_ses_setup();
        System.out.println("WRAP_TEST:  SesSkip");
        test_ses_skip();
        System.out.println("WRAP_TEST:  SesVersion");
        test_ses_version();
        System.out.println("WRAP_TEST:  SesWrite1D");
        test_ses_write_1d();
        System.out.println("WRAP_TEST:  SesWrite2D");
        test_ses_write_2d();
	System.out.println("WRAP_TEST:  SesWrite3D");
        test_ses_write_3d();
	System.out.println("WRAP_TEST:  SesWriteComments");
        test_ses_write_comments();
        System.out.println("WRAP_TEST:  SesWriteNext");
        test_ses_write_next();
        System.out.println("WRAP_TEST:  SesWriteNumber");
        test_ses_write_number();
	System.out.println("WRAP_TEST:  SesWritePairs");
        test_ses_write_pairs();
	System.out.println("WRAP_TEST:  SesWriteWord");
        test_ses_write_word();


        //Not yet implemented correctly

        //System.out.println("WRAP_TEST:  SesReadPairs");
        //////test_ses_read_pairs(); 
 
        //Not yet implemented

	//System.out.println("WRAP_TEST:  SesAccessDirectory");
        //////test_ses_access_directory();
        //System.out.println("WRAP_TEST:  SesAccessTableIndex");
	//////test_ses_access_table_index();
        //System.out.println("WRAP_TEST:  SesComments");
        //////test_ses_comments();
	System.out.println("WRAP_TEST:  SesSetFormat");
	test_ses_set_format();
        
     }

     public static void test_ses_access_table_index() {
     }
     public static void test_ses_access_directory() {
     }
     public static void test_ses_set_format() {
        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame_format");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

        ses_error_flag grid_error = new ses_error_flag();
	grid_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, 1, 1, -1);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_error_flag set_format = new ses_error_flag();
	set_format.the_value = my_ses_io.SesSetFormat(the_handle.the_value, 'A');
        System.out.println("TEST:  set_format returned " + set_format.the_value);

    }
     public static void test_ses_comments() {
        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(101);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

	ses_string the_comments = new ses_string();
	
	the_comments.the_value = my_ses_io.SesComments(the_handle.the_value);
	System.out.println("TEST:  the_comments are " + the_comments.the_value);

     }


     public static void test_ses_array_size_next() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

 	ses_number return_value = new ses_number(0);
        try {

  	   System.out.println("TEST:  Calling ses_array_size_next with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST: The array size next is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");

        ses_error_flag didit_close = new ses_error_flag();
        didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);

        if (return_value.the_value >= 0) {
	    System.out.println("TEST: ArraySizeNext:  PASS");
	}
        else {
	    System.out.println("TEST: ArraySizeNext:  FAIL");
	}
     }   

     public static void test_ses_change_next() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame_change");
        ses_open_type open_flags = new ses_open_type('C');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_label myLabel = new ses_label();

	ses_error_flag return_value = new ses_error_flag();
	ses_error_flag skip_errors = new ses_error_flag();
	ses_error_flag didit_close = new ses_error_flag();

  	ses_number dim1 = new ses_number(0);

        try {

	    while (my_ses_io.SesHasNext(the_handle.the_value) == true) {

                myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
		System.out.println("TEST:  myLabel is " + myLabel.the_value);
		if (myLabel.the_value.equals("p - pressure (GPa)")) {

		    dim1.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
	            ses_word_reference buffer = new ses_word_reference((int)dim1.the_value, 0.0);

		    for (int i = 0; i < dim1.the_value; i++) {
			buffer.the_value[i] = 3.0;
		    }
		    return_value.the_value = 
		        my_ses_io.SesChangeNext(the_handle.the_value, buffer.the_value, dim1.the_value);
		    System.out.println("TEST: The return_value is  " + return_value.the_value);
		}
                else {
		    System.out.println("TEST:  calling ses_skip");
                    skip_errors.the_value = my_ses_io.SesSkip(the_handle.the_value);
		}
	    }

            didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException: " + e.getMessage());
        }
        System.out.println("\n");
     }

     public static void test_ses_close() {

        my_ses_io = new SesIO();
 	ses_error_flag return_value = new ses_error_flag();

        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        try {

  	   System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesClose(the_handle.the_value);
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
     }   
     public static void test_ses_combine() {

        my_ses_io = new SesIO();

 	ses_error_flag return_value = new ses_error_flag();

        ses_string filename1 = new ses_string("sesame1");
        ses_open_type open_flags1 = new ses_open_type('R');
	ses_file_handle the_handle1 = new ses_file_handle();
	the_handle1.the_value = my_ses_io.SesOpen(filename1.the_value, open_flags1.the_value);
        ses_string filename2 = new ses_string("sesame2");
        ses_open_type open_flags2 = new ses_open_type('R');
	ses_file_handle the_handle2 = new ses_file_handle();
	the_handle2.the_value = my_ses_io.SesOpen(filename2.the_value, open_flags2.the_value);

        ses_string new_filename = new ses_string("new_filename");

        try {

  	   System.out.println("TEST:  Calling ses_combine with handle1 = " + the_handle1.the_value);
  	   System.out.println("TEST:  Calling ses_combine with handle2 = " + the_handle2.the_value);
           System.out.println("TEST:  Calling ses_combine with new_filename = " + new_filename.the_value);
           return_value.the_value = my_ses_io.SesCombine(the_handle1.the_value, the_handle2.the_value, new_filename.the_value);
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
     } 

     public static void test_ses_date() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

 	ses_string return_value = new ses_string();
        try {

  	   System.out.println("TEST:  Calling ses_date with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesDate(the_handle.the_value);
           System.out.println("TEST: The date is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }

	ses_error_flag didit_close = new ses_error_flag();

  	 System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
         didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
         System.out.println("TEST: The return_value is " + didit_close.the_value);
         System.out.println("\n");
     }   
     public static void test_ses_define_table() {

        my_ses_io = new SesIO();

	ses_error_flag return_value = new ses_error_flag();
	ses_table_id the_tid = new ses_table_id(657);
        ses_label description = new ses_label("hello belly");
        int num_arrays = 4;
	int nr = 1;
	int nt = 1;

        long size_arrays[] = new long[4];
        size_arrays[0] = 3;
        size_arrays[1] = 4;
        size_arrays[2] = 5;
        size_arrays[3] = 6;
        String the_labels[] = new String[4];
        try {
	   the_labels[0] = new String("hello");
	   the_labels[1] = new String("hello, mate");
	   the_labels[2] = new String("guten tag");
           the_labels[3] = new String("allo");

	   System.out.println("TEST: Passing table_id of " + the_tid.the_value);
	   System.out.println("TEST: Passing description  of " + description.the_value);
	   System.out.println("TEST:  Passing num_arrays of " + num_arrays);
	   System.out.println("TEST:  Passing size_arrays[0] of " + size_arrays[0]);
	   System.out.println("TEST:  Passing size_arrays[1] of " + size_arrays[1]);
	   System.out.println("TEST:  Passing size_arrays[2] of " + size_arrays[2]);
	   System.out.println("TEST:  Passing size_arrays[3] of " + size_arrays[3]);
	   System.out.println("TEST:  Passing the_labels[0] of " + the_labels[0]);
	   System.out.println("TEST:  Passing the_labels[1] of " + the_labels[1]);
	   System.out.println("TEST:  Passing the_labels[2] of " + the_labels[2]);
	   System.out.println("TEST:  Passing the_labels[3] of " + the_labels[3]);
 	   return_value.the_value = my_ses_io.SesDefineTable(the_tid.the_value, 
                  description.the_value, nr, nt, num_arrays, size_arrays, the_labels);
           System.out.println("TEST: The return_value is  " + return_value.the_value);
       }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
     }

    public static void test_ses_delete_next() {


        my_ses_io = new SesIO();

        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	the_handle.the_value = 1;
 
        try {

  	   System.out.println("TEST:  Calling ses_delete_next with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesDeleteNext(the_handle.the_value);
           System.out.println("TEST: The return error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_delete_table() {

        my_ses_io = new SesIO();

        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame_delete_table");
        ses_open_type open_flags = new ses_open_type('C');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
 
        try {

  	   System.out.println("TEST:  Calling ses_delete_table with handle = " 
                                     + the_handle.the_value);
           return_value.the_value = my_ses_io.SesDeleteTable(the_handle.the_value);
           System.out.println("TEST: The return error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
 	ses_error_flag didit_close = new ses_error_flag();

  	 System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
         didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
         System.out.println("TEST: The return_value is " + didit_close.the_value);
         System.out.println("\n");
    }

    public static void test_ses_get_label() {
        my_ses_io = new SesIO();

        ses_label return_value = new ses_label();
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
 
        try {

  	   System.out.println("TEST:  Calling ses_get_label with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST: The return label is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
	ses_error_flag didit_close = new ses_error_flag();

  	 System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
         didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
         System.out.println("TEST: The return_value is " + didit_close.the_value);
         System.out.println("\n");
        
    }

    public static void test_ses_get_material_id() {
        my_ses_io = new SesIO();

        ses_material_id return_value = new ses_material_id();
	ses_string material_name = new ses_string("lead");
 
        try {

	    System.out.println("TEST:  Calling ses_get_material_id with material name = "
			       + material_name.the_value);
	    return_value.the_value = my_ses_io.SesGetMaterialID(material_name.the_value);
            System.out.println("TEST: The material id is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
   }

    public static void test_ses_get_materials() {
        my_ses_io = new SesIO();

  
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
        int the_size = 3;
 
        try {

  	   System.out.println("TEST:  Calling ses_get_materials with handle = " 
                           + the_handle.the_value);


          ses_material_id_reference return_value = new ses_material_id_reference();
          return_value.the_value = my_ses_io.SesGetMaterials(the_handle.the_value);
           System.out.println("TEST: returned from SesGetMaterials");
	   return_value.the_size = return_value.the_value.length;
           System.out.println("TEST:  the_size is " + return_value.the_size);
    
           if (return_value.the_size >= 0) {
	       //System.out.println("TEST:  rv is " + return_value.the_value);
	       for (int j = 0; j < (int)(return_value.the_size); j++) {
                 System.out.println("TEST: The material list [" + j + "]  is " + return_value.the_value[j]);
	         
	       }
	   }
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
	ses_error_flag didit_close = new ses_error_flag();

  	 System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
         didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
         System.out.println("TEST: The return_value is " + didit_close.the_value);
         System.out.println("\n");
    }

    public static void test_ses_get_table_ids() {
        my_ses_io = new SesIO();

        ses_table_id_reference return_value = new ses_table_id_reference();

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
        int the_size = 3;
 
        try {

  	   System.out.println("TEST:  Calling ses_get_table ids with handle = " 
                           + the_handle.the_value);
           return_value.the_value = my_ses_io.SesGetTableIDS(the_handle.the_value, the_mid.the_value);
           return_value.the_size = return_value.the_value.length;
           System.out.println("TEST: returned from SesGetTableIDs");
           System.out.println("TEST:  the_size is " + return_value.the_size);
           if (return_value.the_size > 0) {
	       for (int j = 0; j < return_value.the_size; j++) {
                 System.out.println("TEST: The table id list  is " + return_value.the_value[j]);
	       }
	   }
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_has_next() {
        my_ses_io = new SesIO();
        ses_boolean return_value = new ses_boolean(false);
	ses_file_handle the_handle = new ses_file_handle();
        long the_size;
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_label myLabel = new ses_label();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
	ses_error_flag skip_errors = new ses_error_flag();
        ses_error_flag didit_close = new ses_error_flag();
 
        boolean saw_loop = false;
       try {

 	    
	    while (my_ses_io.SesHasNext(the_handle.the_value) == true) {

                myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
		System.out.println("TEST:  myLabel is " + myLabel.the_value);
		System.out.println("TEST:  calling ses_skip");
                skip_errors.the_value = my_ses_io.SesSkip(the_handle.the_value);
                saw_loop = true;
		
	    }

            didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException: " + e.getMessage());
        }
        System.out.println("Saw loop is " + saw_loop);
        System.out.println("\n");
    }

    public static void test_ses_indicates_error() {
        my_ses_io = new SesIO();
        ses_boolean return_value = new ses_boolean(false);
	ses_error_flag the_error_flag = new ses_error_flag(); 
        the_error_flag.the_value = ses_error_flag.SES_INVALID_FILE_HANDLE;
        try {

  	   System.out.println("TEST:  Calling ses_indicates_error with error flag = " 
                           + the_error_flag.the_value);
           return_value.the_value = my_ses_io.SesIndicatesError(the_error_flag.the_value);
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_is_valid() {
        my_ses_io = new SesIO();
        ses_boolean return_value = new ses_boolean(false);
	ses_file_handle the_handle = new ses_file_handle(); 
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        try {

	   System.out.println("TEST:  Calling ses_is_valid with handle of "  + the_handle.the_value);
           return_value.the_value = my_ses_io.SesIsValid(the_handle.the_value);
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_open() {
        my_ses_io = new SesIO();

        ses_file_handle return_value = new ses_file_handle();
	ses_string filename = new ses_string("sesame");
        ses_open_type the_flag = new ses_open_type('R');
 
        try {

  	   System.out.println("TEST:  Calling ses_open with filename = " + filename.the_value);
           System.out.println("TEST:  Calling ses_open with the_flag = " + the_flag.the_value);
           return_value.the_value = my_ses_io.SesOpen(filename.the_value, the_flag.the_value);
           System.out.println("TEST: The return handle is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_print_error_condition() {
        my_ses_io = new SesIO();
        ses_string return_value = new ses_string();
	ses_file_handle the_handle = new ses_file_handle();
        ses_error_flag the_flag = new ses_error_flag();
        ses_string filename = new ses_string("sesame");
        ses_open_type the_flags = new ses_open_type('R');

        the_handle.the_value = my_ses_io.SesOpen(filename.the_value, the_flags.the_value);
 
        try {

           System.out.println("TEST: The return value is " + return_value.the_value);
  	   System.out.println("TEST:  Calling ses_print_error_condition with handle = " 
                                       + the_handle.the_value);
           return_value.the_value = my_ses_io.SesPrintErrorCondition(the_handle.the_value);
           System.out.println("TEST:  After SesPrintErrorCondition\n");
           System.out.println("TEST: The return value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }
 
    public static void test_ses_read_1d() {

        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

	ses_number array_size = new ses_number(0);
        ses_label myLabel = new ses_label();
        ses_error_flag return_value = new ses_error_flag();


						  
        try {

           System.out.println("TEST:  Calling ses_read_number to get nr");

           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

           ses_number nr = new ses_number(0);
           nr.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nr is " + nr.the_value);
           ses_number nt = new ses_number(0);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           nt.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nt is " + nt.the_value);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

	   ses_error_flag return_flag = new ses_error_flag();
	   ses_word_reference buffer = new ses_word_reference((int)array_size.the_value);
	   ses_number dim = new ses_number(array_size.the_value);
	   System.out.println("TEST:  before, buffer(3) is " + buffer.the_value[3]);
	   buffer.the_value  = my_ses_io.SesRead1D(the_handle.the_value, (int)dim.the_value);
	   System.out.println("TEST:  after, buffer(3) is " + buffer.the_value[3]);
   
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_read_2d() {
        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

	ses_number array_size = new ses_number(0);
        ses_label myLabel = new ses_label();
        ses_error_flag return_value = new ses_error_flag();


						  
        try {

           System.out.println("TEST:  Calling ses_read_number to get nr");

           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

           ses_number nr = new ses_number(0);
           nr.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nr is " + nr.the_value);
           ses_number nt = new ses_number(0);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           nt.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nt is " + nt.the_value);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

	   ses_error_flag return_flag = new ses_error_flag();
	   ses_word_reference buffer = new ses_word_reference((int)array_size.the_value);
	   ses_number dim = new ses_number(array_size.the_value);
	   System.out.println("TEST:  before, buffer(3) is " + buffer.the_value[3]);
	   buffer.the_value = my_ses_io.SesRead1D(the_handle.the_value, (int)dim.the_value);
	   System.out.println("TEST:  after, buffer(3) is " + buffer.the_value[3]);

           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);
	   System.out.println("TEST:  before, buffer(3) is " + buffer.the_value[3]);
	   buffer.the_value = my_ses_io.SesRead1D(the_handle.the_value, (int)dim.the_value);
	   System.out.println("TEST:  after, buffer(3) is " + buffer.the_value[3]);

           ses_word_reference buffer2 = new ses_word_reference((int)(nr.the_value*nt.the_value));
	   ses_number dim1 = new ses_number(nr.the_value);
           ses_number dim2 = new ses_number(nt.the_value);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);
	   System.out.println("TEST:  before, buffer2(3) is " + buffer2.the_value[3]);
	   buffer2.the_value = my_ses_io.SesRead2D(the_handle.the_value, (int)dim1.the_value, (int)dim2.the_value);
	   System.out.println("TEST:  after, buffer2(3) is " + buffer2.the_value[3]);
   
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");    }

    public static void test_ses_read_3d() {
        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

	ses_number array_size = new ses_number(0);
        ses_label myLabel = new ses_label();
        ses_error_flag return_value = new ses_error_flag();


						  
        try {

           System.out.println("TEST:  Calling ses_read_number to get nr");

           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

           ses_number nr = new ses_number(0);
           nr.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nr is " + nr.the_value);
           ses_number nt = new ses_number(0);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           nt.the_value = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST:  after SesReadNumber");
           System.out.println("TEST:  nt is " + nt.the_value);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);

	   ses_error_flag return_flag = new ses_error_flag();
	   ses_word_reference buffer = new ses_word_reference((int)array_size.the_value);
	   ses_number dim = new ses_number(array_size.the_value);
	   System.out.println("TEST:  before, buffer(3) is " + buffer.the_value[3]);
	   buffer.the_value = my_ses_io.SesRead1D(the_handle.the_value, (int)dim.the_value);
	   System.out.println("TEST:  after, buffer(3) is " + buffer.the_value[3]);

           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);
	   System.out.println("TEST:  before, buffer(3) is " + buffer.the_value[3]);
	   buffer.the_value = my_ses_io.SesRead1D(the_handle.the_value, (int)dim.the_value);
	   System.out.println("TEST:  after, buffer(3) is " + buffer.the_value[3]);

           ses_word_reference buffer2 = new ses_word_reference((int)(nr.the_value*nt.the_value));
	   ses_number dim1 = new ses_number(nr.the_value);
           ses_number dim2 = new ses_number(nt.the_value);
           myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
           System.out.println("TEST:  myLabel is " + myLabel.the_value);
           array_size.the_value = my_ses_io.SesArraySizeNext(the_handle.the_value);
           System.out.println("TEST:  array_size is " + array_size.the_value);
	   System.out.println("TEST:  before, buffer2(3) is " + buffer2.the_value[3]);
	   buffer2.the_value = my_ses_io.SesRead3D(the_handle.the_value, (int)dim1.the_value, (int)dim2.the_value, 1);
	   System.out.println("TEST:  after, buffer2(3) is " + buffer2.the_value[3]);
   
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");    
     }

    public static void test_ses_read_named_array() {
        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

        ses_error_flag return_value = new ses_error_flag();
        ses_label the_label = new ses_label("nt (number temperatures)");
        ses_word_reference the_buffer = new ses_word_reference(1);
  
        try {

  	   System.out.println("TEST:  Calling ses_read_named_array with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_read_named_array with label = " + the_label.the_value);
           the_buffer.the_value = my_ses_io.SesReadNamedArray(the_handle.the_value,
                                                              the_label.the_value, 1);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_read_next() {
        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

        ses_error_flag return_value = new ses_error_flag();
        ses_label the_label = new ses_label("nt (number temperatures)");
        ses_word_reference the_buffer = new ses_word_reference(1);
  
        try {

  	   System.out.println("TEST:  Calling ses_read_next with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_read_next with label = " + the_label.the_value);
           the_buffer.the_value = my_ses_io.SesReadNext(the_handle.the_value,
							1);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
 
    }

    public static void test_ses_read_number() {
        my_ses_io = new SesIO();

        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

        ses_error_flag return_value = new ses_error_flag();
        ses_label the_label = new ses_label("nt (number temperatures)");
        ses_number_reference the_buffer = new ses_number_reference(1);
  
        try {

  	   System.out.println("TEST:  Calling ses_read_number with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_read_number with label = " + the_label.the_value);
           the_buffer.the_value[0] = my_ses_io.SesReadNumber(the_handle.the_value);
           System.out.println("TEST: return_value  is " + the_buffer.the_value[0]);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_read_pairs() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();

	ses_file_handle the_handle = new ses_file_handle();
	ses_word_reference the_buffer1 = new ses_word_reference(3);
	ses_word_reference the_buffer2 = new ses_word_reference(3);
        ses_number dim = new ses_number(3);
  
        try {

  	   System.out.println("TEST:  Calling ses_read_pairs  with handle = " 
                                               + the_handle.the_value);
           return_value.the_value = my_ses_io.SesReadPairs(the_handle.the_value,
							   the_buffer1.the_value, 
                                                           the_buffer2.the_value, dim.the_value);
           System.out.println("TEST: The returned buffer1[0] is " + the_buffer1.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_read_word() {
        my_ses_io = new SesIO();
        //  open the file and get the file handle

	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        //  setup the file to a specific material id and table id

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);
	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, 
                         the_mid.the_value, the_tid.the_value);

        ses_error_flag return_value = new ses_error_flag();
        ses_label the_label = new ses_label("nt (number temperatures)");
        ses_word_reference the_buffer = new ses_word_reference(1);
  
        try {

  	   System.out.println("TEST:  Calling ses_read_word with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_read_word with label = " + the_label.the_value);
           the_buffer.the_value[0] = my_ses_io.SesReadWord(the_handle.the_value);
           System.out.println("TEST: return_value  is " + the_buffer.the_value[0]);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }
 

    public static void test_ses_set_array_order() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	ses_array_order the_order = new ses_array_order('C');
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);


        try {

  	   System.out.println("TEST:  Calling ses_set_array_order with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_array_order with order = " + the_order.the_value);
           return_value.the_value = my_ses_io.SesSetArrayOrder(the_handle.the_value,
							       the_order.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_set_date() {
        my_ses_io = new SesIO();
	long the_date = 34244;
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	ses_array_order the_order = new ses_array_order('C');
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        try {

  	   System.out.println("TEST:  Calling ses_set_date with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_date with date = " + the_date);
           return_value.the_value = my_ses_io.SesSetDate(the_handle.the_value,
							 the_date);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
   }

    public static void test_ses_set_grid() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	int nr = 3;
        int nt = 6;
        int ntab = -1;
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame_write_test");
        ses_open_type open_flags = new ses_open_type('W');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        try {

  	   System.out.println("TEST:  Calling ses_set_grid with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_grid with nr = " + nr);
 	   System.out.println("TEST:  Calling ses_set_grid with nt = " + nt);
 	   System.out.println("TEST:  Calling ses_set_grid with ntab = " + ntab);
           return_value.the_value = my_ses_io.SesSetGrid(the_handle.the_value,
							 nr, nt, ntab);
           System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
           System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

  	   setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_set_label() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	ses_label the_label = new ses_label("the_label");

        ses_string filename = new ses_string("ses_writer");
        ses_open_type open_flags = new ses_open_type('W');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

	int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_error_flag setup_error = new ses_error_flag();


        try {

 	   System.out.println("TEST:  Calling ses_set_grid with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_grid with nr = " + nr);
 	   System.out.println("TEST:  Calling ses_set_grid with nt = " + nt);
 	   System.out.println("TEST:  Calling ses_set_grid with ntab = " + ntab);
           return_value.the_value = my_ses_io.SesSetGrid(the_handle.the_value,
							 nr, nt, ntab);
           System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
           System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

  	   setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);  
 	   System.out.println("TEST:  Calling ses_set_label with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_label with label = " + the_label.the_value);
           return_value.the_value = my_ses_io.SesSetLabel(the_handle.the_value,
							       the_label.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_set_material_order() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        try {

  	   System.out.println("TEST:  Calling ses_set_material_order with handle = " 
                                               + the_handle.the_value);
           return_value.the_value = my_ses_io.SesSetMaterialOrder(the_handle.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_set_significant_digits() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
        long the_digits = 6;
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        try {

  	   System.out.println("TEST:  Calling ses_set_significant_digits with handle = " 
                                               + the_handle.the_value);
           return_value.the_value = my_ses_io.SesSetSignificantDigits(the_handle.the_value, the_digits);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_set_validate() {
        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        try {

  	   System.out.println("TEST:  Calling ses_set_validate with handle = " 
                                               + the_handle.the_value);
           return_value.the_value = my_ses_io.SesSetValidate(the_handle.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
       
    }

    public static void test_ses_set_version() {
        my_ses_io = new SesIO();


        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	long the_version = 342342;
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);
        try {

  	   System.out.println("TEST:  Calling ses_set_version with handle = " 
                                               + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_set_version with vresion = " + the_version);
           return_value.the_value = my_ses_io.SesSetVersion(the_handle.the_value,
							       the_version);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_setup() {

        my_ses_io = new SesIO();
        ses_error_flag return_value = new ses_error_flag();
	ses_file_handle the_handle = new ses_file_handle();
	ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        try {

  	   System.out.println("TEST:  Calling ses_setup with handle = " 
                                               + the_handle.the_value);
           return_value.the_value = my_ses_io.SesSetup(the_handle.the_value,
						       the_mid.the_value, the_tid.the_value);
           System.out.println("TEST: The error flag is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    
    }

    public static void test_ses_skip() {

        my_ses_io = new SesIO();
        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_label myLabel = new ses_label();

	ses_error_flag return_value = new ses_error_flag();
	ses_error_flag skip_errors = new ses_error_flag();
	ses_error_flag didit_close = new ses_error_flag();

  	ses_number dim1 = new ses_number(0);

        try {

	    while (my_ses_io.SesHasNext(the_handle.the_value) == true) {

                myLabel.the_value = my_ses_io.SesGetLabel(the_handle.the_value);
		System.out.println("TEST:  myLabel is " + myLabel.the_value);
		if (myLabel.the_value.equals("p - pressure (GPa)")) {

		    System.out.println("TEST:  found pressure array");
		    skip_errors.the_value = my_ses_io.SesSkip(the_handle.the_value);
		}
                else {
		    System.out.println("TEST:  calling ses_skip");
                    skip_errors.the_value = my_ses_io.SesSkip(the_handle.the_value);
		}
	    }

            didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException: " + e.getMessage());
        }
        System.out.println("\n");
   }

    public static void test_ses_version() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame");
        ses_open_type open_flags = new ses_open_type('R');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        ses_error_flag setup_error = new ses_error_flag();
        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

 	ses_string return_value = new ses_string();
        try {

  	   System.out.println("TEST:  Calling ses_version with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesVersion(the_handle.the_value);
           System.out.println("TEST: The version is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }

	ses_error_flag didit_close = new ses_error_flag();

  	 System.out.println("TEST:  Calling ses_close with handle = " + the_handle.the_value);
         didit_close.the_value = my_ses_io.SesClose(the_handle.the_value);
         System.out.println("TEST: The return_value is " + didit_close.the_value);
         System.out.println("\n");





 
    }

    public static void test_ses_write_1d() {
        my_ses_io = new SesIO();


	ses_error_flag return_value = new ses_error_flag();
        ses_word_reference buffer = new ses_word_reference(10);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_string filename = new ses_string("sesame_write_test1");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_number dim = new ses_number(10);
      


        try {

	   buffer.the_value[0] = 3.25;
  	   System.out.println("TEST:  Calling ses_write_1D with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesWrite1D(the_handle.the_value,
							  buffer.the_value, (int)dim.the_value );
           System.out.println("TEST: The version is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_write_2d() {
        my_ses_io = new SesIO();

	ses_error_flag return_value = new ses_error_flag();
        ses_word_reference buffer = new ses_word_reference(10*10);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_string filename = new ses_string("sesame_write_test2");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_number dim1 = new ses_number(10);
        ses_number dim2 = new ses_number(10);
      


        try {

	   buffer.the_value[0] = 3.25;
  	   System.out.println("TEST:  Calling ses_write_2D with handle = " + the_handle.the_value);
           System.out.println("TEST:  Calling ses_write_2D with dim1 = " + dim1.the_value);
           System.out.println("TEST:  Calling ses_write_2D with dim2 = " + dim2.the_value);
           return_value.the_value = my_ses_io.SesWrite2D(the_handle.the_value,
                                                         buffer.the_value, (int)dim1.the_value, (int)dim2.the_value );
           System.out.println("TEST: The version is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");



    }

    public static void test_ses_write_3d() {
        my_ses_io = new SesIO();
	ses_error_flag return_value = new ses_error_flag();
        ses_word_reference buffer = new ses_word_reference(10*10*2);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_string filename = new ses_string("sesame_write_test3");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_number dim1 = new ses_number(10);
        ses_number dim2 = new ses_number(10);
        ses_number dim3 = new ses_number(2);
      


        try {

	   buffer.the_value[0] = 3.25;
  	   System.out.println("TEST:  Calling ses_write_3D with handle = " + the_handle.the_value);
           System.out.println("TEST:  Calling ses_write_3D with dim1 = " + dim1.the_value);
           System.out.println("TEST:  Calling ses_write_3D with dim2 = " + dim2.the_value);
           System.out.println("TEST:  Calling ses_write_3D with dim3 = " + dim3.the_value);
           return_value.the_value = my_ses_io.SesWrite3D(the_handle.the_value,
                                                         buffer.the_value, (int)dim1.the_value, (int)dim2.the_value, (int)dim3.the_value );
           System.out.println("TEST: The version is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_write_comments() {
        my_ses_io = new SesIO();
	ses_error_flag return_value = new ses_error_flag();
        ses_word_reference buffer = new ses_word_reference(10*10*2);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_string filename = new ses_string("sesame_write_comments");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_number dim1 = new ses_number(10);
        ses_number dim2 = new ses_number(10);
        ses_number dim3 = new ses_number(2);

        ses_string the_string = new ses_string("hello hello");
        ses_number dim = new ses_number(12);
      
         try {

  	   System.out.println("TEST:  Calling ses_write_comments with handle = " + the_handle.the_value);
           System.out.println("TEST:  Calling ses_write_comments with string = " + the_string.the_value);
           System.out.println("TEST:  Calling ses_write_comments with dim = " + dim.the_value);
           return_value.the_value = my_ses_io.SesWriteComments(the_handle.the_value,
                                                         the_string.the_value,
							 dim.the_value);
							
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");

    }

    public static void test_ses_write_next() {
        my_ses_io = new SesIO();
        ses_string filename = new ses_string("sesame_write_test_next");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

       ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

        ses_label myLabel = new ses_label();

	ses_error_flag return_value = new ses_error_flag();
	ses_error_flag skip_errors = new ses_error_flag();
	ses_error_flag didit_close = new ses_error_flag();

  	ses_number dim1 = new ses_number(100);

	ses_label the_label = new ses_label("allo");

        ses_word_reference the_buffer = new ses_word_reference(100);
	the_buffer.the_value[0] = 3.25;

        try {
  	   System.out.println("TEST:  Calling ses_write_next with handle = " + the_handle.the_value);
  	   System.out.println("TEST:  Calling ses_write_next with buffer[0] = " + the_buffer.the_value[0]);
  	   System.out.println("TEST:  Calling ses_write_next with dim1 = " + dim1.the_value);
  	   System.out.println("TEST:  Calling ses_write_next with label = " + the_label.the_value);
           return_value.the_value = my_ses_io.SesWriteNext(the_handle.the_value,
                                                           the_buffer.the_value,
							   (int)dim1.the_value,
                                                           the_label.the_value);
							
           System.out.println("TEST: The return_value is " + return_value.the_value);


        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException: " + e.getMessage());
        }
        System.out.println("\n");

    }

    public static void test_ses_write_number() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame_write_test_number");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

  	ses_number the_buffer = new ses_number(64);
        ses_error_flag return_value = new ses_error_flag();

        try {

  	   System.out.println("TEST:  Calling ses_write_number with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesWriteNumber(the_handle.the_value,
                                                             the_buffer.the_value);
							
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }

    public static void test_ses_write_pairs() {

        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame_write_test_pairs");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);


 	ses_error_flag return_value = new ses_error_flag();
        ses_word_reference  buffer1 = new ses_word_reference(10);
	ses_word_reference buffer2 = new ses_word_reference(10);
        ses_number dim = new ses_number(10);
        try {

  	   System.out.println("TEST:  Calling ses_write_pairs with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesWritePairs(the_handle.the_value,
                                                            buffer1.the_value,
							    buffer2.the_value,
							    dim.the_value);
							
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");

    }

    public static void test_ses_write_word() {


        my_ses_io = new SesIO();

        ses_string filename = new ses_string("sesame_write_test_word");
        ses_open_type open_flags = new ses_open_type('W');
	ses_file_handle the_handle = new ses_file_handle();

	the_handle.the_value = my_ses_io.SesOpen(filename.the_value, open_flags.the_value);
        int nr = 10;
        int nt = 10;
        int ntab = -1;

        ses_error_flag setup_error = new ses_error_flag();
        setup_error.the_value = my_ses_io.SesSetGrid(the_handle.the_value, nr, nt, ntab);

        ses_material_id the_mid = new ses_material_id(2030);
        ses_table_id the_tid = new ses_table_id(301);

        System.out.println("TEST:  calling ses setup with mid = " + the_mid.the_value);
        System.out.println("TEST:  calling ses setup with tid = " + the_tid.the_value);

	setup_error.the_value = my_ses_io.SesSetup(the_handle.the_value, the_mid.the_value, the_tid.the_value);

  	ses_word_reference the_buffer = new ses_word_reference(1);
        the_buffer.the_value[0] = 64.3;
        ses_error_flag return_value = new ses_error_flag();

        try {

  	   System.out.println("TEST:  Calling ses_write_word with handle = " + the_handle.the_value);
           return_value.the_value = my_ses_io.SesWriteWord(the_handle.the_value,
                                                             the_buffer.the_value[0]);
							
           System.out.println("TEST: The return_value is " + return_value.the_value);
        }
        catch (NullPointerException e) {
           System.out.println("NullPointerException" + e.getMessage());
        }
        System.out.println("\n");
    }
    

    
}

